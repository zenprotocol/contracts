module Bet
(*
This contract issues two tokens, for positive and negative outcomes.
The tokens are redeemable for 1ZP in the event that their corresponding outcome occurs.
*)

open Zen.Base
open Zen.Cost
open Zen.Data
open Zen.Types

module Asset = Zen.Asset
module CID = Zen.ContractId
module CR = Zen.ContractResult
module D  = Zen.Dictionary
module Hash = Zen.Hash
module OT = Zen.OptionT
module RT = Zen.ResultT
module String = FStar.String
module TX = Zen.TxSkeleton
module U64 = FStar.UInt64
module Wallet = Zen.Wallet

let oracleContractID = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
let ticker = "AMD"
let strike = 65000UL // USD price multiplied by 1000
let unixtime = 0UL

// gets a return Address from the message body
val getReturnAddress: option data -> option lock `cost` 70
let getReturnAddress messageBody = //7
    messageBody >!= tryDict //4
                >?= D.tryFind "returnAddress" //64
                >?= tryLock //2

val hashParams:
    time:U64.t
    -> ticker:string{String.length ticker <= 4}
    -> price:U64.t
    -> hash `cost` 776
let hashParams time ticker price = let open Hash in
    let! timeHash = updateU64 time empty //48
                    >>= finalize in //20
    let! tickerHash = begin updateString ticker empty
                      |> inc (6 * (4 - String.length ticker)) //24
                      >>= finalize //20
                      end <: hash `cost` 44 in //20
    let! priceHash = updateU64 price empty //48
                     >>= finalize in //20

    updateHash timeHash empty //192
    >>= updateHash tickerHash //192
    >>= updateHash priceHash //192
    >>= finalize //20


val checkBullCondition: messageBody: option data -> bool `cost` 1044
let checkBullCondition (messageBody: option data) = let open U64 in
    let! dict = messageBody >!= tryDict in //4
    let! msgTime = dict >!= D.tryFind "Time" //64
                        >?= tryU64 in //2
    let! msgTicker = dict >!= D.tryFind "Ticker" //64
                          >?= tryString in //2
    let! msgPrice = dict >!= D.tryFind "Price" //64
                         >?= tryU64 in //2
    let! msgHash = dict >!= D.tryFind "Hash" //64
                        >?= tryHash in //2
    match msgTime, msgTicker, msgPrice, msgHash with
    | Some msgTime, Some msgTicker, Some msgPrice, Some msgHash ->
        if msgTicker = ticker
        && msgTime = unixtime
        && msgPrice >=^ strike
        then begin
            let! hash = hashParams msgTime msgTicker msgPrice in //776
            ret (hash = msgHash)
            end
        else autoRet false
    | _ -> autoRet false


val checkBearCondition: messageBody: option data -> bool `cost` 1044
let checkBearCondition (messageBody: option data) = let open U64 in
    let! dict = messageBody >!= tryDict in //4
    let! msgTime = dict >!= D.tryFind "Time" //64
                        >?= tryU64 in //2
    let! msgticker = dict >!= D.tryFind "ticker" //64
                              >?= tryString in //2
    let! msgPrice = dict >!= D.tryFind "Price" //64
                         >?= tryU64 in //2
    let! msgHash = dict >!= D.tryFind "Hash" //64
                        >?= tryHash in //2
    match msgTime, msgticker, msgPrice, msgHash with
    | Some msgTime, Some msgticker, Some msgPrice, Some msgHash ->
        if msgticker = ticker
        && msgTime = unixtime
        && msgPrice <^ strike
        then begin
            let! hash = hashParams msgTime msgticker msgPrice in //776
            ret (hash = msgHash)
            end
        else autoRet false
    | _ -> autoRet false

val buy: txSkeleton -> contractId -> messageBody: option data -> CR.t `cost` 585
let buy txSkeleton contractId messageBody =
    let! returnAddress = messageBody >!= tryDict //4
                                     >?= D.tryFind "returnAddress" //64
                                     >?= tryLock in //2
    match returnAddress with
    | Some returnAddress ->
        let! bullToken = Asset.fromSubtypeString contractId "Bull" in //64
        let! bearToken = Asset.fromSubtypeString contractId "Bear" in //64
        // the amount of ZP available
        let! amount = TX.getAvailableTokens Asset.zenAsset txSkeleton in //64
        // lock all available ZP to this contract
        TX.lockToContract Asset.zenAsset amount contractId txSkeleton //64
        // mint an equivalent amount of bull and bear tokens
        >>= TX.mint amount bullToken //64
        >>= TX.mint amount bearToken //64
        // lock bull and bear tokens to the returnAddress
        >>= TX.lockToAddress bullToken amount returnAddress //64
        >>= TX.lockToAddress bearToken amount returnAddress //64
        >>= CR.ofTxSkel //3
    | None -> RT.autoFailw "Could not parse returnAddress from messageBody"

//#set-options "--z3rlimit 8000000"

val redeemBull:
    txSkeleton
    -> contractId
    -> oracleContractID: contractId
    -> returnAddress: lock
    -> messageBody: option data
    -> wallet: wallet
    -> CR.t `cost` (Wallet.size wallet * 128 + 1498)
let redeemBull txSkeleton contractId oracleContractID returnAddress messageBody wallet =
    // message to invoke the oracle contract with
    let message = { recipient=oracleContractID;
                    command="Verify";
                    body=messageBody } in
    // check that the bull condition occured
    if! checkBullCondition messageBody then begin //1044
    let! bullToken = Asset.fromSubtypeString contractId "Bull" in //64
    // amount of bull tokens received
    let! amount = TX.getAvailableTokens bullToken txSkeleton in //64
    // destroy the bull tokens received
    TX.destroy amount bullToken txSkeleton //64
    // send the same amount of ZP to the returnAddress
    >>= TX.lockToAddress Asset.zenAsset amount returnAddress //64
    >>= TX.fromWallet Asset.zenAsset amount contractId wallet // 128 * size wallet + 192
    >>= CR.ofOptionTxSkel "Cound not construct tx from wallet" //3
    >>= CR.setMessage message //3
    end <: (CR.t `cost` (Wallet.size wallet * 128 + 454))
    else RT.incFailw (Wallet.size wallet * 128 + 454) "Invalid message body"

val redeemBear:
    txSkeleton
    -> contractId
    -> oracleContractID: contractId
    -> returnAddress: lock
    -> messageBody: option data
    -> wallet: wallet
    -> CR.t `cost` (Wallet.size wallet * 128 + 1498)
let redeemBear txSkeleton contractId oracleContractID returnAddress messageBody wallet =
    // message to invoke the oracle contract with
    let message = { recipient=oracleContractID;
                    command="Verify";
                    body=messageBody } in
    // check that the bear condition occured
    if! checkBearCondition messageBody then begin //1044
    let! bearToken = Asset.fromSubtypeString contractId "Bear" in //64
    // amount of bear tokens received
    let! amount = TX.getAvailableTokens bearToken txSkeleton in //64
    // destroy the bear tokens received
    TX.destroy amount bearToken txSkeleton //64
    // send the same amount of ZP to the returnAddress
    >>= TX.lockToAddress Asset.zenAsset amount returnAddress //64
    >>= TX.fromWallet Asset.zenAsset amount contractId wallet // 128 * size wallet + 192
    >>= CR.ofOptionTxSkel "Cound not construct tx from wallet" //3
    >>= CR.setMessage message //3
    end <: (CR.t `cost` (Wallet.size wallet * 128 + 454))
    else RT.incFailw (Wallet.size wallet * 128 + 454) "Invalid message body"

let main (txSkeleton: txSkeleton) _ (contractId: contractId) (command: string)
         _ (messageBody: option data) (wallet: wallet) _
         : CR.t `cost` ( match command with
                         | "Buy" -> 585
                         | "RedeemBear"
                         | "RedeemBull" -> Wallet.size wallet * 128 + 1632
                         | _ -> 0 ) =
    match command with
    | "Buy" -> buy txSkeleton contractId messageBody
               <: (CR.t `cost` ( match command with
                                 | "Buy" -> 585
                                 | "RedeemBear"
                                 | "RedeemBull" -> Wallet.size wallet * 128 + 1632
                                 | _ -> 0 ))
    | "RedeemBear" ->
        let! returnAddress = messageBody >!= tryDict //4
                                         >?= D.tryFind "returnAddress" //64
                                         >?= tryLock in //2
        let! oracleContractID = CID.parse oracleContractID in //64
        begin match returnAddress, oracleContractID with
        | Some returnAddress, Some oracleContractID ->
            redeemBear txSkeleton contractId oracleContractID returnAddress messageBody wallet
        | _ -> RT.autoFailw "Something went wrong! could not parse returnAddress/oracleContractID"
        end
    | "RedeemBull" ->
        let! returnAddress = messageBody >!= tryDict //4
                                         >?= D.tryFind "returnAddress" //64
                                         >?= tryLock in //2
        let! oracleContractID = CID.parse oracleContractID in //64
        begin match returnAddress, oracleContractID with
        | Some returnAddress, Some oracleContractID ->
            redeemBull txSkeleton contractId oracleContractID returnAddress messageBody wallet
        | _ -> RT.autoFailw "Something went wrong! could not parse returnAddress/oracleContractID"
        end
    | _ -> RT.failw "Invalid command specified"
