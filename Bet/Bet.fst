module Bet
(*
This contract issues two tokens, for positive and negative outcomes.
The tokens are redeemable for 1ZP in the event that their corresponding outcome occurs.

This contract will require a z3rlimit of at least 3000000 in order to successfully record hints.
*)

open Zen.Base
open Zen.Cost
open Zen.Data
open Zen.Types

module Asset = Zen.Asset
module CR = Zen.ContractResult
module Dict  = Zen.Dictionary
module Hash = Zen.Hash
module RT = Zen.ResultT
module TX = Zen.TxSkeleton
module U64 = FStar.UInt64
module Wallet = Zen.Wallet

val oracleContractID: s:string {FStar.String.length s = 72}
let oracleContractID = "00000000ca055cc0af4d25ea1c8bbbf41444aadd68a168558397516b2f64727d87e72f97"

val ticker: s: string{FStar.String.length s <= 4}
let ticker = "AMD"

let strike = 65000UL // USD price multiplied by 1000

let time = 1539264654UL

type messageParams = {
    price: U64.t;
    returnAddress: lock
}

val getReturnAddress: option (Dict.t data) -> option lock `cost` 71
let getReturnAddress dict = // 5
    dict >!= Dict.tryFind "returnAddress" // 64
         >?= tryLock // 2

val getPrice: option (Dict.t data) -> option U64.t `cost` 71
let getPrice dict = // 5
    dict >!= Dict.tryFind "Price" // 64
         >?= tryU64 // 2

val getParams: option (Dict.t data) -> result messageParams `cost` 161
let getParams dict = // 19
    let! msgPrice = getPrice dict in // 71
    let! returnAddress = getReturnAddress dict in // 71
    match msgPrice, returnAddress with
    | Some msgPrice, Some returnAddress ->
        RT.ok ({ price=msgPrice; returnAddress=returnAddress })
    | None, _ ->
        RT.failw "Could not parse Price from messageBody"
    | _, None ->
        RT.failw "Could not parse returnAddress from messageBody"

val hashData: U64.t -> hash `cost` 812
let hashData price = let open Hash in // 36
    let! timeHash = updateU64 time empty // 48
                    >>= finalize in // 20
    let! tickerHash = begin updateString ticker empty
                      |> inc (6 * (4 - FStar.String.length ticker)) //24
                      >>= finalize //20
                      end <: hash `cost` 44 in //20
    let! priceHash = updateU64 price empty // 48
                     >>= finalize in // 20

    updateHash timeHash empty // 192
    >>= updateHash tickerHash // 192
    >>= updateHash priceHash // 192
    >>= finalize // 20

val buyTx: txSkeleton -> contractId -> lock -> U64.t -> CR.t `cost` 483
let buyTx tx contractID returnAddress amount = // 32
    let! bullToken = Asset.fromSubtypeString contractID "Bull" in // 64
    let! bearToken = Asset.fromSubtypeString contractID "Bear" in // 64
    // lock all available ZP to this contract
    TX.lockToContract Asset.zenAsset amount contractID tx // 64
    // mint an equivalent amount of bull and bear tokens
    >>= TX.mint amount bullToken // 64
    >>= TX.mint amount bearToken // 64
    // lock bull and bear tokens to the returnAddress
    >>= TX.lockToAddress bullToken amount returnAddress // 64
    >>= TX.lockToAddress bearToken amount returnAddress // 64
    >>= CR.ofTxSkel // 3

val buy: txSkeleton -> contractId -> messageBody: option data -> CR.t `cost` 718
let buy tx contractID messageBody = // 32
    let! returnAddress = messageBody >!= tryDict // 4
                                     >>= getReturnAddress in // 71
    let! oracleContractID = Zen.ContractId.parse oracleContractID in //64
    let! amount = TX.getAvailableTokens Asset.zenAsset tx in // 64
    match returnAddress, oracleContractID with
    | Some returnAddress, Some oracleContractID ->
        if amount <> 0UL then
            buyTx tx contractID returnAddress amount // 483
        else
            RT.incFailw 483 "Cannot buy with 0ZP in txSkeleton"
    | None, _ ->
        RT.incFailw 483 "Could not parse returnAddress from messageBody"
    | _, None ->
        RT.incFailw 483 "Could not parse oracleContractID. This contract will be unusable. Please redeploy this contract with a valid oracleContractID."

val oracleMessage: U64.t -> Dict.t data -> contractId -> message `cost` 892
let oracleMessage price dict oracleContractID = // 16
    let! hash = hashData price in // 812
    let! dict = Dict.add "Hash" (Hash hash) dict in // 64
    ret ({ recipient=oracleContractID;
           command="Verify";
           body=Some(Collection (Dict dict)) })

// invokes the oracle to validate inclusion of the message
val invokeOracle:
    U64.t
    -> option (Dict.t data)
    -> option txSkeleton
    -> CR.t `cost` 987
let invokeOracle price dict tx = // 31
    let! oracleContractID = Zen.ContractId.parse oracleContractID in // 64
    match dict, oracleContractID, tx with
    | Some dict, Some oracleContractID, Some tx ->
        let! msg = oracleMessage price dict oracleContractID in // 892
        RT.ok ({ tx=tx; message=Some msg; state=NoChange })
    | None, _, _ ->
        RT.incFailw 892 "Something went wrong! messageBody should not be empty"
    | _, None, _ ->
        RT.incFailw 892 "Something went wrong! could not parse oracleContractID"
    | _, _, None ->
        RT.incFailw 892 "Could not construct tx from wallet"

val redeemTx:
    asset
    -> txSkeleton
    -> contractId
    -> messageParams
    -> option (Dict.t data)
    -> w: wallet
    -> CR.t `cost` (Wallet.size w * 128 + 1395)
let redeemTx contractAsset tx contractID messageParams dict wallet = // 24
    // amount of the contract asset received
    let! amount = TX.getAvailableTokens contractAsset tx in // 64
    // destroy the contract asset received
    begin TX.destroy amount contractAsset tx // 64
    // send the same amount of ZP to the returnAddress
    >>= TX.lockToAddress Asset.zenAsset amount messageParams.returnAddress // 64
    >>= TX.fromWallet Asset.zenAsset amount contractID wallet // 128 * size wallet + 192
    <: (option txSkeleton `cost` (Wallet.size wallet * 128 + 320))
    end
    // check with the oracle
    >>= invokeOracle messageParams.price dict // 987

val redeem:
    txSkeleton
    -> contractId
    -> messageBody: option data
    -> wallet: wallet
    -> CR.t `cost` (Wallet.size wallet * 128 + 1652)
let redeem tx contractID messageBody wallet = let open U64 in // 28
    let! dict = messageBody >!= tryDict in // 4
    let! messageParams = getParams dict in // 161
    match messageParams with
    | OK messageParams -> begin
        let! winningAsset = begin //64
            if messageParams.price >=^ strike
            then Asset.fromSubtypeString contractID "Bull"
            else Asset.fromSubtypeString contractID "Bear"
            end in
        redeemTx winningAsset tx contractID messageParams dict wallet // Wallet.size w * 128 + 1395
        end <: CR.t `cost` (Wallet.size wallet * 128 + 1459)
    | ERR msg ->
        RT.incFailw (Wallet.size wallet * 128 + 1459) msg
    | _ ->
        RT.incFailw (Wallet.size wallet * 128 + 1459) "Something went wrong! unexpected error"

let main (tx: txSkeleton) _ (contractID: contractId) (command: string) _
         (messageBody: option data) (wallet: wallet) _
         : CR.t `cost` ( match command with
                         | "Buy" -> 725
                         | "Redeem" -> Wallet.size wallet * 128 + 1659
                         | _ -> 7 ) = // 7
    match command with
    | "Buy" ->
        buy tx contractID messageBody
        <: (CR.t `cost` ( match command with
                          | "Buy" -> 718
                          | "Redeem" -> Wallet.size wallet * 128 + 1652
                          | _ -> 0 ))
    | "Redeem" ->
        redeem tx contractID messageBody wallet
    | _ ->
        RT.failw "Invalid command"

let cf _ _ (command: string) _ _ (wallet: wallet) _ : nat `cost` 9 = // 9
    match command with
    | "Buy" -> ret 725
    | "Redeem" -> ret (Wallet.size wallet * 128 + 1659)
    | _ -> ret 7
