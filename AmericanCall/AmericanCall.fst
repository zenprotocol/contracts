module AmericanCall

open Zen.Base
open Zen.Cost
open Zen.Data
open Zen.Types

module Arr = Zen.Array
module Asset = Zen.Asset
module CR = Zen.ContractResult
module Dict = Zen.Dictionary
module Hash = Zen.Hash
module OT = Zen.OptionT
module RT = Zen.ResultT
module TX = Zen.TxSkeleton
module U32 = FStar.UInt32
module U64 = FStar.UInt64
module W = Zen.Wallet

// compressed public key
type cpk = byte ** hash

// positive UInt64
type p64 = x:U64.t{x <> 0UL}

// the contracts arguments
type args =
    { baseAsset: asset;
      pairAsset: asset;
      strike: p64;
      payout: p64;
      expiry: p64;
      issuer: option cpk;
      options: option p64; }

// the contract's assets
type assets =
    { optionAsset: asset;
      outstandingAsset: asset; }

// compress a public key
val compress: publicKey -> cpk `cost` 305
let compress pk = let open FStar.UInt8 in // 13
    let parity = (Arr.item 32 pk %^ 2uy) +^ 2uy in
    let aux (i:nat{i < 32}): byte `cost` 5 = ret (Arr.item (31-i) pk) in // 5
    let! x = Arr.init_pure 32 aux in // 292
    ret (parity, x)

val getAsset: option (Dict.t data) -> string -> option asset `cost` 137
let getAsset dict fieldName = // 7
    dict >!= Dict.tryFind fieldName // 64
         >?= tryString // 2
         >?= Zen.Asset.parse // 64

val getU64: option (Dict.t data) -> string -> option p64 `cost` 79
let getU64 dict fieldName = // 13
    let! x = dict >!= Dict.tryFind fieldName // 64
                  >?= tryU64 in // 2
    match x with
    | None | Some 0UL -> OT.none
    | Some x -> OT.some (x <: p64)

val getIssuer: option (Dict.t data) -> option cpk `cost` 380
let getIssuer dict = // 9
    dict >!= Dict.tryFind "Issuer" // 64
         >?= tryPublicKey // 2
         >?= (compress >> OT.liftCost) // 305

let parseArgs (dict:option (Dict.t data)): result args `cost` 1045 = // 75
    let! baseAsset = getAsset dict "Base" in // 137
    let! pairAsset = getAsset dict "Pair" in // 137
    let! strike = getU64 dict "Strike" in // 79
    let! payout = getU64 dict "Payout" in // 79
    let! expiry = getU64 dict "Expiry" in // 79
    let! issuer = getIssuer dict in  // 380
    let! options = getU64 dict "Options" in  // 79
    match baseAsset, pairAsset, strike, payout, expiry with
    | Some baseAsset, Some pairAsset, Some strike, Some payout, Some expiry ->
        RT.ok ({ baseAsset=baseAsset; pairAsset=pairAsset;
                 strike=strike; payout=payout; expiry=expiry;
                 issuer=issuer; options=options })
    | None, _, _, _, _ -> RT.autoFailw "Could not parse Base"
    | _, None, _, _, _ -> RT.autoFailw "Could not parse Pair"
    | _, _, None, _, _ -> RT.autoFailw "Could not parse Strike, or Strike was 0"
    | _, _, _, None, _ -> RT.autoFailw "Could not parse Payout, or Payout was 0"
    | _, _, _, _, None -> RT.autoFailw "Could not parse Expiry, or Expiry was 0"

// update a hasher with a compressed public key
val updateCPK: cpk -> Zen.Hash.Sha3.t -> Zen.Hash.Sha3.t `cost` 203
let updateCPK (parity, x) sha3 = // 5
    Hash.updateByte parity sha3 // 6
    >>= Hash.updateHash x // 192

let hashArgs (args:args{Some? args.issuer}): hash `cost` 1162 = // 27
    Hash.updateAsset args.baseAsset Hash.empty // 384
    >>= Hash.updateAsset args.pairAsset // 384
    >>= Hash.updateU64 args.strike // 48
    >>= Hash.updateU64 args.payout // 48
    >>= Hash.updateU64 args.expiry // 48
    >>= updateCPK (Some?.v args.issuer) // 203
    >>= Hash.finalize // 20

// returns the option asset and the outstanding asset
val getAssets: contractId -> args:args{Some? args.issuer} -> assets `cost` 1392
let getAssets (version, contractHash) args = // 18
    let! hashArgs = hashArgs args in // 1162
    let! hash2Args = Hash.updateHash hashArgs Hash.empty // 192
                     >>= Hash.finalize in // 20
    ret ({ optionAsset      = version, contractHash, hashArgs;
           outstandingAsset = version, contractHash, hash2Args })

// gets a lock from a cpk
let cpkLock (cpk:cpk): lock `cost` 231 = // 8
    let! cpkHash = updateCPK cpk Hash.empty // 203
                   >>= Hash.finalize in //20
    ret (PKLock cpkHash)

// add 2 spends to a tx from the wallet
val spend2FromWallet: contractId -> w:wallet -> spend -> spend -> txSkeleton
                      -> CR.t `cost` (W.size w * 256 + 405)
let spend2FromWallet contractID wallet s1 s2 tx = // 18
    TX.fromWallet s1.asset s1.amount contractID wallet tx // W.size wallet * 128 + 192
    `OT.bind` TX.fromWallet s2.asset s2.amount contractID wallet // W.size wallet * 128 + 192
    >>= CR.ofOptionTxSkel "Could not generate tx from wallet" // 3

//
// issuing
//

val issue: txSkeleton -> contractId -> cpk -> args -> CR.t `cost` 2070
let issue tx contractID issuerCPK args = let open U64 in // 60
    // get the amount of collateral available in the tx
    let! collateral = TX.getAvailableTokens args.pairAsset tx in // 64
    // get the issuer's address from their pubkey
    let! issuerAddress = cpkLock issuerCPK in // 231
    if collateral <> 0UL && collateral %^ args.payout = 0UL then
        // compute the number of options to issue
        let options: p64 = collateral /^ args.payout in
        // get the assetIDs for the option
        let! assets = getAssets contractID ({args with issuer=Some issuerCPK; // 1392
                                                       options=Some options }) in
        // lock collateral to the contract
        TX.lockToContract args.pairAsset collateral contractID tx // 64
        // mint callAsset and outstandingAsset
        >>= TX.mint options assets.optionAsset // 64
        >>= TX.mint options assets.outstandingAsset // 64
        // lock callAsset to issuer
        >>= TX.lockToAddress assets.optionAsset options issuerAddress // 64
        // lock outstandingAsset to the contract
        >>= TX.lockToContract assets.outstandingAsset options contractID // 64
        >>= CR.ofTxSkel // 3
    else RT.autoFailw "Collateral must be a nonzero multiple of Payout"

//
// exercising
//

val exercise: txSkeleton -> context -> contractId -> cpk -> w:wallet -> args
              -> CR.t `cost` (W.size w * 256 + 2742)
let exercise tx ({timestamp=ts}) contractID senderCPK wallet args = // 99
    match args.issuer with
    | Some _ -> begin
        let! (senderAddress, issuerAddress, assets, options, baseAmount) = begin // 1982
            // get the sender and issuer's addresses from their pubkeys
            let! senderAddress = cpkLock senderCPK in // 231
            let! issuerAddress = cpkLock (Some?.v args.issuer) in // 231
            // get the assetIDs for the option
            let! assets = getAssets contractID args in // 1392
            // get the number of options available in the tx
            let! options = TX.getAvailableTokens assets.optionAsset tx in // 64
            // get the amount of the base asset available in the tx
            let! baseAmount = TX.getAvailableTokens args.baseAsset tx in // 64
            ret (senderAddress, issuerAddress, assets, options, baseAmount)
            end <: (lock ** lock ** assets ** U64.t ** U64.t) `cost` 1982 in
        let open U64 in
        match options <> 0UL, baseAmount /^ args.strike = options, ts <^ args.expiry with
        | true, true, true -> begin
            let totalPrice = args.strike *%^ options in
            let totalPayout = args.payout *%^ options in
            let tx: txSkeleton `cost` 256 =
                // destroy the callAssets and outstandingAssets
                TX.destroy options assets.optionAsset tx // 64
                >>= TX.destroy options assets.outstandingAsset // 64
                // lock the payment to the issuer
                >>= TX.lockToAddress args.baseAsset totalPrice issuerAddress // 64
                // lock the payout to the sender
                >>= TX.lockToAddress args.pairAsset totalPayout senderAddress in // 64
            // add spends from contract wallet
            tx >>= spend2FromWallet contractID wallet // W.size wallet * 256 + 405
                        ({ asset=args.pairAsset;          amount=totalPayout })
                        ({ asset=assets.outstandingAsset; amount=options     })
            end <: CR.t `cost` (W.size wallet * 256 + 661)
        | false, _, _ -> RT.autoFailw "Must send nonzero number of calls"
        | _, false, _ -> RT.autoFailw "PaymentAmount must be equal to calls * Strike"
        | _, _, false -> RT.autoFailw "Cannot exercise after expiry"
        end <: CR.t `cost` (W.size wallet * 256 + 2643)
    | None ->
        RT.incFailw (W.size wallet * 256 + 2643) "Could not parse Issuer"


//
// expiring
//

val expireTX: txSkeleton -> contractId
              -> (args:args{Some? args.issuer/\ Some? args.options}) -> w:wallet
              -> CR.t `cost` (W.size w * 256 + 2197)
let expireTX tx contractID args wallet = // 41
    let options = Some?.v args.options in
    let totalPayout = U64.checked_mul options args.payout in
    match totalPayout with
    | Some totalPayout -> begin
        let! issuerAddress = cpkLock (Some?.v args.issuer) in // 231
        let! assets = getAssets contractID args in // 1392
        // destroy the outstandingAssets
        TX.destroy options assets.outstandingAsset tx // 64
        // lock the pair assets to the issuer address
        >>= TX.lockToAddress args.pairAsset totalPayout issuerAddress // 64
        // add outstandingAsset and pairAsset inputs from wallet
        >>= spend2FromWallet contractID wallet // W.size wallet * 256 + 405
                ({ asset=args.pairAsset;          amount=totalPayout })
                ({ asset=assets.outstandingAsset; amount=options })
        end <: CR.t `cost` (W.size wallet * 256 + 2156)
    | None ->
        RT.autoFailw "Overflow in calculating total payout. Try again with lower amount"

val expire: txSkeleton -> context -> contractId -> cpk -> w:wallet -> args
            -> CR.t `cost` (W.size w * 256 + 2220)
let expire tx ({timestamp=ts}) contractID senderCPK wallet args = // 23
    let issuer = match args.issuer with
                 | Some issuer -> issuer
                 | None -> senderCPK in
    match U64.(ts >^ args.expiry), args.options with
    | true, Some _ -> // W.size wallet * 256 + 2197
        expireTX tx contractID ({args with issuer=Some issuer}) wallet
    | false, _ ->
        RT.autoFailw "Cannot close before expiry"
    | _, None ->
        RT.autoFailw "Could not parse Options"

//
// exports
//

type mainCR (command:string) (wallet:wallet) (n:nat) = CR.t `cost` begin
    n + ( match command with
          | "Issue" -> 2070
          | "Exercise" -> W.size wallet * 256 + 2742
          | "Expire" -> W.size wallet * 256 + 2220
          | _ -> 0 ) end

val main: txSkeleton -> context -> contractId -> command:string -> sender
          -> option data -> w:wallet -> option data
          -> mainCR command w 1377
let main tx context contractID command sender msg wallet _ = // 13
    let aux (args:args) (senderCPK:cpk): mainCR command wallet 10 = // 10
        match command with
        | "Issue" ->
            issue tx contractID senderCPK args // 2070
            <: mainCR command wallet 0
        | "Exercise" -> // W.size wallet * 256 + 2742
            exercise tx context contractID senderCPK wallet args
        | "Expire" -> // W.size wallet * 256 + 2220
            expire tx context contractID senderCPK wallet args
        | _ -> // 0
            RT.failw "Invalid Command" in
    let args: result args `cost` 1049 =
        Zen.Data.( msg >!= tryDict) // 4
        >>= parseArgs in // 1045
    let senderCPK: result cpk `cost` 305 =
        match sender with
        | PK senderPK -> // 305
            compress senderPK >>= RT.ok
        | _ ->
            RT.autoFailw "Sender must be PK" in
    RT.bind2 args senderCPK aux // 1049 + 305 + 10 + match command with ...

val cf: txSkeleton -> context -> command:string -> sender -> option data
        -> w:wallet -> option data
        -> nat `cost` 12
let cf _ _ command _ _ wallet _ = // 12
    ret ( 1377 + begin match command with
                 | "Issue" -> 2070
                 | "Exercise" -> W.size wallet * 256 + 2742
                 | "Expire" -> W.size wallet * 256 + 2220
                 | _ -> 0 end )
