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
val compress: publicKey -> cpk `cost` 132
let compress pk = let open FStar.UInt8 in // 13
    let parity = (Arr.item 32 pk %^ 2uy) +^ 2uy in
    let aux (i:nat{i < 32}): byte `cost` 0 = ret (Arr.item (31-i) pk) in // 5
    let! x = Arr.init_pure 32 aux in // 132 // 292
    ret (parity, x)

val getAsset: option (Dict.t data) -> string -> option asset `cost` 130
let getAsset dict fieldName = // 7
    dict >!= Dict.tryFind fieldName // 64
         >?= tryString // 2
         >?= Zen.Asset.parse // 64

val getU64: option (Dict.t data) -> string -> option p64 `cost` 66
let getU64 dict fieldName = // 14
    let! x = dict >!= Dict.tryFind fieldName // 64
                  >?= tryU64 in // 2
    match x with
    | None | Some 0UL -> OT.none
    | Some x -> OT.some (x <: p64)

val getIssuer: option (Dict.t data) -> option cpk `cost` 198
let getIssuer dict = // 5
    dict >!= Dict.tryFind "Issuer" // 64
         >?= tryPublicKey // 2
         >?= (compress >> OT.liftCost) // 132

let parseArgs (dict:option (Dict.t data)): result args `cost` 722 =
    let! baseAsset = getAsset dict "Base" in // 130
    let! pairAsset = getAsset dict "Pair" in // 130
    let! strike = getU64 dict "Strike" in // 66
    let! payout = getU64 dict "Strike" in // 66
    let! expiry = getU64 dict "Expiry" in // 66
    let! issuer = getIssuer dict in  // 198
    let! options = getU64 dict "Options" in  // 66
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
val updateCPK: cpk -> Zen.Hash.Sha3.t -> Zen.Hash.Sha3.t `cost` 198
let updateCPK (parity, x) sha3 =
    Hash.updateByte parity sha3 // 6
    >>= Hash.updateHash x // 192

let hashArgs (args:args{Some? args.issuer}): hash `cost` 1130 =
    Hash.updateAsset args.baseAsset Hash.empty // 384
    >>= Hash.updateAsset args.pairAsset // 384
    >>= Hash.updateU64 args.strike // 48
    >>= Hash.updateU64 args.payout // 48
    >>= Hash.updateU64 args.expiry // 48
    >>= updateCPK (Some?.v args.issuer) // 198
    >>= Hash.finalize // 20

// returns the option asset and the outstanding asset
let getAssets ((version, contractHash):contractId)
              (args:args{Some? args.issuer})
              : assets `cost` 1342 =
    let! hashArgs = hashArgs args in // 1130
    let! hash2Args = Hash.updateHash hashArgs Hash.empty // 192
                     >>= Hash.finalize in // 20
    ret ({ optionAsset      = version, contractHash, hashArgs;
           outstandingAsset = version, contractHash, hash2Args })

// gets a lock from a cpk
let cpkLock (cpk:cpk): lock `cost` 218 =
    let! cpkHash = updateCPK cpk Hash.empty // 198
                   >>= Hash.finalize in //20
    ret (PKLock cpkHash)

// add 2 spends to a tx from the wallet
let spend2FromWallet (contractID:contractId)
                     (wallet:wallet)
                     (s1:spend)
                     (s2:spend)
                     (tx:txSkeleton)
                     : CR.t `cost` (W.size wallet * 256 + 387) =
    TX.fromWallet s1.asset s1.amount contractID wallet tx // W.size wallet * 128 + 192
    `OT.bind` TX.fromWallet s2.asset s2.amount contractID wallet // W.size wallet * 128 + 192
    >>= CR.ofOptionTxSkel "Could not generate tx from wallet" // 3

//
// issuing
//

let issue (tx:txSkeleton) (contractID:contractId) (issuerCPK:cpk) (args:args)
          : CR.t `cost` 1947 = let open U64 in
    // get the amount of collateral available in the tx
    let! collateral = TX.getAvailableTokens args.pairAsset tx in // 64
    // get the issuer's address from their pubkey
    let! issuerAddress = cpkLock issuerCPK in // 218
    if collateral <> 0UL && collateral %^ args.payout = 0UL then
        // compute the number of options to issue
        let options: p64 = collateral /^ args.payout in
        // get the assetIDs for the option
        let! assets = getAssets contractID ({args with issuer=Some issuerCPK; // 1342
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

let exercise (tx:txSkeleton) ({timestamp=ts}:context) (contractID:contractId)
             (senderCPK:cpk) (wallet:wallet) (args:args)
             : CR.t `cost` (W.size wallet * 256 + 2549) = let open U64 in
    if Some? args.issuer then begin
        let! (senderAddress, issuerAddress, assets, options, baseAmount) = begin // 1906
            // get the sender and issuer's addresses from their pubkeys
            let! senderAddress = cpkLock senderCPK in // 218
            let! issuerAddress = cpkLock (Some?.v args.issuer) in // 218
            // get the assetIDs for the option
            let! assets = getAssets contractID args in // 1342
            // get the number of options available in the tx
            let! options = TX.getAvailableTokens assets.optionAsset tx in // 64
            // get the amount of the base asset available in the tx
            let! baseAmount = TX.getAvailableTokens args.baseAsset tx in // 64
            ret (senderAddress, issuerAddress, assets, options, baseAmount)
            end <: (lock ** lock ** assets ** U64.t ** U64.t) `cost` 1906 in
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
            tx >>= spend2FromWallet contractID wallet // W.size wallet * 256 + 387
                        ({ asset=args.pairAsset;          amount=totalPayout })
                        ({ asset=assets.outstandingAsset; amount=options     })
            end <: CR.t `cost` (W.size wallet * 256 + 643)
        | false, _, _ -> RT.autoFailw "Must send nonzero number of calls"
        | _, false, _ -> RT.autoFailw "PaymentAmount must be equal to calls * Strike"
        | _, _, false -> RT.autoFailw "Cannot exercise after expiry"
        end <: CR.t `cost` (W.size wallet * 256 + 2549)
    else RT.incFailw (W.size wallet * 256 + 2549) "Could not parse Issuer"


//
// expiring
//

let expireTX (tx:txSkeleton) (contractID:contractId)
            (args:args{Some? args.issuer/\ Some? args.options})
            (wallet:wallet): CR.t `cost` (W.size wallet * 256 + 2075) =
    let options = Some?.v args.options in
    let totalPayout = U64.checked_mul options args.payout in
    match totalPayout with
    | Some totalPayout ->
        let! issuerAddress = cpkLock (Some?.v args.issuer) in // 218
        let! assets = getAssets contractID args in // 1342
        // destroy the outstandingAssets
        TX.destroy options assets.outstandingAsset tx // 64
        // lock the pair assets to the issuer address
        >>= TX.lockToAddress args.pairAsset totalPayout issuerAddress // 64
        // add outstandingAsset and pairAsset inputs from wallet
        >>= spend2FromWallet contractID wallet // W.size wallet * 256 + 387
                ({ asset=args.pairAsset;          amount=totalPayout })
                ({ asset=assets.outstandingAsset; amount=options })
    | None ->
        RT.autoFailw "Overflow in calculating total payout. Try again with lower amount"

val expire: txSkeleton -> context -> contractId -> cpk -> wallet:wallet -> args
            -> CR.t `cost` (W.size wallet * 256 + 2075)
let expire tx ({timestamp=timestamp}) contractID senderCPK wallet args =
    let issuer = match args.issuer with
                 | Some issuer -> issuer
                 | None -> senderCPK in
    match U64.(timestamp >^ args.expiry), Some? args.options with
    | true, true ->
        expireTX tx contractID ({ args with issuer = Some issuer }) wallet
    | false, _ ->
        RT.autoFailw "Cannot close before expiry"
    | _, false ->
        RT.autoFailw "Could not parse Options"

//
// exports
//

type mainCR (command:string) (wallet:wallet) (n:nat) = CR.t `cost` begin
    n + ( match command with
          | "Issue" -> 1947
          | "Exercise" -> W.size wallet * 256 + 2549
          | "Expire" -> W.size wallet * 256 + 2075
          | _ -> 0 ) end

val main: txSkeleton -> context -> contractId -> command:string
          -> sender -> option data -> wallet:wallet -> option data
          -> mainCR command wallet 858
let main tx context contractID command sender msg wallet _ =
    let aux (args:args) (senderCPK:cpk): mainCR command wallet 0 =
        match command with
        | "Issue" ->
            issue tx contractID senderCPK args // 1947
            <: mainCR command wallet 0
        | "Exercise" ->
            exercise tx context contractID senderCPK wallet args // W.size wallet * 256 + 2549
        | "Expire" ->
            expire tx context contractID senderCPK wallet args // W.size wallet * 256 + 2075
        | _ ->
            RT.failw "Invalid Command" in
    let args: result args `cost` 726 =
        Zen.Data.( msg >!= tryDict) // 4
        >>= parseArgs in // 722
    let senderCPK: result cpk `cost` 132 =
        match sender with
        | PK senderPK ->
            compress senderPK >>= RT.ok // 132
        | _ ->
            RT.autoFailw "Sender must be PK" in
    RT.bind2 args senderCPK aux

let cf _ _ _ command _ _ wallet _ =
    ret (858 + ( match command with
                 | "Issue" -> 2805
                 | "Exercise" -> W.size wallet * 256 + 3407
                 | "Expire" -> W.size wallet * 256 + 2933
                 | _ -> 0 ))
