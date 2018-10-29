module Dex001

open Zen.Cost
open Zen.Data
open Zen.Types

module CR = Zen.ContractResult
module Dict = Zen.Dictionary
module Hash = Zen.Hash
module RT = Zen.ResultT
module TX = Zen.TxSkeleton
module U64 = FStar.UInt64
module W = Zen.Wallet

type order = {
    underlyingAsset: asset;
    underlyingAmount: U64.t; // amount of collateral for the order
    pairAsset: asset;
    orderTotal: U64.t; // the amount of the pair required to take the entire order
    makerPubKey: publicKey // the public key of the order maker
}

// A double uint64, needed for multiplying two arbitrary uint64s without overflow
type d64 = { hi:U64.t; lo:U64.t }

// multiply two uint64s without overflow
// algorithm adapted from 'The Art of Computer Programming' by Donald E. Knuth
val dmul64: U64.t -> U64.t -> d64 `cost` 39
let dmul64 x y = let open U64 in // 39
    let m32 = 4294967296UL in // 2^32
    let xhi = x %^ m32 in // 32 high bits of x
    let xlo = x /^ m32 in // 32 low bits of x
    let yhi = y %^ m32 in // 32 high bits of y
    let ylo = y /^ m32 in // 32 low bits of y

    let t0 = xhi *%^ yhi in
    let t1 = (xlo *%^ yhi) +%^ (t0 /^ m32) in
    let t2 = (xhi *%^ ylo) +%^ (t1 %^ m32) in

    let hi = (xlo *%^ ylo) +%^ (t1 %^ m32) in
    let lo = ((t2 *%^ m32) *%^ m32) +%^ (t0 %^ m32) in
    ret ({hi=hi; lo=lo})

val mkAsset: contractId -> hash -> asset `cost` 4
let mkAsset (version, contractHash) hash = // 4
    ret (version, contractHash, hash)

val hashAsset: asset -> hash `cost` 408
let hashAsset asset = // 4
    Hash.updateAsset asset Hash.empty // 384
    >>= Hash.finalize // 20

val hashU64: U64.t -> hash `cost` 72
let hashU64 x = // 4
    Hash.updateU64 x Hash.empty // 48
    >>= Hash.finalize // 20

val hashPubkey: publicKey -> hash `cost` 408
let hashPubkey pk = // 4
    Hash.updateByteArray pk Hash.empty // 384
    >>= Hash.finalize // 20

val hashOrder: order -> hash `cost` 2384
let hashOrder order = // 36
    let! underlyingAssetHash = hashAsset order.underlyingAsset in // 408
    let! underlyingAmountHash = hashU64 order.underlyingAmount in // 72
    let! pairAssetHash = hashAsset order.pairAsset in // 408
    let! orderTotalHash = hashU64 order.orderTotal in // 72
    let! makerPubKeyHash = hashPubkey order.makerPubKey in // 408
        Hash.updateHash underlyingAssetHash Hash.empty // 192
    >>= Hash.updateHash underlyingAmountHash // 192
    >>= Hash.updateHash pairAssetHash // 192
    >>= Hash.updateHash orderTotalHash // 192
    >>= Hash.updateHash makerPubKeyHash // 192
    >>= Hash.finalize // 20

val getUnderlyingAsset: option (Dict.t data) -> result asset `cost` 241
let getUnderlyingAsset dict = // 43
    let! version = dict >!= Dict.tryFind "UnderlyingAssetVersion" // 64
                        >?= tryU32 in // 2
    let! contractHash = dict >!= Dict.tryFind "UnderlyingContractHash" // 64
                             >?= tryHash in // 2
    let! subID = dict >!= Dict.tryFind "UnderlyingSubIdentifier" // 64
                      >?= tryHash in // 2
    match version, contractHash, subID with
    | Some version, Some contractHash, Some subID ->
        RT.ok (version, contractHash, subID)
    | None, _, _ ->
        RT.failw "Message Body must include valid UnderlyingAssetVersion"
    | _, None, _ ->
        RT.failw "Message Body must include valid UnderlyingContractHash"
    | _, _, None ->
        RT.failw "Message Body must include valid UnderlyingSubIdentifier"

val getUnderlyingAmount: option (Dict.t data) -> result U64.t `cost` 79
let getUnderlyingAmount dict = // 13
    let! underlyingAmount = dict >!= Dict.tryFind "UnderlyingAmount" // 64
                                 >?= tryU64 in // 2
    match underlyingAmount with
    | Some 0UL ->
        RT.failw "UnderlyingAmount cannot be 0"
    | Some underlyingAmount ->
        RT.ok underlyingAmount
    | None ->
        RT.failw "Message Body must include valid UnderlyingAmount"

val getPairAsset: option (Dict.t data) -> result asset `cost` 241
let getPairAsset dict = // 43
    let! version = dict >!= Dict.tryFind "PairAssetVersion" // 64
                        >?= tryU32 in // 2
    let! contractHash = dict >!= Dict.tryFind "PairContractHash" // 64
                             >?= tryHash in // 2
    let! subID = dict >!= Dict.tryFind "PairSubIdentifier" // 64
                      >?= tryHash in // 2
    match version, contractHash, subID with
    | Some version, Some contractHash, Some subID ->
        RT.ok (version, contractHash, subID)
    | None, _, _ ->
        RT.failw "Message Body must include valid PairAssetVersion"
    | _, None, _ ->
        RT.failw "Message Body must include valid PairContractHash"
    | _, _, None ->
        RT.failw "Message Body must include valid PairSubIdentifier"

val getOrderTotal: option (Dict.t data) -> result U64.t `cost` 79
let getOrderTotal dict = // 13
    let! orderTotal = dict >!= Dict.tryFind "OrderTotal" // 64
                           >?= tryU64 in // 2
    match orderTotal with
    | Some 0UL ->
        RT.failw "OrderTotal cannot be 0"
    | Some orderTotal ->
        RT.ok orderTotal
    | None ->
        RT.failw "Message Body must include valid OrderTotal"

val getMakerPK: option (Dict.t data) -> result publicKey `cost` 77
let getMakerPK dict = // 11
    let! makerPK = dict >!= Dict.tryFind "MakerPK" // 64
                        >?= tryPublicKey in // 2
    match makerPK with
    | Some makerPK ->
        RT.ok makerPK
    | None ->
        RT.failw "Message Body must include valid MakerPK"

val getReturnAddress: option (Dict.t data) -> result lock `cost` 77
let getReturnAddress dict = // 11
    let! returnAddress = dict >!= Dict.tryFind "returnAddress" // 64
                              >?= tryLock in // 2
    match returnAddress with
    | Some returnAddress ->
        RT.ok returnAddress
    | None ->
        RT.failw "Message Body must include valid returnAddress"

val getOrderAsset: contractId -> order -> asset `cost` 2393
let getOrderAsset contractID order = // 5
    let! orderHash = hashOrder order in // 2384
    mkAsset contractID orderHash // 4

val getRequestedPayout: option (Dict.t data) -> result U64.t `cost` 79
let getRequestedPayout dict = // 13
    let! requestedPayout = dict >!= Dict.tryFind "RequestedPayout" // 64
                                >?= tryU64 in // 2
    match requestedPayout with
    | Some 0UL ->
        RT.failw "RequestedPayout cannot be 0"
    | Some requestedPayout ->
        RT.ok requestedPayout
    | None ->
        RT.failw "Message Body must include valid RequestedPayout"

// mints an order asset and locks it to the contract
val createOrder: order -> contractId -> txSkeleton -> txSkeleton `cost` 2533
let createOrder order contractID tx = // 12
    let! orderAsset = getOrderAsset contractID order in // 2393
    TX.mint 1UL orderAsset tx // 64
    >>= TX.lockToContract orderAsset 1UL contractID // 64

// destroys an order if it exists in the wallet
val destroyOrder:
    order
    -> contractId
    -> w: wallet
    -> txSkeleton
    -> option txSkeleton `cost` (W.size w * 128 + 2662)
let destroyOrder order contractID w tx = // 13
    let! orderAsset = getOrderAsset contractID order in // 2393
    begin
    TX.destroy 1UL orderAsset tx // 64
    >>= TX.fromWallet orderAsset 1UL contractID w // W.size w * 128 + 192
    <: option txSkeleton `cost` (W.size w * 128 + 256)
    end

//////////////////
// Making an order
//////////////////
type makeParams = {
    makeUnderlyingAsset: asset;
    makePairAsset: asset;
    makeOrderTotal: U64.t
}

val mkMakeParams: asset -> asset -> U64.t -> result makeParams `cost` 4
let mkMakeParams underlyingAsset pairAsset orderTotal = // 4
    RT.ok ( { makeUnderlyingAsset=underlyingAsset;
              makePairAsset=pairAsset;
              makeOrderTotal=orderTotal } )

val parseMake: option data -> result makeParams `cost` 580
let parseMake messageBody = // 11
    let! dict = messageBody >!= tryDict in // 4
    let underlyingAsset = getUnderlyingAsset dict in // 241
    let pairAsset = getPairAsset dict in // 241
    let orderTotal = getOrderTotal dict in // 79
    RT.bind3 underlyingAsset pairAsset orderTotal mkMakeParams // 4

val makeOrder: makeParams -> publicKey -> U64.t -> order `cost` 9
let makeOrder makeParams senderPK underlyingAmount = // 9
    ret ( { underlyingAsset=makeParams.makeUnderlyingAsset;
            underlyingAmount=underlyingAmount;
            pairAsset=makeParams.makePairAsset;
            orderTotal=makeParams.makeOrderTotal;
            makerPubKey=senderPK } )

val makeTx: txSkeleton -> contractId -> publicKey -> makeParams -> CR.t `cost` 2699
let makeTx tx contractID senderPK makeParams = // 26
    let! underlyingAmount = TX.getAvailableTokens makeParams.makeUnderlyingAsset tx in // 64
    if underlyingAmount <> 0UL then begin
        let! order = makeOrder makeParams senderPK underlyingAmount in // 9
        // issue a token with the hash of the order as the subidentifier, and lock it to the contract
        createOrder order contractID tx // 2533
        // lock the underlying to the contract
        >>= TX.lockToContract order.underlyingAsset underlyingAmount contractID // 64
        >>= CR.ofTxSkel // 3
        end
    else RT.autoFailw "UnderlyingAmount cannot be 0"

val make: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 3288
let make tx contractID sender messageBody = // 9
    match sender with
    | PK senderPK ->
        let makeParams = parseMake messageBody in // 580
        makeParams `RT.bind` makeTx tx contractID senderPK // 2699
    | _ ->
        RT.autoFailw "Sender must authenticate with public key"

//////////////////
// Cancel an order
//////////////////

val getCancelOrder: publicKey -> option data -> result order `cost` 705
let getCancelOrder makerPK messageBody = // 61
    let! dict = messageBody >!= tryDict in // 4
    let! underlyingAsset = getUnderlyingAsset dict in // 241
    let! underlyingAmount = getUnderlyingAmount dict in // 79
    let! pairAsset = getPairAsset dict in // 241
    let! orderTotal = getOrderTotal dict in // 79
    match underlyingAsset, underlyingAmount, pairAsset, orderTotal with
    | OK underlyingAsset, OK underlyingAmount, OK pairAsset, OK orderTotal ->
        RT.ok ( { underlyingAsset=underlyingAsset;
                  underlyingAmount=underlyingAmount;
                  pairAsset=pairAsset;
                  orderTotal=orderTotal;
                  makerPubKey=makerPK } )
    | ERR msg, _, _, _
    | OK _, ERR msg, _, _
    | OK _, OK _, ERR msg, _
    | OK _, OK _, OK _, ERR msg -> RT.failw msg
    | _ -> RT.failw "Something went wrong! Please file a bug report"

val cancelTx:
    txSkeleton
    -> contractId
    -> publicKey
    -> w: wallet
    -> order
    -> CR.t `cost` (W.size w * 128 + 2672)
let cancelTx tx contractID senderPK w order = // 7
    // destroy the order
    destroyOrder order contractID w tx // W.size w * 128 + 2662
    >>= CR.ofOptionTxSkel "Could not find order in wallet" // 3
    <: CR.t `cost` (W.size w * 128 + 2665)

val cancel:
    txSkeleton
    -> contractId
    -> sender
    -> option data
    -> w: wallet
    -> CR.t `cost` (W.size w * 128 + 3388)
let cancel tx contractID sender messageBody w = // 11
    match sender with
    | PK senderPK ->
        begin let order = getCancelOrder senderPK messageBody in // 705
        order `RT.bind` cancelTx tx contractID senderPK w // W.size w * 128 + 2672
        end <: CR.t `cost` (W.size w * 128 + 3377)
    | _ ->
        RT.autoFailw "Sender must authenticate with public key"

//////////////////
// Taking an order
//////////////////

// check that the requestedPayout is ok
val checkRequestedPayout:
    order
    -> requestedPayout: U64.t
    -> paymentAmount: U64.t
    -> bool `cost` 153
let checkRequestedPayout { underlyingAmount=ua; orderTotal=ot} rp pa = // 75
    // we want to check that
    // requestedPayout = floor (underlyingAmount * (paymentAmount / orderTotal))
    // which is equivalent to
    // underlyingAmount * paymentAmount
    // < requestedPayout * orderTotal + orderTotal
    // <= underlyingAmount * paymentAmount + orderTotal

    let open U64 in
    // 2^64 - 1
    let max64 = 18446744073709551615UL in

    // compute underlyingAmount * paymentAmount
    let! ua_pa = dmul64 ua pa in // 39
    // compute requestedPayout * orderTotal
    let! rp_ot = dmul64 rp ot in // 39
    // compute requestedPayout * orderTotal + orderTotal
    let rp_ot_ot = { hi = if rp_ot.lo >=^ max64 -%^ ot // will adding low 64 bits overflow
                          then rp_ot.hi +%^ 1UL // this never overflows
                          else rp_ot.hi;
                     lo = rp_ot.lo +%^ ot } in
    // compute underlyingAmount * paymentAmount + orderTotal
    let ua_pa_ot = { hi = if ua_pa.lo >=^ max64 -%^ ot // will adding low 64 bits overflow
                          then ua_pa.hi +%^ 1UL // this never overflows
                          else ua_pa.hi;
                     lo = ua_pa.lo +%^ ot } in

    // underlyingAmount * paymentAmount < requestedPayout * orderTotal + orderTotal
    let ua_pa_lt_rp_ot_ot = (ua_pa.hi <^ rp_ot.hi)
                         || (ua_pa.hi = rp_ot.hi && ua_pa.lo <^ rp_ot.lo) in
    // requestedPayout * orderTotal + orderTotal <= underlyingAmount * paymentAmount + orderTotal
    let rp_ot_ot_lte_ua_pa_ot = (rp_ot_ot.hi <^ ua_pa_ot.hi)
                             || (rp_ot_ot.hi = ua_pa_ot.hi && rp_ot_ot.lo <=^ ua_pa_ot.lo) in
    // both conditions must hold
    ret (ua_pa_lt_rp_ot_ot && rp_ot_ot_lte_ua_pa_ot)

// parse the messageBody to get the order being taken
val parseTake: option (Dict.t data) -> result order `cost` 794
let parseTake dict = // 77
    let! underlyingAsset = getUnderlyingAsset dict in // 241
    let! underlyingAmount = getUnderlyingAmount dict in // 79
    let! pairAsset = getPairAsset dict in // 241
    let! orderTotal = getOrderTotal dict in // 79
    let! makerPK = getMakerPK dict in // 77
    match underlyingAsset, underlyingAmount, pairAsset, orderTotal, makerPK with
    | OK underlyingAsset, OK underlyingAmount, OK pairAsset, OK orderTotal, OK makerPK ->
         RT.ok ({ underlyingAsset=underlyingAsset;
                  underlyingAmount=underlyingAmount;
                  pairAsset=pairAsset;
                  orderTotal=orderTotal;
                  makerPubKey=makerPK })
    | ERR msg, _, _, _, _
    | OK _, ERR msg, _, _, _
    | OK _, OK _, ERR msg, _, _
    | OK _, OK _, OK _, ERR msg, _
    | OK _, OK _, OK _, OK _, ERR msg -> RT.failw msg
    | _ -> RT.failw "Something went wrong! Please file a bug report"

// updates an order in the case of a partial fill
val updateOrder:
    contractId
    -> order
    -> U64.t
    -> U64.t
    -> txSkeleton
    -> txSkeleton `cost` 2550
let updateOrder contractID order paymentAmount payoutAmount tx = let open U64 in // 17
    if paymentAmount <^ order.orderTotal // partial fill, so need to update the order
    then // create the new order
        let newOrder = { order with
                         underlyingAmount=order.underlyingAmount-%^payoutAmount;
                         orderTotal=order.orderTotal-%^paymentAmount } in
        createOrder newOrder contractID tx // 2533
    else incRet 2533 tx

// destroys the order and adds the necessary inputs from the wallet
val takeAddWalletInputs:
    order
    -> U64.t
    -> contractId
    -> w: wallet
    -> txSkeleton
    -> option txSkeleton `cost` (W.size w * 256 + 2865)
let takeAddWalletInputs order payoutAmount contractID w tx = // 11
    // add payout inputs from wallet
    TX.fromWallet order.underlyingAsset payoutAmount contractID w tx // W.size w * 128 + 192
    // destroy the order
    >?= destroyOrder order contractID w // W.size w * 128 + 2662
    <: option txSkeleton `cost` (W.size w * 256 + 2854)

val takeTx:
    txSkeleton
    -> contractId
    -> w: wallet
    -> U64.t
    -> U64.t
    -> order
    -> lock
    -> CR.t `cost` (W.size w * 256 + 5984)
let takeTx tx contractID w paymentAmount payoutAmount order returnAddress = // 30
    let! makerPubKeyHash = hashPubkey order.makerPubKey in // 408
    // lock the underlying to the returnAddress
    TX.lockToAddress order.underlyingAsset payoutAmount returnAddress tx // 64
    // lock the paymentAmount to the maker
    >>= TX.lockToPubKey order.pairAsset paymentAmount makerPubKeyHash // 64
    //  create a new order if partial fill
    >>= updateOrder contractID order paymentAmount payoutAmount // 2550
    // add inputs from wallet, destroying the order
    >>= takeAddWalletInputs order payoutAmount contractID w // W.size w * 256 + 2865
    >>= CR.ofOptionTxSkel "Could not find order in wallet. Ensure that both the order and the correct amount of the underlying are present." // 3

val take':
    txSkeleton
    -> contractId
    -> w: wallet
    -> order
    -> U64.t
    -> lock
    -> CR.t `cost` (W.size w * 256 + 6221)
let take' tx contractID w order requestedPayout returnAddress = // 20
    //begin
    let! paymentAmount = TX.getAvailableTokens order.pairAsset tx in // 64
    begin
    let! paymentAmountOK = checkRequestedPayout order requestedPayout paymentAmount in // 153
    if paymentAmountOK
    then takeTx tx contractID w paymentAmount requestedPayout order returnAddress // W.size w * 256 + 5984
    else RT.incFailw (W.size w * 256 + 5984) "Incorrect requestedPayout"
    end <: CR.t `cost` (W.size w * 256 + 6137)
    //end <: CR.t `cost` (W.size w * 256 + 6201)

val take:
    txSkeleton
    -> contractId
    -> option data
    -> w: wallet
    -> CR.t `cost` (W.size w * 256 + 7189)
let take tx contractID messageBody w = // 14
    let! dict = messageBody >!= tryDict in // 4
    let order = parseTake dict in // 794
    //begin
    let requestedPayout = getRequestedPayout dict in // 79
    //begin
    let returnAddress = getReturnAddress dict in // 77
    RT.bind3 order requestedPayout returnAddress (take' tx contractID w)
    <: CR.t `cost` (W.size w * 256 + 7171)

//////////
// exports
//////////

val main:
    txSkeleton
    -> context
    -> contractId
    -> command: string
    -> sender
    -> option data
    -> w: wallet
    -> option data
    -> CR.t `cost` ( 9 + begin match command with
                         | "Make" -> 3288
                         | "Cancel" -> W.size w * 128 + 3388
                         | "Take" -> W.size w * 256 + 7189
                         | _ -> 0 end )
let main tx _ contractID command sender messageBody w _ = // 9
    begin
    match command with
    | "Make" ->
        make tx contractID sender messageBody // 3288
        <: CR.t `cost` begin match command with
                       | "Make" -> 3288
                       | "Cancel" -> W.size w * 128 + 3388
                       | "Take" -> W.size w * 256 + 7189
                       | _ -> 0 end
    | "Cancel" ->
        cancel tx contractID sender messageBody w // W.size w * 128 + 3388
    | "Take" ->
        take tx contractID messageBody w // W.size w * 256 + 7178
    | _ ->
        RT.failw "Unrecognised command"
    end <: CR.t `cost` begin match command with
                       | "Make" -> 3288
                       | "Cancel" -> W.size w * 128 + 3388
                       | "Take" -> W.size w * 256 + 7189
                       | _ -> 0 end

val cf:
    txSkeleton
    -> context
    -> string
    -> sender
    -> option data
    -> wallet
    -> option data
    -> nat `cost` 12
let cf _ _ command _ _ w _ = // 12
    ret ( 9 + begin match command with
              | "Make" -> 3288
              | "Cancel" -> W.size w * 128 + 3388
              | "Take" -> W.size w * 256 + 7189
              | _ -> 0 end )
