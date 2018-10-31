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
    makerPubKeyHash: hash // the public key of the order maker
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

val hashOrder: order -> hash `cost` 1973
let hashOrder order = // 33
    let! underlyingAssetHash = hashAsset order.underlyingAsset in // 408
    let! underlyingAmountHash = hashU64 order.underlyingAmount in // 72
    let! pairAssetHash = hashAsset order.pairAsset in // 408
    let! orderTotalHash = hashU64 order.orderTotal in // 72
        Hash.updateHash underlyingAssetHash Hash.empty // 192
    >>= Hash.updateHash underlyingAmountHash // 192
    >>= Hash.updateHash pairAssetHash // 192
    >>= Hash.updateHash orderTotalHash // 192
    >>= Hash.updateHash order.makerPubKeyHash // 192
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

val getUnderlyingAmount: option (Dict.t data) -> result U64.t `cost` 82
let getUnderlyingAmount dict = // 16
    let! underlyingAmount = dict >!= Dict.tryFind "UnderlyingAmount" // 64
                                 >?= tryU64 in // 2
    match underlyingAmount with
    | Some underlyingAmount ->
        if underlyingAmount <> 0UL
        then RT.ok underlyingAmount
        else RT.failw "UnderlyingAmount cannot be 0"
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

val getOrderTotal: option (Dict.t data) -> result U64.t `cost` 82
let getOrderTotal dict = // 16
    let! orderTotal = dict >!= Dict.tryFind "OrderTotal" // 64
                           >?= tryU64 in // 2
    match orderTotal with
    | Some orderTotal ->
        if  orderTotal <> 0UL
        then RT.ok orderTotal
        else RT.failw "OrderTotal cannot be 0"
    | None ->
        RT.failw "Message Body must include valid OrderTotal"

val getMakerPKHash: option (Dict.t data) -> result hash `cost` 77
let getMakerPKHash dict = // 11
    let! makerPKHash = dict >!= Dict.tryFind "MakerPKHash" // 64
                            >?= tryHash in // 2
    match makerPKHash with
    | Some makerPKHash ->
        RT.ok makerPKHash
    | None ->
        RT.failw "Message Body must include valid MakerPKHash"

val getReturnAddress: option (Dict.t data) -> result lock `cost` 77
let getReturnAddress dict = // 11
    let! returnAddress = dict >!= Dict.tryFind "returnAddress" // 64
                              >?= tryLock in // 2
    match returnAddress with
    | Some returnAddress ->
        RT.ok returnAddress
    | None ->
        RT.failw "Message Body must include valid returnAddress"

val getOrderAsset: contractId -> order -> asset `cost` 1982
let getOrderAsset contractID order = // 5
    let! orderHash = hashOrder order in // 1973
    mkAsset contractID orderHash // 4

val getRequestedPayout: option (Dict.t data) -> result U64.t `cost` 82
let getRequestedPayout dict = // 16
    let! requestedPayout = dict >!= Dict.tryFind "RequestedPayout" // 64
                                >?= tryU64 in // 2
    match requestedPayout with
    | Some requestedPayout ->
        if requestedPayout <> 0UL
        then RT.ok requestedPayout
        else RT.failw "RequestedPayout cannot be 0"
    | None ->
        RT.failw "Message Body must include valid RequestedPayout"

// mints an order asset and locks it to the contract, as well as the underlying
val createOrder: order -> contractId -> txSkeleton -> txSkeleton `cost` 2193
let createOrder order contractID tx = // 19
    let! orderAsset = getOrderAsset contractID order in // 1982
    TX.mint 1UL orderAsset tx // 64
    >>= TX.lockToContract orderAsset 1UL contractID // 64
    >>= TX.lockToContract order.underlyingAsset order.underlyingAmount contractID // 64

// destroys an order if it exists in the wallet,
// and adds the underlying to the inputs.
val destroyOrder:
    order
    -> contractId
    -> w: wallet
    -> txSkeleton
    -> CR.t `cost` (W.size w * 256 + 2457)
let destroyOrder order contractID w tx = // 24
    let! orderAsset = getOrderAsset contractID order in // 1982
    begin
    // destroy the order
    TX.destroy 1UL orderAsset tx // 64
    >>= TX.fromWallet orderAsset 1UL contractID w // W.size w * 128 + 192
    // add the underlying to the inputs
    >?= TX.fromWallet order.underlyingAsset order.underlyingAmount contractID w // W.size w * 128 + 192
    >>= CR.ofOptionTxSkel "Could not find order in wallet. Ensure that both the order and the correct amount of the underlying are present." // 3
    <: CR.t `cost` (W.size w * 256 + 451)
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

val parseMake: option data -> result makeParams `cost` 583
let parseMake messageBody = // 11
    let! dict = messageBody >!= tryDict in // 4
    let underlyingAsset = getUnderlyingAsset dict in // 241
    let pairAsset = getPairAsset dict in // 241
    let orderTotal = getOrderTotal dict in // 82
    RT.bind3 underlyingAsset pairAsset orderTotal mkMakeParams // 4

val makeOrder: makeParams -> publicKey -> U64.t -> order `cost` 420
let makeOrder makeParams senderPK underlyingAmount = // 12
    let! senderPKHash = hashPubkey senderPK in // 408
    ret ( { underlyingAsset=makeParams.makeUnderlyingAsset;
            underlyingAmount=underlyingAmount;
            pairAsset=makeParams.makePairAsset;
            orderTotal=makeParams.makeOrderTotal;
            makerPubKeyHash=senderPKHash } )

val makeTx: txSkeleton -> contractId -> publicKey -> makeParams -> CR.t `cost` 2700
let makeTx tx contractID senderPK makeParams = // 20
    let! underlyingAmount = TX.getAvailableTokens makeParams.makeUnderlyingAsset tx in // 64
    if underlyingAmount <> 0UL then begin
        let! order = makeOrder makeParams senderPK underlyingAmount in // 420
        // issue a token with the hash of the order as the subidentifier,
        // and lock it to the contract, with the underlying
        createOrder order contractID tx // 2193
        >>= CR.ofTxSkel // 3
        end
    else RT.autoFailw "UnderlyingAmount cannot be 0"

val make: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 3292
let make tx contractID sender messageBody = // 9
    match sender with
    | PK senderPK ->
        let makeParams = parseMake messageBody in // 583
        makeParams `RT.bind` makeTx tx contractID senderPK // 2700
    | _ ->
        RT.autoFailw "Sender must authenticate with public key"

//////////////////
// Cancel an order
//////////////////

val getCancelOrder: publicKey -> option data -> result order `cost` 1122
let getCancelOrder makerPK messageBody = // 64
    let! dict = messageBody >!= tryDict in // 4
    let! underlyingAsset = getUnderlyingAsset dict in // 241
    let! underlyingAmount = getUnderlyingAmount dict in // 82
    let! pairAsset = getPairAsset dict in // 241
    let! orderTotal = getOrderTotal dict in // 82
    let! makerPKHash = hashPubkey makerPK in // 408
    match underlyingAsset, underlyingAmount, pairAsset, orderTotal with
    | OK underlyingAsset, OK underlyingAmount, OK pairAsset, OK orderTotal ->
        RT.ok ( { underlyingAsset=underlyingAsset;
                  underlyingAmount=underlyingAmount;
                  pairAsset=pairAsset;
                  orderTotal=orderTotal;
                  makerPubKeyHash=makerPKHash } )
    | ERR msg, _, _, _
    | OK _, ERR msg, _, _
    | OK _, OK _, ERR msg, _
    | OK _, OK _, OK _, ERR msg -> RT.failw msg
    | _ -> RT.failw "Something went wrong! Please file a bug report"

val cancelTx:
    txSkeleton
    -> contractId
    -> w: wallet
    -> order
    -> CR.t `cost` (W.size w * 256 + 2533)
let cancelTx tx contractID w order = // 12
    // lock the underlying to the maker's pk
    TX.lockToPubKey order.underlyingAsset order.underlyingAmount order.makerPubKeyHash tx // 64
    // destroy the order
    >>= destroyOrder order contractID w // W.size w * 256 + 2457
    <: CR.t `cost` (W.size w * 256 + 2521)

val cancel:
    txSkeleton
    -> contractId
    -> sender
    -> option data
    -> w: wallet
    -> CR.t `cost` (W.size w * 256 + 3665)
let cancel tx contractID sender messageBody w = // 10
    match sender with
    | PK senderPK ->
        begin let order = getCancelOrder senderPK messageBody in // 1122
        order `RT.bind` cancelTx tx contractID w // W.size w * 256 + 2533
        end <: CR.t `cost` (W.size w * 256 + 3655)
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
    -> bool `cost` 155
let checkRequestedPayout { underlyingAmount=ua; orderTotal=ot} rp pa = // 77
    // we want to check that
    // requestedPayout = floor (underlyingAmount * (paymentAmount / orderTotal))
    // which is equivalent to
    // underlyingAmount * paymentAmount
    // < requestedPayout * orderTotal + orderTotal
    // <= underlyingAmount * paymentAmount + orderTotal

    let open U64 in
    // maximum 64 bit unsigned integer
    let max64 = 0UL -%^ 1UL in

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
val parseTake: option (Dict.t data) -> result order `cost` 800
let parseTake dict = // 77
    let! underlyingAsset = getUnderlyingAsset dict in // 241
    let! underlyingAmount = getUnderlyingAmount dict in // 82
    let! pairAsset = getPairAsset dict in // 241
    let! orderTotal = getOrderTotal dict in // 82
    let! makerPKHash = getMakerPKHash dict in // 77
    match underlyingAsset, underlyingAmount, pairAsset, orderTotal, makerPKHash with
    | OK underlyingAsset, OK underlyingAmount, OK pairAsset, OK orderTotal, OK makerPKHash ->
         RT.ok ({ underlyingAsset=underlyingAsset;
                  underlyingAmount=underlyingAmount;
                  pairAsset=pairAsset;
                  orderTotal=orderTotal;
                  makerPubKeyHash=makerPKHash })
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
    -> txSkeleton `cost` 2210
let updateOrder contractID order paymentAmount payoutAmount tx = let open U64 in // 17
    if paymentAmount <^ order.orderTotal // partial fill, so need to update the order
    then // create the new order
        let newOrder = { order with
                         underlyingAmount=order.underlyingAmount-%^payoutAmount;
                         orderTotal=order.orderTotal-%^paymentAmount } in
        createOrder newOrder contractID tx // 2193
    else incRet 2193 tx

val takeTx:
    txSkeleton
    -> contractId
    -> w: wallet
    -> U64.t
    -> U64.t
    -> order
    -> lock
    -> CR.t `cost` (W.size w * 256 + 4818)
let takeTx tx contractID w paymentAmount payoutAmount order returnAddress = // 23
    //  lock the payout to the taker
    TX.lockToAddress order.underlyingAsset payoutAmount returnAddress tx // 64
    // lock the paymentAmount to the maker
    >>= TX.lockToPubKey order.pairAsset paymentAmount order.makerPubKeyHash // 64
    //  create a new order if partial fill, locking the remainder of the underlying to the contract
    >>= updateOrder contractID order paymentAmount payoutAmount // 2210
    // add inputs from wallet, destroying the order
    >>= destroyOrder order contractID w // W.size w * 256 + 2457

val take':
    txSkeleton
    -> contractId
    -> w: wallet
    -> order
    -> U64.t
    -> lock
    -> CR.t `cost` (W.size w * 256 + 5057)
let take' tx contractID w order requestedPayout returnAddress = // 20
    //begin
    let! paymentAmount = TX.getAvailableTokens order.pairAsset tx in // 64
    begin
    let! paymentAmountOK = checkRequestedPayout order requestedPayout paymentAmount in // 155
    if paymentAmountOK
    then takeTx tx contractID w paymentAmount requestedPayout order returnAddress // W.size w * 256 + 4818
    else RT.incFailw (W.size w * 256 + 4818) "Incorrect requestedPayout"
    end <: CR.t `cost` (W.size w * 256 + 4973)
    //end <: CR.t `cost` (W.size w * 256 + 6201)

val take:
    txSkeleton
    -> contractId
    -> option data
    -> w: wallet
    -> CR.t `cost` (W.size w * 256 + 6034)
let take tx contractID messageBody w = // 14
    let! dict = messageBody >!= tryDict in // 4
    let order = parseTake dict in // 800
    //begin
    let requestedPayout = getRequestedPayout dict in // 82
    //begin
    let returnAddress = getReturnAddress dict in // 77
    RT.bind3 order requestedPayout returnAddress (take' tx contractID w) // 5057
    <: CR.t `cost` (W.size w * 256 + 6016)

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
                         | "Make" -> 3292
                         | "Cancel" -> W.size w * 256 + 3665
                         | "Take" -> W.size w * 256 + 6034
                         | _ -> 0 end )
let main tx _ contractID command sender messageBody w _ = // 9
    begin
    match command with
    | "Make" ->
        make tx contractID sender messageBody // 3292
        <: CR.t `cost` begin match command with
                       | "Make" -> 3292
                       | "Cancel" -> W.size w * 256 + 3665
                       | "Take" -> W.size w * 256 + 6034
                       | _ -> 0 end
    | "Cancel" ->
        cancel tx contractID sender messageBody w // W.size w * 256 + 3665
    | "Take" ->
        take tx contractID messageBody w // W.size w * 256 + 6034
    | _ ->
        RT.failw "Unrecognised command"
    end <: CR.t `cost` begin match command with
                       | "Make" -> 3292
                       | "Cancel" -> W.size w * 256 + 3665
                       | "Take" -> W.size w * 256 + 6034
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
              | "Make" -> 3292
              | "Cancel" -> W.size w * 256 + 3665
              | "Take" -> W.size w * 256 + 6034
              | _ -> 0 end )
