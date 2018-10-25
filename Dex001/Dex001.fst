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
val dmul64: U64.t -> U64.t -> d64 `cost` 0
let dmul64 x y = let open U64 in
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

val mkAsset: contractId -> hash -> asset `cost` 0
let mkAsset (version, contractHash) hash =
    ret (version, contractHash, hash)

val hashAsset: asset -> hash `cost` 404
let hashAsset asset =
    Hash.updateAsset asset Hash.empty // 384
    >>= Hash.finalize // 20

val hashU64: U64.t -> hash `cost` 68
let hashU64 x =
    Hash.updateU64 x Hash.empty // 48
    >>= Hash.finalize // 20

val hashPubkey: publicKey -> hash `cost` 404
let hashPubkey pk =
    Hash.updateByteArray pk Hash.empty // 384
    >>= Hash.finalize // 20

val hashOrder: order -> hash `cost` 2328
let hashOrder order =
    let! underlyingAssetHash = hashAsset order.underlyingAsset in // 404
    let! underlyingAmountHash = hashU64 order.underlyingAmount in // 68
    let! pairAssetHash = hashAsset order.pairAsset in // 404
    let! orderTotalHash = hashU64 order.orderTotal in // 68
    let! makerPubKeyHash = hashPubkey order.makerPubKey in // 404
        Hash.updateHash underlyingAssetHash Hash.empty // 192
    >>= Hash.updateHash underlyingAmountHash // 192
    >>= Hash.updateHash pairAssetHash // 192
    >>= Hash.updateHash orderTotalHash // 192
    >>= Hash.updateHash makerPubKeyHash // 192
    >>= Hash.finalize // 20

val getUnderlyingAsset: option (Dict.t data) -> result asset `cost` 198
let getUnderlyingAsset dict =
    let! version = dict >!= Dict.tryFind "UnderlyingAssetVersion" // 64
                        >?= tryU32 in //2
    let! contractHash = dict >!= Dict.tryFind "UnderlyingContractHash" // 64
                             >?= tryHash in //2
    let! subID = dict >!= Dict.tryFind "UnderlyingSubIdentifier" // 64
                      >?= tryHash in //2
    match version, contractHash, subID with
    | Some version, Some contractHash, Some subID ->
        RT.ok (version, contractHash, subID)
    | None, _, _ ->
        RT.failw "Message Body must include valid UnderlyingAssetVersion"
    | _, None, _ ->
        RT.failw "Message Body must include valid UnderlyingContractHash"
    | _, _, None ->
        RT.failw "Message Body must include valid UnderlyingSubIdentifier"

val getUnderlyingAmount: option (Dict.t data) -> result U64.t `cost` 66
let getUnderlyingAmount dict =
    let! underlyingAmount = dict >!= Dict.tryFind "UnderlyingAmount" // 64
                                 >?= tryU64 in //2
    match underlyingAmount with
    | Some 0UL ->
        RT.failw "UnderlyingAmount cannot be 0"
    | Some underlyingAmount ->
        RT.ok underlyingAmount
    | None ->
        RT.failw "Message Body must include valid UnderlyingAmount"

val getPairAsset: option (Dict.t data) -> result asset `cost` 198
let getPairAsset dict =
    let! version = dict >!= Dict.tryFind "PairAssetVersion" // 64
                        >?= tryU32 in //2
    let! contractHash = dict >!= Dict.tryFind "PairContractHash" // 64
                             >?= tryHash in //2
    let! subID = dict >!= Dict.tryFind "PairSubIdentifier" // 64
                      >?= tryHash in //2
    match version, contractHash, subID with
    | Some version, Some contractHash, Some subID ->
        RT.ok (version, contractHash, subID)
    | None, _, _ ->
        RT.failw "Message Body must include valid PairAssetVersion"
    | _, None, _ ->
        RT.failw "Message Body must include valid PairContractHash"
    | _, _, None ->
        RT.failw "Message Body must include valid PairSubIdentifier"

val getOrderTotal: option (Dict.t data) -> result U64.t `cost` 66
let getOrderTotal dict =
    let! orderTotal = dict >!= Dict.tryFind "OrderTotal" // 64
                           >?= tryU64 in //2
    match orderTotal with
    | Some 0UL ->
        RT.failw "OrderTotal cannot be 0"
    | Some orderTotal ->
        RT.ok orderTotal
    | None ->
        RT.failw "Message Body must include valid OrderTotal"

val getMakerPK: option (Dict.t data) -> result publicKey `cost` 66
let getMakerPK dict =
    let! makerPK = dict >!= Dict.tryFind "MakerPK" // 64
                        >?= tryPublicKey in //2
    match makerPK with
    | Some makerPK ->
        RT.ok makerPK
    | None ->
        RT.failw "Message Body must include valid MakerPK"

val getReturnAddress: option (Dict.t data) -> result lock `cost` 66
let getReturnAddress dict =
    let! returnAddress = dict >!= Dict.tryFind "returnAddress" // 64
                              >?= tryLock in //2
    match returnAddress with
    | Some returnAddress ->
        RT.ok returnAddress
    | None ->
        RT.failw "Message Body must include valid returnAddress"

val getOrderAsset: contractId -> order -> asset `cost` 2328
let getOrderAsset contractID order =
    let! orderHash = hashOrder order in // 2328
    mkAsset contractID orderHash

val getRequestedPayout: option (Dict.t data) -> result U64.t `cost` 66
let getRequestedPayout dict =
    let! requestedPayout = dict >!= Dict.tryFind "RequestedPayout" // 64
                                >?= tryU64 in //2
    match requestedPayout with
    | Some 0UL ->
        RT.failw "RequestedPayout cannot be 0"
    | Some requestedPayout ->
        RT.ok requestedPayout
    | None ->
        RT.failw "Message Body must include valid RequestedPayout"

//////////////////
// Making an order
//////////////////
type makeParams = {
    makeUnderlyingAsset: asset;
    makePairAsset: asset;
    makeOrderTotal: U64.t
}

val mkMakeParams: asset -> asset -> U64.t -> result makeParams `cost` 0
let mkMakeParams underlyingAsset pairAsset orderTotal =
    RT.ok ( { makeUnderlyingAsset=underlyingAsset;
              makePairAsset=pairAsset;
              makeOrderTotal=orderTotal } )

val parseMake: option data -> result makeParams `cost` 466
let parseMake messageBody =
    let! dict = messageBody >!= tryDict in // 4
    let underlyingAsset = getUnderlyingAsset dict in // 198
    let pairAsset = getPairAsset dict in // 198
    let orderTotal = getOrderTotal dict in // 66
    RT.bind3 underlyingAsset pairAsset orderTotal mkMakeParams

val makeOrder: makeParams -> publicKey -> U64.t -> order `cost` 0
let makeOrder makeParams senderPK underlyingAmount =
    ret ( { underlyingAsset=makeParams.makeUnderlyingAsset;
            underlyingAmount=underlyingAmount;
            pairAsset=makeParams.makePairAsset;
            orderTotal=makeParams.makeOrderTotal;
            makerPubKey=senderPK } )

val makeTx: txSkeleton -> contractId -> publicKey -> makeParams -> CR.t `cost` 2587
let makeTx txSkeleton contractID senderPK makeParams =
    let! underlyingAmount =
        TX.getAvailableTokens makeParams.makeUnderlyingAsset txSkeleton in // 64
    if underlyingAmount <> 0UL then begin // 2563
        let! order = makeOrder makeParams senderPK underlyingAmount in
        // issue a token with the hash of the order as the subidentifier, and lock it to the contract
        let! orderAsset = getOrderAsset contractID order in // 2328
        TX.mint 1UL orderAsset txSkeleton //64
        >>= TX.lockToContract orderAsset 1UL contractID //64
        // lock the underlying to the contract
        >>= TX.lockToContract order.underlyingAsset underlyingAmount contractID //64
        >>= CR.ofTxSkel // 3
        end
    else RT.autoFailw "UnderlyingAmount cannot be 0"

val make: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 3053
let make txSkeleton contractID sender messageBody =
    match sender with
    | PK senderPK ->
        let makeParams = parseMake messageBody in // 466
        makeParams `RT.bind` makeTx txSkeleton contractID senderPK // 2587
    | _ ->
        RT.autoFailw "Sender must authenticate with public key"

//////////////////
// Cancel an order
//////////////////

val getCancelOrder: publicKey -> option data -> result order `cost` 532
let getCancelOrder makerPK messageBody =
    let! dict = messageBody >!= tryDict in // 4
    let! underlyingAsset = getUnderlyingAsset dict in // 198
    let! underlyingAmount = getUnderlyingAmount dict in // 66
    let! pairAsset = getPairAsset dict in // 198
    let! orderTotal = getOrderTotal dict in // 66
    match underlyingAsset, underlyingAmount, pairAsset, orderTotal with
    | OK underlyingAsset, OK underlyingAmount, OK pairAsset, OK orderTotal ->
        RT.ok ( { underlyingAsset=underlyingAsset;
                  underlyingAmount=underlyingAmount;
                  pairAsset=pairAsset;
                  orderTotal=orderTotal;
                  makerPubKey=makerPK } )
    | _ ->
        RT.failw "Bad messageBody"

val cancelTx:
    txSkeleton
    -> contractId
    -> publicKey
    -> w: wallet
    -> order
    -> CR.t `cost` (W.size w * 128 + 2587)
let cancelTx txSkeleton contractID senderPK wallet order =
    // destroy the order
    let! orderAsset = getOrderAsset contractID order in //2328
    TX.destroy 1UL orderAsset txSkeleton // 64
    >>= TX.fromWallet orderAsset 1UL contractID wallet // W.size wallet * 128 + 192
    >>= CR.ofOptionTxSkel "Could not find order in wallet" //3

val cancel:
    txSkeleton
    -> contractId
    -> sender
    -> option data
    -> w: wallet
    -> CR.t `cost` (W.size w * 128 + 3119)
let cancel txSkeleton contractID sender messageBody wallet =
    match sender with
    | PK senderPK ->
        let cancelOrder = getCancelOrder senderPK messageBody in // 532
        cancelOrder `RT.bind` cancelTx txSkeleton contractID senderPK wallet // W.size wallet * 128 + 2587
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
    -> bool `cost` 0
let checkRequestedPayout { underlyingAmount=ua; orderTotal=ot} rp pa =
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
    let! ua_pa = dmul64 ua pa in
    // compute requestedPayout * orderTotal
    let! rp_ot = dmul64 rp ot in
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
val parseTake: option (Dict.t data) -> result order `cost` 594
let parseTake dict =
    let! underlyingAsset = getUnderlyingAsset dict in // 198
    let! underlyingAmount = getUnderlyingAmount dict in // 66
    let! pairAsset = getPairAsset dict in // 198
    let! orderTotal = getOrderTotal dict in // 66
    let! makerPK = getMakerPK dict in // 66
    match underlyingAsset, underlyingAmount, pairAsset, orderTotal, makerPK with
    | OK underlyingAsset, OK underlyingAmount, OK pairAsset, OK orderTotal, OK makerPK ->
         RT.ok ({ underlyingAsset=underlyingAsset;
                  underlyingAmount=underlyingAmount;
                  pairAsset=pairAsset;
                  orderTotal=orderTotal;
                  makerPubKey=makerPK })
    | _ ->
        RT.failw "Bad messageBody"

val takeTx:
    txSkeleton
    -> contractId
    -> w: wallet
    -> U64.t
    -> U64.t
    -> order
    -> lock
    -> CR.t `cost` (W.size w * 256 + 3311)
let takeTx txSkeleton contractID wallet paymentAmount payoutAmount order returnAddress =
    let! orderAsset = getOrderAsset contractID order in // 2328
    let! makerPubKeyHash = hashPubkey order.makerPubKey in // 404
    // lock the underlying to the returnAddress
    TX.lockToAddress order.underlyingAsset payoutAmount returnAddress txSkeleton // 64
    // lock the paymentAmount to the maker
    >>= TX.lockToPubKey order.pairAsset paymentAmount makerPubKeyHash // 64
    // destroy the order
    >>= TX.destroy 1UL orderAsset // 64
    // add inputs from wallet
    >>= TX.fromWallet order.underlyingAsset order.underlyingAmount contractID wallet // W.size wallet * 128 + 192
    >?= TX.fromWallet orderAsset 1UL contractID wallet // W.size wallet * 128 + 192
    >>= CR.ofOptionTxSkel "Could not find order in wallet. Ensure that both the order and the correct amount of the underlying are present." // 3

val take':
    txSkeleton
    -> contractId
    -> w: wallet
    -> order
    -> U64.t
    -> lock
    -> CR.t `cost` (W.size w * 256 + 3375)
let take' tx contractID w order requestedPayout returnAddress =
    let! paymentAmount = TX.getAvailableTokens order.pairAsset tx in // 64
    let! paymentAmountOK = checkRequestedPayout order requestedPayout paymentAmount in
    if paymentAmountOK
    then takeTx tx contractID w paymentAmount requestedPayout order returnAddress // W.size w * 256 + 3311
    else RT.incFailw (W.size w * 256 + 3311) "Incorrect requestedPayout"

val take:
    txSkeleton
    -> contractId
    -> option data
    -> w: wallet
    -> CR.t `cost` (W.size w * 256 + 4105)
let take tx contractID messageBody w =
    let! dict = messageBody >!= tryDict in // 4
    let order = parseTake dict in // 594
    let requestedPayout = getRequestedPayout dict in // 66
    let returnAddress = getReturnAddress dict in // 66
    RT.bind3 order requestedPayout returnAddress (take' tx contractID w)
