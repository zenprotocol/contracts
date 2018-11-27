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
val dmul64: U64.t -> U64.t -> d64 `cost` 43
let dmul64 x y = let open U64 in // 39
    let m32 = 4294967296UL in // 2^32
    let xlo = x %^ m32 in // 32 low bits of x
    let xhi = x /^ m32 in // 32 high bits of x
    let ylo = y %^ m32 in // 32 low bits of y
    let yhi = y /^ m32 in // 32 high bits of y

    let t0 = xlo *%^ ylo in
    let t1 = (xhi *%^ ylo) +%^ (t0 /^ m32) in
    let t2 = (xlo *%^ yhi) +%^ (t1 %^ m32) in

    let hi = (xhi *%^ yhi) +%^ (t1 /^ m32) +%^ (t2 /^ m32) in
    let lo = ((t2 %^ m32) *%^ m32) +%^ (t0 %^ m32) in
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

val updatePubKey': pk: list byte{length pk == 64} -> Zen.Hash.Sha3.t -> Zen.Hash.Sha3.t `cost` 648
let updatePubKey' pk s = // 4
    Zen.List.foldT (Zen.Base.flip Hash.updateByte) s pk // 64 * 10 + 4
    <: Zen.Hash.Sha3.t `cost` 644

val updatePubKey: publicKey -> Zen.Hash.Sha3.t -> Zen.Hash.Sha3.t `cost` 783
let updatePubKey pk s = // 5
    Zen.Array.toList pk // 130
    >>= Zen.Base.flip updatePubKey' s // 648

val hashPubKey: publicKey -> hash `cost` 807
let hashPubKey pk = // 4
    updatePubKey pk Hash.empty // 783
    >>= Hash.finalize // 20

val hashOrder: order -> hash `cost` 2783
let hashOrder order = // 36
    let! underlyingAssetHash = hashAsset order.underlyingAsset in // 408
    let! underlyingAmountHash = hashU64 order.underlyingAmount in // 72
    let! pairAssetHash = hashAsset order.pairAsset in // 408
    let! orderTotalHash = hashU64 order.orderTotal in // 72
    let! makerPubKeyHash = hashPubKey order.makerPubKey in // 807
        Hash.updateHash underlyingAssetHash Hash.empty // 192
    >>= Hash.updateHash underlyingAmountHash // 192
    >>= Hash.updateHash pairAssetHash // 192
    >>= Hash.updateHash orderTotalHash // 192
    >>= Hash.updateHash makerPubKeyHash // 192
    >>= Hash.finalize // 20

val getAsset: option (Dict.t data) -> string -> option asset `cost` 137
let getAsset dict fieldName = // 7
    dict >!= Dict.tryFind fieldName // 64
         >?= tryString // 2
         >?= Zen.Asset.parse // 64

val getU64: option (Dict.t data) -> string -> option U64.t `cost` 80
let getU64 dict fieldName = // 14
    let! x = dict >!= Dict.tryFind fieldName // 64
                  >?= tryU64 in // 2
    ret ( if x <> Some 0UL then x else None )

val getHash: option (Dict.t data) -> string -> option hash `cost` 71
let getHash dict fieldName = // 5
    dict >!= Dict.tryFind fieldName // 64
         >?= tryHash // 2

val getMakerPubKey: option (Dict.t data) -> option publicKey `cost` 71
let getMakerPubKey dict = // 5
    dict >!= Dict.tryFind "MakerPubKey" // 64
         >?= tryPublicKey // 2

val getReturnAddress: option (Dict.t data) -> option lock `cost` 71
let getReturnAddress dict = // 5
    dict >!= Dict.tryFind "returnAddress" // 64
                              >?= tryLock // 2

val getOrder: option (Dict.t data) -> result order `cost` 570
let getOrder dict = // 65
    let! underlyingAsset = getAsset dict "UnderlyingAsset" in // 137
    let! underlyingAmount = getU64 dict "UnderlyingAmount" in // 80
    let! pairAsset = getAsset dict "PairAsset"in // 137
    let! orderTotal = getU64 dict "OrderTotal" in // 80
    let! makerPubKey = getMakerPubKey dict in // 71
    match underlyingAsset, underlyingAmount, pairAsset, orderTotal, makerPubKey with
    | Some underlyingAsset, Some underlyingAmount,
      Some pairAsset, Some orderTotal, Some makerPubKey ->
        RT.ok ({ underlyingAsset=underlyingAsset;
                 underlyingAmount=underlyingAmount;
                 pairAsset=pairAsset;
                 orderTotal=orderTotal;
                 makerPubKey=makerPubKey })
    | None, _, _, _, _ -> RT.autoFailw "Could not parse UnderlyingAsset"
    | _, None, _, _, _ -> RT.autoFailw "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"
    | _, _, None, _, _ -> RT.autoFailw "Could not parse PairAsset"
    | _, _, _, None, _ -> RT.autoFailw "Could not parse OrderTotal, or OrderTotal was 0"
    | _, _, _, _, None -> RT.autoFailw "Could not parse MakerPubKey"

val getOrderAsset: contractId -> order -> asset `cost` 2792
let getOrderAsset contractID order = // 5
    let! orderHash = hashOrder order in // 2783
    mkAsset contractID orderHash // 4

val lockToPubKey: asset -> U64.t -> publicKey -> txSkeleton -> txSkeleton `cost` 878
let lockToPubKey asset amount pubKey tx = // 7
    let! pubKeyHash = hashPubKey pubKey in // 807
    TX.lockToPubKey asset amount pubKeyHash tx // 64

// mints an order asset and locks it to the contract, as well as the underlying
val createOrder: order -> contractId -> txSkeleton -> txSkeleton `cost` 3003
let createOrder order contractID tx = // 19
    let! orderAsset = getOrderAsset contractID order in // 2792
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
    -> CR.t `cost` (W.size w * 256 + 3267)
let destroyOrder order contractID w tx = // 24
    // the rewrites are not necessary, but vastly improve verification time
    begin
        let! orderAsset = getOrderAsset contractID order in // 2792
        begin
            begin
                // destroy the order
                begin TX.destroy 1UL orderAsset tx // 64
                      >>= TX.fromWallet orderAsset 1UL contractID w // W.size w * 128 + 192
                      <: option txSkeleton `cost` (W.size w * 128 + 256)
                end
                // add the underlying to the inputs
                >?= TX.fromWallet order.underlyingAsset order.underlyingAmount contractID w // W.size w * 128 + 192
                <: option txSkeleton `cost` (W.size w * 256 + 448)
            end
            >>= CR.ofOptionTxSkel "Could not find order in wallet. Ensure that both the order and the correct amount of the underlying are present." // 3
            <: CR.t `cost` (W.size w * 256 + 451)
        end
    end <: CR.t `cost` (W.size w * 256 + 3243)

//////////////////
// Making an order
//////////////////

val makeTx: txSkeleton -> contractId -> publicKey -> order -> CR.t `cost` 3097
let makeTx tx contractID senderPubKey order = // 27
    let! underlyingReceived = TX.getAvailableTokens order.underlyingAsset tx in // 64
    let! tx = // issue a token with the hash of the order as the subidentifier,
              // and lock it to the contract, with the underlying
              createOrder order contractID tx // 3003
              >>= CR.ofTxSkel in // 3
    match underlyingReceived = order.underlyingAmount, senderPubKey = order.makerPubKey with
    | true, true -> ret tx
    | false, _ -> RT.failw "Incorrect amount of UnderlyingAsset Received"
    | _, false -> RT.failw "SenderPubKey must match MakerPubKey"

val make: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 3684
let make tx contractID sender messageBody = // 13
    match sender with
    | PK senderPubKey ->
        let! dict = messageBody >!= tryDict in // 4
        getOrder dict // 570
        `RT.bind`
        makeTx tx contractID senderPubKey // 3097
    | _ -> RT.autoFailw "Must authenticate with PubKey"

//////////////////
// Cancel an order
//////////////////

val cancelTx:
    txSkeleton
    -> contractId
    -> w: wallet
    -> publicKey
    -> order
    -> CR.t `cost` (W.size w * 256 + 4163)
let cancelTx tx contractID w senderPubKey order = // 18
    if senderPubKey = order.makerPubKey
    then // lock the underlying to the maker's pk
         lockToPubKey order.underlyingAsset order.underlyingAmount order.makerPubKey tx // 878
         // destroy the order
         >>= destroyOrder order contractID w // W.size w * 256 + 3267
         <: CR.t `cost` (W.size w * 256 + 4145)
    else RT.autoFailw "SenderPubKey must match MakerPubKey"

val cancel:
    txSkeleton
    -> contractId
    -> sender
    -> option data
    -> w: wallet
    -> CR.t `cost` (W.size w * 256 + 4751)
let cancel tx contractID sender messageBody w = // 14
    match sender with
    | PK senderPubKey ->
        let! dict = messageBody >!= tryDict in //4
        begin let order = getOrder dict in // 570
        order `RT.bind` cancelTx tx contractID w senderPubKey // W.size w * 256 + 4163
        end <: CR.t `cost` (W.size w * 256 + 4733)
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
    -> bool `cost` 163
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
    let! ua_pa = dmul64 ua pa in // 43
    // compute requestedPayout * orderTotal
    let! rp_ot = dmul64 rp ot in // 43
    // compute requestedPayout * orderTotal + orderTotal
    let rp_ot_ot = { hi = if rp_ot.lo >^ max64 -%^ ot // will adding low 64 bits overflow
                          then rp_ot.hi +%^ 1UL // this never overflows
                          else rp_ot.hi;
                     lo = rp_ot.lo +%^ ot } in
    // compute underlyingAmount * paymentAmount + orderTotal
    let ua_pa_ot = { hi = if ua_pa.lo >^ max64 -%^ ot // will adding low 64 bits overflow
                          then ua_pa.hi +%^ 1UL // this never overflows
                          else ua_pa.hi;
                     lo = ua_pa.lo +%^ ot } in

    // underlyingAmount * paymentAmount < requestedPayout * orderTotal + orderTotal
    let ua_pa_lt_rp_ot_ot = (ua_pa.hi <^ rp_ot_ot.hi)
                         || (ua_pa.hi = rp_ot_ot.hi && ua_pa.lo <^ rp_ot_ot.lo) in
    // requestedPayout * orderTotal + orderTotal <= underlyingAmount * paymentAmount + orderTotal
    let rp_ot_ot_lte_ua_pa_ot = (rp_ot_ot.hi <^ ua_pa_ot.hi)
                             || (rp_ot_ot.hi = ua_pa_ot.hi && rp_ot_ot.lo <=^ ua_pa_ot.lo) in
    // both conditions must hold
    ret (ua_pa_lt_rp_ot_ot && rp_ot_ot_lte_ua_pa_ot)

// updates an order in the case of a partial fill
val updateOrder:
    contractId
    -> order
    -> U64.t
    -> U64.t
    -> txSkeleton
    -> txSkeleton `cost` 3020
let updateOrder contractID order paymentAmount payoutAmount tx = let open U64 in // 17
    if paymentAmount <^ order.orderTotal // partial fill, so need to update the order
    then // create the new order
        let newOrder = { order with
                         underlyingAmount=order.underlyingAmount-%^payoutAmount;
                         orderTotal=order.orderTotal-%^paymentAmount } in
        createOrder newOrder contractID tx // 3003
    else incRet 3003 tx

val takeTx:
    txSkeleton
    -> contractId
    -> w: wallet
    -> U64.t
    -> U64.t
    -> order
    -> lock
    -> CR.t `cost` (W.size w * 256 + 7252)
let takeTx tx contractID w paymentAmount payoutAmount order returnAddress = // 23
    //  lock the payout to the taker
    TX.lockToAddress order.underlyingAsset payoutAmount returnAddress tx // 64
    // lock the paymentAmount to the maker
    >>= lockToPubKey order.pairAsset paymentAmount order.makerPubKey // 878
    //  create a new order if partial fill, locking the remainder of the underlying to the contract
    >>= updateOrder contractID order paymentAmount payoutAmount // 3020
    // add inputs from wallet, destroying the order
    >>= destroyOrder order contractID w // W.size w * 256 + 3764

val take':
    txSkeleton
    -> contractId
    -> w: wallet
    -> U64.t
    -> lock
    -> order
    -> CR.t `cost` (W.size w * 256 + 7499)
let take' tx contractID w requestedPayout returnAddress order = // 20
    let! paymentAmount = TX.getAvailableTokens order.pairAsset tx in // 64
    begin
    let! paymentAmountOK = checkRequestedPayout order requestedPayout paymentAmount in // 163
    if paymentAmountOK
    then takeTx tx contractID w paymentAmount requestedPayout order returnAddress // W.size w * 256 + 7252
    else RT.incFailw (W.size w * 256 + 7252) "Incorrect requestedPayout"
    end <: CR.t `cost` (W.size w * 256 + 7415)

val take:
    txSkeleton
    -> contractId
    -> option data
    -> w: wallet
    -> CR.t `cost` (W.size w * 256 + 8253)
let take tx contractID messageBody w = // 29
    let! dict = messageBody >!= tryDict in // 4
    //begin
    let! requestedPayout = getU64 dict "RequestedPayout" in // 80
    //begin
    let! returnAddress = getReturnAddress dict in // 71
    match requestedPayout, returnAddress with
    | Some requestedPayout, Some returnAddress ->
        let order = getOrder dict in // 570
        order `RT.bind` take' tx contractID w requestedPayout returnAddress // W.size w * 256 + 7499
        <: CR.t `cost` (W.size w * 256 + 8069)
    | None, _ ->
        RT.autoFailw "Could not parse requestedPayout, or requestedPayout was 0"
    | _, None ->
        RT.autoFailw "Could not parse returnAddress"

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
                         | "Make" -> 3684
                         | "Cancel" -> W.size w * 256 + 4751
                         | "Take" -> W.size w * 256 + 8253
                         | _ -> 0 end )
let main tx _ contractID command sender messageBody w _ = // 9
    begin
    match command with
    | "Make" ->
        make tx contractID sender messageBody // 3684
        <: CR.t `cost` begin match command with
                       | "Make" -> 3684
                       | "Cancel" -> W.size w * 256 + 4751
                       | "Take" -> W.size w * 256 + 8253
                       | _ -> 0 end
    | "Cancel" ->
        cancel tx contractID sender messageBody w // W.size w * 256 + 4751
    | "Take" ->
        take tx contractID messageBody w // W.size w * 256 + 8253
    | _ ->
        RT.failw "Unrecognised command"
    end <: CR.t `cost` begin match command with
                       | "Make" -> 3684
                       | "Cancel" -> W.size w * 256 + 4751
                       | "Take" -> W.size w * 256 + 8253
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
              | "Make" -> 3684
              | "Cancel" -> W.size w * 256 + 4751
              | "Take" -> W.size w * 256 + 8253
              | _ -> 0 end )
