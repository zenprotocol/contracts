module MOfN

open Zen.Cost
open Zen.Data
open Zen.Types

module CR = Zen.ContractResult
module Dict = Zen.Dictionary
module OT = Zen.OptionT
module RT = Zen.ResultT
module Sha3 = Zen.Hash.Sha3
module TX = Zen.TxSkeleton
module U32 = FStar.UInt32
module U64 = FStar.UInt64
module W = Zen.Wallet

let n: nat = 3
let (m: nat{m <= n}) = 2

val pubKeys: (pks: list string{length pks == n}) `cost` (2 * n)
let pubKeys = autoRet [""; ""; ""]

val tryZip'(#a #b: Type):
    lx:list a
    -> ly:list b
    -> option (lz:list (a ** b){length lz == length lx}) `cost` (21 * length lx + 21)
let rec tryZip' #a #b lx ly = inc 21 begin
    match lx, ly with
    | x::xs, y::ys ->
        begin
        let! tl = tryZip' xs ys in
        match tl with
        | Some tl ->
            let (lz: list (a ** b){length lz == length lx}) = (x,y) :: tl in
            OT.some lz
        | None -> OT.none
        end <: option (lz:list (a ** b){length lz == length lx}) `cost` (21 * length lx)
    | [], [] -> OT.some []
    | _ -> OT.incNone (21 * length lx) end

val tryZip(#a #b: Type):
    lx:list a{length lx == n}
    -> ly:list b
    -> option (lz:list (a ** b){length lz == n}) `cost` (21 * n + 21)
let tryZip #_ #_ lx ly = tryZip' lx ly

val checkPair: option hash -> (string ** data) -> bool `cost` 920
let checkPair msgHash (pubKey, signature) =
    let! pk = Zen.Crypto.parsePublicKey pubKey in // 120
    match msgHash, pk, signature with
    | Some msgHash, Some pk, Signature sig -> Zen.Crypto.verify pk sig msgHash // 800
    | _ -> autoRet false

val hashByte: byte -> hash `cost` 26
let hashByte x = // 4
    Sha3.updateByte x Sha3.empty // 6
    >>= Sha3.finalize // 20

val hashU32: U32.t -> hash `cost` 44
let hashU32 x =
    Sha3.updateU32 x Sha3.empty // 24
    >>= Sha3.finalize // 20

val hashContractID: contractId -> hash `cost` 448
let hashContractID (version, cHash) =
    let! versionHash = hashU32 version in // 44
    Sha3.updateHash versionHash Sha3.empty // 192
    >>= Sha3.updateHash cHash // 192
    >>= Sha3.finalize // 20

val hashLock: lock -> option hash `cost` 878
let hashLock l =
    match l with
    | ContractLock cid ->
        let! indexHash = hashByte 0x01uy in // 26
        let! cidHash = hashContractID cid in // 448
        Sha3.updateHash indexHash Sha3.empty // 192
        >>= Sha3.updateHash cidHash // 192
        >>= Sha3.finalize     // 20
        >>= OT.some
    | PKLock h -> autoInc begin
        let! indexHash = hashByte 0x00uy in // 26
        Sha3.updateHash indexHash Sha3.empty // 192
        >>= Sha3.updateHash h // 192
        >>= Sha3.finalize     // 20
        >>= OT.some end
    | _ -> OT.autoNone

val hashAsset: asset -> hash `cost` 404
let hashAsset asset = // 4
    Sha3.updateAsset asset Sha3.empty // 384
    >>= Sha3.finalize // 20

val hashU64: U64.t -> hash `cost` 68
let hashU64 x = // 4
    Sha3.updateU64 x Sha3.empty // 48
    >>= Sha3.finalize // 20

val hashPayout: asset -> U64.t -> lock -> U64.t -> option hash `cost` 2206
let hashPayout asset amount l nonce =
    let! assetHash = hashAsset asset in // 404
    let! amountHash = hashU64 amount in // 68
    let! lockHash = hashLock l in // 878
    let! nonceHash = hashU64 nonce in // 68
    match lockHash with
    | Some lockHash -> Sha3.updateHash assetHash Sha3.empty // 192
                       >>= Sha3.updateHash amountHash // 192
                       >>= Sha3.updateHash lockHash // 192
                       >>= Sha3.updateHash nonceHash // 192
                       >>= Sha3.finalize // 20
                       >>= OT.some
    | None -> OT.autoNone

val countValid':
    option hash
    -> pairs: list (string ** data)
    -> int `cost` (length pairs * 937 + 17)
let rec countValid' msgHash pairs = inc 17 begin
    match pairs with
    | p::ps -> begin
        let! check = checkPair msgHash p in // 920
        let! count = countValid' msgHash ps in
        ret (if check then 1 + count else count)
        end <: int `cost` (length pairs * 937)
    | [] -> autoRet 0 end

val countValid:
    option hash
    -> pairs: list (string ** data) {length pairs == n}
    -> int `cost` (937 * n + 17)
let countValid msgHash pairs = countValid' msgHash pairs

val checkSigs: option hash -> list data -> bool `cost` (960 * n + 38)
let checkSigs msgHash sigs =
    let! pubKeys = pubKeys in // 2 * n
    let! pairs = tryZip pubKeys sigs in // 21 * n + 21
    match pairs with
    | Some pairs ->
        let! count = countValid msgHash pairs in // 937 * n + 17
        ret (count >= m)
    | None -> autoRet false

type messageParams = {
    asset: asset;
    amount: U64.t;
    lock: lock;
    sigs: list data
}

val checksPass: option data -> messageParams -> bool `cost` (960 * n + 2244)
let checksPass state ({asset=asset; amount=amount; lock=lock; sigs=sigs}) =
    let nonce = match state with
                | Some (U64 nonce) -> nonce
                | _ -> 0UL in
    let! payoutHash = hashPayout asset amount lock nonce in // 2206
    checkSigs payoutHash sigs // 960 * n + 38

val updateState: option data -> CR.t -> CR.t `cost` 3
let updateState state cr = let open U64 in
    let newState = match state with
                   | Some (U64 nonce) -> U64 (nonce +%^ 1UL)
                   | _ -> U64 1UL in
    CR.setStateUpdate newState cr // 3

val parseMsg: option data -> result messageParams `cost` 334
let parseMsg messageBody =
    let! dict = messageBody >!= tryDict in // 4
    let! asset = dict >!= Dict.tryFind "Asset" // 64
                      >?= tryString // 2
                      >?= Zen.Asset.parse in // 64
    let! amount = dict >!= Dict.tryFind "Amount" // 64
                       >?= tryU64 in // 2
    let! lock = dict >!= Dict.tryFind "Lock" // 64
                     >?= tryLock in // 2
    let! sigs = dict >!= Dict.tryFind "Signatures" // 64
                     >?= tryList in // 4
    match asset, amount, lock, sigs with
    | Some asset, Some amount, Some lock, Some sigs ->
        RT.ok ({asset=asset; amount=amount; lock=lock; sigs=sigs})
    | _ -> RT.failw "Could not parse messageBody"

val mainTx:
    txSkeleton
    -> contractId
    -> messageParams
    -> w: wallet
    -> option data
    -> CR.t `cost` (W.size w * 128 + 262)
let mainTx tx contractID ({asset=asset; amount=amount; lock=lock}) w state =
    TX.lockToAddress asset amount lock tx // 64
    >>= TX.fromWallet asset amount contractID w // W.size w * 128 + 192
    >>= CR.ofOptionTxSkel "Could not get funds from wallet" // 3
    >>= updateState state // 3

val main':
    txSkeleton
    -> contractId
    -> w: wallet
    -> option data
    -> messageParams
    -> CR.t `cost` ((W.size w * 128) + (960 * n) + 2506)
let main' tx contractID w state msgParams = begin
    let! checksPass = checksPass state msgParams in // 960 * n + 2244
    if checksPass
    then mainTx tx contractID msgParams w state // W.size w * 128 + 262
    else RT.autoFailw "Signature validation failed"
    end <:  CR.t `cost` ((W.size w * 128) + (960 * n) + 2506)

val main:
    txSkeleton
    -> context
    -> contractId
    -> command: string
    -> sender
    -> option data
    -> w: wallet
    -> option data
    -> CR.t `cost` ((W.size w * 128) + (960 * n) + 2840)
let main tx _ contractID _ _ messageBody w state =
    parseMsg messageBody // 334
    `RT.bind`
    main' tx contractID w state // (W.size w * 128) + (960 * n) + 2506
