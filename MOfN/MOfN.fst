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

val pubKeys: (pks: list string{length pks == n}) `cost` (2 * n + 1)
let pubKeys = ret [""; ""; ""]

val tryZip'(#a #b: Type):
    lx:list a
    -> ly:list b
    -> option (lz:list (a ** b){length lz == length lx}) `cost` (21 * length lx + 21)
let rec tryZip' #a #b lx ly = // 21
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
    | _ -> OT.incNone (21 * length lx)

val tryZip(#a #b: Type):
    lx:list a{length lx == n}
    -> ly:list b
    -> option (lz:list (a ** b){length lz == n}) `cost` (21 * n + 23)
let tryZip #_ #_ lx ly = tryZip' lx ly // 2

val checkPair: option hash -> (string ** data) -> bool `cost` 936
let checkPair msgHash (pubKey, signature) = // 16
    let! pk = Zen.Crypto.parsePublicKey pubKey in // 120
    match msgHash, pk, signature with
    | Some msgHash, Some pk, Signature sig -> Zen.Crypto.verify pk sig msgHash // 800
    | _ -> autoRet false

val hashByte: byte -> hash `cost` 30
let hashByte x = // 4
    Sha3.updateByte x Sha3.empty // 6
    >>= Sha3.finalize // 20

val hashU32: U32.t -> hash `cost` 48
let hashU32 x = // 4
    Sha3.updateU32 x Sha3.empty // 24
    >>= Sha3.finalize // 20

val hashContractID: contractId -> hash `cost` 462
let hashContractID (version, cHash) = // 10
    let! versionHash = hashU32 version in // 48
    Sha3.updateHash versionHash Sha3.empty // 192
    >>= Sha3.updateHash cHash // 192
    >>= Sha3.finalize // 20

val hashLock: lock -> option hash `cost` 916
let hashLock l = // 20
    match l with
    | ContractLock cid ->
        let! indexHash = hashByte 0x01uy in // 30
        let! cidHash = hashContractID cid in // 462
        Sha3.updateHash indexHash Sha3.empty // 192
        >>= Sha3.updateHash cidHash // 192
        >>= Sha3.finalize     // 20
        >>= OT.some
    | PKLock h -> autoInc begin
        let! indexHash = hashByte 0x00uy in // 30
        Sha3.updateHash indexHash Sha3.empty // 192
        >>= Sha3.updateHash h // 192
        >>= Sha3.finalize     // 20
        >>= OT.some end
    | _ -> OT.autoNone

val hashAsset: asset -> hash `cost` 408
let hashAsset asset = // 4
    Sha3.updateAsset asset Sha3.empty // 384
    >>= Sha3.finalize // 20

val hashU64: U64.t -> hash `cost` 72
let hashU64 x = // 4
    Sha3.updateU64 x Sha3.empty // 48
    >>= Sha3.finalize // 20

val hashPayout: asset -> U64.t -> lock -> U64.t -> option hash `cost` 2286
let hashPayout asset amount l nonce = // 30
    let! assetHash = hashAsset asset in // 408
    let! amountHash = hashU64 amount in // 72
    let! lockHash = hashLock l in // 916
    let! nonceHash = hashU64 nonce in // 72
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
    -> int `cost` (length pairs * 953 + 17)
let rec countValid' msgHash pairs =
    match pairs with
    | p::ps -> begin
        let! check = checkPair msgHash p in // 936
        let! count = countValid' msgHash ps in
        ret (if check then 1 + count else count)
        end <: int `cost` (length pairs * 953)
    | [] -> autoRet 0

val countValid:
    option hash
    -> pairs: list (string ** data) {length pairs == n}
    -> int `cost` (953 * n + 19)
let countValid msgHash pairs = countValid' msgHash pairs // 2

val checkSigs: option hash -> list data -> bool `cost` (976 * n + 59)
let checkSigs msgHash sigs = // 16
    let! pubKeys = pubKeys in // 2 * n + 1
    let! pairs = tryZip pubKeys sigs in // 21 * n + 23
    match pairs with
    | Some pairs ->
        let! count = countValid msgHash pairs in // 953 * n + 19
        ret (count >= m)
    | None -> autoRet false

type messageParams = {
    asset: asset;
    amount: U64.t;
    lock: lock;
    sigs: list data
}

val checksPass: option data -> messageParams -> bool `cost` (976 * n + 2357)
let checksPass state ({asset=asset; amount=amount; lock=lock; sigs=sigs}) = // 12
    let nonce = match state with
                | Some (U64 nonce) -> nonce
                | _ -> 0UL in
    let! payoutHash = hashPayout asset amount lock nonce in // 2286
    checkSigs payoutHash sigs // 976 * n + 59

val updateState: option data -> CR.t -> CR.t `cost` 12
let updateState state cr = let open U64 in // 9
    let newState = match state with
                   | Some (U64 nonce) -> U64 (nonce +%^ 1UL)
                   | _ -> U64 1UL in
    CR.setStateUpdate newState cr // 3

val parseMsg: option data -> result messageParams `cost` 386
let parseMsg messageBody = // 52
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
    -> CR.t `cost` (W.size w * 128 + 287)
let mainTx tx contractID ({asset=asset; amount=amount; lock=lock}) w state = // 16
    TX.lockToAddress asset amount lock tx // 64
    >>= TX.fromWallet asset amount contractID w // W.size w * 128 + 192
    >>= CR.ofOptionTxSkel "Could not get funds from wallet" // 3
    >>= updateState state // 12

val main':
    txSkeleton
    -> contractId
    -> w: wallet
    -> option data
    -> messageParams
    -> CR.t `cost` ((W.size w * 128) + (976 * n) + 2656)
let main' tx contractID w state msgParams = begin // 12
    let! checksPass = checksPass state msgParams in // 976 * n + 2357
    if checksPass
    then mainTx tx contractID msgParams w state // W.size w * 128 + 287
    else RT.autoFailw "Signature validation failed"
    end <:  CR.t `cost` ((W.size w * 128) + (976 * n) + 2644)

val main:
    txSkeleton
    -> context
    -> contractId
    -> command: string
    -> sender
    -> option data
    -> w: wallet
    -> option data
    -> CR.t `cost` ((W.size w * 128) + (976 * n) + 3049)
let main tx _ contractID _ _ messageBody w state = // 7
    parseMsg messageBody // 386
    `RT.bind`
    main' tx contractID w state // (W.size w * 128) + (976 * n) + 2656

val cf:
    txSkeleton
    -> context
    -> string
    -> sender
    -> option data
    -> wallet
    -> option data
    -> nat `cost` 10
let cf _ _ _ _ _ w _ = // 10
    ret ((W.size w * 128) + (976 * n) + 3049)
