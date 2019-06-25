module Oracle2

open Zen.Base
open Zen.Cost
open Zen.Types

module CR = Zen.ContractResult
module Dict = Zen.Dictionary
module OT = Zen.OptionT
module RT = Zen.ResultT
module Sha3 = Zen.Hash.Sha3
module TX = Zen.TxSkeleton
module W = Zen.Wallet

type args =
    { commit: hash;
      timestamp : timestamp;
      oraclePubKey: option publicKey }

type assets =
    { commitment: asset;
      attestation: asset }

// compressed public key
type cpk = byte ** hash

let parseArgs (msg: option data): result args `cost` 235 = // 41
    let open OT in
    match msg with
    | Some (Collection (Dict dict)) -> begin
        let! commit = Dict.tryFind "Commit" dict in // 64
        let! timestamp = Dict.tryFind "Timestamp" dict in // 64
        let! oraclePubKey = Dict.tryFind "OraclePubKey" dict // 64
                            >>= Zen.Data.tryPublicKey in //2
        match commit, timestamp with
        | Some (Hash commit), Some (U64 timestamp) ->
            RT.ok ({ commit=commit;
                     timestamp=timestamp;
                     oraclePubKey=oraclePubKey })
        | Some _, _ | None, _ ->
            RT.failw "Could not parse Commit"
        | _, Some _ | _, None ->
            RT.failw "Could not parse Timestamp"
        end
    | _ -> RT.autoFailw "MessageBody must be a dictionary."

// compress a public key
let compress (pk:publicKey): cpk `cost` 305 = let open FStar.UInt8 in // 13
    let parity = (Zen.Array.item 32 pk %^ 2uy) +^ 2uy in
    let aux (i:nat{i < 32}): byte `cost` 5 =
        ret (Zen.Array.item (31-i) pk) in
    let! x = Zen.Array.init_pure 32 aux in // 292
    ret (parity, x)

let updateCPK ((parity, h):cpk) (s:Sha3.t): Sha3.t `cost` 203 = // 5
    Sha3.updateByte parity s
    >>= Sha3.updateHash h

// hash a compressed publicKey
let hashCPK (cpk:cpk): hash `cost` 227 = // 4
    updateCPK cpk Sha3.empty // 203
    >>= Sha3.finalize // 20

let hashArgs (args:args{Some? args.oraclePubKey}): hash `cost` 786 = // 18
    let! cpk = compress (Some?.v args.oraclePubKey) in // 305
    Sha3.updateHash args.commit Sha3.empty // 192
    >>= Sha3.updateU64 args.timestamp // 48
    >>= updateCPK cpk // 203
    >>= Sha3.finalize // 20

val mkAssets: contractId -> args:args{Some? args.oraclePubKey} -> assets `cost` 1016
let mkAssets (v, h) args = // 18
    let! argsHash = hashArgs args in // 786
    let! arsgHash2 = Sha3.updateHash argsHash Sha3.empty // 192
                     >>= Sha3.finalize in // 20
    ret ({ commitment=v,h,argsHash; attestation=v,h,arsgHash2 })

//
// Making a commitment
//

val commit: txSkeleton -> contractId -> publicKey -> args -> CR.t `cost` 1163
let commit tx cid senderPK args = // 16
    let args = { args with oraclePubKey=Some senderPK } in
    let! ({commitment=commitment}) = mkAssets cid args in // 1016
    // mint the commitment token
    TX.mint 1UL commitment tx // 64
    // lock the commitment token to the contract
    >>= TX.lockToContract commitment 1UL cid // 64
    // return the tx
    >>= CR.ofTxSkel // 3

//
// Attesting to a commitment
//

let lockToPK (s:spend) (pk:publicKey) (tx:txSkeleton): txSkeleton `cost` 608 = // 12
    let! pkHash = compress pk >>= hashCPK in // 305 + 227
    TX.lockToAddress s.asset s.amount (PKLock pkHash) tx // 64

// The attestation transaction, minus contract wallet inputs
let attestTx tx cid senderPK assets: txSkeleton `cost` 891 = // 27
    // destroy the attestation token
    TX.destroy 1UL assets.commitment tx // 64
    // mint the new commitment and attestation tokens
    >>= TX.mint 1UL assets.commitment // 64
    >>= TX.mint 1UL assets.attestation // 64
    // lock the attestation token to the user
    >>= lockToPK ({asset=assets.attestation; amount=1UL}) senderPK // 608
    // lock the commitment token to the contract
    >>= TX.lockToContract assets.commitment 1UL cid // 64

let attest' tx cid senderPK w (args:args) = // 22
    match args.oraclePubKey with
    | Some _ -> begin
        let! assets = mkAssets cid args in // 1016
        // the tx, minus contract wallet inputs
        attestTx tx cid senderPK assets // 891
        // add a commitment token from the contract wallet
        >>= TX.fromWallet assets.commitment 1UL cid w // W.size w * 128 + 192
        // return the tx
        >>= CR.ofOptionTxSkel "Could not spend from wallet" // 3
        end
    | None ->
        RT.autoFailw "Could not parse OraclePubKey"

let attest: txSkeleton -> contractId -> publicKey -> w:wallet -> args
            -> CR.t `cost` (W.size w * 128 + 2124) = attest'

//
// Exports
//

val main: txSkeleton -> context -> contractId -> command: string
          -> sender: sender -> option data -> w: wallet -> option data
          -> CR.t `cost` begin match command, sender with
                         | "Commit", PK _ -> 1418
                         | "Attest", PK _ -> Zen.Wallet.size w * 128 + 2379
                         | _ -> 20 end
let main tx _ cid command sender msg w _ = // 20
    match command, sender with
    | "Commit", PK pk ->
        parseArgs msg `RT.bind` commit tx cid pk // 235 + 1163
        <: CR.t `cost` begin match command, sender with
                       | "Commit", PK _ -> 1398
                       | "Attest", PK _ -> Zen.Wallet.size w * 128 + 2359
                       | _ -> 0 end
    | "Attest", PK pk ->
        parseArgs msg `RT.bind` attest tx cid pk w // 235 + (Zen.Wallet.size w * 128 + 2124)
    | _, PK pk ->
        RT.failw "a"
    | _, _ ->
        RT.failw "b"

let cf _ _ command sender _ w _ : nat `cost` 15 = // 15
    ret begin match command, sender with
        | "Commit", PK _ -> 1418
        | "Attest", PK _ -> Zen.Wallet.size w * 128 + 2379
        | _ -> 20 end
