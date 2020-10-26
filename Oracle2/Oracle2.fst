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

type commitData =
    { commit       : hash
    ; oraclePubKey : publicKey
    }

type attestData =
    { commitData : commitData
    ; recipient  : lock
    }

type assets =
    { commitment  : asset
    ; attestation : asset
    }

// compressed public key
type cpk = byte ** hash



(*
-------------------------------------------------------------------------------
========== COMPRESSED PUBLIC KEY ==============================================
-------------------------------------------------------------------------------
*)

val compress : publicKey -> cpk `cost` 305
let compress pk = // 13
    let open FStar.UInt8 in
    let parity = (Zen.Array.item 32 pk %^ 2uy) +^ 2uy in
    let aux (i:nat{i < 32}): byte `cost` 5 = ret (Zen.Array.item (31-i) pk) in
    let! x = Zen.Array.init_pure 32 aux in // 292
    ret (parity , x)

val updateCPK : cpk -> Sha3.t -> Sha3.t `cost` 205
let updateCPK (parity , h) s = // 7
    ret s
    >>= Sha3.updateByte parity
    >>= Sha3.updateHash h

val hashCPK : cpk -> hash `cost` 231
let hashCPK cpk = // 6
    ret Sha3.empty
    >>= updateCPK cpk // 205
    >>= Sha3.finalize // 20



(*
-------------------------------------------------------------------------------
========== DATA PARSING =======================================================
-------------------------------------------------------------------------------
*)

val parseCommitData : publicKey -> option data -> commitData `RT.t` 81
let parseCommitData sender msgBody = // 17
    begin match msgBody with
    | Some (Collection (Dict dict)) ->
        let! commit = Dict.tryFind "Commit" dict in // 64
        begin match commit with
        | Some (Hash commit) ->
            { commit       = commit
            ; oraclePubKey = sender
            } |> RT.ok
        | _ ->
            RT.failw "Could not parse Commit"
        end
    | _ ->
        RT.autoFailw "MessageBody must be a dictionary."
    end

val senderToLock : sender -> lock `OT.t` 551
let senderToLock sender = // 15
    begin match sender with
    | PK pk ->
        let! pkHash =
            ret pk
            >>= compress // 305
            >>= hashCPK  // 231
        in ret (Some (PKLock pkHash))
    | Contract cid ->
        Some (ContractLock cid)
        |> incRet 536
    | Anonymous ->
        None
        |> incRet 536
    end

val parseAttestData : sender -> option data -> attestData `RT.t` 790
let parseAttestData sender msgBody = // 47
    begin match msgBody with
    | Some (Collection (Dict dict)) ->
        let! commit       = Dict.tryFind "Commit"       dict in // 64
        let! oraclePubKey = Dict.tryFind "OraclePubKey" dict in // 64
        let! recipient    = Dict.tryFind "Recipient"    dict in // 64
        let! senderLock   = senderToLock sender              in // 551
        begin match commit with
        | Some (Hash commit) ->
            begin match oraclePubKey with
            | Some (PublicKey pk) ->
                begin match recipient with
                | Some (Lock recip) ->
                    { commitData =
                        { commit       = commit
                        ; oraclePubKey = pk
                        }
                    ; recipient = recip
                    } |> RT.ok
                | Some _ ->
                    RT.failw "Recipient (if specified) must be a lock"
                | None ->
                    begin match senderLock with
                    | Some lock ->
                        { commitData =
                            { commit       = commit
                            ; oraclePubKey = pk
                            }
                        ; recipient = lock
                        } |> RT.ok
                    | None ->
                        RT.failw "When the recipient is unspecified the sender can't be anonymous"
                    end
                end
            | Some _ ->
                RT.failw "OraclePubKey must be a public key"
            | None ->
                RT.failw "Couldn't find OraclePubKey in message body"
            end
        | Some _ ->
            RT.failw "Commit must be an hash"
        | None ->
            RT.failw "Couldn't find Commit in message body"
        end
    | _ -> RT.autoFailw "MessageBody must be a dictionary."
    end



(*
-------------------------------------------------------------------------------
========== TOKENIZATION =======================================================
-------------------------------------------------------------------------------
*)

val hashCommitData : commitData -> hash `cost` 736
let hashCommitData data = // 14
    let! cpk = compress data.oraclePubKey in // 305
    ret Sha3.empty
    >>= Sha3.updateHash data.commit // 192
    >>= updateCPK cpk               // 205
    >>= Sha3.finalize               // 20

val mkAssets : contractId -> commitData -> assets `cost` 968
let mkAssets (v, h) data = // 20
    let! dataHash =
        hashCommitData data in // 736
    let! dataHash2 =
        ret Sha3.empty
        >>= Sha3.updateHash dataHash // 192
        >>= Sha3.finalize in         // 20
    ret ({ commitment=v,h,dataHash; attestation=v,h,dataHash2 })



(*
-------------------------------------------------------------------------------
========== COMMAND: Commit ====================================================
-------------------------------------------------------------------------------
*)

val dataCommit :
    txSkeleton
    -> contractId
    -> commitData
    -> txSkeleton `cost` 1110
let dataCommit txSkel cid args = // 14
    let! ({commitment = commitment}) = mkAssets cid args in // 968
    ret txSkel
    >>= TX.mint 1UL commitment               // 64
    >>= TX.lockToContract commitment 1UL cid // 64

val commit :
  contractId
  -> sender
  -> option data
  -> txSkeleton
  -> CR.t `cost` 1210
let commit cid sender msgBody txSkel = // 16
    let open RT in
    let! tx =
        begin match sender with
        | PK pk ->
            ret msgBody
            >>= parseCommitData pk                  // 81
            >>= (liftCost << dataCommit txSkel cid) // 1110
        | _ ->
            "Sender must be a public key"
            |> RT.incFailw 1191
        end
    in CR.ofResultTxSkel tx // 3



(*
-------------------------------------------------------------------------------
========== COMMAND: Attest ====================================================
-------------------------------------------------------------------------------
*)

val attestTx :
    assets
    -> contractId
    -> (w : wallet)
    -> lock
    -> txSkeleton
    -> txSkeleton `OT.t` (128 + (W.size w * 128 + 192) + 64 + 31)
let attestTx assets cid (w : wallet) (recipient : lock) txSkel = // 31
    let open OT in
    ret txSkel
    >>= (liftCost << TX.mint 1UL assets.attestation) // 64
    >>= (liftCost << TX.lockToAddress assets.attestation 1UL recipient) // 64
    >>= TX.fromWallet assets.commitment 1UL cid w // W.size w * 128 + 192
    >>= (liftCost << TX.lockToContract assets.commitment 1UL cid) // 64

val dataAttest' :
    contractId
    -> (w : wallet)
    -> txSkeleton
    -> attestData
    -> txSkeleton `RT.t` (968 + (0 + (128 + (W.size w * 128 + 192) + 64 + 31) + 0) + 16)
let dataAttest' cid w txSkel args = // 16
    let! assets = mkAssets cid args.commitData in // 968
    ret txSkel
    >>= attestTx assets cid w args.recipient
    >>= RT.ofOption "Could not spend from wallet"

val dataAttest :
    contractId
    -> (w : wallet)
    -> txSkeleton
    -> attestData
    -> txSkeleton `RT.t` (W.size w * 128 + 1405)
let dataAttest cid w txSkel args = // 16
    dataAttest' cid w txSkel args
    |> (fun x -> x <: txSkeleton `RT.t` (W.size w * 128 + 1399))

val attest :
    contractId ->
    (w : wallet) ->
    sender ->
    option data ->
    txSkeleton ->
    CR.t `cost` (W.size w * 128 + 2208)
let attest cid w sender msgBody txSkel = // 10
    let open RT in
    let! tx =
        parseAttestData sender msgBody // 790
        >>= dataAttest cid w txSkel // W.size w * 128 + 1405
    in CR.ofResultTxSkel tx // 3



(*
-------------------------------------------------------------------------------
========== MAIN ===============================================================
-------------------------------------------------------------------------------
*)

val main :
       txSkel  : txSkeleton
    -> context : context
    -> cid     : contractId
    -> command : string
    -> sender  : sender
    -> msgBody : option data
    -> w       : wallet
    -> state   : option data
    -> CR.t `cost`
        begin match command with
        | "Commit" -> 1218
        | "Attest" -> W.size w * 128 + 2216
        | _        -> 8
        end
let main txSkel _ cid command sender msgBody w _ = // 8
    begin match command with
    | "Commit" ->
        commit cid sender msgBody txSkel // 1210
        <: CR.t `cost`
            begin match command with
            | "Commit" -> 1210
            | "Attest" -> W.size w * 128 + 2208
            | _        -> 0
            end
    | "Attest" ->
        attest cid w sender msgBody txSkel // W.size w * 128 + 2208
    | _ ->
        RT.failw "Command not recognized"
    end

val cf :
       txSkel     : txSkeleton
    -> context    : context
    -> command    : string
    -> sender     : sender
    -> messageBody: option data
    -> w          : wallet
    -> state      : option data
    -> nat `cost` 10
let cf _ _ command _ _ w _ = // 10
    begin match command with
    | "Commit" -> 1218
    | "Attest" -> W.size w * 128 + 2216
    | _        -> 8
    end
    |> ret
