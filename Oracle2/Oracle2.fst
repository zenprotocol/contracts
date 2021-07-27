module Oracle2

open Zen.Base
open Zen.Cost
open Zen.Types
open Zen.Data

module CR = Zen.ContractResult
module Dict = Zen.Dictionary
module OT = Zen.OptionT
module RT = Zen.ResultT
module Sha3 = Zen.Hash.Sha3
module TX = Zen.TxSkeleton
module W = Zen.Wallet
module U64 = FStar.UInt64
module Asset = Zen.Asset


type commitData =
    { commit       : hash
    ; oraclePubKey : publicKey
    ; feeAsset     : asset
    ; feeAmount    : option U64.t
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

val parseDict: option data -> result (Dict.t data) `cost` 12
let parseDict data = // 8
    match data with
    | Some data ->
        data
        |> tryDict // 4
        |> RT.ofOptionT "Data parsing failed - the message body isn't a dictionary"
    | None ->
        RT.incFailw 4 "Data parsing failed - the message body is empty"

val parseFeeAsset : Dict.t data -> asset `cost` 145
let parseFeeAsset dict  = // 10
    Dict.tryFind "FeeAsset" dict // 64
    >?= tryString // 2
    >?= Asset.parse // 64
    >>= OT.maybeT Asset.zenAsset ret // 5

val parseFeeAmount : Dict.t data -> option U64.t `cost` 70
let parseFeeAmount dict = // 4
    Dict.tryFind "FeeAmount" dict // 64
    >?= tryU64 // 2

val parseCommit : Dict.t data -> result hash `cost` 73
let parseCommit dict = // 7
    Dict.tryFind "Commit" dict // 64
    >?= tryHash // 2
    |> RT.ofOptionT "Could not parse Commit"

val parseCommitData : publicKey -> option data -> commitData `RT.t` 318
let parseCommitData sender msgBody = // (3 + 3 + 12)
    let open RT in
    parseDict msgBody >>= (fun dict   -> // 12
    parseCommit dict  >>= (fun commit -> // 73
        let! feeAsset  = parseFeeAsset  dict in // 145
        let! feeAmount = parseFeeAmount dict in // 70
        { commit       = commit
        ; oraclePubKey = sender
        ; feeAsset     = feeAsset
        ; feeAmount    = feeAmount
        } |> ret
    ))

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

val parseAttestData : sender -> option data -> attestData `RT.t` 1013
let parseAttestData sender msgBody = // 55
    begin match msgBody with
    | Some (Collection (Dict dict)) ->
        let! commit       = Dict.tryFind "Commit"       dict in // 64
        let! oraclePubKey = Dict.tryFind "OraclePubKey" dict in // 64
        let! recipient    = Dict.tryFind "Recipient"    dict in // 64
        let! feeAsset     = parseFeeAsset               dict in // 145
        let! feeAmount    = parseFeeAmount              dict in // 70
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
                        ; feeAsset     = feeAsset
                        ; feeAmount    = feeAmount
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
                            ; feeAsset     = feeAsset
                            ; feeAmount    = feeAmount
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

val hashCommitData : commitData -> hash `cost` 1179
let hashCommitData data = // 25
    let! cpk = compress data.oraclePubKey in // 305
    match data.feeAmount with
    | None ->
        ret Sha3.empty
        >>= Sha3.updateHash data.commit // 192
        >>= updateCPK cpk               // 205
        >>= Sha3.finalize               // 20
        |> inc 432
    | Some amount ->
        ret Sha3.empty
        >>= Sha3.updateHash data.commit    // 192
        >>= updateCPK cpk                  // 205
        >>= Sha3.updateAsset data.feeAsset // 384
        >>= Sha3.updateU64 amount          // 48
        >>= Sha3.finalize                  // 20

val hashAttestData : commitData -> hash `cost` 736
let hashAttestData data = // 14
    let! cpk = compress data.oraclePubKey in // 305
    ret Sha3.empty
    >>= Sha3.updateHash data.commit // 192
    >>= updateCPK cpk               // 205
    >>= Sha3.finalize               // 20

val mkAssets : contractId -> commitData -> assets `cost` 2150
let mkAssets (v, h) data = // 23
    let! commitHash =
        hashCommitData data in // 1179
    let! attestHash1 = 
        hashAttestData data in // 736
    let! attestHash2 =
        ret Sha3.empty
        >>= Sha3.updateHash attestHash1 // 192
        >>= Sha3.finalize in            // 20
    ret ({ commitment=v,h,commitHash; attestation=v,h,attestHash2 })



(*
-------------------------------------------------------------------------------
========== COMMAND: Commit ====================================================
-------------------------------------------------------------------------------
*)

val dataCommit :
    txSkeleton
    -> contractId
    -> commitData
    -> txSkeleton `cost` 2292
let dataCommit txSkel cid args = // 14
    let! ({commitment = commitment}) = mkAssets cid args in // 2150
    ret txSkel
    >>= TX.mint 1UL commitment               // 64
    >>= TX.lockToContract commitment 1UL cid // 64

val commit :
  contractId
  -> sender
  -> option data
  -> txSkeleton
  -> CR.t `cost` 2629
let commit cid sender msgBody txSkel = // 16
    let open RT in
    let! tx =
        begin match sender with
        | PK pk ->
            ret msgBody
            >>= parseCommitData pk                  // 318
            >>= (liftCost << dataCommit txSkel cid) // 2292
        | _ ->
            "Sender must be a public key"
            |> RT.incFailw 2610
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
    -> attestData
    -> txSkeleton
    -> txSkeleton `OT.t` (0 + 64 + 64 + (Zen.Wallet.size w * 128 + 192) + 64 + 610 + 48)
let attestTx assets cid (w : wallet) (data : attestData) txSkel = // 48
    let open OT in
    ret txSkel
    >>= (liftCost << TX.mint 1UL assets.attestation) // 64
    >>= (liftCost << TX.lockToAddress assets.attestation 1UL data.recipient) // 64
    >>= TX.fromWallet assets.commitment 1UL cid w // W.size w * 128 + 192
    >>= (liftCost << TX.lockToContract assets.commitment 1UL cid) // 64
    >>= begin match data.commitData.feeAmount with
        | Some amount ->
            TX.lockToPublicKey data.commitData.feeAsset amount data.commitData.oraclePubKey // 610
            >> liftCost
        | None ->
            incRet 610
            >> liftCost
        end

val dataAttest' :
    contractId
    -> (w : wallet)
    -> txSkeleton
    -> attestData
    -> txSkeleton `RT.t` (2150 + (0 + (0 + 64 + 64 + (W.size w * 128 + 192) + 64 + 610 + 48) + 0) + 15)
let dataAttest' cid w txSkel data = // 16
    let! assets = mkAssets cid data.commitData in // 2150
    ret txSkel
    >>= attestTx assets cid w data // ...
    >>= RT.ofOption "Could not spend from wallet"

val dataAttest :
    contractId
    -> (w : wallet)
    -> txSkeleton
    -> attestData
    -> txSkeleton `RT.t` (W.size w * 128 + 3213)
let dataAttest cid w txSkel data = // 6
    dataAttest' cid w txSkel data
    |> (fun x -> x <: txSkeleton `RT.t` (W.size w * 128 + 3207))

val attest :
    contractId ->
    (w : wallet) ->
    sender ->
    option data ->
    txSkeleton ->
    CR.t `cost` (W.size w * 128 + 4239)
let attest cid w sender msgBody txSkel = // 10
    let open RT in
    let! tx =
        parseAttestData sender msgBody // 1013
        >>= dataAttest cid w txSkel // W.size w * 128 + 3213
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
        | "Commit" -> 2637
        | "Attest" -> W.size w * 128 + 4247
        | _        -> 8
        end
let main txSkel _ cid command sender msgBody w _ = // 8
    begin match command with
    | "Commit" ->
        commit cid sender msgBody txSkel // 2629
        <: CR.t `cost`
            begin match command with
            | "Commit" -> 2629
            | "Attest" -> W.size w * 128 + 4239
            | _        -> 0
            end
    | "Attest" ->
        attest cid w sender msgBody txSkel // W.size w * 128 + 4239
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
    | "Commit" -> 2637
    | "Attest" -> W.size w * 128 + 4247
    | _        -> 8
    end
    |> ret
