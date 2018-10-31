module AuthenticatedSupply

open Zen.Cost
open Zen.Data
open Zen.Types

module CR = Zen.ContractResult
module Dict = Zen.Dictionary
module RT = Zen.ResultT
module TX = Zen.TxSkeleton
module U64 = FStar.UInt64

// the public key that messages must be signed with
let authenticatedPubKey = ""

val getReturnAddress: option (Dict.t data) -> result lock `cost` 77
let getReturnAddress dict = // 11
    let! returnAddress = dict >!= Dict.tryFind "returnAddress" // 64
                              >?= tryLock in // 2
    match returnAddress with
    | Some returnAddress ->
        RT.ok returnAddress
    | None ->
        RT.failw "Message Body must include valid returnAddress"

val getAmount: option (Dict.t data) -> result U64.t `cost` 79
let getAmount dict = // 13
    let! orderTotal = dict >!= Dict.tryFind "Amount" // 64
                           >?= tryU64 in // 2
    match orderTotal with
    | Some 0UL ->
        RT.failw "Amount cannot be 0"
    | Some orderTotal ->
        RT.ok orderTotal
    | None ->
        RT.failw "Message Body must include valid Amount"

// checks that the sender matches the authenticatedPubKey
val authenticate: sender -> result bool `cost` 135
let authenticate sender = // 15
    let! authenticatedPubKey = Zen.Crypto.parsePublicKey authenticatedPubKey in // 120
    match authenticatedPubKey, sender with
    | Some authenticatedPubKey, PK senderPubKey ->
        RT.ok (authenticatedPubKey = senderPubKey)
    | None, _ ->
        RT.failw "Could not parse authenticatedPubKey. Please re-deploy this contract with a valid authenticatedPubKey."
    | _ ->
        RT.failw "Sender must sign with Public Key"

val issueTX: txSkeleton -> contractId -> U64.t -> lock -> bool -> CR.t `cost` 211
let issueTX tx contractID amount returnAddress authenticated = // 16
    if authenticated then begin
        let! asset = Zen.Asset.getDefault contractID in // 64
        // mint the required amount of the asset
        TX.mint amount asset tx // 64
        // lock to the return address
        >>= TX.lockToAddress asset amount returnAddress // 64
        >>= CR.ofTxSkel end // 3
    else
        RT.autoFailw "Authentication failed"

val issue: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 519
let issue tx contractID sender messageBody = // 13
    let! dict = messageBody >!= tryDict in // 4
    let amount = getAmount dict in // 79
    let returnAddress = getReturnAddress dict in // 77
    let authenticated = authenticate sender in // 135
    RT.bind3 amount returnAddress authenticated (issueTX tx contractID) // 211

val destroyTX: txSkeleton -> contractId -> U64.t -> bool -> CR.t `cost` 142
let destroyTX tx contractID amount authenticated = // 11
    if authenticated then begin
        let! asset = Zen.Asset.getDefault contractID in // 64
        // destroy the required amount of the asset
        TX.destroy amount asset tx // 64
        >>= CR.ofTxSkel end // 3
    else
        RT.autoFailw "Authentication failed"

val destroy: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 371
let destroy tx contractID sender messageBody = // 11
    let! dict = messageBody >!= tryDict in // 4
    let amount = getAmount dict in // 79
    let authenticated = authenticate sender in // 135
    RT.bind2 amount authenticated (destroyTX tx contractID) // 142

val main:
    txSkeleton
    -> context
    -> contractId
    -> command: string
    -> sender
    -> option data
    -> wallet
    -> option data
    -> CR.t `cost` begin match command with
                   | "Issue" -> 526
                   | "Destroy" -> 378
                   | _ -> 7 end
let main tx _ contractID command sender messageBody _ _ = // 7
    match command with
    | "Issue" -> issue tx contractID sender messageBody // 519
                 <: CR.t `cost` begin match command with
                                | "Issue" -> 519
                                | "Destroy" -> 371
                                | _ -> 0 end
    | "Destroy" -> destroy tx contractID sender messageBody // 371
    | _ -> RT.failw "Invalid Command"

val cf:
    txSkeleton
    -> context
    -> string
    -> sender
    -> option data
    -> wallet
    -> option data
    -> nat `cost` 4
let cf _ _ command _ _ _ _ = // 4
    ret begin match command with
        | "Issue" -> 526
        | "Destroy" -> 378
        | _ -> 7 end
