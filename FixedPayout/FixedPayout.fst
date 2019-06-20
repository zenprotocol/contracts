module FixedPayout

open Zen.Base
open Zen.Cost
open Zen.Types
open Zen.Data

module U8    = FStar.UInt8
module U64   = FStar.UInt64
module RT    = Zen.ResultT
module Dict  = Zen.Dictionary
module Sha3  = Zen.Hash.Sha3
module TX    = Zen.TxSkeleton
module CR    = Zen.ContractResult
module Asset = Zen.Asset
module Opt   = Zen.Option
module OptT  = Zen.OptionT
module Array = Zen.Array
module Str   = FStar.String
module CId   = Zen.ContractId
module SMT   = Zen.SparseMerkleTree

type ticker = s:string {Str.length s <= 4}

// compressed public key
type cpk = byte ** hash

type preAuditPath = p: list data{length p == 256}

type auditPath = p: list hash{length p == 256}

type attestation = {
    timestamp  : timestamp;
    commit     : hash;
    pubKey     : publicKey;
}

type position =
    | Bull
    | Bear

type event = {
    oraclePubKey     : publicKey;
    oracleContractId : contractId;
    ticker           : ticker;
    priceLow         : U64.t;
    priceHigh        : option U64.t;
    timeLow          : U64.t;
    timeHigh         : option U64.t;
}

type bet = {
    event    : event;
    position : position;
}

type proof = {
    key         : ticker;
    value       : U64.t;
    root        : hash;
    auditPath   : auditPath;
    cwt         : string;
    defaultHash : hash;
}

type redemption = {
    bet         : bet;
    attestation : attestation;
    proof       : proof;
}



(*
-------------------------------------------------------------------------------
========== UTILITY FUNCTIONS ==================================================
-------------------------------------------------------------------------------
*)

// tries to map a function over a list.
// if all of the mappings return Some, then returns Some list.
// otherwise returns None.
val tryMap(#a #b: Type)(#n: nat):
  (a -> option b `cost` n)
  -> ls:list a
  -> option (ls':list b{length ls' == length ls}) `cost` (length ls * n)
let rec tryMap #a #b #n f ls = //20
    match ls with
    | hd::tl ->
        let! hd' = f hd in
        let! tl' = tryMap f tl in
        begin match hd', tl' with
        | Some hd', Some tl' ->
            let (result: list b{length result == length ls}) = hd'::tl' in
            OptT.some result
        | _ -> OptT.none
        end
    | [] -> [] |> OptT.incSome (length ls * n)

let runOpt (#a #s:Type) (#m:nat) (update:a -> s -> s `cost` m) (x:option a) (st:s): s `cost` m =
    Opt.maybe (incRet m) update x st

// compress a public key
let compress (pk:publicKey): cpk `cost` 132 =
    let open U8 in
    let parity = (Array.item 32 pk %^ 2uy) +^ 2uy in
    let aux (i:nat{i < 32}): byte `cost` 0 =
        ret (Array.item (31-i) pk) in
    let! x = Array.init_pure 32 aux in
    ret (parity, x)

let updateCPK ((parity, h):cpk) (s:Sha3.t): Sha3.t `cost` 198 =
    ret s
    >>= Sha3.updateByte parity
    >>= Sha3.updateHash h

// hash a compressed publicKey
let hashCPK (cpk:cpk): hash `cost` 218 =
    ret Sha3.empty
    >>= updateCPK cpk
    >>= Sha3.finalize

val lockToPubKey: asset -> U64.t -> publicKey -> txSkeleton -> txSkeleton `cost` 414
let lockToPubKey asset amount pk tx =
    let! cpk = compress pk in
    let! cpkHash = hashCPK cpk in
    TX.lockToPubKey asset amount cpkHash tx

val lockToSender: asset -> U64.t -> sender -> txSkeleton -> txSkeleton `cost` 414
let lockToSender asset amount sender =
    match sender with
    | PK pk ->
        lockToPubKey asset amount pk
    | Contract cid ->
        TX.lockToContract asset amount cid
        >> inc 350
    | Anonymous ->
        incRet 414



(*
-------------------------------------------------------------------------------
========== DATA PARSING =======================================================
-------------------------------------------------------------------------------
*)

val parseDict: option data -> result (option (Dict.t data)) `cost` 4
let parseDict data =
    match data with
    | Some data ->
        data
        |> tryDict
        |> RT.ofOptionT "Data parsing failed - the message body isn't a dictionary"
        |> RT.map Some
    | None ->
        RT.incFailw 4 "Data parsing failed - the message body is empty"

val parseField (#a:Type) (#m:nat)
    : (data -> option a `cost` m)
    -> fieldName:string
    -> errMsg:string
    -> option (Dict.t data)
    -> result a `cost` (m + 64)
let parseField #_ #_ parser fieldName errMsg dict =
    let! value = dict >!= Dict.tryFind fieldName >?= parser in
    match value with
    | Some value ->
        RT.ok value
    | None ->
        RT.failw errMsg

val parseOptField (#a:Type) (#m:nat)
    : (data -> option a `cost` m)
    -> fieldName:string
    -> option (Dict.t data)
    -> result (option a) `cost` (m + 64)
let parseOptField #_ #_ parser fieldName dict =
    dict
    >!= Dict.tryFind fieldName
    >?= parser
    >>= RT.ok

val parseTicker: string -> string -> option (Dict.t data) -> result ticker `cost` 66
let parseTicker fieldName errMsg dict =
    let open RT in
    parseField tryString fieldName errMsg dict >>=
    begin fun s ->
        if Str.length s <= 4
            then RT.ok (s <: ticker)
            else RT.failw "Ticker size can't be bigger than 4"
    end

val extractHashes: string -> ls:list data -> result (ls':list hash {length ls' == length ls}) `cost` (length ls * 2)
let extractHashes errMsg ls =
    tryMap tryHash ls
    |> RT.ofOptionT errMsg

val extractAuditPath: preAuditPath -> result auditPath `cost` 512
let extractAuditPath ls =
    let open RT in
    extractHashes "All the items in the audit path must be hashes" ls
    $> (fun xs -> let (xs:list hash {length xs == 256}) = xs in xs)

val parsePreAuditPath: string -> string -> option (Dict.t data) -> result preAuditPath `cost` 68
let parsePreAuditPath fieldName errMsg dict =
    let open RT in
    parseField tryList fieldName errMsg dict >>=
    begin fun ls ->
        if length ls = 256
            then RT.ok (ls <: preAuditPath)
            else RT.failw "AuditPath length must be 256"
    end

val parseAuditPath: string -> string -> option (Dict.t data) -> result auditPath `cost` 580
let parseAuditPath fieldName errMsg =
    let open RT in
    parsePreAuditPath fieldName errMsg
    >=> extractAuditPath

val parsePosition: string -> string -> option (Dict.t data) -> result position `cost` 66
let parsePosition fieldName errMsg dict =
    let open RT in
    parseField tryString fieldName errMsg dict >>=
    begin function
    | "Bull" -> ret Bull
    | "Bear" -> ret Bear
    | _      -> RT.failw "Position must be either Bull or Bear"
    end

val parseContractId: string -> string -> option (Dict.t data) -> result contractId `cost` 130
let parseContractId fieldName errMsg dict =
    let open RT in
    parseField tryString fieldName errMsg dict >>=
    begin fun s ->
        if Str.length s = 72
            then
                let (s:string { Str.length s = 72 }) = s in
                s
                |> CId.parse
                |> RT.ofOptionT "The given OracleContractId is not a valid contractId"
            else
                RT.incFailw 64 "OracleContractId must be 72 characters long"
    end

let getTimestamp        = parseField      tryU64       "Timestamp"        "Could not parse Timestamp"
let getCommit           = parseField      tryHash      "Commit"           "Could not parse Commit"
let getOraclePubKey     = parseField      tryPublicKey "OraclePubKey"     "Could not parse OraclePubKey"
let getTicker           = parseTicker                  "Ticker"           "Could not parse Ticker"
let getPriceLow         = parseField      tryU64       "PriceLow"         "Could not parse PriceLow"
let getPriceHigh        = parseOptField   tryU64       "PriceHigh"
let getTimeLow          = parseField      tryU64       "TimeLow"          "Could not parse TimeLow"
let getTimeHigh         = parseOptField   tryU64       "TimeHigh"
let getAuditPath        = parseAuditPath               "AuditPath"        "Could not parse AuditPath"
let getValue            = parseField      tryU64       "Value"            "Could not parse Value"
let getCWT              = parseField      tryString    "CWT"              "Could not parse CWT"
let getDefaultHash      = parseField      tryHash      "DefaultHash"      "Could not parse DefaultHash"
let getPosition         = parsePosition                "Position"         "Could not parse Position"
let getOracleContractId = parseContractId              "OracleContractId" "Could not parse OracleContractId"

val parseProof: option (Dict.t data) -> result proof `cost` 910
let parseProof dict =
    let open RT in
    dict |> getTicker      >>= (fun key         ->
    dict |> getValue       >>= (fun value       ->
    dict |> getCommit      >>= (fun root        ->
    dict |> getAuditPath   >>= (fun auditPath   ->
    dict |> getCWT         >>= (fun cwt         ->
    dict |> getDefaultHash >>= (fun defaultHash ->
        RT.ok ({
            key         = key;
            value       = value;
            root        = root;
            auditPath   = auditPath;
            cwt         = cwt;
            defaultHash = defaultHash;
        })))))))

val parseAttestation: option (Dict.t data) -> result attestation `cost` 198
let parseAttestation dict =
    let open RT in
    dict |> getTimestamp    >>= (fun timestamp ->
    dict |> getCommit       >>= (fun commit    ->
    dict |> getOraclePubKey >>= (fun pubKey    ->
        RT.ok ({
            timestamp = timestamp;
            commit    = commit;
            pubKey    = pubKey;
        }))))

val parseEvent: option (Dict.t data) -> result event `cost` 526
let parseEvent dict =
    let open RT in
    dict |> getOraclePubKey     >>= (fun oraclePubKey     ->
    dict |> getOracleContractId >>= (fun oracleContractId ->
    dict |> getTicker           >>= (fun ticker           ->
    dict |> getPriceLow         >>= (fun priceLow         ->
    dict |> getPriceHigh        >>= (fun priceHigh        ->
    dict |> getTimeLow          >>= (fun timeLow          ->
    dict |> getTimeHigh         >>= (fun timeHigh         ->
        RT.ok ({
            oraclePubKey     = oraclePubKey;
            oracleContractId = oracleContractId;
            ticker           = ticker;
            priceLow         = priceLow;
            priceHigh        = priceHigh;
            timeLow          = timeLow;
            timeHigh         = timeHigh;
        }))))))))

val parseRedemption: option (Dict.t data) -> result redemption `cost` 1700
let parseRedemption dict =
    let open RT in
    dict |> parseEvent       >>= (fun event       ->
    dict |> getPosition      >>= (fun position    ->
    dict |> parseAttestation >>= (fun attestation ->
    dict |> parseProof       >>= (fun proof       ->
        RT.ok ({
            bet         = {
                event    = event;
                position = position;
            };
            attestation = attestation;
            proof       = proof;
        })))))



(*
-------------------------------------------------------------------------------
========== TOKENIZATION =======================================================
-------------------------------------------------------------------------------
*)

let updatePublicKey (pk:publicKey) (s:Sha3.t): Sha3.t `cost` 330 =
    let! cpk = compress pk in
    ret s
    >>= updateCPK cpk

// Sha3.updateString with a constant cost
let updateTicker (tick:ticker) (s:Sha3.t): Sha3.t `cost` 24 =
    ret s
    >>= Sha3.updateString tick
    >>= incRet (6 * (4 - Str.length tick))

let updateContractId ((v,h):contractId) (s:Sha3.t): Sha3.t `cost` 216 =
    ret s
    >>= Sha3.updateU32 v
    >>= Sha3.updateHash h

let updateEvent (event:event) (s:Sha3.t): Sha3.t `cost` 762 =
    ret s
    >>= updatePublicKey         event.oraclePubKey
    >>= updateContractId        event.oracleContractId
    >>= updateTicker            event.ticker
    >>= Sha3.updateU64          event.priceLow
    >>= Sha3.updateU64 `runOpt` event.priceHigh
    >>= Sha3.updateU64          event.timeLow
    >>= Sha3.updateU64 `runOpt` event.timeHigh

let updatePosition (position:position) (s:Sha3.t): Sha3.t `cost` 24 =
    ret s
    >>= Sha3.updateString
    begin match position with
    | Bull -> "Bull"
    | Bear -> "Bear"
    end

let hashCommitment (attestation:attestation): hash `cost` 590 =
    ret Sha3.empty
    >>= Sha3.updateHash attestation.commit
    >>= Sha3.updateU64  attestation.timestamp
    >>= updatePublicKey attestation.pubKey
    >>= Sha3.finalize

let hashAttestation (attestation:attestation): hash `cost` 802 =
    let! commit = hashCommitment attestation in
    ret Sha3.empty
    >>= Sha3.updateHash commit
    >>= Sha3.finalize

let hashBet (bet:bet): hash `cost` 806 =
    ret Sha3.empty
    >>= updateEvent    bet.event
    >>= updatePosition bet.position
    >>= Sha3.finalize

let mkBetToken ((v, h):contractId) (bet:bet): asset `cost` 806 =
    let! betHash = hashBet bet in
    ret (v, h, betHash)

let mkAttestToken ((v,h):contractId) (attestation:attestation): asset `cost` 802 =
    let! attestHash = hashAttestation attestation in
    ret (v, h, attestHash)



(*
-------------------------------------------------------------------------------
========== VALIDATION =========================================================
-------------------------------------------------------------------------------
*)

val inBounds : U64.t -> option U64.t -> U64.t -> bool `cost` 0
let inBounds low high value =
    (low `U64.lte` value && Opt.maybe true (U64.lte value) high)
    |> ret

val validateTime: redemption -> result redemption `cost` 0
let validateTime redemption =
    let  event = redemption.bet.event             in
    let  low   = event.timeLow                    in
    let  high  = event.timeHigh                   in
    let  value = redemption.attestation.timestamp in
    let! inb   = inBounds low high value          in
    if inb
        then RT.ok redemption
        else RT.failw "Attestation time is not within the given time bounds"

val validatePrice: redemption -> result redemption `cost` 0
let validatePrice redemption =
    let  event = redemption.bet.event    in
    let  low   = event.priceLow          in
    let  high  = event.priceHigh         in
    let  value = redemption.proof.value  in
    let! inb   = inBounds low high value in
    let  pos   = redemption.bet.position in
    match inb, pos with
    | true , Bull
    | false, Bear ->
        RT.ok redemption
    | _ ->
        RT.failw "Position doesn't match the event"

val hashKey : ticker -> hash `cost` 44
let hashKey s =
    Sha3.ofString s
    |> inc (24 - (6 * Str.length s))

val verifyAuditPath : proof -> bool `cost` 107568
let verifyAuditPath proof =
    let! key   = hashKey proof.key            in
    let  value = SMT.serializeU64 proof.value in
    SMT.verify proof.cwt proof.defaultHash proof.root proof.auditPath key (Some value)

val validateAuditPath: redemption -> result redemption `cost` 107568
let validateAuditPath redemption =
    let! b = verifyAuditPath redemption.proof in
    if b
        then RT.ok redemption
        else RT.failw "Invalid audit path"

val validateRedemption: redemption -> result redemption `cost` 107568
let validateRedemption =
    let open RT in
    ret
    >=> validateTime
    >=> validatePrice
    >=> validateAuditPath

(*
-------------------------------------------------------------------------------
========== COMMAND: Buy  ======================================================
-------------------------------------------------------------------------------
*)

val buyEvent: txSkeleton -> contractId -> sender -> event -> CR.t `cost` 2635
let buyEvent txSkel contractId sender event =
    let! bullToken = mkBetToken contractId ({event=event; position=Bull}) in
    let! bearToken = mkBetToken contractId ({event=event; position=Bear}) in
    let! m         = TX.getAvailableTokens Asset.zenAsset txSkel          in
    ret txSkel
    >>= TX.mint m bullToken
    >>= TX.mint m bearToken
    >>= lockToSender bullToken m sender
    >>= lockToSender bearToken m sender
    >>= CR.ofTxSkel

val buy: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 3165
let buy txSkel contractId sender =
    let open RT in
    ret
    >=> parseDict
    >=> parseEvent
    >=> buyEvent txSkel contractId sender



(*
-------------------------------------------------------------------------------
========== COMMAND: Redeem ====================================================
-------------------------------------------------------------------------------
*)

val redeemRedemption: txSkeleton -> contractId -> sender -> redemption -> CR.t `cost` 2217
let redeemRedemption txSkel contractId sender redemption =
    let! betToken         = mkBetToken contractId redemption.bet       in
    let  oracleContractId = redemption.bet.event.oracleContractId      in
    let  attestation      = redemption.attestation                     in
    let! attestToken      = mkAttestToken oracleContractId attestation in
    let! m                = TX.getAvailableTokens betToken txSkel      in
    ret txSkel
    >>= TX.destroy m betToken
    >>= TX.destroy 1UL attestToken
    >>= lockToSender Asset.zenAsset m sender
    >>= CR.ofTxSkel

val redeem: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 111489
let redeem txSkel contractId sender =
    let open RT in
    ret
    >=> parseDict
    >=> parseRedemption
    >=> validateRedemption
    >=> redeemRedemption txSkel contractId sender

(*
-------------------------------------------------------------------------------
========== MAIN ===============================================================
-------------------------------------------------------------------------------
*)

val main:
       txSkel      : txSkeleton
    -> context     : context
    -> contractId  : contractId
    -> command     : string
    -> sender      : sender
    -> messageBody : option data
    -> wallet      : wallet
    -> state       : option data
    -> CR.t `cost` 111489
let main txSkel context contractId command sender messageBody wallet state =
    match command with
    | "Buy" ->
        buy txSkel contractId sender messageBody
        |> inc 108324
    | "Redeem" ->
        redeem txSkel contractId sender messageBody
    | _ ->
        RT.failw "Unsupported command"
        |> inc 111489
