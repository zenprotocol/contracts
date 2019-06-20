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

type parser (a:Type) (m:nat) =
    option (Dict.t data) -> result a `cost` m

type hashUpdate (a:Type) (m:nat) =
    a -> Sha3.t -> Sha3.t `cost` m

type ticker =
    s:string { Str.length s <= 4 }

// compressed public key
type cpk =
    byte ** hash

type preAuditPath =
    p: list data { length p == 256 }

type auditPath =
    p: list hash { length p == 256 }

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
    -> option (ls':list b{length ls' == length ls}) `cost` (length ls * (n + 20) + 20)
let rec tryMap #a #b #n f ls = //20
    match ls with
    | hd::tl ->
        let! hd' = f hd in
        let! tl' = tryMap f tl in
        begin match hd', tl' with
        | Some hd', Some tl' ->
            let (result: list b{length result == length ls}) = hd'::tl' in
            OptT.some result
        | _ -> OptT.none end
    | [] -> [] |> OptT.incSome (length ls * (n + 20))


let runOpt (#a #s:Type) (#m:nat) (update:a -> s -> s `cost` m) (x:option a) (st:s): s `cost` (m + 5) =
    Opt.maybe (incRet m) update x st

// compress a public key
let compress (pk:publicKey): cpk `cost` 305 = // 13
    let open U8 in
    let aux (i : nat { i < 32 }) : byte `cost` 5 =
        ret (Array.item (31 - i) pk)
    in
    let  parity = (Array.item 32 pk %^ 2uy) +^ 2uy in
    let! x      = Array.init_pure 32 aux           in // 292
    ret (parity, x)

let updateCPK ((parity, h):cpk) (s:Sha3.t): Sha3.t `cost` 205 = // 7
    ret s
    >>= Sha3.updateByte parity // 6
    >>= Sha3.updateHash h // 192

// hash a compressed publicKey
let hashCPK (cpk:cpk): hash `cost` 231 = // 6
    ret Sha3.empty
    >>= updateCPK cpk // 205
    >>= Sha3.finalize // 20

val lockToPubKey: asset -> U64.t -> publicKey -> txSkeleton -> txSkeleton `cost` 610
let lockToPubKey asset amount pk tx = // 10
    let! cpk     = compress pk in // 305
    let! cpkHash = hashCPK cpk in // 231
    TX.lockToPubKey asset amount cpkHash tx // 64

val lockToSender: asset -> U64.t -> sender -> txSkeleton -> txSkeleton `cost` 623
let lockToSender asset amount sender txSkel = // 13
    match sender with
    | PK pk ->
        txSkel
        |> lockToPubKey asset amount pk // 610
    | Contract cid ->
        txSkel
        |> TX.lockToContract asset amount cid // 64
        |> inc 546
    | Anonymous ->
        txSkel
        |> incRet 610



(*
-------------------------------------------------------------------------------
========== DATA PARSING =======================================================
-------------------------------------------------------------------------------
*)

val parseDict: option data -> result (option (Dict.t data)) `cost` 15
let parseDict data = // 11
    match data with
    | Some data ->
        data
        |> tryDict // 4
        |> RT.ofOptionT "Data parsing failed - the message body isn't a dictionary"
        |> RT.map Some
    | None ->
        RT.incFailw 4 "Data parsing failed - the message body is empty"

val parseField (#a:Type) (#m:nat)
    : (data -> option a `cost` m)
    -> fieldName:string
    -> errMsg:string
    -> option (Dict.t data)
    -> result a `cost` (m + 75)
let parseField #_ #_ parser fieldName errMsg dict = // 11
    let! value = dict >!= Dict.tryFind fieldName >?= parser in // (m + 64)
    match value with
    | Some value ->
        RT.ok value
    | None ->
        RT.failw errMsg

val parseOptField (#a:Type) (#m:nat)
    : (data -> option a `cost` m)
    -> fieldName:string
    -> option (Dict.t data)
    -> result (option a) `cost` (m + 71)
let parseOptField #_ #_ parser fieldName dict = // 7
    dict
    >!= Dict.tryFind fieldName // 64
    >?= parser // m
    >>= RT.ok

val parseTicker: string -> string -> option (Dict.t data) -> result ticker `cost` 90
let parseTicker fieldName errMsg dict = // 6
    let open RT in
    parseField tryString fieldName errMsg dict >>= // 77
    begin fun s -> // 7
        if Str.length s <= 4
            then RT.ok (s <: ticker)
            else RT.failw "Ticker size can't be bigger than 4"
    end

//val extractHashes: string -> ls:list data -> result (ls':list hash { length ls' == length ls }) `cost` (length ls * 22 + 25)
let extractHashes errMsg ls = // 5
    tryMap tryHash ls // (length ls * 22 + 20)
    |> RT.ofOptionT errMsg

val extractAuditPath: preAuditPath -> result auditPath `cost` 5661
let extractAuditPath ls = // 4
    let open RT in
    extractHashes "All the items in the audit path must be hashes" ls // 5657 = 256 * 22 + 25
    $> (fun xs -> let (xs:list hash { length xs == 256 }) = xs in xs)

val parsePreAuditPath: string -> string -> option (Dict.t data) -> result preAuditPath `cost` 92
let parsePreAuditPath fieldName errMsg dict = // 6
    let open RT in
    parseField tryList fieldName errMsg dict >>= // 79
    begin fun ls -> // 7
        if length ls = 256
            then RT.ok (ls <: preAuditPath)
            else RT.failw "AuditPath length must be 256"
    end

val parseAuditPath: string -> string -> option (Dict.t data) -> result auditPath `cost` 5760
let parseAuditPath fieldName errMsg dict = // 7
    let open RT in
    ret dict
    >>= parsePreAuditPath fieldName errMsg // 92
    >>= extractAuditPath // 5661

val parsePosition: string -> string -> option (Dict.t data) -> result position `cost` 87
let parsePosition fieldName errMsg dict = // 6
    let open RT in
    parseField tryString fieldName errMsg dict >>= // 77
    begin fun s -> match s with // 4
    | "Bull" -> ret Bull
    | "Bear" -> ret Bear
    | _      -> RT.failw "Position must be either Bull or Bear"
    end

val parseContractId: string -> string -> option (Dict.t data) -> result contractId `cost` 158
let parseContractId fieldName errMsg dict = // 6
    let open RT in
    parseField tryString fieldName errMsg dict >>= // 77
    begin fun s -> // 11
        if Str.length s = 72
            then
                let (s:string { Str.length s = 72 }) = s in
                s
                |> CId.parse // 64
                |> RT.ofOptionT "The given OracleContractId is not a valid contractId"
            else
                RT.incFailw 64 "OracleContractId must be 72 characters long"
    end

val getTimestamp        : parser U64.t          82
val getCommit           : parser hash           82
val getOraclePubKey     : parser publicKey      82
val getTicker           : parser ticker         94
val getPriceLow         : parser U64.t          82
val getPriceHigh        : parser (option U64.t) 77
val getTimeLow          : parser U64.t          82
val getTimeHigh         : parser (option U64.t) 77
val getAuditPath        : parser auditPath      5764
val getValue            : parser U64.t          82
val getCWT              : parser string         82
val getDefaultHash      : parser hash           82
val getPosition         : parser position       91
val getOracleContractId : parser contractId     162

let getTimestamp        dict = dict |>
    parseField      tryU64       "Timestamp"        "Could not parse Timestamp"
let getCommit           dict = dict |>
    parseField      tryHash      "Commit"           "Could not parse Commit"
let getOraclePubKey     dict = dict |>
    parseField      tryPublicKey "OraclePubKey"     "Could not parse OraclePubKey"
let getTicker           dict = dict |>
    parseTicker                  "Ticker"           "Could not parse Ticker"
let getPriceLow         dict = dict |>
    parseField      tryU64       "PriceLow"         "Could not parse PriceLow"
let getPriceHigh        dict = dict |>
    parseOptField   tryU64       "PriceHigh"
let getTimeLow          dict = dict |>
    parseField      tryU64       "TimeLow"          "Could not parse TimeLow"
let getTimeHigh         dict = dict |>
    parseOptField   tryU64       "TimeHigh"
let getAuditPath        dict = dict |>
    parseAuditPath               "AuditPath"        "Could not parse AuditPath"
let getValue            dict = dict |>
    parseField      tryU64       "Value"            "Could not parse Value"
let getCWT              dict = dict |>
    parseField      tryString    "CWT"              "Could not parse CWT"
let getDefaultHash      dict = dict |>
    parseField      tryHash      "DefaultHash"      "Could not parse DefaultHash"
let getPosition         dict = dict |>
    parsePosition                "Position"         "Could not parse Position"
let getOracleContractId dict = dict |>
    parseContractId              "OracleContractId" "Could not parse OracleContractId"

val parseProof: option (Dict.t data) -> result proof `cost` 6217
let parseProof dict = // 31
    let open RT in
    dict |> getTicker      >>= (fun key         -> // 94
    dict |> getValue       >>= (fun value       -> // 82
    dict |> getCommit      >>= (fun root        -> // 82
    dict |> getAuditPath   >>= (fun auditPath   -> // 5764
    dict |> getCWT         >>= (fun cwt         -> // 82
    dict |> getDefaultHash >>= (fun defaultHash -> // 82
        RT.ok ({
            key         = key;
            value       = value;
            root        = root;
            auditPath   = auditPath;
            cwt         = cwt;
            defaultHash = defaultHash;
        })))))))

val parseAttestation: option (Dict.t data) -> result attestation `cost` 262
let parseAttestation dict = // 16
    let open RT in
    dict |> getTimestamp    >>= (fun timestamp -> // 82
    dict |> getCommit       >>= (fun commit    -> // 82
    dict |> getOraclePubKey >>= (fun pubKey    -> // 82
        RT.ok ({
            timestamp = timestamp;
            commit    = commit;
            pubKey    = pubKey;
        }))))

val parseEvent: option (Dict.t data) -> result event `cost` 692
let parseEvent dict = // 36
    let open RT in
    dict |> getOraclePubKey     >>= (fun oraclePubKey     -> // 82
    dict |> getOracleContractId >>= (fun oracleContractId -> // 162
    dict |> getTicker           >>= (fun ticker           -> // 94
    dict |> getPriceLow         >>= (fun priceLow         -> // 82
    dict |> getPriceHigh        >>= (fun priceHigh        -> // 77
    dict |> getTimeLow          >>= (fun timeLow          -> // 82
    dict |> getTimeHigh         >>= (fun timeHigh         -> // 77
        RT.ok ({
            oraclePubKey     = oraclePubKey;
            oracleContractId = oracleContractId;
            ticker           = ticker;
            priceLow         = priceLow;
            priceHigh        = priceHigh;
            timeLow          = timeLow;
            timeHigh         = timeHigh;
        }))))))))

val parseRedemption: option (Dict.t data) -> result redemption `cost` 7284
let parseRedemption dict = // 22
    let open RT in
    dict |> parseEvent       >>= (fun event       -> // 692
    dict |> getPosition      >>= (fun position    -> // 91
    dict |> parseAttestation >>= (fun attestation -> // 262
    dict |> parseProof       >>= (fun proof       -> // 6217
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

val updatePublicKey : hashUpdate publicKey 517
let updatePublicKey pk s = // 7
    let! cpk = compress pk in // 305
    ret s
    >>= updateCPK cpk // 205

// Sha3.updateString with a constant cost
val updateTicker : hashUpdate ticker 36
let updateTicker tick s = // 12
    ret s
    >>= Sha3.updateString tick // (6 * Str.length tick)
    >>= incRet (6 * (4 - Str.length tick))

val updateContractId : hashUpdate contractId 223
let updateContractId (v,h) s = // 7
    ret s
    >>= Sha3.updateU32  v // 24
    >>= Sha3.updateHash h // 192

val updateEvent : hashUpdate event 1009
let updateEvent event s = // 31
    ret s
    >>= updatePublicKey         event.oraclePubKey     // 517
    >>= updateContractId        event.oracleContractId // 223
    >>= updateTicker            event.ticker           // 36
    >>= Sha3.updateU64          event.priceLow         // 48
    >>= Sha3.updateU64 `runOpt` event.priceHigh        // 53
    >>= Sha3.updateU64          event.timeLow          // 48
    >>= Sha3.updateU64 `runOpt` event.timeHigh         // 53

val updatePosition : hashUpdate position 30
let updatePosition position s = // 6
    ret s
    >>= Sha3.updateString // 24
    begin match position with
    | Bull -> "Bull"
    | Bear -> "Bear"
    end

val hashCommitment : attestation -> hash `cost` 792
let hashCommitment attestation = // 15
    ret Sha3.empty
    >>= Sha3.updateHash attestation.commit    // 192
    >>= Sha3.updateU64  attestation.timestamp // 48
    >>= updatePublicKey attestation.pubKey    // 517
    >>= Sha3.finalize                         // 20

val hashAttestation : attestation -> hash `cost` 1013
let hashAttestation attestation = // 9
    let! commit = hashCommitment attestation in // 792
    ret Sha3.empty
    >>= Sha3.updateHash commit // 192
    >>= Sha3.finalize          // 20

val hashBet : bet -> hash `cost` 1070
let hashBet bet = // 11
    ret Sha3.empty
    >>= updateEvent    bet.event    // 1009
    >>= updatePosition bet.position // 30
    >>= Sha3.finalize               // 20

val mkBetToken : contractId -> bet -> asset `cost` 1077
let mkBetToken (v, h) bet = // 7
    let! betHash = hashBet bet in
    ret (v, h, betHash)

val mkAttestToken : contractId -> attestation -> asset `cost` 1020
let mkAttestToken (v,h) attestation = // 7
    let! attestHash = hashAttestation attestation in
    ret (v, h, attestHash)



(*
-------------------------------------------------------------------------------
========== VALIDATION =========================================================
-------------------------------------------------------------------------------
*)

val inBounds : U64.t -> option U64.t -> U64.t -> bool `cost` 10
let inBounds low high value =
    (low `U64.lte` value && Opt.maybe true (U64.lte value) high)
    |> ret

val validateTime: redemption -> result redemption `cost` 25
let validateTime redemption = // 15
    let  event = redemption.bet.event             in
    let  low   = event.timeLow                    in
    let  high  = event.timeHigh                   in
    let  value = redemption.attestation.timestamp in
    let! inb   = inBounds low high value          in // 10
    if inb
        then RT.ok redemption
        else RT.failw "Attestation time is not within the given time bounds"

val validatePrice: redemption -> result redemption `cost` 31
let validatePrice redemption = // 21
    let  event = redemption.bet.event    in
    let  low   = event.priceLow          in
    let  high  = event.priceHigh         in
    let  value = redemption.proof.value  in
    let! inb   = inBounds low high value in // 10
    let  pos   = redemption.bet.position in
    match inb, pos with
    | true , Bull
    | false, Bear ->
        RT.ok redemption
    | _ ->
        RT.failw "Position doesn't match the event"

val hashKey : ticker -> hash `cost` 53
let hashKey s = // 9
    Sha3.ofString s // (6 * (Str.length s) + 20)
    |> inc (24 - (6 * Str.length s)) // 44

val verifyAuditPath : proof -> bool `cost` 107594
let verifyAuditPath proof = // 17
    let! key   = hashKey proof.key            in // 53
    let  value = SMT.serializeU64 proof.value in
    SMT.verify proof.cwt proof.defaultHash proof.root proof.auditPath key (Some value) // 107524

val validateAuditPath: redemption -> result redemption `cost` 107602
let validateAuditPath redemption = // 8
    let! b = verifyAuditPath redemption.proof in // 107594
    if b
        then RT.ok redemption
        else RT.failw "Invalid audit path"

val validateRedemption: redemption -> result redemption `cost` 107665
let validateRedemption redemption = // 7
    let open RT in
    ret redemption
    >>= validateTime      // 25
    >>= validatePrice     // 31
    >>= validateAuditPath // 107602



(*
-------------------------------------------------------------------------------
========== COMMAND: Buy  ======================================================
-------------------------------------------------------------------------------
*)

val buyEvent: txSkeleton -> contractId -> sender -> event -> CR.t `cost` 3632
let buyEvent txSkel contractId sender event = // 37
    let! bullToken = mkBetToken contractId ({ event=event; position=Bull }) in // 1077
    let! bearToken = mkBetToken contractId ({ event=event; position=Bear }) in // 1077
    let! m         = TX.getAvailableTokens Asset.zenAsset txSkel            in // 64
    ret txSkel
    >>= TX.mint m bullToken             // 64
    >>= TX.mint m bearToken             // 64
    >>= lockToSender bullToken m sender // 623
    >>= lockToSender bearToken m sender // 623
    >>= CR.ofTxSkel                     // 3

val buy: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 4349
let buy txSkel contractId sender dict = // 10
    let open RT in
    ret dict
    >>= parseDict                         // 15
    >>= parseEvent                        // 692
    >>= buyEvent txSkel contractId sender // 3632



(*
-------------------------------------------------------------------------------
========== COMMAND: Redeem ====================================================
-------------------------------------------------------------------------------
*)

val redeemRedemption: txSkeleton -> contractId -> sender -> redemption -> CR.t `cost` 2948
let redeemRedemption txSkel contractId sender redemption = // 33
    let! betToken         = mkBetToken contractId redemption.bet       in // 1077
    let  oracleContractId = redemption.bet.event.oracleContractId      in
    let  attestation      = redemption.attestation                     in
    let! attestToken      = mkAttestToken oracleContractId attestation in // 1020
    let! m                = TX.getAvailableTokens betToken txSkel      in // 64
    ret txSkel
    >>= TX.destroy m betToken                // 64
    >>= TX.destroy 1UL attestToken           // 64
    >>= lockToSender Asset.zenAsset m sender // 623
    >>= CR.ofTxSkel                          // 3

val redeem: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 117924
let redeem txSkel contractId sender dict = // 12
    let open RT in
    ret dict
    >>= parseDict                                 // 15
    >>= parseRedemption                           // 7284
    >>= validateRedemption                        // 107665
    >>= redeemRedemption txSkel contractId sender // 2948



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
    -> CR.t `cost` 117934
let main txSkel context contractId command sender messageBody wallet state = // 10
    match command with
    | "Buy" ->
        buy txSkel contractId sender messageBody // 4349
        |> inc 113575
    | "Redeem" ->
        redeem txSkel contractId sender messageBody // 117924
    | _ ->
        RT.failw "Unsupported command"
        |> inc 117924

val cf:
       txSkel     : txSkeleton
    -> context    : context
    -> command    : string
    -> sender     : sender
    -> messageBody: option data
    -> wallet     : wallet
    -> state      : option data
    -> nat `cost` 2
let cf _ _ _ _ _ _ _ =
    (117934 <: nat) |> ret
