module FixedPayout

open Zen.Base
open Zen.Cost
open Zen.Types
open Zen.Data

module U8     = FStar.UInt8
module U64    = FStar.UInt64
module RT     = Zen.ResultT
module Dict   = Zen.Dictionary
module Sha3   = Zen.Hash.Sha3
module TX     = Zen.TxSkeleton
module CR     = Zen.ContractResult
module Asset  = Zen.Asset
module Opt    = Zen.Option
module OptT   = Zen.OptionT
module Array  = Zen.Array
module Str    = FStar.String
module CId    = Zen.ContractId
module Merkle = Zen.MerkleTree
module W      = Zen.Wallet



let auditPathMaxLength : nat = 30

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
    p: list data { length p <= auditPathMaxLength }

type auditPath =
    p: list hash { length p <= auditPathMaxLength }

type commit = {
    c_root      : hash;
    c_timestamp : timestamp;
}

type attestation = {
    commit : commit;
    pubKey : publicKey;
}

type position =
    | Bull
    | Bear

type betEvent = {
    oraclePubKey     : publicKey;
    oracleContractId : contractId;
    ticker           : ticker;
    strike           : U64.t;
    start            : U64.t;
    expiry           : option U64.t;
    collateral       : asset;
}

type bet = {
    bevent   : betEvent;
    position : position;
}

type proof = {
    key         : ticker;
    value       : U64.t;
    root        : hash;
    auditPath   : auditPath;
    index       : U64.t;
}

type redemption = {
    bet         : bet;
    attestation : attestation;
    timestamp   : timestamp;
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
val tryMap (#a #b : Type) (#n : nat) :
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

let runOpt (#a #s:Type) (#m:nat) (update:a -> s -> s `cost` m) (x:option a) (st:s) : s `cost` (m + 5) =
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

val lockToSender: asset -> U64.t -> sender -> txSkeleton -> result txSkeleton `cost` 624
let lockToSender asset amount sender txSkel = // 14
    match sender with
    | PK pk ->
        ret txSkel
        >>= lockToPubKey asset amount pk // 610
        >>= RT.ok
    | Contract cid ->
        ret txSkel
        >>= TX.lockToContract asset amount cid // 64
        >>= RT.incOK 546
    | Anonymous ->
        RT.incFailw 610 "Sender can't be anonymous"



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

val extractHashes: string -> ls:list data -> result (ls':list hash { length ls' == length ls }) `cost` (length ls * (2 + 20) + 20 + 5)
let extractHashes errMsg ls = // 5
    tryMap tryHash ls // (length ls * 22 + 20)
    |> RT.ofOptionT errMsg

val extractAuditPath': ls:preAuditPath -> result auditPath `cost` (length ls * 22 + 29)
let extractAuditPath' ls = // 4
    let open RT in
    extractHashes "All the items in the audit path must be hashes" ls // = X * 22 + 25
    $> (fun xs -> let (xs:list hash { length xs <= auditPathMaxLength }) = xs in xs)

val extractAuditPath: ls:preAuditPath
    -> result auditPath `cost` (auditPathMaxLength * 22 + 40)
let extractAuditPath (ls:preAuditPath) =
    extractAuditPath' ls
    |> inc ((auditPathMaxLength - length ls) * 22)
    |> (fun x -> x <: result auditPath `cost` (auditPathMaxLength * 22 + 29))

val parsePreAuditPath: string -> string -> option (Dict.t data) -> result preAuditPath `cost` 92
let parsePreAuditPath fieldName errMsg dict = // 6
    let open RT in
    parseField tryList fieldName errMsg dict >>= // 79
    begin fun ls -> // 7
        if length ls <= auditPathMaxLength
            then RT.ok (ls <: preAuditPath)
            else RT.failw "AuditPath length must be 256"
    end

val parseAuditPath: string -> string -> option (Dict.t data)
    -> result auditPath `cost` (auditPathMaxLength * 22 + 139)
let parseAuditPath fieldName errMsg dict = // 7
    let open RT in
    ret dict
    >>= parsePreAuditPath fieldName errMsg // 92
    >>= extractAuditPath // (length ls * 22 + 25 + (auditPathMaxLength - length ls) * 22 + 12)

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

val parseAsset: string -> string -> option (Dict.t data) -> result asset `cost` 150
let parseAsset fieldName errMsg dict = // 5
    let open RT in
    parseField tryString fieldName errMsg dict >>= // 73
    begin
    Asset.parse // 64
    >> RT.ofOptionT "The given asset is not a valid asset"
    end

val getTimestamp        : parser U64.t          82
val getRoot             : parser hash           82
val getOraclePubKey     : parser publicKey      82
val getTicker           : parser ticker         94
val getStrike           : parser U64.t          82
val getStart            : parser U64.t          82
val getExpiry           : parser (option U64.t) 77
val getAuditPath        : parser auditPath      (auditPathMaxLength * 22 + 139 + 4)
val getValue            : parser U64.t          82
val getIndex            : parser U64.t          82
val getPosition         : parser position       91
val getOracleContractId : parser contractId     162
val getCollateral       : parser asset          154

let getTimestamp        dict = dict |>
    parseField      tryU64       "Timestamp"        "Could not parse Timestamp"
let getRoot             dict = dict |>
    parseField      tryHash      "Root"             "Could not parse Root"
let getOraclePubKey     dict = dict |>
    parseField      tryPublicKey "OraclePubKey"     "Could not parse OraclePubKey"
let getTicker           dict = dict |>
    parseTicker                  "Ticker"           "Could not parse Ticker"
let getStrike           dict = dict |>
    parseField      tryU64       "Strike"           "Could not parse Strike"
let getStart            dict = dict |>
    parseField      tryU64       "Start"            "Could not parse Start"
let getExpiry           dict = dict |>
    parseOptField   tryU64       "Expiry"
let getAuditPath        dict = dict |>
    parseAuditPath               "AuditPath"        "Could not parse AuditPath"
let getValue            dict = dict |>
    parseField      tryU64       "Value"            "Could not parse Value"
let getIndex            dict = dict |>
    parseField      tryU64       "Index"            "Could not parse Index"
let getPosition         dict = dict |>
    parsePosition                "Position"         "Could not parse Position"
let getOracleContractId dict = dict |>
    parseContractId              "OracleContractId" "Could not parse OracleContractId"
let getCollateral       dict = dict |>
    parseAsset                   "Collateral"       "Could not parse Collateral"

val parseProof': option (Dict.t data)
    -> result proof `cost` (94 + (auditPathMaxLength * 22 + 415))
let parseProof' dict = // 31
    let open RT in
    dict |> getTicker    >>= (fun key         -> // 94
    dict |> getValue     >>= (fun value       -> // 82
    dict |> getRoot      >>= (fun root        -> // 82
    dict |> getAuditPath >>= (fun auditPath   -> // auditPathMaxLength * 22 + 139 + 4
    dict |> getIndex     >>= (fun index       -> // 82
        RT.ok ({
            key       = key;
            value     = value;
            root      = root;
            auditPath = auditPath;
            index     = index;
        }))))))

val parseProof: option (Dict.t data)
    -> result proof `cost` (auditPathMaxLength * 22 + 512)
let parseProof dict =
    parseProof' dict
    |> (fun x -> x <: result proof `cost` (auditPathMaxLength * 22 + 509))

val parseCommit: option (Dict.t data) -> result commit `cost` 175
let parseCommit dict = // 11
    let open RT in
    dict |> getRoot         >>= (fun root      -> // 82
    dict |> getTimestamp    >>= (fun timestamp -> // 82
        RT.ok ({
            c_root      = root;
            c_timestamp = timestamp;
        })))

val parseAttestation: option (Dict.t data) -> result attestation `cost` 268
let parseAttestation dict = // 11
    let open RT in
    dict |> parseCommit     >>= (fun commit    -> // 175
    dict |> getOraclePubKey >>= (fun pubKey    -> // 82
        RT.ok ({
            commit    = commit;
            pubKey    = pubKey;
        })))

val parseEvent: option (Dict.t data) -> result betEvent `cost` 769
let parseEvent dict = // 36
    let open RT in
    dict |> getOraclePubKey     >>= (fun oraclePubKey     -> // 82
    dict |> getOracleContractId >>= (fun oracleContractId -> // 162
    dict |> getTicker           >>= (fun ticker           -> // 94
    dict |> getStrike           >>= (fun strike           -> // 82
    dict |> getStart            >>= (fun start            -> // 82
    dict |> getExpiry           >>= (fun expiry           -> // 77
    dict |> getCollateral       >>= (fun collateral       -> // 154
        RT.ok ({
            oraclePubKey     = oraclePubKey;
            oracleContractId = oracleContractId;
            ticker           = ticker;
            strike           = strike;
            start            = start;
            expiry           = expiry;
            collateral       = collateral;
        }))))))))

val parseRedemption': option (Dict.t data) -> result redemption `cost` (1051 + (auditPathMaxLength * 22 + 698))
let parseRedemption' dict = // 22
    let open RT in
    dict |> parseEvent       >>= (fun bevent      -> // 769
    dict |> getPosition      >>= (fun position    -> // 91
    dict |> parseAttestation >>= (fun attestation -> // 262
    dict |> getTimestamp     >>= (fun timestamp   -> // 82
    dict |> parseProof       >>= (fun proof       -> // (auditPathMaxLength * 22 + 512)
        RT.ok ({
            bet         = {
                bevent   = bevent;
                position = position;
            };
            attestation = attestation;
            timestamp   = timestamp;
            proof       = proof;
        }))))))

val parseRedemption: option (Dict.t data) -> result redemption `cost` (auditPathMaxLength * 22 + 1752)
let parseRedemption dict = // 3
    parseRedemption' dict
    |> (fun x -> x <: result redemption `cost` (auditPathMaxLength * 22 + 1749))



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

val updateEvent : hashUpdate betEvent 1339
let updateEvent bevent s = // 30
    ret s
    >>= updatePublicKey         bevent.oraclePubKey     // 517
    >>= updateContractId        bevent.oracleContractId // 223
    >>= updateTicker            bevent.ticker           // 36
    >>= Sha3.updateU64          bevent.strike           // 48
    >>= Sha3.updateU64          bevent.start            // 48
    >>= Sha3.updateU64 `runOpt` bevent.expiry           // 53
    >>= Sha3.updateAsset        bevent.collateral       // 384

val updatePosition : hashUpdate position 28
let updatePosition position s = // 4
    match position with
    | Bull -> Sha3.updateString "Bull" s // 24
    | Bear -> Sha3.updateString "Bear" s // 24

val hashCommit : commit -> hash `cost` 271
let hashCommit commit = // 11
    ret Sha3.empty
    >>= Sha3.updateHash commit.c_root      // 192
    >>= Sha3.updateU64  commit.c_timestamp // 48
    >>= Sha3.finalize                      // 20

val hashCommitment : attestation -> hash `cost` 1014
let hashCommitment attestation = // 14
    let! commitHash = hashCommit attestation.commit in // 271
    ret Sha3.empty
    >>= Sha3.updateHash commitHash         // 192
    >>= updatePublicKey attestation.pubKey // 517
    >>= Sha3.finalize                      // 20

val hashAttestation : attestation -> hash `cost` 1235
let hashAttestation attestation = // 9
    let! commit = hashCommitment attestation in // 1014
    ret Sha3.empty
    >>= Sha3.updateHash commit // 192
    >>= Sha3.finalize          // 20

val hashBet : bet -> hash `cost` 1398
let hashBet bet = // 11
    ret Sha3.empty
    >>= updateEvent    bet.bevent   // 1339 
    >>= updatePosition bet.position // 28
    >>= Sha3.finalize               // 20

val mkBetToken : contractId -> bet -> asset `cost` 1405
let mkBetToken (v, h) bet = // 7
    let! betHash = hashBet bet in
    ret (v, h, betHash)

val mkAttestToken : contractId -> attestation -> asset `cost` 1242
let mkAttestToken (v,h) attestation = // 7
    let! attestHash = hashAttestation attestation in // 1235
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

val validateTime: redemption -> result redemption `cost` 24
let validateTime redemption = // 14
    let  bevent = redemption.bet.bevent   in
    let  low    = bevent.start            in
    let  high   = bevent.expiry           in
    let  value  = redemption.timestamp    in
    let! inb    = inBounds low high value in // 10
    if inb
        then RT.ok redemption
        else RT.failw "Attestation time is not within the given time bounds"

val validatePrice: redemption -> result redemption `cost` 17
let validatePrice redemption = // 17
    let  bevent = redemption.bet.bevent   in
    let  strike = bevent.strike           in
    let  value  = redemption.proof.value  in
    let  pos    = redemption.bet.position in
    match strike `U64.lte` value, pos with
    | true , Bull
    | false, Bear ->
        RT.ok redemption
    | _ ->
        RT.failw "Position doesn't match the event"

val hashLeaf : key:ticker -> U64.t -> hash `cost` 111
let hashLeaf (key : ticker) (value : U64.t) = // 19
    ret Sha3.empty
    >>= Sha3.updateString key // 6 * 4
    >>= Sha3.updateU64 value // 48
    >>= Sha3.finalize // 20
    |> inc (24 - 6 * Str.length key)
    |> (fun x -> x <: hash `cost` 92)

val verifyAuditPath' : proof:proof
    -> bool `cost` (111 + (length proof.auditPath * 420 + 4 + (auditPathMaxLength * 420 - length proof.auditPath * 420)) + 25)
let verifyAuditPath' proof = // 14
    let! leaf = hashLeaf proof.key proof.value in // 111
    Merkle.verify proof.root proof.auditPath (U64.v proof.index) leaf // (length proof.auditPath * 420 + 4)
    |> inc (auditPathMaxLength * 420 - length proof.auditPath * 420)

val verifyAuditPath : proof:proof -> bool `cost` (auditPathMaxLength * 420 + 143)
let verifyAuditPath proof = // 3
    verifyAuditPath' proof
    |> (fun x -> x <: bool `cost` (auditPathMaxLength * 420 + 140))

val validateAuditPath: redemption -> result redemption `cost` (auditPathMaxLength * 420 + 151)
let validateAuditPath redemption = // 8
    let! b = verifyAuditPath redemption.proof in // (auditPathMaxLength * 420 + 143)
    if b
        then RT.ok redemption
        else RT.failw "Invalid audit path"

val validateRedemption: redemption -> result redemption `cost` (auditPathMaxLength * 420 + 199)
let validateRedemption redemption = // 7
    let open RT in
    ret redemption
    >>= validateTime      // 24
    >>= validatePrice     // 17
    >>= validateAuditPath // (auditPathMaxLength * 420 + 151)



(*
-------------------------------------------------------------------------------
========== COMMAND: Issue  ====================================================
-------------------------------------------------------------------------------
*)

val issueEvent: txSkeleton -> contractId -> sender -> betEvent -> CR.t `cost` 4369
let issueEvent txSkel contractId sender bevent = // 52
    let! bullToken = mkBetToken contractId ({ bevent=bevent; position=Bull }) in // 1405
    let! bearToken = mkBetToken contractId ({ bevent=bevent; position=Bear }) in // 1405
    let! m         = TX.getAvailableTokens bevent.collateral txSkel           in // 64
    let open RT in
    ret txSkel
    >>= (TX.safeMint m bullToken >> ofOptionT "No collateral provided") // 64
    >>= (TX.safeMint m bearToken >> ofOptionT "No collateral provided") // 64
    >>= lockToSender bullToken m sender                                 // 624
    >>= lockToSender bearToken m sender                                 // 624
    >>= (TX.lockToContract bevent.collateral m contractId >> liftCost)  // 64
    >>= CR.ofTxSkel                                                     // 3

val issue: txSkeleton -> contractId -> sender -> option data -> CR.t `cost` 5163
let issue txSkel contractId sender dict = // 10
    let open RT in
    ret dict
    >>= parseDict                           // 15
    >>= parseEvent                          // 769
    >>= issueEvent txSkel contractId sender // 4369



(*
-------------------------------------------------------------------------------
========== COMMAND: Redeem ====================================================
-------------------------------------------------------------------------------
*)

val redeemRedemption' :
    (w:wallet)
    -> txSkeleton
    -> contractId
    -> sender
    -> redemption
    -> CR.t `cost` (1405 + (1242 + (64 + (0 + 64 + (Zen.Wallet.size w * 128 + 192) + 624 + (Zen.Wallet.size w * 128 + 192) + 64 + 3))) + 62)
let redeemRedemption' w txSkel contractId sender redemption = // 62
    let! betToken         = mkBetToken contractId redemption.bet       in // 1405
    let  oracleContractId = redemption.bet.bevent.oracleContractId     in
    let  attestation      = redemption.attestation                     in
    let! attestToken      = mkAttestToken oracleContractId attestation in // 1242
    let! m                = TX.getAvailableTokens betToken txSkel      in // 64
    let open RT in
    ret txSkel
    >>= (liftCost << TX.destroy m betToken)                                                               // 64
    >>= (ofOptionT "Insufficient funds" << TX.fromWallet redemption.bet.bevent.collateral m contractId w) // W.size w * 128 + 192
    >>= lockToSender redemption.bet.bevent.collateral m sender                                            // 624
    >>= (ofOptionT "Attestation token not found" << TX.fromWallet attestToken 1UL contractId w)           // W.size wallet * 128 + 192
    >>= (liftCost << TX.lockToContract attestToken 1UL contractId)                                        // 64
    >>= CR.ofTxSkel                                                                                       // 3

val redeemRedemption: (w:wallet) -> txSkeleton -> contractId -> sender -> redemption
    -> CR.t `cost` (W.size w * 256 + 3919)
let redeemRedemption w txSkel contractId sender redemption = // 7
    redeemRedemption' w txSkel contractId sender redemption
    |> (fun x -> x <: CR.t `cost` (W.size w * 256 + 3912))

val redeem' :
    (w:wallet)
    -> txSkeleton
    -> contractId
    -> sender
    -> option data
    -> CR.t `cost` (0 + 15 + (auditPathMaxLength * 22 + 1752) + (auditPathMaxLength * 420 + 199) + (W.size w * 256 + 3919) + 13)
let redeem' w txSkel contractId sender dict = // 13
    let open RT in
    ret dict
    >>= parseDict                                   // 15
    >>= parseRedemption                             // (auditPathMaxLength * 22 + 1752)
    >>= validateRedemption                          // (auditPathMaxLength * 420 + 199)
    >>= redeemRedemption w txSkel contractId sender // (W.size w * 256 + 3919)

val redeem: (w:wallet) -> txSkeleton -> contractId -> sender
    -> option data -> CR.t `cost` (auditPathMaxLength * 442 + W.size w * 256 + 5905)
let redeem w txSkel contractId sender dict = // 7
    redeem' w txSkel contractId sender dict
    |> (fun x -> x <: CR.t `cost` (auditPathMaxLength * 442 + W.size w * 256 + 5898))



(*
-------------------------------------------------------------------------------
========== COMMAND: Cancel ====================================================
-------------------------------------------------------------------------------
*)

val cancelEqualTokens :
  contractId
  -> (w : wallet)
  -> sender
  -> U64.t
  -> asset
  -> asset
  -> txSkeleton
  -> betEvent
  -> txSkeleton `RT.t` (0 + (W.size w * 128 + 192) + 624 + 64 + 64 + 29)
let cancelEqualTokens contractId w sender m bullToken bearToken txSkel bevent = // 29
    let open RT in
    ret txSkel
    >>= (ofOptionT "Insufficient funds" << TX.fromWallet bevent.collateral m contractId w) // W.size w * 128 + 192
    >>= lockToSender bevent.collateral m sender                                            // 624
    >>= (liftCost << TX.destroy m bullToken)                                               // 64
    >>= (liftCost << TX.destroy m bearToken)                                               // 64

val cancelEvent':
    (w : wallet)
    -> contractId
    -> sender
    -> txSkeleton
    -> betEvent
    -> CR.t `cost` (1405 + (1405 + (64 + (64 + (0 + (W.size w * 128 + 192) + 624 + 64 + 64 + 29 + 3)))) + 45)
let cancelEvent' w contractId sender txSkel bevent = // 45
    let! bullToken = mkBetToken contractId ({ bevent=bevent; position=Bull }) in // 1405
    let! bearToken = mkBetToken contractId ({ bevent=bevent; position=Bear }) in // 1405
    let! mBull     = TX.getAvailableTokens bullToken txSkel                   in // 64
    let! mBear     = TX.getAvailableTokens bearToken txSkel                   in // 64
    let open RT in
    begin if U64.eq mBull mBear then
        cancelEqualTokens contractId w sender mBull bullToken bearToken txSkel bevent // (0 + (W.size w * 128 + 192) + 624 + 64 + 64 + 29)
    else
        "There must be an equal amount of both bull and bear tokens in the transaction"
        |> incFailw (0 + (W.size w * 128 + 192) + 624 + 64 + 64 + 29)
    end
    >>= CR.ofTxSkel // 3

val cancelEvent:
    (w : wallet)
    -> contractId
    -> sender
    -> txSkeleton
    -> betEvent
    -> CR.t `cost` (W.size w * 128 + 3966)
let cancelEvent w contractId sender txSkel bevent = // 7
    cancelEvent' w contractId sender txSkel bevent
    |> (fun x -> x <: CR.t `cost` (W.size w * 128 + 3959))

val cancel'  :
  (w : wallet)
  -> contractId
  -> sender
  -> txSkeleton
  -> option data
  -> CR.t `cost` (0 + 15 + 769 + (W.size w * 128 + 3966) + 11)
let cancel' w contractId sender txSkel dict = // 11
    let open RT in
    ret dict
    >>= parseDict                              // 15
    >>= parseEvent                             // 769
    >>= cancelEvent w contractId sender txSkel // (W.size w * 128 + 3966)

val cancel  :
  (w : wallet)
  -> contractId
  -> sender
  -> txSkeleton
  -> option data
  -> CR.t `cost` (W.size w * 128 + 4768)
let cancel w contractId sender txSkel dict = // 7
    cancel' w contractId sender txSkel dict
    |> (fun x -> x <: CR.t `cost` (W.size w * 128 + 4761))



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
    -> w           : wallet
    -> state       : option data
    -> CR.t `cost` (
        match command with
        | "Issue" ->
            5163 + 9
        | "Redeem" ->
            auditPathMaxLength * 442 + W.size w * 256 + 5905 + 9
        | "Cancel" ->
            W.size w * 128 + 4768 + 9
        | _ ->
            9)
let main txSkel context contractId command sender messageBody w state = // 9
    match command with
    | "Issue" ->
        issue txSkel contractId sender messageBody // 5163
        <: CR.t `cost` (
            match command with
            | "Issue" ->
                5163
            | "Redeem" ->
                auditPathMaxLength * 442 + W.size w * 256 + 5905
            | "Cancel" ->
                W.size w * 128 + 4768
            | _ ->
                0)
    | "Redeem" ->
        redeem w txSkel contractId sender messageBody // auditPathMaxLength * 442 + W.size w * 256 + 5905
    | "Cancel" ->
        cancel w contractId sender txSkel messageBody // W.size w * 128 + 4768
    | _ ->
        RT.failw "Unsupported command"

val cf:
       txSkel     : txSkeleton
    -> context    : context
    -> command    : string
    -> sender     : sender
    -> messageBody: option data
    -> w          : wallet
    -> state      : option data
    -> nat `cost` 17
let cf _ _ command _ _ w _ =
    ((
        match command with
        | "Issue" ->
            5163 + 9
        | "Redeem" ->
            auditPathMaxLength * 442 + W.size w * 256 + 5905 + 9
        | "Cancel" ->
            W.size w * 128 + 4768 + 9
        | _ ->
            9
     ) <: nat) |> ret


