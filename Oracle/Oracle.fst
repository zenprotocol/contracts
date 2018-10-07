module Oracle

open Zen.Types
open Zen.Base
open Zen.Cost
open Zen.Data
open FStar.UInt32

module Asset = Zen.Asset
module Crypto = Zen.Crypto
module D = Zen.Dictionary
module CR = Zen.ContractResult
module List = Zen.List
module MT = Zen.MerkleTree
module OT = Zen.OptionT
module RT = Zen.ResultT
module TX = Zen.TxSkeleton
module U32 = FStar.UInt32

// The public key of the oracle
let oraclePubKey = ""

// The length of the backlog of merkle roots
let backLogLength = 10

// takes the first n values in a list
val take(#a: Type): n:nat -> list a -> (ls': list a {length ls' <= n}) `cost` (17 * n + 17)
let rec take #a n ls =
    match ls with
    | hd::tl ->
        if n <> 0 then begin
            let! tl = take (n-1) tl in
            ret ((hd::tl) <: (ls': list a{length ls' <= n})) end
        else autoRet []
    | [] -> autoRet []

// trims a list to `backLogLength`
val trimBackLog(#a: Type): list a
    -> (ls:list a{length ls <= backLogLength}) `cost` (17 * backLogLength + 19)
let trimBackLog #_ ls = take backLogLength ls

// trims an auditPath to length 31
val trimAuditPath(#a: Type): list a
    -> (ls:list a{length ls <= 31}) `cost` 546
let trimAuditPath #_ ls = take 31 ls

// checks if the sender is the oracle key
let authenticateSender (sender: sender): bool `cost` 133 = //13
    let! oraclePubKey = Crypto.parsePublicKey oraclePubKey in //120
    match sender, oraclePubKey with
    | PK pk, Some oraclePubKey -> ret (pk = oraclePubKey)
    | _ -> autoRet false

// gets a merkle root from the message body
let getNewMerkleRoot (messageBody: option data): option hash `cost` 4 = //2
    messageBody >!= tryHash //2

// adds a merkle root to the backlog in the state
let addMerkleRoot (merkleRoot: hash) (state: option data)
                  : data `cost` (17 * backLogLength + 34) = //15
    // get the backlog of merkle roots
    let backlog = match state with
                  | Some (Collection (List hashes)) -> hashes
                  | _ -> [] in
    // add the new merkle root to the backlog, and trim the oldest entry if needed.
    let (backlog: (ls:list data{length ls <= backLogLength})
                  `cost` (17 * backLogLength + 19)) =
        (Hash merkleRoot::backlog) |> trimBackLog in
    let! backlog = backlog in
    ret (Collection (List backlog))

// when invoked by the authenticated party, adds a merkle root to the backlog
let add (txSkel: txSkeleton) (contractId: contractId) (sender: sender)
        (messageBody: option data) (state: option data)
        : CR.t `cost` (17 * backLogLength + 398) = //29
    let! contractToken = Asset.getDefault contractId in //64
    let! newMerkleRoot = getNewMerkleRoot messageBody in //4
    match newMerkleRoot with
    | Some newMerkleRoot ->
        let! newState = addMerkleRoot newMerkleRoot state in //17*backLogLength+34
        if! authenticateSender sender then //133
            // Need to mint and destroy one token, since contracts cannot do nothing
            TX.mint 1UL contractToken txSkel //64
            >>= TX.destroy 1UL contractToken //64
            >>= CR.ofTxSkel //3
            >>= CR.setStateUpdate newState //3
        else RT.incFailw 134 "Could not authenticate sender"
    | None -> "Message body must contain a new merkle root"
              |> RT.incFailw (17 * backLogLength + 301)

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
          OT.some result
      | _ -> OT.none end
  | [] -> [] |> OT.incSome (length ls * (n+20))

val exists_ (#a:Type)(#n:nat):
    (a -> bool `cost` n)
    -> ls: list a
    -> bool `cost` (length ls * (n + 13) + 13)
let rec exists_ #_ #n f ls = //13
    match ls with
    | hd::tl ->
        ( let! test = f hd in
          let! rest = exists_ f tl in
          ret (test || rest) )
        <: bool `cost` (length ls * (n+13))
    | [] -> ret false

// gets a hash from the message body
let getHash (messageBody: option data): option hash `cost` 77 = //7
    messageBody >!= tryDict //4
                >?= D.tryFind "Hash" //64
                >?= tryHash //2

// gets the audit path index from the message body
let getIndex (messageBody: option data): option U32.t `cost` 77 = //7
    messageBody >!= tryDict //4
                >?= D.tryFind "Index" //64
                >?= tryU32 //2

// tries a list of data of length less than or equal to 31 as an audit path.
val tryAuditPath: (auditPathData: list data{length auditPathData <= 31})
    -> option (auditPath: list hash{length auditPath <= 31}) `cost` 717
let tryAuditPath auditPathData = //15
    // try the audit path data as a list of hashes
    let (auditPath: option (auditPath: list hash{length auditPath == length auditPathData})
                    `cost` (length auditPathData * 22 + 20)) =
        tryMap tryHash auditPathData in
    // increment the cost to make the cost of this function constant
    let (auditPath: option (auditPath: list hash{length auditPath == length auditPathData})
                    `cost` 702) =
        inc ((31 - length auditPathData) * 22) auditPath in
    // ensure the inner refinement on the return type
    let! auditPath = auditPath in
    match auditPath with
    | Some auditPath ->
        let (auditPath: list hash{length auditPath <= 31}) = auditPath in
        OT.some auditPath
    | None -> OT.none

// gets the audit path from the message body
let getAuditPath (messageBody: option data)
                 : option (auditPath: list hash{length auditPath <= 31})
                   `cost` 1350 = //16
    let! auditPathData = messageBody >!= tryDict //4
                                     >?= D.tryFind "AuditPath" //64
                                     >?= tryList in //2
    match auditPathData with
    | Some auditPathData ->
        trimAuditPath auditPathData >>= tryAuditPath //1263
    | None -> OT.incNone 1263

// tries a list of data of length less than or equal to the
// maximum backlog length as a list of merkle roots.
val tryRoots: (rootsData: list data{length rootsData <= backLogLength})
    -> option (roots: list hash{length roots <= backLogLength})
       `cost` (backLogLength * 22 + 35)
let tryRoots rootsData = //15
    // try the roots data as a list of hashes
    let (roots: option (roots: list hash{length roots == length rootsData})
                `cost` (length rootsData * 22 + 20)) =
        tryMap tryHash rootsData in
    // increment the cost to make the cost of this function constant
    let (roots: option (roots: list hash{length roots == length rootsData})
                `cost` (backLogLength * 22 + 20)) =
        inc ((backLogLength - length rootsData) * 22) roots in
    // ensure the inner refinement on the return type
    let! roots = roots in
    match roots with
    | Some roots ->
        let (roots: list hash{length roots <= backLogLength}) = roots in
        OT.some roots
    | None -> OT.none

// gets a list of merkle roots from the state
let getRoots (state: option data):
    option (roots: list hash{length roots <= backLogLength})
    `cost` (backLogLength * 39 + 64) = //10
    match state with
    | Some (Collection (List rootsData)) ->
        trimBackLog rootsData >>= tryRoots //(17 * backLogLength + 19) + (backLogLength * 22 + 35)
    | _ -> OT.incNone (backLogLength * 39 + 54)

// Verifies the audit path against a single root
let verifyAuditPath' (auditPath: list hash{length auditPath <= 31})
                     (index: U32.t)
                     (h: hash)
                     (root: hash)
                     : bool `cost` 13037 = //13
    MT.verify root auditPath (U32.v index) h
    // increment to give this function constant cost
    |> inc ((31 - length auditPath) * 420) //13024

// verifies the audit path against several roots
val verifyAuditPath: hash
    -> (index: U32.t)
    -> (roots: list hash{length roots <= backLogLength})
    -> (auditPath: list hash{length auditPath <= 31})
    -> bool `cost` (13050 * backLogLength + 26)
let verifyAuditPath h index roots auditPath = //13
    begin exists_ (verifyAuditPath' auditPath index h) roots //l roots * (13037 + 13) + 13
    // increment to give this function constant cost
    |> inc (13050 * (backLogLength - length roots))
    end <: bool `cost` (13050 * backLogLength + 13)

// when invoked with a hash, audit path, and audit path index,
// verifies that the provided hash is included in
// one of the merkle trees in the backlog.
let verify (txSkel: txSkeleton) (messageBody: option data) (state: option data)
           : CR.t `cost` (13089 * backLogLength + 1653) = //56
    let! hash = getHash messageBody in //77
    let! index = getIndex messageBody in //77
    let! auditPath = getAuditPath messageBody in //1350
    let! roots = getRoots state in // 39 * backLogLength + 64
    match hash, index, auditPath, roots with
    | Some hash, Some index, Some auditPath, Some roots ->
        if! verifyAuditPath hash index roots auditPath then //(13050 * backLogLength) + 26)
            CR.ofTxSkel txSkel //3
        else "Verification failed" |> RT.incFailw 3
    | None, _, _, _ ->
        "Message body must contain Hash"
        |> RT.incFailw (13050 * backLogLength + 29)
    | Some _, None, _, _ ->
        "Message body must contain valid Index"
        |> RT.incFailw (13050 * backLogLength + 29)
    | Some _, Some _, None, _ ->
        "Message body must contain valid AuditPath"
        |> RT.incFailw (13050 * backLogLength + 29)
    | Some _, Some _, Some _, None ->
        "Something went wrong! could not get roots from state!"
        |> RT.incFailw (13050 * backLogLength + 29)

let main (txSkel: txSkeleton) _ (contractId: contractId) (command: string)
         (sender: sender) (messageBody: option data) (wallet: wallet)
         (state: option data)
         : CR.t `cost` ( match command with
                         | "Verify" -> 13089 * backLogLength + 1661
                         | "Add" -> 17 * backLogLength + 406
                         | _ -> 8 ) = //8
    begin match command with
    | "Verify" -> verify txSkel messageBody state
                  <: (CR.t `cost` ( match command with
                                  | "Verify" -> 13089 * backLogLength + 1653
                                  | "Add" -> 17 * backLogLength + 398
                                  | _ -> 0 ))
    | "Add" -> add txSkel contractId sender messageBody state // 17 * backLogLength + 398
               //|> inc (15636 * backLogLength + 1464)
    | _ -> "Unsupported command"
            |> RT.failw
           //|> RT.incFailw (15653 * backLogLength + 1659)
    end

let cf _ _ command _ _ _ _ = ret ((
    match command with
    | "Verify" -> 13089 * backLogLength + 1661 <: nat
    | "Add" -> 17 * backLogLength + 406
    | _ -> 8) <: nat)
