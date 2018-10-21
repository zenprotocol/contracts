#load "Bet.fsx"

open Bet
open Consensus
open Infrastructure
open Crypto
open Types
open Hash
open Zen.Types.Data
open Zen.Data
open Zen.Types

module Asset      = Consensus.Asset
module Contract   = Consensus.Contract
module ContractId = Consensus.ContractId
module Hash       = Consensus.Hash
module Result     = Infrastructure.Result
module Tx         = Consensus.TxSkeleton
module Types      = Consensus.Types
module Merkle     = Consensus.MerkleTree


let oracleDLL = "../Oracle/output/Oracle.dll"
let oracleSrc = "../Oracle/Oracle.fst"
let betDLL    = "output/Bet.dll"
let betSrc    = "Bet.fst"

let operatorId = "02add2eb8158d81b7ea51e35ddde9c8a95e24449bdab8391f40c5517bdb9a3e117"

let otherLeafData = "123xyz"B

let cmd_oracle_Add = "Add"


(*
    ---------------------------------------- HELPER FUNCTIONS ----------------------------------------------------------
*)
// Load contract's main function
let loadContract dll =
    let fs = System.Reflection.Assembly.LoadFrom dll |> Contract.getFunctions |> Result.get
    in match fs with | main, _ -> main

// Bet contract data
let mkBetData returnAddress price index auditPath =
       emptyDict
    |> addToDict ( "returnAddress"B , returnAddress                )
    |> addU64    ( "Price"B         , price                        )
    |> addU32    ( "Index"B         , index                        )
    |> addList   ( "AuditPath"B     , ZFStar.fsToFstList auditPath )
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some

// Convert Consensus.Hash to Types.Data
let datahash (Hash.Hash hash) = Data.Hash hash


(*
    ---------------------------------------- INITIALIZE GENERAL PARAMETERS ---------------------------------------------
*)
// Empty wallet
let emptyWallet : list<PointedOutput> = []

// Arbitrary context
let context = {blockNumber=1ul;timestamp=0UL}

// Transaction with 1 bull token
let tx1Bull = mkTx [mkInput contractLock bullToken 1UL] []

// Empty transaction
let tx : Tx.T = Tx.empty

// Initial empty state
let initState : data option = None


(*
    ---------------------------------------- BUILDING MERKLE TREE ------------------------------------------------------
    
    Creating a Merkle tree with 2 leaves:   /\
                                           x o
*)
// Point of interest hash (the left leaf)
let poiHash = hashParams strike

// Other hash (the right leaf)
let otherHash =
       otherLeafData
    |> Hash.compute
    |> Hash.bytes
    |> Hash.Hash

// List of leaves
let hashes = [poiHash; otherHash]

// Merkle root
let merkleRoot = Merkle.computeRoot hashes

// Commitment on the merkle root
let commitment =
    match merkleRoot with
    | Hash.Hash hash -> Some (Zen.Types.Data.Hash hash)

// False commitment on the merkle root
let falseCommitment =
    match otherHash with
    | Hash.Hash hash -> Some (Zen.Types.Data.Hash hash)

// Index of the point of intereset's leaf
let poiIndex = 0u

// Path to the point of intereset's leaf
let auditPath =
       Merkle.createAuditPath hashes (Checked.int poiIndex)
    |> List.map datahash

// Create valid bet contract data
let okData = mkBetData returnAddress strike poiIndex auditPath


(*
    ---------------------------------------- PREPARE ORACLE ------------------------------------------------------------
*)
let oracle_main = loadContract oracleDLL

// Contract ID
let oracle_id = System.IO.File.ReadAllText oracleSrc |> Contract.makeContractId Types.Version0

// Oracle operator identity
let (Crypto.PublicKey oracle_operatorPK') =
       operatorId
    |> FsBech32.Base16.decode
    |> Option.bind PublicKey.deserialize
    |> Option.get

let oracle_operatorPK = Main.PK oracle_operatorPK'


(*
    ---------------------------------------- REDEEM BULL TOKEN ---------------------------------------------------------
*)
let txResult, cmd_oracle_Verify, proof =
    match redeem tx1Bull okData wallet50ZP with
    | Ok (tx, optmsg, _) ->
        match optmsg with
        | Some msg -> tx, msg.command, msg.body
        | None -> failwith "No message was received"
    | Error e -> failwithf "Failed with unexpected error: `%A`" e


(*
    ---------------------------------------- MORE HELPER FUNCTIONS -----------------------------------------------------
*)
// Add commits from a list by chaining "Add" requests   
let rec addCommitsToOracle commits lastState =
    match commits with
    | [] -> lastState
    | commit :: commits ->
        let state =
            match oracle_main tx context oracle_id cmd_oracle_Add oracle_operatorPK commit emptyWallet lastState with
            | Ok (_, _, Main.Update state) -> Some state
            | Ok _ -> None
            | Error err -> failwith err
        in addCommitsToOracle commits state

// Add commits to oracle and send proof for verification 
let addCommitsAndVerify commits =
    let lastState = addCommitsToOracle commits initState
    in oracle_main txResult context oracle_id cmd_oracle_Verify oracle_operatorPK proof emptyWallet lastState

// Validate transaction
let validTX tx =
    tx = txResult && (tx |> ZFStar.fsToFstTxSkeleton |> Zen.TxSkeleton.isValid |> Zen.Cost.Realized.__force)

let testSuccess commitments =
    match addCommitsAndVerify commitments with
        | Ok (tx, _, _) ->
            if validTX tx
                then printfn "OK: The transaction has succeeded"
                else failwith "FAIL: The transaction has failed"
        | Error err -> failwith err

let testFailure commitments =
    match addCommitsAndVerify commitments with
        | Ok (tx, _, _) ->
            if validTX tx
                then failwith "FAIL: The transaction has succeeded"
                else failwith "FAIL: Something unexpected has happened"
        | Error err -> printfn "OK: The transaction has failed"


(*
    ---------------------------------------- TESTS ---------------------------------------------------------------------
*)
printfn "Testing with a single commitment in the oracle... (should SUCCEED)"
testSuccess [commitment]

printfn "Testing with 2 identical commitment in the oracle... (should SUCCEED)"
testSuccess [commitment; commitment]

printfn "Testing with no commitment in the oracle... (should FAIL)"
testFailure []

printfn "Testing with a false commitment in the oracle... (should FAIL)"
testFailure [falseCommitment]

printfn "Testing with a commitment followed by a false commitment... (should SUCCEED)"
testSuccess [commitment; falseCommitment]

printfn "Testing with a false commitment followed by a commitment... (should SUCCEED)"
testSuccess [falseCommitment; commitment]
