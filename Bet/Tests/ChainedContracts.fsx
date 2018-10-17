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

module Asset = Consensus.Asset
module Contract = Consensus.Contract
module ContractId = Consensus.ContractId
module Hash = Consensus.Hash
module Result = Infrastructure.Result
module Tx = Consensus.TxSkeleton 
module Types = Consensus.Types
module Merkle = Consensus.MerkleTree

let betDLL    = "../output/Bet.dll"
let oracleDLL = "../../Oracle/Tests/output/Oracle.dll"
let oracleSrc = "../../Oracle/Oracle.fst"
let betSrc    = "../Bet.fst"

let cmd_oracle_Add     = "Add"
let cmd_oracle_Verify  = "Verify"
let cmd_bet_Buy        = "Buy"
let cmd_bet_RedeemBear = "RedeemBear"
let cmd_bet_RedeemBull = "RedeemBull"


(*
    HELPER FUNCTIONS
*)
        
let loadContract dll =
    let fs = System.Reflection.Assembly.LoadFrom dll |> Contract.getFunctions |> Result.get
    in match fs with | main, _ -> main

let mkMerkleData' returnAddress time ticker price (Hash.Hash hash) index auditPath =
    addToDict ("returnAddress"B, returnAddress) emptyDict
    |> addU64 ("Time"B, time)
    |> addString ("Ticker"B, ticker)
    |> addU64 ("Price"B, price)
    |> addHash ("Hash"B, hash)
    |> addU32 ("Index"B, index)
    |> addList ("AuditPath"B, ZFStar.fsToFstList auditPath)
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some

let mkMerkleData returnAddress time ticker price index auditPath =
    mkMerkleData' returnAddress time ticker price (hashParams time ticker price) index auditPath

let datahash (Hash.Hash hash) = Data.Hash hash


(*
    INITIALIZE GENERAL PARAMETERS
*)
// Empty wallet
let emptyWallet : list<PointedOutput> = []

// Arbitrary context
let context = {blockNumber=1ul;timestamp=0UL}

// Commitment

let poiHash = hashParams unixtime ticker strike

let otherHash =
       "123xyz"B
    |> Hash.compute
    |> Hash.bytes
    |> Hash.Hash

let tx1Bull = mkTx [(mkInput contractLock bullToken 1UL)] []

let hashes = [poiHash; otherHash]

let merkleRoot = Merkle.computeRoot hashes

let commitment =
    match merkleRoot with
    | Hash.Hash hash -> Some (Zen.Types.Data.Hash hash)

let poiIndex = 0u

let auditPath =
       Merkle.createAuditPath hashes (Checked.int poiIndex)
    |> List.map datahash

let okData = mkMerkleData returnAddress unixtime ticker strike poiIndex auditPath

// Empty transaction
let tx : Tx.T = Tx.empty

// Initial empty state
let initState : data option = None


(*
    PREPARE ORACLE
*)
let oracle_main = loadContract oracleDLL 

// Contract ID
let oracle_id = System.IO.File.ReadAllText oracleSrc |> Contract.makeContractId Types.Version0

// Oracle operator identity
let (Crypto.PublicKey oracle_operatorPK') =
    "02add2eb8158d81b7ea51e35ddde9c8a95e24449bdab8391f40c5517bdb9a3e117"
    |> FsBech32.Base16.decode
    |> Option.bind PublicKey.deserialize
    |> Option.get

let oracle_operatorPK = Main.PK oracle_operatorPK'

// Add commitment to the oracle
let commitedState =
    match oracle_main tx context oracle_id cmd_oracle_Add oracle_operatorPK commitment emptyWallet initState with
    | Ok (_, _, Main.Update state) -> Some state //when state = ([ someHash ] |> ZFStar.fsToFstList |> Data.List |> Data.Collection) -> ()
    | Ok _ -> None
    | Error err -> failwith err


(*
    REDEEM BULL TOKEN
*)
let txResult, command, proof =
    match redeemBull tx1Bull okData wallet50ZP with
    | Ok (tx, optmsg, _) ->
        match optmsg with
        | Some msg -> tx, msg.command, msg.body
        | None -> failwith "No message was received"
    | Error e -> failwithf "Failed with unexpected error: `%A`" e


(*
    VERIFY TRANSACTION
*)
match oracle_main txResult context oracle_id command oracle_operatorPK proof emptyWallet commitedState with
    | Ok (tx, _, _) ->
        if tx = txResult && (tx |> ZFStar.fsToFstTxSkeleton |> Zen.TxSkeleton.isValid |> Zen.Cost.Realized.__force)
            then printfn "OK: The transaction has succeeded"
            else failwith "The transaction has failed" 
    | Error err -> failwith err
