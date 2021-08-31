open Consensus
open Infrastructure
open Crypto
open Types
open Zen.Types.Data
open Zen.Data
open Zen.Types
module Cost = Zen.Cost.Realized

// Contract Arguments
let contractId = ContractId (Version0, Hash.fromString "b64cae81f046f75a462d7d8aa4a6dfb5e2a0a72b73987e33a0731f3d65ff4230" |> Option.get)

let contractFn, costFn = System.Reflection.Assembly.LoadFrom "output/Oracle.dll"
                         |> Contract.getFunctions
                         |> Result.get

let command = "Verify"

let (Crypto.PublicKey senderPK) =
  "02add2eb8158d81b7ea51e35ddde9c8a95e24449bdab8391f40c5517bdb9a3e117"
  |> FsBech32.Base16.decode
  |> Option.bind PublicKey.deserialize
  |> Option.get

let sender = Main.PK senderPK
let wallet : list<PointedOutput> = []
let context = {blockNumber=1ul;timestamp=0UL}

let someLeafValues =
    [ "value1"B; "value2"B; "value3"B; "value4"B; "value5"B
      "value6"B; "value7"B; "value8"B; "value9"B; "value10"B ]

let hashes =
    someLeafValues
    |> List.map Hash.compute

let root =
    hashes
    |> MerkleTree.computeRoot
    |> Hash.bytes
    |> Zen.Types.Data.data.Hash

let createData hash index =
    let mutable dataDictionary = Zen.Dictionary.empty()

    dataDictionary <-
        Zen.Dictionary.add "Hash"B hash dataDictionary
        |> Zen.Cost.Realized.__force

    dataDictionary <-
        Zen.Dictionary.add "Index"B (Zen.Types.Data.U32 (uint32 index)) dataDictionary
        |> Zen.Cost.Realized.__force

    let auditPath =
        MerkleTree.createAuditPath hashes index
        |> List.map Hash.bytes
        |> List.map Zen.Types.Data.Hash
        |> Consensus.ZFStar.fsToFstList
        |> Zen.Types.Data.List
        |> Zen.Types.Data.Collection

    Zen.Dictionary.add "AuditPath"B auditPath dataDictionary
    |> Zen.Cost.Realized.__force
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection

// Empty transaction
let tx : TxSkeleton.T = TxSkeleton.empty

// State - containing a root
let state : data option =
    [ root ]
    |> ZFStar.fsToFstList
    |> Data.List
    |> Data.Collection
    |> Some

for (index, leaf) in List.indexed someLeafValues do

    // with invalid hash
    ////////////////////////////////////////////////////////////

    let hash =
        "invalid value"B
        |> Hash.compute
        |> Hash.bytes
        |> Zen.Types.Data.data.Hash

    // Data
    let data =
        createData hash index
        |> Some

    match contractFn tx context contractId command sender data wallet state with
    | Error error when error = "Verification failed" -> ()
    | _ -> failwith "'verify' command test failed"

    // with invalid index
    ////////////////////////////////////////////////////////////

    let hash =
        "invalid value"B
        |> Hash.compute
        |> Hash.bytes
        |> Zen.Types.Data.data.Hash

    // Data
    let data =
        createData hash 100
        |> Some

    match contractFn tx context contractId command sender data wallet state with
    | Error error when error = "Verification failed" -> ()
    | _ -> failwith "'verify' command test failed"

    // with valid values
    ////////////////////////////////////////////////////////////

    let hash =
        leaf
        |> Hash.compute
        |> Hash.bytes
        |> Zen.Types.Data.data.Hash

    // Data
    let data =
        createData hash index
        |> Some

    match contractFn tx context contractId command sender data wallet state with
    | Ok _ -> ()
    | _ -> failwith "'verify' command test failed"

    // with non-existant root (empty state)
    ////////////////////////////////////////////////////////////

    let hash =
        leaf
        |> Hash.compute
        |> Hash.bytes
        |> Zen.Types.Data.data.Hash

    // Data
    let data =
        createData hash index
        |> Some

    match contractFn tx context contractId command sender data wallet None with
    | Error error when error = "Something went wrong! could not get roots from state!" -> ()
    | _ -> failwith "'verify' command test failed"

printf "'verify' tests passed."
