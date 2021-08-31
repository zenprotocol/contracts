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

let command = "Add"

let (Crypto.PublicKey senderPK) =
  "02add2eb8158d81b7ea51e35ddde9c8a95e24449bdab8391f40c5517bdb9a3e117"
  |> FsBech32.Base16.decode
  |> Option.bind PublicKey.deserialize
  |> Option.get

let sender = Main.PK senderPK
let wallet : list<PointedOutput> = []
let context = {blockNumber=1ul;timestamp=0UL}

let someHash =
    "some value"B
    |> Hash.compute
    |> Hash.bytes
    |> Data.Hash

// Data
let data = Some someHash

// Empty transaction
let tx : TxSkeleton.T = TxSkeleton.empty

// Empty state
let state : data option = None

// a single 'add' command test
////////////////////////////////////////////////////////////////

match contractFn tx context contractId command sender data wallet state with
| Ok (_, _, Main.Update state) when state = ([ someHash ] |> ZFStar.fsToFstList |> Data.List |> Data.Collection) -> ()
| _ -> failwith "'add' command test failed"


// multiple 'add' commands test - max backlog length
////////////////////////////////////////////////////////////////

let mutable state' : data option = None

for _ in [ 1 .. 20 ] do
    match contractFn tx context contractId command sender data wallet state' with
    | Ok (_, _, Main.Update state) -> state' <- Some state
    | _ -> failwith "multiple 'add' commands test failed"

match state' with
| Some (Collection (List (Prims.list.Cons' (len, _, _)))) when len = Prims.int.Parse "10" -> ()
| _ -> failwith "multiple 'add' commands test failed"


// multiple 'add' commands test - log head should be last entry
////////////////////////////////////////////////////////////////

state' <- None
let mutable lastEntry : data option = None

for i in [ 1uy .. 20uy ] do
    let data' =
        [| i |]
        |> Hash.compute
        |> Hash.bytes
        |> Data.Hash
        |> Some

    lastEntry <- data'

    match contractFn tx context contractId command sender data' wallet state' with
    | Ok (_, _, Main.Update state) -> state' <- Some state
    | _ -> failwith "multiple 'add' commands test failed"

match state' with
| Some (Collection (List (Prims.list.Cons' (_, head, _)))) when head = Option.get lastEntry -> ()
| _ -> failwith "multiple 'add' commands test failed"

printf "'add' tests passed."
