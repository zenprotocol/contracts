open Consensus
open Infrastructure
open Crypto
open Types
open Zen.Types.Data
open Zen.Data
open Zen.Types
module Cost = Zen.Cost.Realized

let contractHashHex = "741eea77a769fc1beedd5ecf20415bd3ad32404c9511bab44d2d8792f5ec3dc8"

let contractHash = Hash.fromString contractHashHex
                   |> Result.get

// hex encoding of ascii value of "Bull"
let bullHex = "42756c6c"
// hex encoding of ascii value of "Bear"
let bearHex = "42656172"

let bullTokenHex = "00000000"
                   + contractHashHex
                   + bullHex
                   + "00000000000000000000000000000000000000000000000000000000"
let bearTokenHex = "00000000"
                   + contractHashHex
                   + bearHex
                   + "00000000000000000000000000000000000000000000000000000000"

let bullToken = Asset.fromString bullTokenHex
                |> Option.get
let bearToken = Asset.fromString bearTokenHex
                |> Option.get

let contractID = ContractId (Version0, contractHash)

let contractFn' = System.Reflection.Assembly.LoadFrom "output/Bet.dll"
                  |> Contract.getFunctions
                  |> Result.get
                  |> fst
// don't care about context, contractID, command, sender, wallet or state
let contractFn (txSkeleton: TxSkeleton.T) messageBody =
    contractFn' txSkeleton
                {blockNumber=1ul;timestamp=0UL}
                contractID
                "Buy"
                Anonymous
                messageBody
                []
                None

// Empty transaction
let emptyTxSkeleton : TxSkeleton.T = TxSkeleton.empty


//////////////////////////////////////////////////////////////////////////
// Buy without returnAddress fails
//////////////////////////////////////////////////////////////////////////

let noReturnAddress: data option = None

match contractFn emptyTxSkeleton noReturnAddress with
| Error "Could not parse returnAddress from messageBody" ->
    printfn "OK: Buy without returnAddress fails"
| Ok _ -> failwith "Should not return ok without returnAddress"
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// Buy with returnAddress only
// Should mint 0 bull & bear tokens and spends them to returnAddress
//////////////////////////////////////////////////////////////////////////

let onlyReturnAddress =
    let returnAddress = PK Hash.zero
                        |> ZFStar.fsToFstLock
                        |> Lock
    Zen.Dictionary.add "returnAddress"B returnAddress Zen.Dictionary.empty
    |> Cost.__force
    |> Dict
    |> Collection
    |> Some

match contractFn emptyTxSkeleton onlyReturnAddress with
| Ok ({pInputs=pInputs; outputs=outputs}, None, Main.NoChange) -> // expect no message or state update
    // check inputs
    match pInputs with
    | [TxSkeleton.Mint mint0; TxSkeleton.Mint mint1] ->
        if mint0.amount <> 0UL
            then failwithf "Expected mint 0 to have amount 0, but got: `%A`" mint0
        if mint0.asset <> bullToken
            then failwithf "Expected mint 0 to be bull token, but got: `%A`" mint0
        if mint1.amount <> 0UL
            then failwithf "Expected mint 1 to have amount 0, but got: `%A`" mint1
        if mint1.asset <> bearToken
            then failwithf "Expected mint 1 to be bear token, but got: `%A`" mint1
    | _ ->
        failwithf "Expected 2 mints but got: `%A`" pInputs
    // check outputs
    match outputs with
    | [output0; output1; output2] ->
        if output0.spend.amount <> 0UL
            then failwithf "Expected output 0 to have amount 0, but got: `%A`" output0
        if output0.spend.asset <> Asset.Zen
            then failwithf "Expected output 0 to be ZP, but got: `%A`" output0
        if output0.lock <> Contract contractID
            then failwithf "Expected output 0 to lock to contract, but got: `%A`" output0
        if output1.spend.amount <> 0UL
            then failwithf "Expected output 1 to have amount 0, but got: `%A`" output1
        if output1.spend.asset <> bullToken
            then failwithf "Expected output 1 to be bear token, but got: `%A`" output1
        if output1.lock <> PK Hash.zero
            then failwithf "Expected output 1 to lock to returnAddress, but got: `%A`" output1
        if output2.spend.amount <> 0UL
            then failwithf "Expected output 2 to have amount 0, but got: `%A`" output2
        if output2.spend.asset <> bearToken
            then failwithf "Expected output 2 to be bear token, but got: `%A`" output2
        if output2.lock <> PK Hash.zero
            then failwithf "Expected output 2 to lock to contract, but got: `%A`" output2
    | _ ->
        failwithf "Expected 3 outputs but got: `%A`" pInputs

    // If you reach here, all is ok!
    printfn "OK: Buy with returnAddress only"
    
| Error e ->
    failwithf "Failed with unexpected error: `%A`" e
