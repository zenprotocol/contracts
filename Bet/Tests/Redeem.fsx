#load "Bet.fsx"

open Bet

module Asset = Consensus.Asset
module Contract = Consensus.Contract
module Hash = Consensus.Hash
module Result = Infrastructure.Result
module Tx = Consensus.TxSkeleton
module Types = Consensus.Types

//////////////////////////////////////////////////////////////////////////
// Redeem with only returnAddress fails
//////////////////////////////////////////////////////////////////////////

match redeem emptyTx onlyReturnAddress emptyWallet with
| Error "Could not parse Price from messageBody" ->
    printfn "OK: Redeem with only returnAddress fails"
| Ok cr -> failwithf "Should not return ok without Price! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// Redeem with only Price fails
//////////////////////////////////////////////////////////////////////////

match redeem emptyTx onlyPrice emptyWallet with
| Error "Could not parse returnAddress from messageBody" ->
    printfn "OK: Redeem with only Price fails"
| Ok cr -> failwithf "Should not return ok without returnAddress! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// Redeem succeeds with OK Data
//////////////////////////////////////////////////////////////////////////

let okData = mkData returnAddress strike
let tx1Bull =
    mkTx [(mkInput contractLock bullToken 1UL)] []
match redeem tx1Bull okData wallet50ZP with
| Ok tx ->
    printfn "OK: Redeem succeeds with OK Data"
| Error e -> failwithf "Failed with unexpected error: `%A`" e

/////////////////////////////////////////////////////////////////////////////
// Redeem with bull token fails with low strike, but succeeds with bear token
/////////////////////////////////////////////////////////////////////////////

let lowStrike = mkData returnAddress (strike-1UL)
let tx25Bull = mkTx [(mkInput contractLock bullToken 25UL)] []
let tx25Bear = mkTx [(mkInput contractLock bearToken 25UL)] []

match redeem tx25Bull lowStrike wallet50ZP with
| Error "Could not construct tx from wallet" ->
    printfn "OK: Redeem with bull tokens fails for low strike"
| Ok cr -> failwithf "Should not return ok! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

match redeem tx25Bear lowStrike wallet50ZP with
| Ok tx ->
    printfn "OK: Redeem with bear tokens succeeds for low strike"
| Error e -> failwithf "Failed with unexpected error: `%A`" e
