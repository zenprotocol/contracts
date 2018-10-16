#load "Bet.fsx"

open Bet

module Asset = Consensus.Asset
module Contract = Consensus.Contract
module Hash = Consensus.Hash
module Result = Infrastructure.Result
module Tx = Consensus.TxSkeleton
module Types = Consensus.Types

//////////////////////////////////////////////////////////////////////////
// RedeemBull without returnAddress fails
//////////////////////////////////////////////////////////////////////////

match redeemBull emptyTx emptyMessageBody emptyWallet with
| Error "Something went wrong! could not parse returnAddress/oracleContractID" ->
    printfn "OK: RedeemBull without returnAddress fails"
| Ok cr -> failwithf "Should not return ok without returnAddress! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// RedeemBull with only returnAddress fails
//////////////////////////////////////////////////////////////////////////

match redeemBull emptyTx onlyReturnAddress emptyWallet with
| Error "Invalid message body" ->
    printfn "OK: RedeemBull with only returnAddress fails"
| Ok cr -> failwithf "Should not return ok without returnAddress! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// RedeemBull fails if ticker is wrong
//////////////////////////////////////////////////////////////////////////

let wrongTicker = mkData returnAddress unixtime "AMZN"B strike

match redeemBull emptyTx wrongTicker emptyWallet with
| Error "Invalid message body" ->
    printfn "OK: RedeemBull fails if ticker is wrong"
| Ok cr -> failwithf "Should not return ok without returnAddress! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// RedeemBull fails if time is wrong
//////////////////////////////////////////////////////////////////////////

let wrongTime = mkData returnAddress 0UL ticker strike

match redeemBull emptyTx wrongTime emptyWallet with
| Error "Invalid message body" ->
    printfn "OK: RedeemBull fails if time is wrong"
| Ok cr -> failwithf "Should not return ok without returnAddress! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// RedeemBull fails if price is too low
//////////////////////////////////////////////////////////////////////////

let badPrice = mkData returnAddress unixtime ticker (strike - 1UL)

match redeemBull emptyTx badPrice emptyWallet with
| Error "Invalid message body" ->
    printfn "OK: RedeemBull fails if price is too low"
| Ok cr -> failwithf "Should not return ok without returnAddress! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// RedeemBull fails if hash is incorrect
//////////////////////////////////////////////////////////////////////////

let badHash = mkData' returnAddress unixtime ticker strike zeroHash

match redeemBull emptyTx badHash emptyWallet with
| Error "Invalid message body" ->
    printfn "OK: RedeemBull fails if hash is incorrect"
| Ok cr -> failwithf "Should not return ok without returnAddress! Got ContractReturn `%A`" cr
| Error e -> failwithf "Failed with unexpected error: `%A`" e

//////////////////////////////////////////////////////////////////////////
// RedeemBull succeeds with OK Data
//////////////////////////////////////////////////////////////////////////

let okData = mkData returnAddress unixtime ticker strike
let tx1Bull =
    mkTx [(mkInput contractLock bullToken 1UL)] []
match redeemBull tx1Bull okData wallet50ZP with
| Ok tx ->
    printfn "OK: RedeemBull succeeds with OK Data"
| Error e -> failwithf "Failed with unexpected error: `%A`" e
