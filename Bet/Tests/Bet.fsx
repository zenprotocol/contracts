module Bet

module Asset = Consensus.Asset
module Contract = Consensus.Contract
module ContractId = Consensus.ContractId
module Hash = Consensus.Hash
module Result = Infrastructure.Result
module Tx = Consensus.TxSkeleton
module Types = Consensus.Types

(*                           *)
(* General utility functions *)
(*                           *)

type Hash = Hash.Hash
type Input = Tx.Input
type Outpoint = Types.Outpoint
type Output = Types.Output
type Spend = Types.Spend

let u64Bytes (u64: uint64) =
    let bytes = System.BitConverter.GetBytes u64
    if System.BitConverter.IsLittleEndian
    then Array.rev bytes
    else bytes

// Zero Hash
let zeroHash = Hash.zero

// Empty transaction
let emptyTx : Tx.T = Tx.empty

// Empty messageBody
let emptyMessageBody: Option<Zen.Types.Data.data> = None

// Empty wallet
let emptyWallet: Contract.ContractWallet = []

// generate a TxSkeleton from pointed inputs and outputs
let mkTx pInputs outputs = {Tx.pInputs=pInputs; Tx.outputs=outputs}

// generates a spend from the specified asset and amount
let mkSpend asset amount = {Spend.asset=asset; Spend.amount=amount}

// generate an output from a lock, asset, and amount
let mkOutput lock asset amount : Output =
    {lock=lock; spend=mkSpend asset amount}

// generate an input from an output
let mkInput lock asset amount : Input =
    let outpoint = {Outpoint.txHash=Hash.zero; Outpoint.index=0u}
    Tx.PointedOutput (outpoint, mkOutput lock asset amount)

// generate a mint input from the specified asset and amount
let mkMint asset amount : Input =
    Tx.Mint (mkSpend asset amount)

// try an input as a pointed output
let tryPointedOutput: Input -> Option<Output> = function
    | Tx.PointedOutput (_, output) -> Some output
    | Tx.Mint _ -> None

// try an input as a mint
let tryMint: Input -> Option<Spend> = function
    | Tx.PointedOutput _ -> None
    | Tx.Mint m -> Some m

// gets all pointed outputs in an input list
let getPointedOutputs: list<Input> -> list<Output> = List.choose tryPointedOutput

// all mints in the input
let getMints: list<Tx.Input> -> list<Spend> = List.choose tryMint

let getSpend: Input -> Spend = function
    | Tx.PointedOutput (_, output) -> output.spend
    | Tx.Mint spend -> spend

// all spends in the input
let getSpends: list<Input> -> list<Spend> = List.map getSpend

// all spends in the output
let outputSpends: list<Output> -> list<Spend> = List.map (fun output -> output.spend)

let hasLock (lock: Types.Lock) (output: Output) = output.lock = lock


(*                                   *)
(* Specialised to Bet contract below *)
(*                                   *)

let ticker = "AMD"B
let unixtime = 1539264654UL
let strike = 65000UL // USD price multiplied by 1000

let oracleContractID = "00000000ca055cc0af4d25ea1c8bbbf41444aadd68a168558397516b2f64727d87e72f97"
                       |> ContractId.fromString
                       |> Option.get

let getRawCode() = System.IO.File.ReadAllText "Bet.fst"

let contractID = getRawCode()
                 |> Contract.makeContractId Types.Version0

let contractHash = match contractID with
                   | Types.ContractId (_, contractHash) -> contractHash

let contractLock = Types.Contract contractID

// hex encoding of ascii value of "Bull"
let bullHex = "42756c6c"
// hex encoding of ascii value of "Bear"
let bearHex = "42656172"

let subTokenFromHex (hex: string): Types.Asset =
    let subTokenID = hex.PadRight(2 * Hash.Length, '0')
                     |> Hash.fromString
                     |> Result.get
    Types.Asset (contractID, subTokenID)

let zp = Asset.Zen
let bullToken = subTokenFromHex bullHex
let bearToken = subTokenFromHex bearHex

let returnAddressPK = Hash.zero

let returnAddress = Types.PK returnAddressPK
                    |> Consensus.ZFStar.fsToFstLock
                    |> Zen.Types.Data.Lock

let emptyDict = Zen.Dictionary.empty
let addToDict (key, value) dict = Zen.Dictionary.add key value dict
                                  |> Zen.Cost.Realized.__force
let addU64 (key, value) = addToDict (key, Zen.Types.Data.U64 value)
let addString (key, value) = addToDict (key, Zen.Types.Data.String value)
let addHash (key, value) = addToDict (key, Zen.Types.Data.Hash value)

let mkData' returnAddress time ticker price (Hash.Hash hash) =
    addToDict ("returnAddress"B, returnAddress) emptyDict
    |> addU64 ("Time"B, time)
    |> addString ("Ticker"B, ticker)
    |> addU64 ("Price"B, price)
    |> addHash ("Hash"B, hash)
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some

// a messageBody consisting of only a return address
let onlyReturnAddress =
    Zen.Dictionary.add "returnAddress"B returnAddress Zen.Dictionary.empty
    |> Zen.Cost.Realized.__force
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some

let hashParams'' time ticker price =
    (System.Reflection.Assembly.LoadFrom "output/Bet.dll")
        .GetModules().[0]  // Should get ModuleName.dll
        .GetTypes().[0]    // Should get ModuleName
        .GetMethod("hashParams") // ModuleName.name
        .Invoke(null, [|time; ticker; price|])
    :?> (Zen.Cost.Realized.cost<Zen.Types.Extracted.hash,unit>)


let hashParams' (time: uint64) ticker (price: uint64) =
    let h = hashParams'' time ticker price
            |> Zen.Cost.Realized.__force
            |> Hash.Hash
    printfn "%A" h
    h

let hashParams (time: uint64) ticker (price: uint64) =
     [ Hash.compute (u64Bytes time)
       Hash.compute ticker
       Hash.compute (u64Bytes price) ]
     |> Hash.joinHashes

let mkData returnAddress time ticker price =
    mkData' returnAddress time ticker price (hashParams time ticker price)

let wallet50ZP = // wallet with one 50ZP input
    let (Tx.PointedOutput p) = mkInput contractLock zp 50UL
    [p]

let fsToFstPointedOutput (opoint: Outpoint, oput: Output) =
    let outpoint = {Zen.Types.Extracted.txHash=Hash.bytes opoint.txHash; Zen.Types.Extracted.index=opoint.index}
    let (Types.Asset (Types.ContractId (version, assetType), subType)) = oput.spend.asset
    let output =
        { Zen.Types.Extracted.lock=Consensus.ZFStar.fsToFstLock oput.lock;
          Zen.Types.Extracted.spend={Zen.Types.Extracted.asset=version, Hash.bytes assetType, Hash.bytes subType; Zen.Types.Extracted.amount = oput.spend.amount}}
    outpoint, output

let fsToFstWallet = List.map fsToFstPointedOutput >> Consensus.ZFStar.fsToFstList

let wallet150ZP = // wallet with multiple inputs totalling 150ZP
    let (Tx.PointedOutput p0) = mkInput contractLock zp 50UL
    let (Tx.PointedOutput p1) = mkInput contractLock zp 30UL
    let (Tx.PointedOutput p2) = mkInput contractLock zp 10UL
    let (Tx.PointedOutput p3) = mkInput contractLock zp 20UL
    let (Tx.PointedOutput p4) = mkInput contractLock zp 40UL
    [p0; p1; p2; p3; p4]

let contractFn = System.Reflection.Assembly.LoadFrom "output/Bet.dll"
                 |> Contract.getFunctions
                 |> Result.get
                 |> fst

// don't care about context, contractID, command, sender, wallet or state
let buy (txSkeleton: Tx.T) messageBody =
    contractFn txSkeleton
               {blockNumber=1ul;timestamp=0UL}
               contractID
               "Buy"
               Types.Anonymous
               messageBody
               []
               None

// don't care about context, contractID, command, sender, or state
let redeemBull (txSkeleton: Tx.T) messageBody wallet =
    contractFn txSkeleton
               {blockNumber=1ul;timestamp=0UL}
               contractID
               "RedeemBull"
               Types.Anonymous
               messageBody
               wallet
               None

// don't care about context, contractID, command, sender, or state
let redeemBear (txSkeleton: Tx.T) messageBody wallet =
    contractFn txSkeleton
               {blockNumber=1ul;timestamp=0UL}
               contractID
               "RedeemBear"
               Types.Anonymous
               messageBody
               wallet
               None
