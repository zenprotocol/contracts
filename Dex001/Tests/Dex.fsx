module Asset     = Consensus.Asset
module Contract   = Consensus.Contract
module ContractId = Consensus.ContractId
module Hash      = Consensus.Hash
module Result    = Infrastructure.Result
module Tx        = Consensus.TxSkeleton
module Types     = Consensus.Types
module Data      = Zen.Types.Data
module Main      = Zen.Types.Main
module String    = FStar.String
module Extracted  = Zen.Types.Extracted
module ZFStar    = Consensus.ZFStar
module Crypto    = Consensus.Crypto

#r "../output/Dex001.dll"
open Dex001

module Dex       = Dex001

let CONTRACT_DLL = "output/Dex001.dll"
let CONTRACT_SRC = "Dex001.fst"

let CMD_MAKE   = "Make"
let CMD_CANCEL = "Cancel"
let CMD_TAKE   = "Take"

let FIELD_UNDERLYING_ASSET  = "UnderlyingAsset"B
let FIELD_UNDERLYING_AMOUNT = "UnderlyingAmount"B
let FIELD_PAIR_ASSET        = "PairAsset"B
let FIELD_ORDER_TOTAL       = "OrderTotal"B
let FIELD_MAKER_PUB_KEY     = "MakerPubKey"B
let FIELD_NONCE             = "Nonce"B

let ZEN_ASSET     = "000000000000000000000000000000000000000000000000000000000000000000000000"
let XYZ_ASSET     = "000000000000000000012345678901234567899121323539832403208234092343590abc"
let INVALID_ASSET = "0000000000000000000123456789012345678991213235398324032082340abcdefghijk"

type orderData = {
    underlyingAsset  : string              option  // The identifier of the underlying asset
    underlyingAmount : uint64              option  // The amount of the underlying asset used to make the order
    pairAsset        : string              option  // The identifier of the pair asset
    orderTotal       : uint64              option  // The total amount of the pair being ordered
    makerPubKey      : Extracted.publicKey option  // The public key of the order maker
    nonce            : uint64              option  // Used to distinguish duplicate orders
}



(*
------------------------------------------------------------------------------------------------------------------------
======================================== OPTION BUILDER ================================================================
------------------------------------------------------------------------------------------------------------------------
*)

type OptionBuilder() =

    member this.Return<'a> (x : 'a) : 'a option =
        Some x

    member this.Bind<'a, 'b> (m : 'a option, f : 'a -> 'b option) : 'b option =
        Option.bind f m

    member this.ReturnFrom<'a> (opt : 'a option) : 'a option =
        opt

    member this.Zero() =
        None

    member this.Yield<'a> (x : 'a) : 'a option =
        Some x

    member this.YieldFrom<'a> (opt : 'a option) : 'a option =
        opt

    member this.Delay (f : unit -> 'a option) = f()
    
    member this.Combine (opt1 : 'a option , opt2 : 'a option) : 'a option =
        Option.bind (fun _ -> opt2) opt1

let option = OptionBuilder()

let (<@|) f x = Option.map f x
let (|@>) x f = Option.map f x
let (=<<) f x = Option.bind
let (>>=) x f = Option.bind f x
let (<*|) f x = Option.bind (fun x -> Option.map (fun f -> f x) f) x
let (|*>) x f = Option.bind (fun x -> Option.map (fun f -> f x) f) x



(*
------------------------------------------------------------------------------------------------------------------------
======================================== REPORTING FUNCTIONS ===========================================================
------------------------------------------------------------------------------------------------------------------------
*)

let disp s x = printfn "\n%s" s; x
let dispf s x = printfn s x; x

let pass() =
    printfn "PASSED"

let passWith e =
    printfn "PASSED with error: %A" e

let failWith e =
    printfn "FAILED with error: %A" e

let UNEXPECTED_SUCCESS = "Unexpected success"

let should_PASS x =
    match x with
    | Ok _    -> pass()
    | Error e -> failWith e
    x
    
let should_FAIL x =
    match x with
    | Ok _    -> failWith UNEXPECTED_SUCCESS
    | Error e -> passWith e
    x 



(*
------------------------------------------------------------------------------------------------------------------------
======================================== LOAD CONTRACT =================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let contractFn, costFn =
    System.Reflection.Assembly.LoadFrom CONTRACT_DLL 
    |> Contract.getFunctions
    |> Result.get
    
let getRawCode() = System.IO.File.ReadAllText CONTRACT_SRC
 
let contractID =
    getRawCode()
    |> Contract.makeContractId Types.Version0

let contractHash =
    match contractID with
    | Types.ContractId (_, contractHash) -> contractHash

let contractLock = Types.Contract contractID



(*
------------------------------------------------------------------------------------------------------------------------
======================================== CREATE DATA ===================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let emptyDict = Zen.Dictionary.empty

let addToDict (key, value) dict =
    Zen.Dictionary.add key value dict
    |> Zen.Cost.Realized.__force

let addU64 (key : String.t) (value : uint64) = addToDict (key, Data.U64 value)

let addPK (key : String.t) (value : Extracted.publicKey) = addToDict (key, Data.PublicKey value)

let addString (key : String.t) (value : string) = addToDict (key, value |> ZFStar.fsToFstString |> Data.String)

let mkOrderDict (odata : orderData) =
    emptyDict
    |> match odata.underlyingAsset  with | None -> id | Some x -> addString  FIELD_UNDERLYING_ASSET   x 
    |> match odata.underlyingAmount with | None -> id | Some x -> addU64     FIELD_UNDERLYING_AMOUNT  x 
    |> match odata.pairAsset        with | None -> id | Some x -> addString  FIELD_PAIR_ASSET         x
    |> match odata.orderTotal       with | None -> id | Some x -> addU64     FIELD_ORDER_TOTAL        x
    |> match odata.makerPubKey      with | None -> id | Some x -> addPK      FIELD_MAKER_PUB_KEY      x
    |> match odata.nonce            with | None -> id | Some x -> addU64     FIELD_NONCE              x

let mkOrderData =
    mkOrderDict
    >> Zen.Types.Data.Dict
    >> Zen.Types.Data.Collection
    
let generatePublicKey() =
    ZFStar.fsToFstPublicKey <| (Crypto.KeyPair.create() |> snd)



(*
------------------------------------------------------------------------------------------------------------------------
======================================== CREATE TRANSACTION ============================================================
------------------------------------------------------------------------------------------------------------------------
*)

type Hash    = Hash.Hash
type Input    = Tx.Input
type Outpoint = Types.Outpoint
type Output   = Types.Output
type Spend    = Types.Spend

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

// generate a pointed output from an output
let mkPointedOutput lock asset amount : Consensus.Types.PointedOutput =
    let outpoint = {Outpoint.txHash=Hash.zero; Outpoint.index=0u}
    (outpoint, mkOutput lock asset amount)

// generate a mint input from the specified asset and amount
let mkMint asset amount : Input =
    Tx.Mint (mkSpend asset amount)

// convert F* asset to F# asset
let fstAssetToFs (version, contractHash, hash) =
    Types.Asset( Types.ContractId(version, Hash.Hash contractHash), Hash.Hash hash)

// compute the order asset from the order data
let computeOrderAsset (odata : orderData) =
    odata
    |> mkOrderDict
    |> Some
    |> Dex.getOrder
    |> Zen.Cost.Realized.__force
    |> ZFStar.toResult
    |> function
       | Ok x    -> Some x
       | Error e -> None
    |> Option.map (Dex.getOrderAsset (ZFStar.fsToFstContractId contractID) >> Zen.Cost.Realized.__force >> fstAssetToFs)



(*
------------------------------------------------------------------------------------------------------------------------
======================================== EXECUTE CONTRACT ==============================================================
------------------------------------------------------------------------------------------------------------------------
*)

let valid_order_make (odata : orderData) =
    let txSkeleton  =
        Option.defaultValue emptyTx <| option {
            let! asset  = odata.underlyingAsset >>= Asset.fromString 
            let! amount = odata.underlyingAmount
            return mkTx [mkInput (Types.PK zeroHash) asset amount] [] 
        }
    let context     = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command     = CMD_MAKE
    let sender      = Main.PK (odata.makerPubKey |> Option.defaultWith generatePublicKey)
    let messageBody = Some <| mkOrderData odata
    let wallet      = []
    let initState   = None
    contractFn txSkeleton context contractID command sender messageBody wallet initState

let valid_order_cancel (odata : orderData) =
    let txSkeleton  = emptyTx
    let context     = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command     = CMD_CANCEL
    let sender      = Main.PK (odata.makerPubKey |> Option.defaultWith generatePublicKey)
    let messageBody = Some <| mkOrderData odata
    let wallet =
        Option.defaultValue [] <| option {
            let! unlAsset  = odata.underlyingAsset >>= Asset.fromString 
            let! unlAmount = odata.underlyingAmount
            let! ordAsset  = computeOrderAsset odata
            return
                [ mkPointedOutput (Types.PK zeroHash)         unlAsset unlAmount
                ; mkPointedOutput (Types.Contract contractID) ordAsset 1UL
                ]
        }
    let initState   = None
    contractFn txSkeleton context contractID command sender messageBody wallet initState

let order_make_modified_tx (txAssetAmount : (string * uint64) option) (odata : orderData) =
    let txSkeleton  =
        Option.defaultValue emptyTx <| option {
            let! asset  = txAssetAmount |@> fst >>= Asset.fromString 
            let! amount = txAssetAmount |@> snd
            return mkTx [mkInput (Types.PK zeroHash) asset amount] [] 
        }
    let context     = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command     = CMD_MAKE
    let sender      = Main.PK (odata.makerPubKey |> Option.defaultWith generatePublicKey)
    let messageBody = Some <| mkOrderData odata
    let wallet      = []
    let initState   = None
    contractFn txSkeleton context contractID command sender messageBody wallet initState

let order_make_modified_sender (pk : Extracted.publicKey) (odata : orderData) =
    let txSkeleton  =
        Option.defaultValue emptyTx <| option {
            let! asset  = odata.underlyingAsset >>= Asset.fromString 
            let! amount = odata.underlyingAmount
            return mkTx [mkInput (Types.PK zeroHash) asset amount] [] 
        }
    let context     = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command     = CMD_MAKE
    let sender      = Main.PK pk
    let messageBody = Some <| mkOrderData odata
    let wallet      = []
    let initState   = None
    contractFn txSkeleton context contractID command sender messageBody wallet initState

let order_make_modified_wallet (unlAmount : uint64 option, pairAmount : uint64 option, ordAmount : uint64 option) (odata : orderData) =
    let txSkeleton  =
        Option.defaultValue emptyTx <| option {
            let! asset  = odata.underlyingAsset >>= Asset.fromString 
            let! amount = odata.underlyingAmount
            return mkTx [mkInput (Types.PK zeroHash) asset amount] [] 
        }
    let context     = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command     = CMD_MAKE
    let sender      = Main.PK (odata.makerPubKey |> Option.defaultWith generatePublicKey)
    let messageBody = Some <| mkOrderData odata
    let wallet      =
        Option.defaultValue [] <| option {
                    let! unlAsset  = odata.underlyingAsset >>= Asset.fromString
                    let! pairAsset = odata.pairAsset       >>= Asset.fromString 
                    let! ordAsset  = computeOrderAsset odata
                    return
                        [ mkPointedOutput (Types.PK zeroHash)         unlAsset  <@| unlAmount
                        ; mkPointedOutput (Types.PK zeroHash)         pairAsset <@| pairAmount
                        ; mkPointedOutput (Types.Contract contractID) ordAsset  <@| ordAmount
                        ] |> List.choose id
                }
    let initState   = None
    contractFn txSkeleton context contractID command sender messageBody wallet initState
