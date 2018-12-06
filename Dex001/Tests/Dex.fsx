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
    underlyingAsset  : string            // The identifier of the underlying asset
    underlyingAmount : uint64            // The amount of the underlying asset used to make the order
    pairAsset       : string            // The identifier of the pair asset
    orderTotal      : uint64            // The total amount of the pair being ordered
    makerPubKey     : Extracted.publicKey  // The public key of the order maker
    nonce          : uint64            // Used to distinguish duplicate orders
}



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

let mkOrderData (odata : orderData) =
    emptyDict
    |> addString  FIELD_UNDERLYING_ASSET   odata.underlyingAsset 
    |> addU64    FIELD_UNDERLYING_AMOUNT  odata.underlyingAmount 
    |> addString  FIELD_PAIR_ASSET        odata.pairAsset
    |> addU64    FIELD_ORDER_TOTAL       odata.orderTotal
    |> addPK     FIELD_MAKER_PUB_KEY     odata.makerPubKey
    |> addU64    FIELD_NONCE            odata.nonce
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection

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

// generate a mint input from the specified asset and amount
let mkMint asset amount : Input =
    Tx.Mint (mkSpend asset amount)



(*
------------------------------------------------------------------------------------------------------------------------
======================================== EXECUTE CONTRACT ==============================================================
------------------------------------------------------------------------------------------------------------------------
*)

let valid_order_make (odata : orderData) =
    let txSkeleton  =
       Option.map (fun asset -> 
        mkTx
            [mkInput (Types.PK zeroHash) asset odata.underlyingAmount]
            []
       ) (Asset.fromString odata.underlyingAsset)
       |> Option.defaultWith (fun() -> mkTx [] [])
    let context    = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command    = CMD_MAKE
    let sender     = Main.PK odata.makerPubKey
    let messageBody = Some <| mkOrderData odata
    let wallet     = []
    let initState   = None
    contractFn txSkeleton context contractID command sender messageBody wallet initState

let valid_order_cancel (odata : orderData) =
    let txSkeleton  =
        mkTx
            []
            []
    let context    = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command    = CMD_CANCEL
    let sender     = Main.PK odata.makerPubKey
    let messageBody = Some <| mkOrderData odata
    let wallet     = []
    let initState   = None    
    contractFn txSkeleton context contractID command sender messageBody wallet initState



(*
------------------------------------------------------------------------------------------------------------------------
======================================== TESTS =========================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let test_make_1 = valid_order_make {
    underlyingAsset  = ZEN_ASSET
    underlyingAmount = 100UL
    pairAsset        = ZEN_ASSET
    orderTotal       = 100UL
    makerPubKey      = ZFStar.fsToFstPublicKey <| (Crypto.KeyPair.create() |> snd)
    nonce            = 1UL
}
printfn "test_make_1 : %A" test_make_1

let test_make_2 = valid_order_make {
    underlyingAsset  = ZEN_ASSET
    underlyingAmount = 100UL
    pairAsset        = ZEN_ASSET
    orderTotal       = 5UL
    makerPubKey      = ZFStar.fsToFstPublicKey <| (Crypto.KeyPair.create() |> snd)
    nonce            = 1UL
}
printfn "test_make_2 : %A" test_make_2

let test_make_3 = valid_order_make {
    underlyingAsset  = ZEN_ASSET
    underlyingAmount = 100UL
    pairAsset        = XYZ_ASSET
    orderTotal       = 5UL
    makerPubKey      = ZFStar.fsToFstPublicKey <| (Crypto.KeyPair.create() |> snd)
    nonce            = 1UL
}
printfn "test_make_3 : %A" test_make_3

let test_make_4 = valid_order_make {
    underlyingAsset  = XYZ_ASSET
    underlyingAmount = 5UL
    pairAsset        = ZEN_ASSET
    orderTotal       = 100UL
    makerPubKey      = ZFStar.fsToFstPublicKey <| (Crypto.KeyPair.create() |> snd)
    nonce            = 1UL
}
printfn "test_make_4 : %A" test_make_4