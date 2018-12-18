module FString    = FStar.String
module Result     = Infrastructure.Result
module Asset      = Consensus.Asset
module Contract   = Consensus.Contract
module ContractId = Consensus.ContractId
module Hash       = Consensus.Hash
module Tx         = Consensus.TxSkeleton
module Types      = Consensus.Types
module ZFStar     = Consensus.ZFStar
module Crypto     = Consensus.Crypto
module Data       = Zen.Types.Data
module Main       = Zen.Types.Main
module Extracted  = Zen.Types.Extracted

type CR = Result<(Consensus.TxSkeleton.T * Option<Consensus.Types.Message> * Zen.Types.Main.stateUpdate),string>

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
let FIELD_REQUESTED_PAYOUT  = "RequestedPayout"B
let FIELD_RETURN_ADDRESS    = "returnAddress"B

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
    requestedPayout  : uint64              option  // The amount of the underlying to pay out
    returnAddress    : Extracted.publicKey option
}

let odataDefault = {
    underlyingAsset  = None
    underlyingAmount = None
    pairAsset        = None
    orderTotal       = None
    makerPubKey      = None
    nonce            = None
    requestedPayout  = None
    returnAddress    = None
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
let sequenceA xs = Infrastructure.Option.traverseA id xs



(*
------------------------------------------------------------------------------------------------------------------------
======================================== REPORTING FUNCTIONS ===========================================================
------------------------------------------------------------------------------------------------------------------------
*)

let disp s x = printfn "\n%s" s; x
let dispf s x = printfn s x; x

let run_test (tests : System.Collections.Generic.Dictionary<int, string * CR>) (counter : int ref) s x =
    printfn "\n\u2B24 (%d) - %s:" !counter s;
    tests.Add(!counter, (s,x));
    counter := !counter + 1
    x

let rec concatWith (sep : 'a list) (xss : 'a list list) : 'a list =
    match xss with
    | [] -> []
    | hd :: tl -> hd @ sep @ concatWith sep tl

//let wrap_internal v maxwidth msg =
//    let v = Seq.toList v
//    Seq.toList msg
//    |> fun xs ->
//        if String.length msg >= maxwidth
//            then xs
//                 |> List.splitInto (String.length msg / maxwidth)
//                 |> List.map (fun s -> v @ s @ v)
//                 |> concatWith ['\n']
//                 |> Array.ofList
//                 |> System.String
//            else msg

let wrap ul ur bl br h v d
         msg =
    let n   = String.length msg
    printfn " %s%s%s" ul (String.replicate (n + d) h) ur
//    printfn "%s" (wrap_internal v 100 msg)
    printfn " %s%s%s" v msg v
    printfn " %s%s%s" bl (String.replicate (n + d) h) br

let wrap_box =
    wrap "\u256D" "\u256E" "\u2570" "\u256F" "\u2500" "\u2502"

let wrap_invisible =
    wrap " " " " " " " " " " " "

let wrap_dbox =
    wrap "\u2554" "\u2557" "\u255A" "\u255D" "\u2550" "\u2551"  

let pass() =
    printfn "  \u2705 PASSED"
//    let msg = sprintf "\u2705 PASSED"
//    wrap_invisible 1 msg

let passWith e =
    printfn "  \u2705 PASSED with error: %A" e
//    let msg = sprintf "\u2705 PASSED with error: %A" e
//    wrap_invisible 1 msg

let failWith e =
    printfn "  \u26D4 FAILED with error: %A" e
//    let msg = sprintf "\u26D4 FAILED with error: %A \u26D4" e
//    wrap_invisible 1 msg
//    wrap_box 2 msg
    

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
    
let should_FAIL_with err x =
    match x with
    | Ok _    -> failWith UNEXPECTED_SUCCESS
    | Error e ->
        if e = err
            then passWith e
            else failWith e
    x

(* QUERY RESULT *)

type TxSkeleton = Consensus.TxSkeleton.T

type TxFailure =
    | MissingInput  of (Types.Lock option * string option * uint64 option)
    | MissingOutput of (Types.Lock option * string option * uint64 option)

type TestFailure =
    | ExecutionFailure of string
    | TxFailure of TxFailure list     

let (?=) x oy = match oy with | Some y -> x = y | None -> true 

let collectTxResults (res : Result<unit list, TxFailure list>) : Result<unit, TestFailure> =
    match res with
    | Ok _ -> Ok()
    | Error xs -> Error <| TxFailure xs 

let checkTx' (tests : (TxSkeleton -> Result<unit, TxFailure>) list) (cr : CR) : Result<unit, TestFailure>  =
    match cr with
    | Ok (tx,_,_) -> Result.traverseResultA (fun f -> f tx |> Result.mapError List.singleton) tests |> collectTxResults
    | Error e -> Error <| ExecutionFailure e 

let renderTxError (e : TxFailure) : string =
    let rndr x = match x with | None -> "_" | Some x -> sprintf "%A" x
    let rtup (lock, asset, amount) = sprintf "(lock=%s, asset=%s, amount=%s)" (rndr lock) (rndr asset) (rndr amount)
    match e with
    | MissingInput t -> sprintf "missing input: %s" (rtup t)
    | MissingOutput t -> sprintf "missing output: %s" (rtup t)

let checkTx (tests : (TxSkeleton -> Result<unit, TxFailure>) list) (cr : CR) : unit  =
    match checkTx' tests cr with
    | Ok() -> pass()
    | Error (TxFailure es) -> List.map (renderTxError >> failWith) es |> ignore
    | Error (ExecutionFailure e) -> failWith e

let hasInput (lock : Types.Lock option) (asset : string option) (amount : uint64 option) (tx : TxSkeleton) : Result<unit,TxFailure> =
    List.exists
        (function | Tx.PointedOutput (_,inp) -> (inp.lock ?= lock) && (inp.spend.asset ?= (asset |> Option.bind Asset.fromString)) && (inp.spend.amount ?= amount))
        tx.pInputs
    |> fun b -> if b then Ok() else Error <| MissingInput (lock, asset, amount)

let hasOutput (lock : Types.Lock option) (asset : string option) (amount : uint64 option) (tx : TxSkeleton) : Result<unit,TxFailure> =
    List.exists
        (function | (outp : Types.Output) -> (outp.lock ?= lock) && (outp.spend.asset ?= (asset |> Option.bind Asset.fromString)) && (outp.spend.amount ?= amount))
        tx.outputs
    |> fun b -> if b then Ok() else Error <| MissingOutput (lock, asset, amount)



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

let SerializedPublicKeyLength = 33

let private context = Crypto.Native.secp256k1_context_create (Crypto.Native.SECP256K1_CONTEXT_SIGN ||| Crypto.Native.SECP256K1_CONTEXT_VERIFY)

let serialize (Crypto.PublicKey publicKey) =
    let bytes = Array.create SerializedPublicKeyLength 0uy
    let mutable length = int64 SerializedPublicKeyLength

    match Crypto.Native.secp256k1_ec_pubkey_serialize(context, bytes, &&length, publicKey, Crypto.Native.SECP256K1_EC_COMPRESSED) with
    | Crypto.Native.Result.Ok ->
        if 33L = length then bytes else failwith "Wrong serialized size"
    | _ -> failwith "failed to serialize public key"

let pk2hash (pk : Extracted.publicKey) : Hash.Hash =
    serialize (Crypto.PublicKey pk) |> Hash.compute

let lockPK pk = Some <| Types.PK (pk2hash pk)

let lockContract = Some <| Types.Contract contractID

let lockDestroy = Some Types.Destroy

let emptyDict = Zen.Dictionary.empty

let addToDict (key, value) dict =
    Zen.Dictionary.add key value dict
    |> Zen.Cost.Realized.__force

let addU64 (key : FString.t) (value : uint64) = addToDict (key, Data.U64 value)

let addPK (key : FString.t) (value : Extracted.publicKey) = addToDict (key, Data.PublicKey value)

let addString (key : FString.t) (value : string) = addToDict (key, value |> ZFStar.fsToFstString |> Data.String)

let addPKLock (key : FString.t) (value : Extracted.publicKey) = addToDict (key, Extracted.PKLock (pk2hash value |> Hash.bytes) |> Data.Lock) 

let mkOrderDict (odata : orderData) =
    emptyDict
    |> match odata.underlyingAsset  with | None -> id | Some x -> addString  FIELD_UNDERLYING_ASSET   x 
    |> match odata.underlyingAmount with | None -> id | Some x -> addU64     FIELD_UNDERLYING_AMOUNT  x 
    |> match odata.pairAsset        with | None -> id | Some x -> addString  FIELD_PAIR_ASSET         x
    |> match odata.orderTotal       with | None -> id | Some x -> addU64     FIELD_ORDER_TOTAL        x
    |> match odata.makerPubKey      with | None -> id | Some x -> addPK      FIELD_MAKER_PUB_KEY      x
    |> match odata.nonce            with | None -> id | Some x -> addU64     FIELD_NONCE              x
    |> match odata.requestedPayout  with | None -> id | Some x -> addU64     FIELD_REQUESTED_PAYOUT   x
    |> match odata.returnAddress    with | None -> id | Some x -> addPKLock  FIELD_RETURN_ADDRESS     x

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

(* MAKE *)

let valid_order_make (odata : orderData) : CR =
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

let order_make_modified_tx (txAssetAmounts : (string * uint64) list) (odata : orderData) : CR =
    let txSkeleton  =
        txAssetAmounts
        |> List.map ( fun (assetString, amount) -> option {
            let! asset = assetString |> Asset.fromString
            return mkInput (Types.PK zeroHash) asset amount
            } )
        |> sequenceA |> function | None -> failwith "ERROR: Invalid asset string" | Some xs -> xs
        |> fun xs -> mkTx xs []
    let context     = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command     = CMD_MAKE
    let sender      = Main.PK (odata.makerPubKey |> Option.defaultWith generatePublicKey)
    let messageBody = Some <| mkOrderData odata
    let wallet      = []
    let initState   = None
    contractFn txSkeleton context contractID command sender messageBody wallet initState

let order_make_modified_sender (pk : Extracted.publicKey) (odata : orderData) : CR =
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

let order_make_modified_wallet (unlAmount : uint64 option, pairAmount : uint64 option, ordAmount : uint64 option) (odata : orderData) : CR =
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

(* CANCEL *)

let valid_order_cancel (odata : orderData) : CR =
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

let order_cancel_modified_wallet (unlAmount : uint64 option, pairAmount : uint64 option, ordAmount : uint64 option) (odata : orderData) : CR =
    let txSkeleton  =
        Option.defaultValue emptyTx <| option {
            let! asset  = odata.underlyingAsset >>= Asset.fromString 
            let! amount = odata.underlyingAmount
            return mkTx [mkInput (Types.PK zeroHash) asset amount] [] 
        }
    let context     = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command     = CMD_CANCEL
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

(* TAKE *)

let valid_order_take_full (odata : orderData) : CR =
    let txSkeleton  =
            Option.defaultValue emptyTx <| option {
                let! asset  = odata.pairAsset >>= Asset.fromString 
                let! amount = odata.orderTotal
                return mkTx [mkInput (Types.PK zeroHash) asset amount] [] 
            }
    let context     = {Types.ContractContext.blockNumber=1ul; Types.ContractContext.timestamp=0UL} : Types.ContractContext 
    let command     = CMD_TAKE 
    let sender      = Main.PK <| Option.defaultWith generatePublicKey odata.returnAddress
    let messageBody = Some <| mkOrderData { odata with requestedPayout = odata.underlyingAmount }
    let wallet      =
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