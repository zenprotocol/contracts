module FString    = FStar.String
module Hash       = Consensus.Hash
module ZFStar     = Consensus.ZFStar
module Crypto     = Consensus.Crypto
module Data       = Zen.Types.Data
module Extracted  = Zen.Types.Extracted
module Sha3       = Zen.Hash.Sha3
module PKModule   = Crypto.PublicKey

#r "/home/ariel/workspace/changes/ContractsTestingLib/src/ContractsTestingLib/bin/Release/ContractsTestingLib.dll"
//open Execute

module Input       = ContractInput
module AddRealized = Input.MessageBody.Realized.Option
module AddInput    = Input.MessageBody.Option

type fpcCommand =
    | CMD_Buy
    | CMD_Redeem
    | CMD_Other

type fpcCid =
    | CID_Oracle
    | CID_FP
    | CID_Other

type fpcPK =
    | PK_Issuer
    | PK_Redeemer
    | PK_Oracle
    | PK_Other

type attestation = {
    timestamp  : uint64;
    commit     : Types.Hash;
    pubKey     : fpcPK;
}

type betEvent = {
    oraclePubKey     : fpcPK  option;
    oracleContractId : fpcCid option;
    ticker           : string option;
    priceLow         : uint64 option;
    priceHigh        : uint64 option;
    timeLow          : uint64 option;
    timeHigh         : uint64 option;
}

type fpcAsset =
    | BullToken of betEvent
    | BearToken of betEvent
    | AttestToken of attestation
    | OtherToken

let CONTRACT_ID_FP     = Load.computeContractId "output/FixedPayout.fst"
let CONTRACT_ID_ORACLE = Load.computeContractId "../Oracle2/output/Oracle2.fst"
let CONTRACT_ID_OTHER = Load.computeContractId "../Dex001/Dex001.fst"

let generatePublicKey() =
    Crypto.KeyPair.create() |> snd

let PK_ISSUER   = generatePublicKey()
let PK_REDEEMER = generatePublicKey()
let PK_ORACLE   = generatePublicKey()
let PK_OTHER    = generatePublicKey()



(*
------------------------------------------------------------------------------------------------------------------------
======================================== CREATE DATA ===================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let FIELD_TIMESTAMP          = "Timestamp"B
let FIELD_COMMIT             = "Commit"B
let FIELD_ORACLE_PUB_KEY     = "OraclePubKey"B
let FIELD_TICKER             = "Ticker"B
let FIELD_PRICE_LOW          = "PriceLow"B
let FIELD_PRICE_HIGH         = "PriceHigh"B
let FIELD_TIME_LOW           = "TimeLow"B
let FIELD_TIME_HIGH          = "TimeHigh"B
let FIELD_AUDIT_PATH         = "AuditPath"B
let FIELD_VALUE              = "Value"B
let FIELD_CWT                = "CWT"B
let FIELD_DEFAULT_HASH       = "DefaultHash"B
let FIELD_POSITION           = "Position"B
let FIELD_ORACLE_CONTRACT_ID = "OracleContractId"B

(*
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

let addToDict (key, value) dict =
    Zen.Dictionary.add key value dict
    |> Zen.Cost.Realized.__force

let addU64 (key : FString.t) (value : uint64) = addToDict (key, Data.U64 value)

let addPK (key : FString.t) (value : Extracted.publicKey) = addToDict (key, Data.PublicKey value)

let addString (key : FString.t) (value : string) = addToDict (key, value |> ZFStar.fsToFstString |> Data.String)

let addPKLock (key : FString.t) (value : Extracted.publicKey) = addToDict (key, Extracted.PKLock (pk2hash value |> Hash.bytes) |> Data.Lock)

let mkBetEvenDict (bevent : betEvent) =
    Zen.Dictionary.empty
    |> match bevent.oraclePubKey     with | None -> id | Some x -> addPK     FIELD_ORACLE_PUB_KEY     x
    |> match bevent.oracleContractId with | None -> id | Some x -> addString FIELD_ORACLE_CONTRACT_ID x
    |> match bevent.ticker           with | None -> id | Some x -> addString FIELD_TICKER             x
    |> match bevent.priceLow         with | None -> id | Some x -> addU64    FIELD_PRICE_LOW          x
    |> match bevent.priceHigh        with | None -> id | Some x -> addU64    FIELD_PRICE_HIGH         x
    |> match bevent.timeLow          with | None -> id | Some x -> addU64    FIELD_TIME_LOW           x
    |> match bevent.timeHigh         with | None -> id | Some x -> addU64    FIELD_TIME_HIGH          x

let mkBetEventData =
    mkBetEvenDict
    >> Zen.Types.Data.Dict
    >> Zen.Types.Data.Collection
    >> Some
*)


(*
------------------------------------------------------------------------------------------------------------------------
======================================== REALIZER ======================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let realizeCommand cmd =
    match cmd with
    | CMD_Buy    -> "Buy"
    | CMD_Redeem -> "Redeem"
    | CMD_Other  -> "Other"

let realizePK pk =
    match pk with
    | PK_Issuer   -> PK_ISSUER
    | PK_Redeemer -> PK_REDEEMER
    | PK_Oracle   -> PK_ORACLE
    | PK_Other    -> PK_OTHER

let realizeContract c =
    match c with
    | CID_Oracle -> CONTRACT_ID_ORACLE
    | CID_FP     -> CONTRACT_ID_FP
    | CID_Other  -> CONTRACT_ID_OTHER

let updateCPK (parity, h) (s:Sha3.t): Sha3.t =
    s
    |> Sha3.updateByte parity |> Zen.Cost.Realized.__force
    |> Sha3.updateHash h      |> Zen.Cost.Realized.__force

let hashCPK cpk =
    Sha3.empty
    |> updateCPK cpk
    |> Sha3.finalize
    |> Zen.Cost.Realized.__force

let compress pk =
    let cpk = PKModule.serialize pk
    (cpk.[0], cpk.[1..])

let updatePublicKey pk s =
    updateCPK (compress pk) s

let updateContractId (v,h) s =
    s
    |> Sha3.updateU32  v |> Zen.Cost.Realized.__force
    |> Sha3.updateHash h |> Zen.Cost.Realized.__force

let runOpt update x s =
    match x with
    | Some x ->
        update x s
    | None ->
        s

let require update ox s =
    Option.map (fun x -> update x s) ox

let updateEvent bevent s =
    let (>>=) x f = Option.bind f x
    Some s
    >>= require updatePublicKey  (bevent.oraclePubKey     |> Option.map realizePK      )
    // >>= require updateContractId (bevent.oracleContractId |> Option.map realizeContract)
    // |> Sha3.updateString       bevent.ticker           |> Zen.Cost.Realized.__force
    // |> Sha3.updateU64          bevent.priceLow         |> Zen.Cost.Realized.__force
    // |> runOpt Sha3.updateU64   bevent.priceHigh        |> Zen.Cost.Realized.__force
    // |> Sha3.updateU64          bevent.timeLow          |> Zen.Cost.Realized.__force
    // |> runOpt Sha3.updateU64   bevent.timeHigh         |> Zen.Cost.Realized.__force

let realizeAsset asset : Option<Types.Asset> =
    match asset with
    | BullToken bevent ->
        //realizePK bevent.oraclePubKey
        //|> updatePublicKey
        failwith ""
    | BearToken bevent ->
        failwith ""
    | AttestToken attest ->
        failwith ""
    | OtherToken ->
        failwith ""

let rec fpcRealizer : AbstractContract.Realizer<fpcPK, fpcCid, fpcAsset, fpcCommand, betEvent> =
    {
        realizePK       = realizePK
        realizeContract = realizeContract
        realizeAsset    = realizeAsset
        realizeCommand  = realizeCommand
        realizeData     = realizeData
        thisContract    = CONTRACT_ID_FP
    }

and realizeData (bevent : betEvent) =
    let rl = fpcRealizer in
    Input.MessageBody.emptyDict
    |> AddRealized.add_pk       rl FIELD_ORACLE_PUB_KEY     bevent.oraclePubKey
    |> AddRealized.add_contract rl FIELD_ORACLE_CONTRACT_ID bevent.oracleContractId
    |> AddInput.add_string         FIELD_TICKER             bevent.ticker
    |> AddInput.add_uint64         FIELD_PRICE_LOW          bevent.priceLow
    |> AddInput.add_uint64         FIELD_PRICE_HIGH         bevent.priceHigh
    |> AddInput.add_uint64         FIELD_TIME_LOW           bevent.timeLow
    |> AddInput.add_uint64         FIELD_TIME_HIGH          bevent.timeHigh
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some
