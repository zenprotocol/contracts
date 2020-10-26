module FString    = FStar.String
module Hash       = Consensus.Hash
module ZFStar     = Consensus.ZFStar
module Crypto     = Consensus.Crypto
module Types      = Consensus.Types
module Data       = Zen.Types.Data
module Extracted  = Zen.Types.Extracted
module Sha3       = Zen.Hash.Sha3
module PKModule   = Crypto.PublicKey

#load "ProofData.fsx"

module Input       = ContractInput
module AddRealized = Input.MessageBody.Realized.Option
module AddInput    = Input.MessageBody.Option
module Abs         = AbstractContract

open TestResult

#r "../output/FixedPayout.dll"

open FixedPayout


type fpcCommand =
    | CMD_Issue
    | CMD_Redeem
    | CMD_Cancel
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

type fpcData = {
    _Timestamp        : uint64            option;
    _Root             : Types.Hash        option;
    _OraclePubKey     : fpcPK             option;
    _Ticker           : string            option;
    _PriceLow         : uint64            option;
    _PriceHigh        : uint64            option;
    _Start            : uint64            option;
    _Expiry           : uint64            option;
    _AuditPath        : (Types.Hash list) option;
    _Value            : uint64            option;
    _Index            : uint64            option;
    _Position         : string            option;
    _OracleContractId : fpcCid            option;
    _Collateral       : fpcAsset          option;
}

and proof = {
    key         : string;
    value       : uint64;
    root        : Types.Hash;
    auditPath   : Types.Hash list;
    index       : uint64;
}

and commit = {
    c_root      : Types.Hash;
    c_timestamp : uint64;
}

and attestation = {
    commit : commit;
    pubKey : fpcPK;
}

and betEvent = {
    oraclePubKey     : fpcPK;
    oracleContractId : fpcCid;
    ticker           : string;
    priceLow         : uint64;
    priceHigh        : uint64 option;
    start            : uint64;
    expiry           : uint64 option;
    collateral       : fpcAsset;
}

and betToken =
    | BullToken of betEvent
    | BearToken of betEvent

and fpcAsset =
    | BetToken of betToken
    | AttestToken of attestation
    | ZenToken
    | OtherToken

let CONTRACT_ID_FP     = Load.computeContractId "output/FixedPayout.fst"
let CONTRACT_ID_ORACLE = Load.computeContractId "../Oracle2/output/Oracle2.fst"
let CONTRACT_ID_OTHER  = Load.computeContractId "../Dex001/Dex001.fst"

let generatePublicKey() =
    Crypto.KeyPair.create() |> snd

let PK_ISSUER   = generatePublicKey()
let PK_REDEEMER = generatePublicKey()
let PK_ORACLE   = ProofData.oracle_pk
let PK_OTHER    = generatePublicKey()

let fpcMain, fpcCost = Load.extractMainAndCost "output/FixedPayout.dll"

let OTHER_TOKEN_STRING = "00000000f24db32aa1881956646d3ccbb647df71455de10cf98b635810e8870906a56b63"

let fromString s =
    match Consensus.Hash.fromString s with
    | Ok h -> Some h
    | Error _ -> None


(*
------------------------------------------------------------------------------------------------------------------------
======================================== CREATE DATA ===================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let FIELD_TIMESTAMP          = "Timestamp"B
let FIELD_ROOT               = "Root"B
let FIELD_ORACLE_PUB_KEY     = "OraclePubKey"B
let FIELD_TICKER             = "Ticker"B
let FIELD_PRICE_LOW          = "PriceLow"B
let FIELD_PRICE_HIGH         = "PriceHigh"B
let FIELD_START              = "Start"B
let FIELD_EXPIRY             = "Expiry"B
let FIELD_AUDIT_PATH         = "AuditPath"B
let FIELD_INDEX              = "Index"B
let FIELD_VALUE              = "Value"B
let FIELD_POSITION           = "Position"B
let FIELD_ORACLE_CONTRACT_ID = "OracleContractId"B
let FIELD_COLLATERAL         = "Collateral"B



(*
------------------------------------------------------------------------------------------------------------------------
======================================== REALIZER ======================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let realizeCommand cmd =
    match cmd with
    | CMD_Issue  -> "Issue"
    | CMD_Redeem -> "Redeem"
    | CMD_Cancel -> "Cancel"
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

let updateContractId (Consensus.Types.ContractId (v, Hash.Hash h)) s =
    s
    |> Sha3.updateU32  v |> Zen.Cost.Realized.__force
    |> Sha3.updateHash h |> Zen.Cost.Realized.__force

let runOpt update ox st =
    match ox with
    | Some x -> update x st |> Zen.Cost.Realized.__force
    | None   -> st

let rec updateEvent bevent s =
    s
    |> updatePublicKey      (bevent.oraclePubKey     |> realizePK           )
    |> updateContractId     (bevent.oracleContractId |> realizeContract     )
    |> Sha3.updateString    (bevent.ticker           |> ZFStar.fsToFstString) |> Zen.Cost.Realized.__force
    |> Sha3.updateU64        bevent.priceLow                                  |> Zen.Cost.Realized.__force
    |> runOpt Sha3.updateU64 bevent.priceHigh
    |> Sha3.updateU64        bevent.start                                     |> Zen.Cost.Realized.__force
    |> runOpt Sha3.updateU64 bevent.expiry
    |> Sha3.updateAsset      (bevent.collateral |> realizeAsset |> Option.get |> fun (Types.Asset (Types.ContractId(v, Consensus.Hash.Hash cid),Consensus.Hash.Hash h)) -> (v,cid,h)) |> Zen.Cost.Realized.__force

and updateString str s =
    s
    |> Sha3.updateString (ZFStar.fsToFstString str)
    |> Zen.Cost.Realized.__force

and hashBet btoken =
    Sha3.empty |>
    (match btoken with
    | BullToken bevent -> updateEvent bevent >> updateString "Bull"
    | BearToken bevent -> updateEvent bevent >> updateString "Bear"
    )
    |> Sha3.finalize
    |> Zen.Cost.Realized.__force
    |> Some

and hashCommit commit =
    Sha3.empty
    |> Sha3.updateHash (commit.c_root |> Hash.bytes) |> Zen.Cost.Realized.__force
    |> Sha3.updateU64  commit.c_timestamp            |> Zen.Cost.Realized.__force
    |> Sha3.finalize                                 |> Zen.Cost.Realized.__force

and hashAttest attest =
    Sha3.empty
    |> Sha3.updateHash
        (Sha3.empty
        |> Sha3.updateHash (hashCommit attest.commit)   |> Zen.Cost.Realized.__force
        |> updatePublicKey (attest.pubKey |> realizePK)
        |> Sha3.finalize                                |> Zen.Cost.Realized.__force
        )
    |> Zen.Cost.Realized.__force
    |> Sha3.finalize |> Zen.Cost.Realized.__force

and realizeAsset asset : Option<Types.Asset> =
    let (|@>) x f = Option.map f x
    match asset with
    | BetToken btoken ->
        hashBet btoken |@> (fun betHash -> Types.Asset (CONTRACT_ID_FP, Hash.Hash betHash))
    | AttestToken attest ->
        match hashAttest attest with | attestHash -> Some (Types.Asset (CONTRACT_ID_ORACLE, Hash.Hash attestHash))
    | ZenToken ->
        Some Consensus.Asset.Zen
    | OtherToken ->
        OTHER_TOKEN_STRING
        |> Consensus.Asset.fromString


let rec fpcRealizer : Abs.Realizer<fpcPK, fpcCid, fpcAsset, fpcCommand, fpcData> =
    {
        realizePK       = realizePK
        realizeContract = realizeContract
        realizeAsset    = realizeAsset
        realizeCommand  = realizeCommand
        realizeData     = realizeData
        thisContract    = CONTRACT_ID_FP
    }

and realizeData (data : fpcData) =
    let rl = fpcRealizer in
    Input.MessageBody.emptyDict
    |> AddInput.add_uint64         FIELD_TIMESTAMP          data._Timestamp
    |> AddInput.add_hash           FIELD_ROOT               data._Root
    |> AddRealized.add_pk       rl FIELD_ORACLE_PUB_KEY     data._OraclePubKey
    |> AddInput.add_string         FIELD_TICKER             data._Ticker
    |> AddInput.add_uint64         FIELD_PRICE_LOW          data._PriceLow
    |> AddInput.add_uint64         FIELD_PRICE_HIGH         data._PriceHigh
    |> AddInput.add_uint64         FIELD_START              data._Start
    |> AddInput.add_uint64         FIELD_EXPIRY             data._Expiry
    |> AddInput.add_hash_list      FIELD_AUDIT_PATH         data._AuditPath
    |> AddInput.add_uint64         FIELD_VALUE              data._Value
    |> AddInput.add_uint64         FIELD_INDEX              data._Index
    |> AddInput.add_string         FIELD_POSITION           data._Position
    |> AddRealized.add_contract rl FIELD_ORACLE_CONTRACT_ID data._OracleContractId
    |> AddInput.add_string         FIELD_COLLATERAL         (data._Collateral |> Option.bind realizeAsset |> Option.map Consensus.Asset.toString)
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some



(*
------------------------------------------------------------------------------------------------------------------------
======================================== TEST DATA =====================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let data001 = realizeData {
    _Timestamp        = Some 1234UL
    _Root             = "3e47241505bca37f3356fd8dda544c2a3c9c043601f147ea0c6da1362c85a472" |> fromString
    _OraclePubKey     = Some PK_Oracle
    _Ticker           = Some "ABCD"
    _PriceLow         = Some 10UL
    _PriceHigh        = Some 20UL
    _Start            = Some 1111UL
    _Expiry           = Some 2222UL
    _AuditPath        = Some ProofData.path
    _Value            = Some 12UL
    _Index            = Some 0UL
    _Position         = Some "Bull"
    _OracleContractId = Some CID_Oracle
    _Collateral       = Some OtherToken
}

let bevent001 = {
    oraclePubKey     = PK_Oracle
    oracleContractId = CID_Oracle
    ticker           = "ABCD"
    priceLow         = 10UL
    priceHigh        = None
    start            = 1111UL
    expiry           = None
    collateral       = OtherToken
}

let hashWith upd x =
    Sha3.empty
    |> upd x
    |> Sha3.finalize
    |> Zen.Cost.Realized.__force

let h_bevent001 = 
    hashWith updateEvent bevent001

let h_bull001 =
    hashBet (BullToken bevent001)
    |> Option.get

let h_bear001 =
    hashBet (BearToken bevent001)
    |> Option.get


let h_publicKey = hashWith updatePublicKey ProofData.oracle_pk
let h_contractId = hashWith updateContractId CONTRACT_ID_ORACLE
let h_ticker = hashWith updateString bevent001.ticker
let h_priceLow = hashWith ((<<) Zen.Cost.Realized.__force << Sha3.updateU64) bevent001.priceLow
let h_start = hashWith ((<<) Zen.Cost.Realized.__force << Sha3.updateU64) bevent001.start
let h_collateral = hashWith ((<<) Zen.Cost.Realized.__force << Sha3.updateAsset) (bevent001.collateral |> realizeAsset |> Option.get |> fun (Types.Asset (Types.ContractId(v, Consensus.Hash.Hash cid),Consensus.Hash.Hash h)) -> (v,cid,h))


printfn "oraclePubKey = %s" ProofData.oracle_pk_str
printfn "oracleContractId = %s" (CONTRACT_ID_ORACLE.ToString())
printfn "ticker = %s" bevent001.ticker
printfn "priceLow = %d" bevent001.priceLow
printfn "start = %d" bevent001.start
printfn "collateral_asset = %s" OTHER_TOKEN_STRING
printfn "h_event = %s" (Consensus.Hash.Hash h_bevent001).AsString
printfn "h_bull = %s" (Consensus.Hash.Hash h_bull001).AsString
printfn "h_bear = %s" (Consensus.Hash.Hash h_bear001).AsString
printfn "h_publicKey = %s" (Consensus.Hash.Hash h_publicKey).AsString
printfn "h_contractId = %s" (Consensus.Hash.Hash h_contractId).AsString
printfn "h_ticker = %s" (Consensus.Hash.Hash h_ticker).AsString
printfn "h_priceLow = %s" (Consensus.Hash.Hash h_priceLow).AsString
printfn "h_start = %s" (Consensus.Hash.Hash h_start).AsString
printfn "h_collateral = %s" (Consensus.Hash.Hash h_collateral).AsString
