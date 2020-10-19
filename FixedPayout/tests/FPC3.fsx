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
}

type proof = {
    key         : string;
    value       : uint64;
    root        : Types.Hash;
    auditPath   : Types.Hash list;
    index       : uint64;
}

type commit = {
    c_root      : Types.Hash;
    c_timestamp : uint64;
}

type attestation = {
    commit : commit;
    pubKey : fpcPK;
}

type betEvent = {
    oraclePubKey     : fpcPK;
    oracleContractId : fpcCid;
    ticker           : string;
    priceLow         : uint64;
    priceHigh        : uint64 option;
    start            : uint64;
    expiry           : uint64 option;
}

type betToken =
    | BullToken of betEvent
    | BearToken of betEvent

type fpcAsset =
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

let updateEvent bevent s =
    s
    |> updatePublicKey      (bevent.oraclePubKey     |> realizePK           )
    |> updateContractId     (bevent.oracleContractId |> realizeContract     )
    |> Sha3.updateString    (bevent.ticker           |> ZFStar.fsToFstString) |> Zen.Cost.Realized.__force
    |> Sha3.updateU64        bevent.priceLow                                  |> Zen.Cost.Realized.__force
    |> runOpt Sha3.updateU64 bevent.priceHigh
    |> Sha3.updateU64        bevent.start                                     |> Zen.Cost.Realized.__force
    |> runOpt Sha3.updateU64 bevent.expiry

let updateString str s =
    s
    |> Sha3.updateString (ZFStar.fsToFstString str)
    |> Zen.Cost.Realized.__force

let hashBet btoken =
    Sha3.empty |>
    (match btoken with
    | BullToken bevent -> updateEvent bevent >> updateString "Bull"
    | BearToken bevent -> updateEvent bevent >> updateString "Bear"
    )
    |> Sha3.finalize
    |> Zen.Cost.Realized.__force
    |> Some

let hashCommit commit =
    Sha3.empty
    |> Sha3.updateHash (commit.c_root |> Hash.bytes) |> Zen.Cost.Realized.__force
    |> Sha3.updateU64  commit.c_timestamp            |> Zen.Cost.Realized.__force
    |> Sha3.finalize                                 |> Zen.Cost.Realized.__force

let hashAttest attest =
    Sha3.empty
    |> Sha3.updateHash
        (Sha3.empty
        |> Sha3.updateHash (hashCommit attest.commit)   |> Zen.Cost.Realized.__force
        |> updatePublicKey (attest.pubKey |> realizePK)
        |> Sha3.finalize                                |> Zen.Cost.Realized.__force
        )
    |> Zen.Cost.Realized.__force
    |> Sha3.finalize |> Zen.Cost.Realized.__force

let realizeAsset asset : Option<Types.Asset> =
    let (|@>) x f = Option.map f x
    match asset with
    | BetToken btoken ->
        hashBet btoken |@> (fun betHash -> Types.Asset (CONTRACT_ID_FP, Hash.Hash betHash))
    | AttestToken attest ->
        match hashAttest attest with | attestHash -> Some (Types.Asset (CONTRACT_ID_ORACLE, Hash.Hash attestHash))
    | ZenToken ->
        Some Consensus.Asset.Zen
    | OtherToken ->
        failwith "not implemented yet"

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
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some



(*
------------------------------------------------------------------------------------------------------------------------
======================================== COMMAND: "Issue" ==============================================================
------------------------------------------------------------------------------------------------------------------------
*)

printfn "\n\n======================================== Issue ========================================================================="

let mutable run_test = Execute.init_testing_environment()

run_test "empty data & empty Tx"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Issue
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            None
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Data parsing failed - the message body is empty"
    end

run_test "empty data & 100 kalapas"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) ZenToken 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Issue
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            None
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Data parsing failed - the message body is empty"
    end

run_test "empty dictionary & empty Tx"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Issue
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                 _Timestamp        = None
                 _Root             = None
                 _OraclePubKey     = None
                 _Ticker           = None
                 _PriceLow         = None
                 _PriceHigh        = None
                 _Start            = None
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = None
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse OraclePubKey"
    end

run_test "valid data & empty Tx"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Issue
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                 _Timestamp        = None
                 _Root             = None
                 _OraclePubKey     = Some PK_Oracle
                 _Ticker           = Some "USD"
                 _PriceLow         = Some 123UL
                 _PriceHigh        = None
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_invalid_amounts
    end

let bevent001 = {
    oraclePubKey     = PK_Oracle//: fpcPK;
    oracleContractId = CID_Oracle//: fpcCid;
    ticker           = "USD"//: string;
    priceLow         = 123UL//: uint64;
    priceHigh        = None//: uint64 option;
    start            = 123UL//: uint64;
    expiry           = None//: uint64 option;
}

run_test "valid data & 100 kalapas"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) ZenToken 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Issue
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                 _Timestamp        = None
                 _Root             = None
                 _OraclePubKey     = Some PK_Oracle
                 _Ticker           = Some "USD"
                 _PriceLow         = Some 123UL
                 _PriceHigh        = None
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| BetToken (BullToken bevent001)) (Some 100UL)
            ; hasMint (Some <| BetToken (BearToken bevent001)) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BearToken bevent001)) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BullToken bevent001)) (Some 100UL)
            ]
            fpcRealizer
    end

run_test "valid data & 100 kalapas but no sender"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) ZenToken 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
             CMD_Issue
             |> realizeCommand
         sender      =
            Abs.AbsAnonymousSender
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                 _Timestamp        = None
                 _Root             = None
                 _OraclePubKey     = Some PK_Oracle
                 _Ticker           = Some "USD"
                 _PriceLow         = Some 123UL
                 _PriceHigh        = None
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Sender can't be anonymous"
    end
