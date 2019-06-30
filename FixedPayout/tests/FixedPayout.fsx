module FString    = FStar.String
module Hash       = Consensus.Hash
module ZFStar     = Consensus.ZFStar
module Crypto     = Consensus.Crypto
module Types      = Consensus.Types
module Data       = Zen.Types.Data
module Extracted  = Zen.Types.Extracted
module Sha3       = Zen.Hash.Sha3
module PKModule   = Crypto.PublicKey

#r "/home/ariel/workspace/changes/ContractsTestingLib/src/ContractsTestingLib/bin/Release/ContractsTestingLib.dll"

#load "ProofData.fsx"

module Input       = ContractInput
module AddRealized = Input.MessageBody.Realized.Option
module AddInput    = Input.MessageBody.Option
module Abs         = AbstractContract

open TestResult

#r "../output/FixedPayout.dll"

open FixedPayout



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

type fpcData = {
    _Timestamp        : uint64            option;
    _Commit           : Types.Hash        option;
    _OraclePubKey     : fpcPK             option;
    _Ticker           : string            option;
    _PriceLow         : uint64            option;
    _PriceHigh        : uint64            option;
    _TimeLow          : uint64            option;
    _TimeHigh         : uint64            option;
    _AuditPath        : (Types.Hash list) option;
    _Value            : uint64            option;
    _CWT              : string            option;
    _DefaultHash      : Types.Hash        option;
    _Position         : string            option;
    _OracleContractId : fpcCid            option;
}

type proof = {
    key         : string;
    value       : uint64;
    root        : Types.Hash;
    auditPath   : Types.Hash list;
    cwt         : string;
    defaultHash : Types.Hash;
}

type attestation = {
    timestamp  : uint64;
    commit     : Types.Hash;
    pubKey     : fpcPK;
}

type betEvent = {
    oraclePubKey     : fpcPK;
    oracleContractId : fpcCid;
    ticker           : string;
    priceLow         : uint64;
    priceHigh        : uint64 option;
    timeLow          : uint64;
    timeHigh         : uint64 option;
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
    |> Sha3.updateU64        bevent.timeLow                                   |> Zen.Cost.Realized.__force
    |> runOpt Sha3.updateU64 bevent.timeHigh

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

let hashAttest attest =
    Sha3.empty
    |> Sha3.updateHash
        (Sha3.empty
        |> Sha3.updateHash (attest.commit |> Hash.bytes) |> Zen.Cost.Realized.__force
        |> Sha3.updateU64   attest.timestamp             |> Zen.Cost.Realized.__force
        |> updatePublicKey (attest.pubKey |> realizePK)
        |> Sha3.finalize                                 |> Zen.Cost.Realized.__force
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
    |> AddInput.add_hash           FIELD_COMMIT             data._Commit
    |> AddRealized.add_pk       rl FIELD_ORACLE_PUB_KEY     data._OraclePubKey
    |> AddInput.add_string         FIELD_TICKER             data._Ticker
    |> AddInput.add_uint64         FIELD_PRICE_LOW          data._PriceLow
    |> AddInput.add_uint64         FIELD_PRICE_HIGH         data._PriceHigh
    |> AddInput.add_uint64         FIELD_TIME_LOW           data._TimeLow
    |> AddInput.add_uint64         FIELD_TIME_HIGH          data._TimeHigh
    |> AddInput.add_hash_list      FIELD_AUDIT_PATH         data._AuditPath
    |> AddInput.add_uint64         FIELD_VALUE              data._Value
    |> AddInput.add_string         FIELD_CWT                data._CWT
    |> AddInput.add_hash           FIELD_DEFAULT_HASH       data._DefaultHash
    |> AddInput.add_string         FIELD_POSITION           data._Position
    |> AddRealized.add_contract rl FIELD_ORACLE_CONTRACT_ID data._OracleContractId
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some








(*
------------------------------------------------------------------------------------------------------------------------
======================================== COMMAND: "Buy" ================================================================
------------------------------------------------------------------------------------------------------------------------
*)

printfn "\n\n======================================== Buy ==========================================================================="

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
            "Buy"
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
            "Buy"
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
            "Buy"
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                 _Timestamp        = None
                 _Commit           = None
                 _OraclePubKey     = None
                 _Ticker           = None
                 _PriceLow         = None
                 _PriceHigh        = None
                 _TimeLow          = None
                 _TimeHigh         = None
                 _AuditPath        = None
                 _Value            = None
                 _CWT              = None
                 _DefaultHash      = None
                 _Position         = None
                 _OracleContractId = None
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL
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
            "Buy"
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                 _Timestamp        = None
                 _Commit           = None
                 _OraclePubKey     = Some PK_Oracle
                 _Ticker           = Some "USD"
                 _PriceLow         = Some 123UL
                 _PriceHigh        = None
                 _TimeLow          = Some 123UL
                 _TimeHigh         = None
                 _AuditPath        = None
                 _Value            = None
                 _CWT              = None
                 _DefaultHash      = None
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
    timeLow          = 123UL//: uint64;
    timeHigh         = None//: uint64 option;
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
            "Buy"
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                 _Timestamp        = None
                 _Commit           = None
                 _OraclePubKey     = Some PK_Oracle
                 _Ticker           = Some "USD"
                 _PriceLow         = Some 123UL
                 _PriceHigh        = None
                 _TimeLow          = Some 123UL
                 _TimeHigh         = None
                 _AuditPath        = None
                 _Value            = None
                 _CWT              = None
                 _DefaultHash      = None
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
            "Buy"
         sender      =
            Abs.AbsAnonymousSender
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                 _Timestamp        = None
                 _Commit           = None
                 _OraclePubKey     = Some PK_Oracle
                 _Ticker           = Some "USD"
                 _PriceLow         = Some 123UL
                 _PriceHigh        = None
                 _TimeLow          = Some 123UL
                 _TimeHigh         = None
                 _AuditPath        = None
                 _Value            = None
                 _CWT              = None
                 _DefaultHash      = None
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









(*
------------------------------------------------------------------------------------------------------------------------
======================================== COMMAND: "Redeem" =============================================================
------------------------------------------------------------------------------------------------------------------------
*)

printfn "\n\n======================================== Redeem ========================================================================"

let beventBull = {
    oraclePubKey     = PK_Oracle
    oracleContractId = CID_Oracle
    ticker           = ProofData.ticker
    priceLow         = ProofData.price - 10UL
    priceHigh        = Some (ProofData.price + 10UL)
    timeLow          = ProofData.timestamp - 10UL
    timeHigh         = Some (ProofData.timestamp + 10UL)
}

let beventBear = {
    oraclePubKey     = PK_Oracle
    oracleContractId = CID_Oracle
    ticker           = ProofData.ticker
    priceLow         = ProofData.price + 10UL
    priceHigh        = Some (ProofData.price + 20UL)
    timeLow          = ProofData.timestamp - 10UL
    timeHigh         = Some (ProofData.timestamp + 10UL)
}

let attest001 = {
    timestamp  = ProofData.timestamp
    commit     = ProofData.root
    pubKey     = PK_Oracle
}

let proof001 = {
    key         = ProofData.ticker;
    value       = ProofData.price;
    root        = ProofData.root;
    auditPath   = ProofData.path;
    cwt         = ProofData.cwt;
    defaultHash = ProofData.defaultHash;
}

run_test <- Execute.init_testing_environment()

run_test "valid Bull redemption (100 ZP)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsContract (Abs.OtherContract CID_Oracle)) (AttestToken attest001) 1UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            "Redeem"
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Commit           = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _PriceLow         = Some beventBull.priceLow
                  _PriceHigh        = beventBull.priceHigh
                  _TimeLow          = Some beventBull.timeLow
                  _TimeHigh         = beventBull.timeHigh
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _CWT              = Some ProofData.cwt
                  _DefaultHash      = Some ProofData.defaultHash
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BullToken beventBull)) (Some 100UL)
            ; hasInput  (Some <| Abs.AbsContract (Abs.OtherContract CID_Oracle)) (Some <| AttestToken attest001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Redeemer) (Some ZenToken) (Some 100UL)
            ]
            fpcRealizer
    end

run_test "valid Bear redemption (100 ZP)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsContract (Abs.OtherContract CID_Oracle)) (AttestToken attest001) 1UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            "Redeem"
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Commit           = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _PriceLow         = Some beventBear.priceLow
                  _PriceHigh        = beventBear.priceHigh
                  _TimeLow          = Some beventBear.timeLow
                  _TimeHigh         = beventBear.timeHigh
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _CWT              = Some ProofData.cwt
                  _DefaultHash      = Some ProofData.defaultHash
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BearToken beventBear)) (Some 100UL)
            ; hasInput  (Some <| Abs.AbsContract (Abs.OtherContract CID_Oracle)) (Some <| AttestToken attest001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Redeemer) (Some ZenToken) (Some 100UL)
            ]
            fpcRealizer
    end

run_test "wrong position"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsContract (Abs.OtherContract CID_Oracle)) (AttestToken attest001) 1UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            "Redeem"
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Commit           = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _PriceLow         = Some beventBull.priceLow
                  _PriceHigh        = beventBull.priceHigh
                  _TimeLow          = Some beventBull.timeLow
                  _TimeHigh         = beventBull.timeHigh
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _CWT              = Some ProofData.cwt
                  _DefaultHash      = Some ProofData.defaultHash
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Position doesn't match the event"
    end

run_test "wrong token"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsContract (Abs.OtherContract CID_Oracle)) (AttestToken attest001) 1UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            "Redeem"
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Commit           = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _PriceLow         = Some beventBear.priceLow
                  _PriceHigh        = beventBear.priceHigh
                  _TimeLow          = Some beventBear.timeLow
                  _TimeHigh         = beventBear.timeHigh
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _CWT              = Some ProofData.cwt
                  _DefaultHash      = Some ProofData.defaultHash
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_invalid_amounts
    end

let beventBull_out_of_time = {
    oraclePubKey     = PK_Oracle
    oracleContractId = CID_Oracle
    ticker           = ProofData.ticker
    priceLow         = ProofData.price - 10UL
    priceHigh        = Some (ProofData.price + 10UL)
    timeLow          = ProofData.timestamp + 10UL
    timeHigh         = Some (ProofData.timestamp + 20UL)
}

run_test "out of time Bull"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull_out_of_time)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsContract (Abs.OtherContract CID_Oracle)) (AttestToken attest001) 1UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            "Redeem"
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Commit           = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _PriceLow         = Some beventBull_out_of_time.priceLow
                  _PriceHigh        = beventBull_out_of_time.priceHigh
                  _TimeLow          = Some beventBull_out_of_time.timeLow
                  _TimeHigh         = beventBull_out_of_time.timeHigh
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _CWT              = Some ProofData.cwt
                  _DefaultHash      = Some ProofData.defaultHash
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Attestation time is not within the given time bounds"
    end

let beventBear_out_of_time = {
    oraclePubKey     = PK_Oracle
    oracleContractId = CID_Oracle
    ticker           = ProofData.ticker
    priceLow         = ProofData.price + 10UL
    priceHigh        = Some (ProofData.price + 20UL)
    timeLow          = ProofData.timestamp + 10UL
    timeHigh         = Some (ProofData.timestamp + 20UL)
}

run_test "out of time Bear"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear_out_of_time)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsContract (Abs.OtherContract CID_Oracle)) (AttestToken attest001) 1UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            "Redeem"
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Commit           = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _PriceLow         = Some beventBear_out_of_time.priceLow
                  _PriceHigh        = beventBear_out_of_time.priceHigh
                  _TimeLow          = Some beventBear_out_of_time.timeLow
                  _TimeHigh         = beventBear_out_of_time.timeHigh
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _CWT              = Some ProofData.cwt
                  _DefaultHash      = Some ProofData.defaultHash
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Attestation time is not within the given time bounds"
    end
