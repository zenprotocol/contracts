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
    | OtherToken

let CONTRACT_ID_FP     = Load.computeContractId "output/FixedPayout.fst"
let CONTRACT_ID_ORACLE = Load.computeContractId "../Oracle2/output/Oracle2.fst"
let CONTRACT_ID_OTHER  = Load.computeContractId "../Dex001/Dex001.fst"

let generatePublicKey() =
    Crypto.KeyPair.create() |> snd

let PK_ISSUER   = generatePublicKey()
let PK_REDEEMER = generatePublicKey()
let PK_ORACLE   = generatePublicKey()
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
        |> Sha3.updateHash (match attest.commit with | Hash.Hash h -> h) |> Zen.Cost.Realized.__force
        |> Sha3.updateU64   attest.timestamp                             |> Zen.Cost.Realized.__force
        |> updatePublicKey (attest.pubKey |> realizePK)
        |> Sha3.finalize                                                 |> Zen.Cost.Realized.__force
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
    | OtherToken ->
        failwith "not implemented yet"

let rec fpcRealizer : AbstractContract.Realizer<fpcPK, fpcCid, fpcAsset, fpcCommand, fpcData> =
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
    |> AddInput.add_uint64         FIELD_PRICE_LOW          data._PriceLow
    |> AddRealized.add_pk       rl FIELD_ORACLE_PUB_KEY     data._OraclePubKey
    |> AddInput.add_string         FIELD_TICKER             data._Ticker
    |> AddInput.add_uint64         FIELD_PRICE_LOW          data._PriceLow
    |> AddInput.add_uint64         FIELD_PRICE_HIGH         data._PriceHigh
    |> AddInput.add_uint64         FIELD_TIME_LOW           data._TimeLow
    |> AddInput.add_uint64         FIELD_TIME_HIGH          data._TimeHigh
    //|> add_hashlist                FIELD_AUDIT_PATH         data._AuditPath
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

let emptyInvocation_test() =
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
            AbstractContract.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            None
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    }

printfn "%A" (emptyInvocation_test())
