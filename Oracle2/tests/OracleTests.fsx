module FString    = FStar.String
module Hash       = Consensus.Hash
module ZFStar     = Consensus.ZFStar
module Crypto     = Consensus.Crypto
module Types      = Consensus.Types
module Data       = Zen.Types.Data
module Extracted  = Zen.Types.Extracted
module Sha3       = Zen.Hash.Sha3
module PKModule   = Crypto.PublicKey

module Input       = ContractInput
module AddRealized = Input.MessageBody.Realized.Option
module AddInput    = Input.MessageBody.Option
module Abs         = AbstractContract

open TestResult

#r "../output/Oracle2.dll"

open Oracle2

type ocCommand =
    | CMD_Commit
    | CMD_Attest

type ocCid =
    | CID_Oracle
    | CID_Other

type ocPK =
    | PK_Oracle
    | PK_Other

type ocData = {
    _Commit       : Types.Hash               option;
    _OraclePubKey : ocPK                     option;
    _Recipient    : Abs.AbsLock<ocPK, ocCid> option;
}

type commitment = {
    commit : Types.Hash;
    pubKey : ocPK;
}

type ocAsset =
    | CommitToken of commitment
    | AttestToken of commitment
    | ZenToken
    | OtherToken

let CONTRACT_ID_ORACLE = Load.computeContractId "output/Oracle2.fst"
let CONTRACT_ID_OTHER  = Load.computeContractId "../FixedPayout/output/FixedPayout.fst"

let generatePublicKey() =
    Crypto.KeyPair.create() |> snd

let PK_ORACLE = Consensus.Crypto.PublicKey "02ad784974b3f86ad97e008e20d2c107429041ed2d991ada2a1461b5077c11944c"B
let PK_OTHER  = generatePublicKey()

let ocMain, ocCost = Load.extractMainAndCost "output/Oracle2.dll"

let FIELD_COMMIT             = "Commit"B
let FIELD_ORACLE_PUB_KEY     = "OraclePubKey"B
let FIELD_RECIPIENT          = "Recipient"B



(*
------------------------------------------------------------------------------------------------------------------------
======================================== REALIZER ======================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let realizeCommand cmd =
    match cmd with
    | CMD_Commit -> "Commit"
    | CMD_Attest -> "Attest"

let realizePK pk =
    match pk with
    | PK_Oracle -> PK_ORACLE
    | PK_Other  -> PK_OTHER

let realizeContract c =
    match c with
    | CID_Oracle -> CONTRACT_ID_ORACLE
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

let hashCommit commit =
    Sha3.empty
    |> Sha3.updateHash (commit.commit |> Hash.bytes) |> Zen.Cost.Realized.__force
    |> updatePublicKey (commit.pubKey |> realizePK)
    |> Sha3.finalize                                 |> Zen.Cost.Realized.__force

let hashAttest commit =
    Sha3.empty
    |> Sha3.updateHash (hashCommit commit)
    |> Zen.Cost.Realized.__force
    |> Sha3.finalize |> Zen.Cost.Realized.__force

let realizeAsset asset : Option<Types.Asset> =
    let (|@>) x f = Option.map f x
    match asset with
    | CommitToken attest ->
        match hashCommit attest with | attestHash -> Some (Types.Asset (CONTRACT_ID_ORACLE, Hash.Hash attestHash))
    | AttestToken attest ->
        match hashAttest attest with | attestHash -> Some (Types.Asset (CONTRACT_ID_ORACLE, Hash.Hash attestHash))
    | ZenToken ->
        Some Consensus.Asset.Zen
    | OtherToken ->
        failwith "not implemented yet"

let rec ocRealizer : Abs.Realizer<ocPK, ocCid, ocAsset, ocCommand, ocData> =
    {
        realizePK       = realizePK
        realizeContract = realizeContract
        realizeAsset    = realizeAsset
        realizeCommand  = realizeCommand
        realizeData     = realizeData
        thisContract    = CONTRACT_ID_ORACLE
    }

and realizeData (data : ocData) =
    let rl = ocRealizer in
    Input.MessageBody.emptyDict
    |> AddInput.add_hash           FIELD_COMMIT             data._Commit
    |> AddRealized.add_pk       rl FIELD_ORACLE_PUB_KEY     data._OraclePubKey
    |> AddRealized.add_lock     rl FIELD_RECIPIENT          data._Recipient
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some
