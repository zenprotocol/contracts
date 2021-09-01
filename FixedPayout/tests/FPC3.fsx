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
    _Price            : uint64            option;
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
    price            : uint64;
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
let CONTRACT_ID_ORACLE = Load.computeContractId "../Oracle2/Oracle2.fst"
let CONTRACT_ID_OTHER  = Load.computeContractId "../Dex/Dex.fst"

let generatePublicKey() =
    Crypto.KeyPair.create() |> snd

let PK_ISSUER   = generatePublicKey()
let PK_REDEEMER = generatePublicKey()
let PK_ORACLE   = ProofData.oracle_pk
let PK_OTHER    = generatePublicKey()

let fpcMain, fpcCost = Load.extractMainAndCost "output/FixedPayout.dll"

let OTHER_TOKEN_STRING = "00000000ea0491531b62de13d9760c6d9dd4046316080d1339daae5d2072811815c6bbe39597cfa4856a2863d63c554f0d9d81541f1de480af3709cd81f4a8d43f3aab8f"



(*
------------------------------------------------------------------------------------------------------------------------
======================================== CREATE DATA ===================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let FIELD_TIMESTAMP          = "Timestamp"B
let FIELD_ROOT               = "Root"B
let FIELD_ORACLE_PUB_KEY     = "OraclePubKey"B
let FIELD_TICKER             = "Ticker"B
let FIELD_PRICE              = "Price"B
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
    |> Sha3.updateU64        bevent.price                                     |> Zen.Cost.Realized.__force
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
        "000000000000000000000000000000000000000000000000000000000000000000000000"
        |> Consensus.Asset.fromString
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
    Input.MessageBody.emptyDict()
    |> AddInput.add_uint64         FIELD_TIMESTAMP          data._Timestamp
    |> AddInput.add_hash           FIELD_ROOT               data._Root
    |> AddRealized.add_pk       rl FIELD_ORACLE_PUB_KEY     data._OraclePubKey
    |> AddInput.add_string         FIELD_TICKER             data._Ticker
    |> AddInput.add_uint64         FIELD_PRICE              data._Price
    |> AddInput.add_uint64         FIELD_START              data._Start
    |> AddInput.add_uint64         FIELD_EXPIRY             data._Expiry
    |> AddInput.add_hash_list      FIELD_AUDIT_PATH         data._AuditPath
    |> AddInput.add_uint64         FIELD_VALUE              data._Value
    |> AddInput.add_uint64         FIELD_INDEX              data._Index
    |> AddInput.add_string         FIELD_POSITION           data._Position
    |> AddRealized.add_contract rl FIELD_ORACLE_CONTRACT_ID data._OracleContractId
    |> AddRealized.add_asset    rl FIELD_COLLATERAL         data._Collateral
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some


(*
------------------------------------------------------------------------------------------------------------------------
======================================== Tokenization Tests ============================================================
------------------------------------------------------------------------------------------------------------------------
*)

let mutable ctr = 0

let cvtAsset (Types.Asset (Types.ContractId (version, assetType), subType)) = (version, Consensus.Hash.bytes assetType, Consensus.Hash.bytes subType)

let CID1 =
   "00000000f24db32aa1881956646d3ccbb647df71455de10cf98b635810e8870906a56b63"
   |> Consensus.ContractId.fromString
   |> Option.get
   |> Consensus.ZFStar.fsToFstContractId

let CID2 =
   "00000000e3113f8bf9cf8b764d945d6f99c642bdb069d137bdd5f7e44f1e75947f58a044"
   |> Consensus.ContractId.fromString
   |> Option.get
   |> Consensus.ZFStar.fsToFstContractId

let ASSET1 =
   "00000000ea0491531b62de13d9760c6d9dd4046316080d1339daae5d2072811815c6bbe39597cfa4856a2863d63c554f0d9d81541f1de480af3709cd81f4a8d43f3aab8f"
   |> Consensus.Asset.fromString
   |> Option.get
   |> cvtAsset

let ASSET2 =
   "00000000f24db32aa1881956646d3ccbb647df71455de10cf98b635810e8870906a56b630000000000000000000000000000000000000000000000000000000000000000"
   |> Consensus.Asset.fromString
   |> Option.get
   |> cvtAsset

printfn "\n\n======================================== Tokenization ==================================================================="

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk2
         oracleContractId = CID2
         ticker = "XYZW" |> Consensus.ZFStar.fsToFstString
         price = 17UL
         start = 4321UL
         expiry = Some 22UL
         collateral = ASSET2
      }
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bear }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash (only pks are different)" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 = { bevent1 with oraclePubKey = pk2 }
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bull }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash (only contract IDs are different)" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 = { bevent1 with oracleContractId = CID2 }
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bull }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash (only tickers are different)" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 = { bevent1 with ticker = "XYZW" |> Consensus.ZFStar.fsToFstString }
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bull }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash (only prices are different)" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 = { bevent1 with price = 17UL }
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bull }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash (only starts are different)" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 = { bevent1 with start = 4321UL }
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bull }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash (only expiries are different)" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 = { bevent1 with expiry = Some 22UL }
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bull }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash (only collaterals are different)" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 = { bevent1 with collateral = ASSET2 }
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bull }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) different data should give different hash (only positions are different)" ctr
   
   let pk1 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   let pk2 = Consensus.ZFStar.fsToFstPublicKey <| generatePublicKey()
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   
   let bevent1 : FixedPayout.betEvent = 
      {
         oraclePubKey = pk1
         oracleContractId = CID1
         ticker = "ABCD" |> Consensus.ZFStar.fsToFstString
         price = 15UL
         start = 1234UL
         expiry = Some 18UL
         collateral = ASSET1
      }

   let bevent2 = bevent1
   
   let bet1 = { bevent = bevent1 ; position = Bull }
   let bet2 = { bevent = bevent2 ; position = Bear }

   let res1 = FixedPayout.hashBet bet1 |> Zen.Cost.Realized.__force
   let res2 = FixedPayout.hashBet bet2 |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

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
                 _Price            = None
                 _Start            = None
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = None
                 _Collateral       = None
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "No collateral provided"
    end

let bevent001 = {
    oraclePubKey     = PK_Oracle//: fpcPK;
    oracleContractId = CID_Oracle//: fpcCid;
    ticker           = "USD"//: string;
    price            = 123UL//: uint64;
    start            = 123UL//: uint64;
    expiry           = None//: uint64 option;
    collateral       = ZenToken
}

let bevent002 = { bevent001 with collateral = OtherToken }

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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some ZenToken
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
            ; hasInput  (Some <| Abs.AbsPK PK_Issuer) (Some <| ZenToken) (Some 100UL)
            ; hasOutput  (Some <| Abs.AbsContract Abs.ThisContract) (Some <| ZenToken) (Some 100UL)
            ]
            fpcRealizer
    end

run_test "valid data & 100 kalapas but no collateral decleration"
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = None
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Collateral"
    end

run_test "valid data & 100 non-zen asset"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) OtherToken 100UL
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some OtherToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| BetToken (BullToken bevent002)) (Some 100UL)
            ; hasMint (Some <| BetToken (BearToken bevent002)) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BearToken bevent002)) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BullToken bevent002)) (Some 100UL)
            ; hasInput  (Some <| Abs.AbsPK PK_Issuer) (Some <| OtherToken) (Some 100UL)
            ; hasOutput  (Some <| Abs.AbsContract Abs.ThisContract) (Some <| OtherToken) (Some 100UL)
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some ZenToken
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
    price            = ProofData.price - 10UL
    start            = ProofData.timestamp - 10UL
    expiry           = Some (ProofData.timestamp + 10UL)
    collateral       = ZenToken
}

let beventBear = {
    oraclePubKey     = PK_Oracle
    oracleContractId = CID_Oracle
    ticker           = ProofData.ticker
    price            = ProofData.price + 10UL
    start            = ProofData.timestamp - 10UL
    expiry           = Some (ProofData.timestamp + 10UL)
    collateral       = ZenToken
}

let commit001 = {
    c_root       = ProofData.root
    c_timestamp  = ProofData.timestamp
}

let attest001 = {
    commit = commit001
    pubKey = PK_Oracle
}

let proof001 = {
    key         = ProofData.ticker;
    value       = ProofData.price;
    root        = ProofData.root;
    auditPath   = ProofData.path;
    index       = ProofData.index;
}

run_test <- Execute.init_testing_environment()

run_test "valid Bull redemption (100 ZP)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
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
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
            }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
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

run_test "valid Bull redemption (100 non-zen asset)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken {beventBull with collateral = OtherToken})) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some OtherToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, OtherToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BullToken {beventBull with collateral = OtherToken})) (Some 100UL)
            ; hasInput  (Some <| Abs.AbsContract (Abs.OtherContract CID_Oracle)) (Some <| AttestToken attest001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Redeemer) (Some OtherToken) (Some 100UL)
            ]
            fpcRealizer
    end

run_test "valid Bear redemption (100 non-zen asset)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken {beventBear with collateral = OtherToken})) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some OtherToken
            }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, OtherToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BearToken {beventBear with collateral = OtherToken})) (Some 100UL)
            ; hasInput  (Some <| Abs.AbsContract (Abs.OtherContract CID_Oracle)) (Some <| AttestToken attest001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Redeemer) (Some OtherToken) (Some 100UL)
            ]
            fpcRealizer
    end

run_test "Bull redemption - 100 non-zen asset but no collateral decleration"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken {beventBull with collateral = OtherToken})) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = None
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, OtherToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Collateral"
    end

run_test "Bear redemption - 100 non-zen asset declared but not provided"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken {beventBear with collateral = OtherToken})) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some OtherToken
            }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "Bear redemption - 100 non-zen asset declared but not provided (provided Zen instead)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken {beventBear with collateral = OtherToken})) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some OtherToken
            }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "Bull redemption (100 ZP) with empty wallet"
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
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
            }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "Bear redemption (100 ZP) with empty wallet"
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
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                   _Timestamp        = Some ProofData.timestamp
                   _Root             = Some ProofData.root
                   _OraclePubKey     = Some PK_Oracle
                   _Ticker           = Some ProofData.ticker
                   _Price            = Some beventBear.price
                   _Start            = Some beventBear.start
                   _Expiry           = beventBear.expiry
                   _AuditPath        = Some ProofData.path
                   _Value            = Some ProofData.price
                   _Index            = Some ProofData.index
                   _Position         = Some "Bear"
                   _OracleContractId = Some CID_Oracle
                   _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "wrong position"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
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
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
        context     =
           Input.Context.empty
           |> Input.Context.realize fpcRealizer
        command     =
           CMD_Redeem
           |> realizeCommand
        sender      =
           Abs.AbsPKSender PK_Redeemer
           |> Input.Sender.realize fpcRealizer
        messageBody =
           realizeData {
                 _Timestamp        = Some ProofData.timestamp
                 _Root             = Some ProofData.root
                 _OraclePubKey     = Some PK_Oracle
                 _Ticker           = Some ProofData.ticker
                 _Price            = Some beventBear.price
                 _Start            = Some beventBear.start
                 _Expiry           = beventBear.expiry
                 _AuditPath        = Some ProofData.path
                 _Value            = Some ProofData.price
                 _Index            = Some ProofData.index
                 _Position         = Some "Bear"
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some ZenToken
           }
        wallet      =
           Input.Wallet.empty
           |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
           |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
           |> Input.Wallet.realize fpcRealizer
        state       =
           None
   } |> should_FAIL_with "Insufficient funds"
    end

let beventBull_out_of_time = {
    oraclePubKey     = PK_Oracle
    oracleContractId = CID_Oracle
    ticker           = ProofData.ticker
    price            = ProofData.price - 10UL
    start            = ProofData.timestamp + 10UL
    expiry           = Some (ProofData.timestamp + 20UL)
    collateral       = ZenToken
}

run_test "out of time Bull"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull_out_of_time)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull_out_of_time.price
                  _Start            = Some beventBull_out_of_time.start
                  _Expiry           = beventBull_out_of_time.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Attestation time is not within the given time bounds"
    end

let beventBear_out_of_time = {
    oraclePubKey     = PK_Oracle
    oracleContractId = CID_Oracle
    ticker           = ProofData.ticker
    price            = ProofData.price + 10UL
    start            = ProofData.timestamp + 10UL
    expiry           = Some (ProofData.timestamp + 20UL)
    collateral       = ZenToken
}

run_test "out of time Bear"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear_out_of_time)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear_out_of_time.price
                  _Start            = Some beventBear_out_of_time.start
                  _Expiry           = beventBear_out_of_time.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Attestation time is not within the given time bounds"
    end

run_test "missing Timestamp (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = None
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Timestamp"
    end

run_test "missing Root (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = None
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Root"
    end

run_test "missing OraclePubKey (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = None
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse OraclePubKey"
    end

run_test "missing Ticker (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = None
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Ticker"
    end

run_test "missing Price (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = None
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Price"
    end

run_test "missing Start (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = None
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Start"
    end

run_test "missing Expiry (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = None
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "missing AuditPath (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = None
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse AuditPath"
    end

run_test "missing Value (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = None
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Value"
    end

run_test "missing Position (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = None
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Position"
    end

run_test "missing OracleContractId (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = None
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse OracleContractId"
    end

run_test "missing attestion token (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken beventBull)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Attestation token not found"
    end

run_test "missing bet token (Bull)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBull.price
                  _Start            = Some beventBull.start
                  _Expiry           = beventBull.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bull"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "missing Timestamp (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = None
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Timestamp"
    end

run_test "missing Root (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = None
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Root"
    end

run_test "missing OraclePubKey (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = None
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse OraclePubKey"
    end

run_test "missing Ticker (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = None
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Ticker"
    end

run_test "missing Price (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = None
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Price"
    end

run_test "missing Start (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = None
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Start"
    end

run_test "missing Expiry (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = None
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "missing AuditPath (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = None
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse AuditPath"
    end

run_test "missing Value (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = None
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Value"
    end

run_test "missing Position (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = None
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Position"
    end

run_test "missing OracleContractId (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = None
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse OracleContractId"
    end

run_test "missing attestion token (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken beventBear)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Attestation token not found"
    end

run_test "missing bet token (Bear)"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Redeem
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Redeemer
            |> Input.Sender.realize fpcRealizer
         messageBody =
             realizeData {
                  _Timestamp        = Some ProofData.timestamp
                  _Root             = Some ProofData.root
                  _OraclePubKey     = Some PK_Oracle
                  _Ticker           = Some ProofData.ticker
                  _Price            = Some beventBear.price
                  _Start            = Some beventBear.start
                  _Expiry           = beventBear.expiry
                  _AuditPath        = Some ProofData.path
                  _Value            = Some ProofData.price
                  _Index            = Some ProofData.index
                  _Position         = Some "Bear"
                  _OracleContractId = Some CID_Oracle
                  _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.add (Abs.AbsContract (Abs.OtherContract CID_Oracle), AttestToken attest001, 1UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end



(*
------------------------------------------------------------------------------------------------------------------------
======================================== COMMAND: "Cancel" ==============================================================
------------------------------------------------------------------------------------------------------------------------
*)

printfn "\n\n======================================== Cancel ========================================================================"

run_test <- Execute.init_testing_environment()

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
            CMD_Cancel
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            None
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
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
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken bevent001)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken bevent001)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Cancel
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Issuer
            |> Input.Sender.realize fpcRealizer
         messageBody =
            None
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Data parsing failed - the message body is empty"
    end

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
            CMD_Cancel
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
                 _Price            = None
                 _Start            = None
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = None
                 _Collateral       = None
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
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
            CMD_Cancel
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "valid data & 100 kalapas"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken bevent001)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken bevent001)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Cancel
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_Issuer) (Some <| ZenToken) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Issuer) (Some <| ZenToken) (Some 100UL)
            ; hasInput (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BearToken bevent001)) (Some 100UL)
            ; hasInput (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BullToken bevent001)) (Some 100UL)
            ]
            fpcRealizer
    end

run_test "valid data & 100 non-zen asset"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken bevent002)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken bevent002)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Cancel
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some OtherToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, OtherToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_Issuer) (Some <| OtherToken) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Issuer) (Some <| OtherToken) (Some 100UL)
            ; hasInput (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BearToken bevent002)) (Some 100UL)
            ; hasInput (Some <| Abs.AbsPK PK_Issuer) (Some <| BetToken (BullToken bevent002)) (Some 100UL)
            ]
            fpcRealizer
    end

run_test "valid data & 100 kalapas without declaring collateral"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken bevent001)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken bevent001)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Cancel
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = None
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Collateral"
    end

run_test "valid data & 100 non-zen asset without declaring collateral"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken bevent002)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken bevent002)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Cancel
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = None
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, OtherToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Collateral"
    end

run_test "valid data & 100 kalapas but no sender"
    begin
    Input.feedContract fpcMain CONTRACT_ID_FP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BearToken bevent001)) 100UL
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Issuer) (BetToken (BullToken bevent001)) 100UL
            |> Input.TxSkeleton.Abstract.realize fpcRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize fpcRealizer
         command     =
            CMD_Cancel
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
                 _Price            = Some 123UL
                 _Start            = Some 123UL
                 _Expiry           = None
                 _AuditPath        = None
                 _Value            = None
                 _Index            = None
                 _Position         = None
                 _OracleContractId = Some CID_Oracle
                 _Collateral       = Some ZenToken
             }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsPK PK_Issuer, ZenToken, 100UL)
            |> Input.Wallet.realize fpcRealizer
         state       =
            None
    } |> should_FAIL_with "Sender can't be anonymous"
    end
