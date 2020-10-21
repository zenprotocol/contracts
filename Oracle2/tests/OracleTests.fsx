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



(*
------------------------------------------------------------------------------------------------------------------------
======================================== DATA ==========================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let fromString s =
    match Consensus.Hash.fromString s with
    | Ok x -> Consensus.Hash.bytes x
    | Error err -> failwithf "%s - %s" err s

let commit001 = {
    commit = fromString "3e47241505bca37f3356fd8dda544c2a3c9c043601f147ea0c6da1362c85a472" |> Hash.Hash
    pubKey = PK_Oracle
}

let commit002 = {
    commit = fromString "3e47241505bca37f3356fd8dda544c2a3c9c043601f147ea0c6da1362c85a472" |> Hash.Hash
    pubKey = PK_Other
}

(*
------------------------------------------------------------------------------------------------------------------------
======================================== COMMAND: "Commit" =============================================================
------------------------------------------------------------------------------------------------------------------------
*)

printfn "\n\n======================================== Commit ========================================================================"

let mutable run_test = Execute.init_testing_environment()

run_test "valid commit - no OraclePubKey"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Commit
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Oracle
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = None
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| CommitToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ]
            ocRealizer
    end

run_test "valid commit - same OraclePubKey"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Commit
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Oracle
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some commit001.pubKey
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| CommitToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ]
            ocRealizer
    end

run_test "valid commit - other OraclePubKey"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Commit
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Oracle
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit002.commit
                  _OraclePubKey = Some commit002.pubKey
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| CommitToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ]
            ocRealizer
    end

run_test "invalid commit - no Commit field"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Commit
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Oracle
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = None
                  _OraclePubKey = Some commit001.pubKey
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Could not parse Commit"
    end

run_test "invalid commit - no sender"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Commit
            |> realizeCommand
         sender      =
            Abs.AbsAnonymousSender
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some commit001.pubKey
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Sender must be a public key"
    end



(*
------------------------------------------------------------------------------------------------------------------------
======================================== COMMAND: "Attest" =============================================================
------------------------------------------------------------------------------------------------------------------------
*)

printfn "\n\n======================================== Attest ========================================================================"

run_test <- Execute.init_testing_environment()

run_test "valid attest - no recipient (attestation token should be sent to sender)"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Other
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some PK_Oracle
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit001, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| AttestToken commit001) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ]
            ocRealizer
    end

run_test "valid attest - recipient is sender"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Other
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some PK_Oracle
                  _Recipient    = Some (Abs.AbsPK PK_Other)
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit001, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| AttestToken commit001) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ]
            ocRealizer
    end

run_test "valid attest - recipient is PK"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Other
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some PK_Oracle
                  _Recipient    = Some (Abs.AbsPK PK_Oracle)
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit001, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Oracle) (Some <| AttestToken commit001) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ]
            ocRealizer
    end

run_test "valid attest - recipient is contract"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Other
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some PK_Oracle
                  _Recipient    = Some (Abs.AbsContract <| Abs.OtherContract CID_Other)
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit001, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit001) (Some 1UL)
            ; hasOutput (Some <| (Abs.AbsContract <| Abs.OtherContract CID_Other)) (Some <| AttestToken commit001) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit001) (Some 1UL)
            ]
            ocRealizer
    end

run_test "invalid attest - no Commit"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Other
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = None
                  _OraclePubKey = Some PK_Oracle
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit001, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Couldn't find Commit in message body"
    end

run_test "invalid attest - no OraclePubKey"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Other
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = None
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit001, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Couldn't find OraclePubKey in message body"
    end

run_test "valid attest - no recipient and anonymous sender"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsAnonymousSender
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some PK_Oracle
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit001, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Unspecified recipient"
    end

run_test "invalid attest - no commit token"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Other
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some PK_Oracle
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Could not spend from wallet"
    end

run_test "invalid attest - wrong commit token"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize ocRealizer
         context     =
            Input.Context.empty
            |> Input.Context.realize ocRealizer
         command     =
            CMD_Attest
            |> realizeCommand
         sender      =
            Abs.AbsPKSender PK_Other
            |> Input.Sender.realize ocRealizer
         messageBody =
             realizeData {
                  _Commit       = Some commit001.commit
                  _OraclePubKey = Some PK_Oracle
                  _Recipient    = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Could not spend from wallet"
    end
