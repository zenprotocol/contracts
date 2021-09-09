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

type commitment = {
    commit : Types.Hash;
    pubKey : ocPK;
    feeAsset : ocAsset;
    feeAmount : uint64 option;
}

and ocAsset =
    | CommitToken of commitment
    | AttestToken of commitment
    | ZenToken
    | OtherToken

type ocData = {
    _Commit        : Types.Hash               option;
    _OraclePubKey  : ocPK                     option;
    _Recipient     : Abs.AbsLock<ocPK, ocCid> option;
    _FeeAsset      : ocAsset                  option;
    _FeeAmount     : uint64                   option;
    _ReturnAddress : Abs.AbsLock<ocPK, ocCid> option;
}

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
let FIELD_FEE_ASSET          = "FeeAsset"B
let FIELD_FEE_AMOUNT         = "FeeAmount"B
let FIELD_RETURN_ADDRESS     = "ReturnAddress"B

let fsToFstAsset (Types.Asset (Types.ContractId (version, assetType), subType)) = 
   (version, Hash.bytes assetType, Hash.bytes subType)

let test_counter = ref 1
let tests = new System.Collections.Generic.Dictionary<int, string * TestResult<unit>>()

let init_testing_environment() =
    Execute.run_test tests test_counter




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

let updateAsset asset s =
   s 
   |> Sha3.updateAsset asset |> Zen.Cost.Realized.__force

let updateAmount amount s = 
   s 
   |> Sha3.updateU64 amount |> Zen.Cost.Realized.__force

let rec hashCommit commit =
   match commit.feeAmount with
   | None ->
      Sha3.empty
      |> Sha3.updateHash (commit.commit |> Hash.bytes)  |> Zen.Cost.Realized.__force
      |> updatePublicKey (commit.pubKey |> realizePK)
      |> Sha3.finalize                                  |> Zen.Cost.Realized.__force
   | Some amount ->
      Sha3.empty
      |> Sha3.updateHash (commit.commit   |> Hash.bytes)   |> Zen.Cost.Realized.__force
      |> updatePublicKey (commit.pubKey   |> realizePK)
      |> updateAsset     (commit.feeAsset |> realizeAsset |> Option.get |> fsToFstAsset)
      |> updateAmount    amount
      |> Sha3.finalize                                     |> Zen.Cost.Realized.__force

and hashAttest commit =
    Sha3.empty
    |> Sha3.updateHash (hashCommit { commit with feeAmount = None })
    |> Zen.Cost.Realized.__force
    |> Sha3.finalize |> Zen.Cost.Realized.__force

and realizeAsset asset : Option<Types.Asset> =
    match asset with
    | CommitToken attest ->
        match hashCommit attest with | attestHash -> Some (Types.Asset (CONTRACT_ID_ORACLE, Hash.Hash attestHash))
    | AttestToken attest ->
        match hashAttest attest with | attestHash -> Some (Types.Asset (CONTRACT_ID_ORACLE, Hash.Hash attestHash))
    | ZenToken ->
        Some Consensus.Asset.Zen
    | OtherToken ->
        Some (Types.Asset (CONTRACT_ID_OTHER, Hash.zero))

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
    Input.MessageBody.emptyDict ()
    |> AddInput.add_hash           FIELD_COMMIT             data._Commit
    |> AddRealized.add_pk       rl FIELD_ORACLE_PUB_KEY     data._OraclePubKey
    |> AddRealized.add_lock     rl FIELD_RECIPIENT          data._Recipient
    |> AddRealized.add_asset    rl FIELD_FEE_ASSET          data._FeeAsset
    |> AddInput.add_uint64         FIELD_FEE_AMOUNT         data._FeeAmount
    |> AddRealized.add_lock     rl FIELD_RETURN_ADDRESS     data._ReturnAddress
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
    | Some x -> Consensus.Hash.bytes x
    | None -> failwithf "Couldn't parse hash from string - %s" s

let commit001 = {
    commit = fromString "3e47241505bca37f3356fd8dda544c2a3c9c043601f147ea0c6da1362c85a472" |> Hash.Hash
    pubKey = PK_Oracle
    feeAsset = ZenToken
    feeAmount = None
}

let commit002 = {
    commit = fromString "3e47241505bca37f3356fd8dda544c2a3c9c043601f147ea0c6da1362c85a472" |> Hash.Hash
    pubKey = PK_Other
    feeAsset = OtherToken
    feeAmount = None
}

let commit003 = {
    commit = fromString "3e47241505bca37f3356fd8dda544c2a3c9c043601f147ea0c6da1362c85a472" |> Hash.Hash
    pubKey = PK_Oracle
    feeAsset = ZenToken
    feeAmount = Some 100UL
}

let commit004 = {
    commit = fromString "3e47241505bca37f3356fd8dda544c2a3c9c043601f147ea0c6da1362c85a472" |> Hash.Hash
    pubKey = PK_Other
    feeAsset = OtherToken
    feeAmount = Some 200UL
}


(*
------------------------------------------------------------------------------------------------------------------------
======================================== Tokenization Tests ============================================================
------------------------------------------------------------------------------------------------------------------------
*)

let mutable ctr = 0

printfn "\n\n======================================== Tokenization ==================================================================="

// hashCommitData

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashCommitData - different data should give different hash" ctr
   let pk1 = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())
   let pk2 = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit1 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]
   let commit2 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 7uy ; 6uy ; 5uy |]

   let res1 = Oracle2.hashCommitData {commit = commit1 ; oraclePubKey = pk1 ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashCommitData {commit = commit2 ; oraclePubKey = pk2 ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashCommitData - different data should give different hash (only pks are different)" ctr
   let pk1 = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())
   let pk2 = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]

   let res1 = Oracle2.hashCommitData {commit = commit ; oraclePubKey = pk1 ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashCommitData {commit = commit ; oraclePubKey = pk2 ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashCommitData - different data should give different hash (only commits are different)" ctr
   let pk = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit1 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]
   let commit2 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 7uy ; 6uy ; 5uy |]

   let res1 = Oracle2.hashCommitData {commit = commit1 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashCommitData {commit = commit2 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashCommitData - when there is a a fee amount specified both the fee amount and the fee asset should be hashed (different assets)" ctr
   let pk = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit1 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]
   let commit2 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 7uy ; 6uy ; 5uy |]

   let res1 = Oracle2.hashCommitData {commit = commit1 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = Some 1UL} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashCommitData {commit = commit1 ; oraclePubKey = pk ; feeAsset = fsToFstAsset (Types.Asset (CONTRACT_ID_OTHER, Hash.zero)) ; feeAmount = Some 1UL} |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashCommitData - when there is a a fee amount specified both the fee amount and the fee asset should be hashed (different amounts)" ctr
   let pk = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit1 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]
   let commit2 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 7uy ; 6uy ; 5uy |]

   let res1 = Oracle2.hashCommitData {commit = commit1 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = Some 1UL} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashCommitData {commit = commit1 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = Some 2UL} |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

// hashAttestData

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashAttestData - different data should give different hash" ctr
   let pk1 = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())
   let pk2 = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit1 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]
   let commit2 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 7uy ; 6uy ; 5uy |]

   let res1 = Oracle2.hashAttestData {commit = commit1 ; oraclePubKey = pk1 ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashAttestData {commit = commit2 ; oraclePubKey = pk2 ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashAttestData - different data should give different hash (only pks are different)" ctr
   let pk1 = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())
   let pk2 = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]

   let res1 = Oracle2.hashAttestData {commit = commit ; oraclePubKey = pk1 ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashAttestData {commit = commit ; oraclePubKey = pk2 ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   if pk1 = pk2 then failwith "error: pk1 = pk2. this should rarely happen - please run the test again"
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashAttestData - different data should give different hash (only commits are different)" ctr
   let pk = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit1 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]
   let commit2 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 7uy ; 6uy ; 5uy |]

   let res1 = Oracle2.hashAttestData {commit = commit1 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashAttestData {commit = commit2 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = None} |> Zen.Cost.Realized.__force
   if res1 = res2 then printfn "  ⛔ FAILED - different data but same hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashAttestData - when there is a a fee amount specified it shouldn't affect hashing (different assets)" ctr
   let pk = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit1 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]
   let commit2 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 7uy ; 6uy ; 5uy |]

   let res1 = Oracle2.hashAttestData {commit = commit1 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = Some 1UL} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashAttestData {commit = commit1 ; oraclePubKey = pk ; feeAsset = fsToFstAsset (Types.Asset (CONTRACT_ID_ORACLE, Hash.zero)) ; feeAmount = Some 1UL} |> Zen.Cost.Realized.__force
   if res1 <> res2 then printfn "  ⛔ FAILED - this difference shouldn't affect hash"
   else printfn "  ✅ PASSED"

let _ =
   ctr <- ctr + 1
   printfn "\n⬤ (%d) hashAttestData - when there is a a fee amount specified it shouldn't affect hashing (different amounts)" ctr
   let pk = Consensus.ZFStar.fsToFstPublicKey (generatePublicKey())

   let commit1 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 1uy ; 2uy ; 3uy |]
   let commit2 = Consensus.Hash.bytes <| Consensus.Hash.compute [| 7uy ; 6uy ; 5uy |]

   let res1 = Oracle2.hashAttestData {commit = commit1 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = Some 1UL} |> Zen.Cost.Realized.__force
   let res2 = Oracle2.hashAttestData {commit = commit1 ; oraclePubKey = pk ; feeAsset = Zen.Asset.zenAsset ; feeAmount = Some 2UL} |> Zen.Cost.Realized.__force
   if res1 <> res2 then printfn "  ⛔ FAILED - this difference shouldn't affect hash"
   else printfn "  ✅ PASSED"



(*
------------------------------------------------------------------------------------------------------------------------
======================================== COMMAND: "Commit" =============================================================
------------------------------------------------------------------------------------------------------------------------
*)

printfn "\n\n======================================== Commit ========================================================================"

let mutable run_test = init_testing_environment()

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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = None
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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

run_test "valid commit - with FeeAsset but no FeeAmount"
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = None
                  _Recipient     = None
                  _FeeAsset      = Some ZenToken
                  _FeeAmount     = None
                  _ReturnAddress = None
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

run_test "valid commit - FeeAmount of 0"
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
                  _Commit        = Some commit003.commit
                  _OraclePubKey  = None
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = Some 0UL
                  _ReturnAddress = None
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

run_test "valid commit - with FeeAmount but no FeeAsset"
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
                  _Commit        = Some commit003.commit
                  _OraclePubKey  = None
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = commit003.feeAmount
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| CommitToken commit003) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit003) (Some 1UL)
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some commit001.pubKey
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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
                  _Commit        = Some commit002.commit
                  _OraclePubKey  = Some commit002.pubKey
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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
                  _Commit        = None
                  _OraclePubKey  = Some commit001.pubKey
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Couldn't find Commit in message body"
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some commit001.pubKey
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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

run_test <- init_testing_environment()

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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = Some (Abs.AbsPK PK_Other)
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = Some (Abs.AbsPK PK_Oracle)
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = Some (Abs.AbsContract <| Abs.OtherContract CID_Other)
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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

run_test "valid attest - with FeeAsset specified but no FeeAmount"
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some commit001.pubKey
                  _Recipient     = None
                  _FeeAsset      = Some ZenToken
                  _FeeAmount     = None
                  _ReturnAddress = None
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

run_test "valid attest - with FeeAmount specified and provided fee, no FeeAsset specified (should be Zen)"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Other) ZenToken (commit003.feeAmount |> Option.get)
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
                  _Commit        = Some commit003.commit
                  _OraclePubKey  = Some commit003.pubKey
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = commit003.feeAmount
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit003, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit003) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| AttestToken commit003) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit003) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit003) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK commit003.pubKey) (Some <| ZenToken) commit003.feeAmount
            ]
            ocRealizer
    end

run_test "valid attest - with FeeAmount specified and provided fee, and FeeAsset specified"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Oracle) commit004.feeAsset (commit004.feeAmount |> Option.get)
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = None
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| AttestToken commit004) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK commit004.pubKey) (Some <| commit004.feeAsset) commit004.feeAmount
            ]
            ocRealizer
    end

run_test "invalid attest - with FeeAmount specified, no FeeAsset, and no provided fee"
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
                  _Commit        = Some commit003.commit
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = None
                  _FeeAsset      = Some commit003.feeAsset
                  _FeeAmount     = commit003.feeAmount
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit003, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient oracle fee"
    end

run_test "invalid attest - with FeeAmount and FeeAsset specified, but no provided fee"
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = None
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient oracle fee"
    end

run_test "invalid attest - with FeeAmount and FeeAsset specified, but insufficient fee"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Oracle) commit004.feeAsset 1UL
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = None
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient oracle fee"
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
                  _Commit        = None
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = None
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit001, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "When the recipient is unspecified the sender can't be anonymous"
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Data wasn't committed"
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
                  _Commit        = Some commit001.commit
                  _OraclePubKey  = Some PK_Oracle
                  _Recipient     = None
                  _FeeAsset      = None
                  _FeeAmount     = None
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Data wasn't committed"
    end

run_test "invalid attest - with FeeAmount and FeeAsset specified, insufficient fee and given return address"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Oracle) commit004.feeAsset 1UL
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = None
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = Some (Abs.AbsPK PK_Other)
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient oracle fee"
    end

run_test "invalid attest - with FeeAmount and FeeAsset specified, sufficient fee, given return address but anoymous sender and no recipient"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Oracle) commit004.feeAsset 250UL
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = None
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = Some (Abs.AbsPK PK_Oracle)
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "When the recipient is unspecified the sender can't be anonymous"
    end

run_test "invalid attest - with FeeAmount and FeeAsset specified, sufficient fee but anoymous sender and no given return address"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Oracle) commit004.feeAsset 250UL
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = Some (Abs.AbsPK PK_Other)
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_FAIL_with "When the sender is anonymous you must provide a returnAddress"
    end

run_test "valid attest - with FeeAmount and FeeAsset specified, sufficient fee, pk sender and given return address"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Oracle) commit004.feeAsset 250UL
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = None
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = Some (Abs.AbsPK PK_Oracle)
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| AttestToken commit004) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK commit004.pubKey) (Some <| commit004.feeAsset) commit004.feeAmount
            ; hasOutput (Some <| Abs.AbsPK PK_Oracle) (Some <| commit004.feeAsset) (Some (250UL - Option.get commit004.feeAmount))
            ]
            ocRealizer
    end

run_test "valid attest - with FeeAmount and FeeAsset specified, sufficient fee, pk sender but no given return address"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Oracle) commit004.feeAsset 250UL
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = None
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = None
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| AttestToken commit004) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK commit004.pubKey) (Some <| commit004.feeAsset) commit004.feeAmount
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| commit004.feeAsset) (Some (250UL - Option.get commit004.feeAmount))
            ]
            ocRealizer
    end

run_test "valid attest - with FeeAmount and FeeAsset specified, sufficient fee, anonymous sender and given return address"
    begin
    Input.feedContract ocMain CONTRACT_ID_ORACLE {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_Oracle) commit004.feeAsset 250UL
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
                  _Commit        = Some commit004.commit
                  _OraclePubKey  = Some commit004.pubKey
                  _Recipient     = Some (Abs.AbsPK PK_Other)
                  _FeeAsset      = Some commit004.feeAsset
                  _FeeAmount     = commit004.feeAmount
                  _ReturnAddress = Some (Abs.AbsPK PK_Other)
              }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit004, 1UL)
            |> Input.Wallet.add (Abs.AbsContract Abs.ThisContract, CommitToken commit002, 1UL)
            |> Input.Wallet.realize ocRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasMint (Some <| AttestToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| AttestToken commit004) (Some 1UL)
            ; hasInput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| CommitToken commit004) (Some 1UL)
            ; hasOutput (Some <| Abs.AbsPK commit004.pubKey) (Some <| commit004.feeAsset) commit004.feeAmount
            ; hasOutput (Some <| Abs.AbsPK PK_Other) (Some <| commit004.feeAsset) (Some (250UL - Option.get commit004.feeAmount))
            ]
            ocRealizer
    end





for test in tests do
   match fst test.Value , Report.report (snd test.Value) with
   | name , Ok _ ->
      ()
   | name , Error err ->
      failwithf "Test %s failed with: %s" name err
