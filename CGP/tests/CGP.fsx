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
module AddInput    = Input.MessageBody
module Abs         = AbstractContract

open TestResult

#r "../output/CGP.dll"

type cgpCommand =
    | CMD_Payout

type cgpCid =
    | CID_CGP

type cgpPK =
    | PK_1
    | PK_2
    | PK_3

type cgpAsset =
    | Asset_Zen
    | Asset_1
    | Asset_2
    | Asset_3

type cgpData = {
    _Outputs : Abs.AbsPointedOutput<cgpPK, cgpCid, cgpAsset> list;
}

let context : Consensus.Types.ContractContext = {
    blockNumber = 210u;
    timestamp   = 123UL;

}
let CONTRACT_ID_CGP =
    Load.computeContractId "output/CGP.fst"

let generatePublicKey() =
    Crypto.KeyPair.create() |> snd

let PK_1' = generatePublicKey()
let PK_2' = generatePublicKey()
let PK_3' = generatePublicKey()

let cgpMain, cgpCost =
    Load.extractMainAndCost "output/CGP.dll"


let FIELD_OUTPUTS = "Outputs"B


let realizeCommand cmd =
    match cmd with
    | CMD_Payout -> "Payout"

let realizePK pk =
    match pk with
    | PK_1 -> PK_1'
    | PK_2 -> PK_2'
    | PK_3 -> PK_3'

let realizeContract c =
    match c with
    | CID_CGP -> CONTRACT_ID_CGP

let realizeAsset asset : Option<Types.Asset> =
    match asset with
    | Asset_Zen ->
        Some Consensus.Asset.Zen
    | Asset_1 ->
        match Zen.Asset.fromSubtypeString (ZFStar.fsToFstContractId CONTRACT_ID_CGP) "A"B |> Zen.Cost.Realized.__force with
        | (v, cid, sub) -> Some (Types.Asset (Types.ContractId (v, Hash.Hash cid), Hash.Hash sub))
    | Asset_2 ->
        match Zen.Asset.fromSubtypeString (ZFStar.fsToFstContractId CONTRACT_ID_CGP) "B"B |> Zen.Cost.Realized.__force with
        | (v, cid, sub) -> Some (Types.Asset (Types.ContractId (v, Hash.Hash cid), Hash.Hash sub))
    | Asset_3 ->
        match Zen.Asset.fromSubtypeString (ZFStar.fsToFstContractId CONTRACT_ID_CGP) "C"B |> Zen.Cost.Realized.__force with
        | (v, cid, sub) -> Some (Types.Asset (Types.ContractId (v, Hash.Hash cid), Hash.Hash sub))

let rec cgpRealizer : Abs.Realizer<cgpPK, cgpCid, cgpAsset, cgpCommand, cgpData> =
    {
        realizePK       = realizePK
        realizeContract = realizeContract
        realizeAsset    = realizeAsset
        realizeCommand  = realizeCommand
        realizeData     = realizeData
        thisContract    = CONTRACT_ID_CGP
    }

and realizeData (data : cgpData) =
    let rl = cgpRealizer in
    Input.MessageBody.emptyDict ()
    |> AddInput.add_list_with mkOutput FIELD_OUTPUTS data._Outputs
    |> Zen.Types.Data.Dict
    |> Zen.Types.Data.Collection
    |> Some

and mkOutput (output : Abs.AbsPointedOutput<cgpPK, cgpCid, cgpAsset>) : Data.data =
    match output with
    | (lock, asset, amount) ->
        match realizeAsset asset with
        | Some asset ->
            begin
            let  sAsset = asset.ToString()
                          |> ZFStar.fsToFstString
                          |> Data.String
            let  lock   = Tx.realizeLock cgpRealizer lock
                          |> ZFStar.fsToFstLock
                          |> Data.Lock
            let  spend  = [ sAsset; Data.U64 amount]
                          |> ZFStar.fsToFstList
                          |> Data.List
                          |> Data.Collection
            let output  = [ lock; spend ]
                          |> ZFStar.fsToFstList
                          |> Data.List
                          |> Data.Collection
            output
            end
        | None ->
            [] |> ZFStar.fsToFstList |> Data.List |> Data.Collection


let test_counter = ref 1
let tests = new System.Collections.Generic.Dictionary<int, string * TestResult<unit>>()

let init_testing_environment() =
    Execute.run_test tests test_counter



(*
------------------------------------------------------------------------------------------------------------------------
======================================== COMMAND: "Payout" =============================================================
------------------------------------------------------------------------------------------------------------------------
*)

printfn "\n\n======================================== Payout ==========================================================================="

let mutable run_test = init_testing_environment()

run_test "empty data & empty Tx"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
            Input.TxSkeleton.Abstract.empty
            |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
             context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            None
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_FAIL_with "Data parsing failed - the message body is empty"
    end

run_test "empty data & 100 kalapas"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.addInput (Abs.AbsPK PK_1) Asset_Zen 100UL
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            None
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_FAIL_with "Data parsing failed - the message body is empty"
    end

run_test "single spend of 100 ZP - empty wallet"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
                    ]
            }
         wallet      =
            Input.Wallet.empty
            |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "single spend of 100 ZP - 50 ZP in wallet"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_Zen , 50UL )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "single spend of 100 ZP - 50 ZP and 200 Asset_1 in wallet"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_Zen , 50UL  )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_1   , 200UL )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end

run_test "single spend of 100 ZP - exactly 100 ZP in wallet"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_1) (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_1) (Some <| Asset_Zen) (Some 100UL)
            ]
            cgpRealizer
    end

run_test "single spend of 100 ZP - 200 ZP in wallet"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_Zen , 200UL )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_Zen) (Some 200UL)
            ; hasOutput (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| Asset_Zen) (Some 100UL)
            ]
            cgpRealizer
    end

run_test "single spend of 100 ZP - 200 ZP and 50 Asset_1 in wallet"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_Zen , 200UL )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_1   , 50UL  )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_Zen) (Some 200UL)
            ; hasOutput (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| Asset_Zen) (Some 100UL)
            ]
            cgpRealizer
    end

run_test "spend of 100 ZP and 50 Asset_1 - 200 ZP and 50 Asset_1 in wallet"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_2 , Asset_Zen , 100UL )
                    ; ( Abs.AbsPK PK_3 , Asset_1   , 50UL  )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_3 , Asset_Zen, 200UL )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_1  , 50UL  )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_3)                   (Some <| Asset_Zen) (Some 200UL)
            ; hasInput  (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_1  ) (Some 50UL )
            ; hasOutput (Some <| Abs.AbsPK PK_2)                   (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_3)                   (Some <| Asset_1  ) (Some 50UL )
            ]
            cgpRealizer
    end

run_test "spend of 100 ZP and 50 Asset_1 - 200 ZP and 200 Asset_1 in wallet"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_2 , Asset_Zen , 100UL )
                    ; ( Abs.AbsPK PK_3 , Asset_1   , 50UL  )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_3 , Asset_Zen , 200UL )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_1   , 200UL )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_3)                   (Some <| Asset_Zen) (Some 200UL)
            ; hasInput  (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_1  ) (Some 200UL)
            ; hasOutput (Some <| Abs.AbsPK PK_2)                   (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_3)                   (Some <| Asset_1  ) (Some 50UL )
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| Asset_1  ) (Some 150UL)
            ]
            cgpRealizer
    end

run_test "spend of 100 ZP, 50 Asset_1, 75 Asset_2, 30 Asset_3 - in wallet 200 ZP, 50 Asset_1, 100 Asset_2, 200 Asset_3"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
                    ; ( Abs.AbsPK PK_1 , Asset_1   , 50UL  )
                    ; ( Abs.AbsPK PK_2 , Asset_2   , 75UL  )
                    ; ( Abs.AbsPK PK_3 , Asset_3   , 30UL  )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_Zen , 200UL )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_1   , 50UL  )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_2   , 100UL )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_3   , 200UL )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_PASS_with_tx
            [ hasInput  (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_Zen) (Some 200UL)
            ; hasInput  (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_1  ) (Some 50UL )
            ; hasInput  (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_2  ) (Some 100UL)
            ; hasInput  (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_3  ) (Some 200UL)
            ; hasOutput (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsPK PK_1)                   (Some <| Asset_1  ) (Some 50UL )
            ; hasOutput (Some <| Abs.AbsPK PK_2)                   (Some <| Asset_2  ) (Some 75UL )
            ; hasOutput (Some <| Abs.AbsPK PK_3)                   (Some <| Asset_3  ) (Some 30UL )
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| Asset_Zen) (Some 100UL)
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| Asset_2  ) (Some 25UL )
            ; hasOutput (Some <| Abs.AbsContract Abs.ThisContract) (Some <| Asset_3  ) (Some 170UL)
            ]
            cgpRealizer
    end

run_test "spend of 100 ZP, 50 Asset_1, 75 Asset_2, 30 Asset_3 - in wallet 200 ZP, 50 Asset_1, 10 Asset_2, 200 Asset_3"
    begin
    Input.feedContract cgpMain CONTRACT_ID_CGP {
         txSkel      =
             Input.TxSkeleton.Abstract.empty
             |> Input.TxSkeleton.Abstract.realize cgpRealizer
         context     =
            context
         command     =
            "Payout"
         sender      =
            Abs.AbsPKSender PK_1
            |> Input.Sender.realize cgpRealizer
         messageBody =
            realizeData {
                _Outputs =
                    [ ( Abs.AbsPK PK_1 , Asset_Zen , 200UL )
                    ; ( Abs.AbsPK PK_1 , Asset_1   , 50UL  )
                    ; ( Abs.AbsPK PK_1 , Asset_2   , 100UL )
                    ; ( Abs.AbsPK PK_1 , Asset_3   , 200UL )
                    ]
            }
         wallet      =
             Input.Wallet.empty
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_Zen , 100UL )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_1   , 50UL  )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_2   , 10UL  )
             |> Input.Wallet.add ( Abs.AbsPK PK_1 , Asset_3   , 200UL )
             |> Input.Wallet.realize cgpRealizer
         state       =
            None
    } |> should_FAIL_with "Insufficient funds"
    end







for test in tests do
   match fst test.Value , Report.report (snd test.Value) with
   | name , Ok _ ->
      ()
   | name , Error err ->
      failwithf "Test %s failed with: %s" name err
