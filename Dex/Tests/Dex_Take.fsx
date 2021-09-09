#load "Dex.fsx"

module Types = Consensus.Types

open Dex

let tests = new System.Collections.Generic.Dictionary<int, string * Result<unit,string>>()
let test_counter = ref 1

let test = run_test tests test_counter

let scale (p : uint8) = Option.map (fun x -> (x * uint64 p) / 100UL)

let _ =
    let name  = "take order 100 ZP -> 600 XYZ"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin valid_order_take_full odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            checkTx
                [ hasInput None            odata.underlyingAsset odata.underlyingAmount
                ; hasOutput (lockPK taker) odata.underlyingAsset odata.underlyingAmount
                
                ; hasInput None            odata.pairAsset       odata.orderTotal
                ; hasOutput (lockPK maker) odata.pairAsset       odata.orderTotal
                
                ; hasInput lockContract    orderAsset            (Some 1UL)
                ; hasOutput lockDestroy    orderAsset            (Some 1UL)
                ]
        end

let _ =
    let name  = "take order 0 ZP -> 600 XYZ"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 0UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin valid_order_take_full odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            should_FAIL_with "Could not parse RequestedPayout, or RequestedPayout was 0"
        end

let _ =
    let name  = "take order 100 ZP -> 0 XYZ"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 0UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 0UL
    }
    test name
        begin valid_order_take_full odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            should_FAIL_with "Could not parse ProvidedAmount, or ProvidedAmount was 0"
        end

let _ =
    let name  = "take order 0 ZP -> 0 XYZ"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 0UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 0UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 0UL
    }
    test name
        begin valid_order_take_full odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            should_FAIL_with "Could not parse RequestedPayout, or RequestedPayout was 0"
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ with 20% partial fill (20 ZP -> 120 XYZ)"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let p     = 20uy 
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 120UL
    }
    test name
        begin valid_order_take_partial p odata
        |> let oldOrderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
        let newOrderAsset =
                computeOrderAsset
                    { odata with
                        underlyingAmount = scale (100uy-p) odata.underlyingAmount
                        orderTotal       = scale (100uy-p) odata.orderTotal
                    }
                    |> Option.map Consensus.Asset.toString in
        checkTx
                [ hasInput None            odata.underlyingAsset odata.underlyingAmount
                ; hasOutput (lockPK taker) odata.underlyingAsset (scale p odata.underlyingAmount)
                ; hasOutput lockContract   odata.underlyingAsset (scale (100uy-p) odata.underlyingAmount)
                
                ; hasInput None            odata.pairAsset       (scale p odata.orderTotal)
                ; hasOutput (lockPK maker) odata.pairAsset       (scale p odata.orderTotal)
                
                ; hasInput lockContract    oldOrderAsset         (Some 1UL)
                ; hasOutput lockDestroy    oldOrderAsset         (Some 1UL)
                
                ; hasMint                  newOrderAsset         (Some 1UL)
                ; hasOutput lockContract   newOrderAsset         (Some 1UL)
                ]
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ with 110% partial fill (110 ZP -> 660 XYZ)"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let p     = 110uy
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin valid_order_take_partial p odata
        |> let oldOrderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
        let newOrderAsset =
                computeOrderAsset
                    { odata with
                        underlyingAmount = scale (100uy-p) odata.underlyingAmount
                        orderTotal       = scale (100uy-p) odata.orderTotal
                    }
                    |> Option.map Consensus.Asset.toString in  
            should_FAIL  
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ with 20% partial fill (20 ZP -> 120 XYZ) (SANITY CHECK! - for modified tx)"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let p     = 20uy 
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            requestedPayout  = Some <| 20UL
            providedAmount   = Some <| 120UL
    }
    test name
        begin order_take_modified_tx [XYZ_ASSET, 120UL] odata
        |> let oldOrderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
        let newOrderAsset =
                computeOrderAsset
                    { odata with
                        underlyingAmount = scale (100uy-p) odata.underlyingAmount
                        orderTotal       = scale (100uy-p) odata.orderTotal
                    }
                    |> Option.map Consensus.Asset.toString in
        checkTx
                [ hasInput None            odata.underlyingAsset odata.underlyingAmount
                ; hasOutput (lockPK taker) odata.underlyingAsset (scale p odata.underlyingAmount)
                ; hasOutput lockContract   odata.underlyingAsset (scale (100uy-p) odata.underlyingAmount)
                
                ; hasInput None            odata.pairAsset       (scale p odata.orderTotal)
                ; hasOutput (lockPK maker) odata.pairAsset       (scale p odata.orderTotal)
                
                ; hasInput lockContract    oldOrderAsset         (Some 1UL)
                ; hasOutput lockDestroy    oldOrderAsset         (Some 1UL)
                
                ; hasMint                  newOrderAsset         (Some 1UL)
                ; hasOutput lockContract   newOrderAsset         (Some 1UL)
                ]
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ with 20% partial fill and wrong requested payout (too much)"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let p     = 20uy 
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            requestedPayout  = Some <| 21UL
            providedAmount   = Some <| 120UL
    }
    test name
        begin order_take_modified_tx [XYZ_ASSET, 120UL] odata
        |> let oldOrderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
        let newOrderAsset =
                computeOrderAsset
                    { odata with
                        underlyingAmount = scale (100uy-p) odata.underlyingAmount
                        orderTotal       = scale (100uy-p) odata.orderTotal
                    }
                    |> Option.map Consensus.Asset.toString in
        should_FAIL_with "Incorrect requestedPayout"
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ with 20% partial fill and wrong requested payout (not enough)"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let p     = 20uy 
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            requestedPayout  = Some <| 19UL
            providedAmount   = Some <| 120UL
    }
    test name
        begin order_take_modified_tx [XYZ_ASSET, 120UL] odata
        |> let oldOrderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
        let newOrderAsset =
                computeOrderAsset
                    { odata with
                        underlyingAmount = scale (100uy-p) odata.underlyingAmount
                        orderTotal       = scale (100uy-p) odata.orderTotal
                    }
                    |> Option.map Consensus.Asset.toString in
        should_FAIL_with "Incorrect requestedPayout"
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ with 20% partial fill and wrong requested payout (0)"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let p     = 20uy 
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            requestedPayout  = Some <| 0UL
            providedAmount   = Some <| 120UL
    }
    test name
        begin order_take_modified_tx [XYZ_ASSET, 120UL] odata
        |> let oldOrderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
        let newOrderAsset =
                computeOrderAsset
                    { odata with
                        underlyingAmount = scale (100uy-p) odata.underlyingAmount
                        orderTotal       = scale (100uy-p) odata.orderTotal
                    }
                    |> Option.map Consensus.Asset.toString in
        should_FAIL_with "Could not parse RequestedPayout, or RequestedPayout was 0"
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ with 20% partial fill and wrong requested payout (none)"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let p     = 20uy 
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            requestedPayout  = None
            providedAmount   = Some <| 120UL
    }
    test name
        begin order_take_modified_tx [XYZ_ASSET, 120UL] odata
        |> let oldOrderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
        let newOrderAsset =
                computeOrderAsset
                    { odata with
                        underlyingAmount = scale (100uy-p) odata.underlyingAmount
                        orderTotal       = scale (100uy-p) odata.orderTotal
                    }
                    |> Option.map Consensus.Asset.toString in
        should_FAIL_with "Could not parse RequestedPayout, or RequestedPayout was 0"
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ - (SANITY CHECK! - for modified wallet)"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin order_take_modified_wallet (odata.underlyingAmount, Some <| 0UL, Some <| 1UL) odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            checkTx
                [ hasInput None            odata.underlyingAsset odata.underlyingAmount
                ; hasOutput (lockPK taker) odata.underlyingAsset odata.underlyingAmount
                
                ; hasInput None            odata.pairAsset       odata.orderTotal
                ; hasOutput (lockPK maker) odata.pairAsset       odata.orderTotal
                
                ; hasInput lockContract    orderAsset            (Some 1UL)
                ; hasOutput lockDestroy    orderAsset            (Some 1UL)
                ]
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ - empty wallet"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin order_take_modified_wallet (None, None, None) odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            should_FAIL
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ - not enough underlying in wallet"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin order_take_modified_wallet (Some <| 99UL, Some <| 0UL, Some <| 1UL) odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            should_FAIL
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ - 0 order token in wallet"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin order_take_modified_wallet (Some <| 100UL, Some <| 0UL, Some <| 0UL) odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            should_FAIL
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ - more than 1 order token"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin order_take_modified_wallet (odata.underlyingAmount, Some <| 0UL, Some <| 5UL) odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            checkTx
                [ hasInput  None           odata.underlyingAsset odata.underlyingAmount
                ; hasOutput (lockPK taker) odata.underlyingAsset odata.underlyingAmount
                
                ; hasInput  None           odata.pairAsset       odata.orderTotal
                ; hasOutput (lockPK maker) odata.pairAsset       odata.orderTotal
                
                ; hasInput  lockContract   orderAsset            (Some 5UL)
                ; hasOutput lockContract   orderAsset            (Some 4UL)
                ; hasOutput lockDestroy    orderAsset            (Some 1UL)
                ]
        end

let _ =
    let name  = "take order 100 ZP -> 600 XYZ - wallet with surplus"
    let maker = generatePublicKey()
    let taker = generatePublicKey()
    let odata = {
        odataDefault with
            underlyingAsset  = Some <| ZEN_ASSET
            underlyingAmount = Some <| 100UL
            pairAsset        = Some <| XYZ_ASSET
            orderTotal       = Some <| 600UL
            makerPubKey      = Some <| maker
            nonce            = Some <| 1UL
            returnAddress    = Some <| taker
            providedAmount   = Some <| 600UL
    }
    test name
        begin order_take_modified_wallet (Some <| 9100UL, Some <| 9000UL, Some <| 9001UL) odata
        |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
            checkTx
                [ hasInput  None           odata.underlyingAsset (Some 9100UL)
                ; hasOutput (lockPK taker) odata.underlyingAsset (Some  100UL)
                ; hasOutput lockContract   odata.underlyingAsset (Some 9000UL)
                
                ; hasInput  None           odata.pairAsset       odata.orderTotal
                ; hasOutput (lockPK maker) odata.pairAsset       odata.orderTotal
                
                ; hasInput  lockContract   orderAsset            (Some 9001UL)
                ; hasOutput lockContract   orderAsset            (Some 9000UL)
                ; hasOutput lockDestroy    orderAsset            (Some 1UL)
                ]
        end




for test in tests do
   match fst test.Value , snd test.Value with
   | name , Ok _ ->
      ()
   | name , Error err ->
      failwithf "Test %s failed with: %s" name err