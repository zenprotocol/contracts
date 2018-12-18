#load "Dex.fsx"

module Types = Consensus.Types

open Dex

let tests = new System.Collections.Generic.Dictionary<int, string * CR>()
let test_counter = ref 1

let test = run_test tests test_counter

let _ =
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
    }
    test "take order 100 ZP -> 600 XYZ"
    <| valid_order_take_full odata
//    |> printfn "%A"
    |> let orderAsset = computeOrderAsset odata |> Option.map Consensus.Asset.toString in
        checkTx
            [ hasInput lockContract    orderAsset            (Some 1UL)
            ; hasInput None            odata.underlyingAsset odata.underlyingAmount
            ; hasInput None            odata.pairAsset       odata.orderTotal
            ; hasOutput (lockPK maker) odata.pairAsset       odata.orderTotal
            ; hasOutput (lockPK taker) odata.underlyingAsset odata.underlyingAmount
            ; hasOutput lockDestroy    orderAsset            (Some 1UL)
            ]
    // computeOrderAsset // TODO

test "take order 10 ZP -> 10 ZP" 
    <| valid_order_take_full {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 10UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 10UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
        returnAddress    = Some <| generatePublicKey()
    }
    |> should_PASS
