#load "Dex.fsx"

open Dex

let tests = new System.Collections.Generic.Dictionary<int, string * CR>()
let test_counter = ref 1

let test = run_test tests test_counter

test "valid Cancel order - 100 ZP -> 100 ZP"
    <| valid_order_cancel {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

test "valid Cancel order - 100 ZP -> 600 XYZ"
    <| valid_order_cancel {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

test "invalid Cancel order - 0 ZP -> 600 XYZ"
    <| valid_order_cancel {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"

test "invalid Cancel order - 100 ZP -> 0 XYZ"
    <| valid_order_cancel {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse OrderTotal, or OrderTotal was 0"
    
test "invalid Cancel order - 0 ZP -> 0 XYZ"
    <| valid_order_cancel {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"

test "underlying amount is bigger than in wallet"
    <| order_cancel_modified_wallet (Some <| 99UL, Some <| 600UL, Some <| 1UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

test "pair amount is bigger than in wallet"
    <| order_cancel_modified_wallet (Some <| 100UL, Some <| 599UL, Some <| 1UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

test "no order token"
    <| order_cancel_modified_wallet (Some <| 100UL, Some <| 600UL, None) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

test "0 order token"
    <| order_cancel_modified_wallet (Some <| 100UL, Some <| 600UL, Some <| 0UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

test "excess underlying amount"
    <| order_cancel_modified_wallet (Some <| 101UL, Some <| 600UL, Some <| 1UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

test "excess pair amount"
    <| order_cancel_modified_wallet (Some <| 100UL, Some <| 601UL, Some <| 1UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

test "excess order token"
    <| order_cancel_modified_wallet (Some <| 100UL, Some <| 600UL, Some <| 2UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS
