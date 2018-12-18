#load "Dex.fsx"

open Dex
open System

let tests = new System.Collections.Generic.Dictionary<int, string * CR>()
let test_counter = ref 1

let test = run_test tests test_counter

test "valid Make order - 100 ZP -> 100 ZP"
    <| valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

test "valid Make order - 100 ZP -> 5 ZP"
    <| valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 5UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS 

test "valid Make order - 100 ZP -> 5 XYZ"
    <| valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 5UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

test "Valid Make order - 5 XYZ -> 100 ZP" 
    <| valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| XYZ_ASSET
        underlyingAmount = Some <| 5UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS 

test "0 underying amount" 
    <| valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"

test "0 pair amount" 
    <| valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse OrderTotal, or OrderTotal was 0"

test "0 underlying & pair amount" 
    <| valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"
 
test "no underlying asset in body" 
    <| order_make_modified_tx [ZEN_ASSET, 100UL] {
    odataDefault with
        underlyingAsset  = None
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAsset"
 
test "no underlying amount in body" 
    <| order_make_modified_tx [ZEN_ASSET, 100UL] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = None
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"

test "no underlying asset & amount in body" 
    <| order_make_modified_tx [ZEN_ASSET, 100UL] {
    odataDefault with
        underlyingAsset  = None
        underlyingAmount = None
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAsset"

test "empty tx"
    <| order_make_modified_tx [] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Incorrect amount of UnderlyingAsset Received"

test "composite tx"
    <| order_make_modified_tx [(XYZ_ASSET, 5UL); (ZEN_ASSET, 50UL); (XYZ_ASSET, 5UL); (ZEN_ASSET, 50UL); (XYZ_ASSET, 5UL); (ZEN_ASSET, 50UL); (XYZ_ASSET, 5UL)] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

test "incorrect asset in tx"
    <| order_make_modified_tx [XYZ_ASSET, 100UL] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Incorrect amount of UnderlyingAsset Received"

test "incorrect amount in tx (amount too small)"
    <| order_make_modified_tx [ZEN_ASSET, 99UL] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Incorrect amount of UnderlyingAsset Received"


test "incorrect amount in tx (amount too big)"
    <| order_make_modified_tx [ZEN_ASSET, 101UL] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Incorrect amount of UnderlyingAsset Received"

test "wrong maker"
    <| order_make_modified_sender (generatePublicKey()) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "SenderPubKey must match MakerPubKey"

test "nonempty wallet"
    <| order_make_modified_wallet (Some 100UL, Some 600UL, Some 10UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS