#load "Dex.fsx"

open Dex
open System  

let test_make_1 =
    disp "TEST 1 - valid Make order - 100 ZP -> 100 ZP:"
    <| valid_order_make {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

let test_make_2 =
    disp "TEST 2 - valid Make order - 100 ZP -> 5 ZP:"
    <| valid_order_make {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 5UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS 

let test_make_3 =
    disp "TEST 3 - valid Make order - 100 ZP -> 5 XYZ:"
    <| valid_order_make {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 5UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS

let test_make_4 =
    disp "TEST 4 - Valid Make order - 5 XYZ -> 100 ZP:" 
    <| valid_order_make {
        underlyingAsset  = Some <| XYZ_ASSET
        underlyingAmount = Some <| 5UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS 

let test_make_5 =
    disp "TEST 5 - 0 underying amount:" 
    <| valid_order_make {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_6 =
    disp "TEST 6 - 0 pair amount:" 
    <| valid_order_make {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_7 =
    disp "TEST 7 - 0 underlying & pair amount:" 
    <| valid_order_make {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_8 =
    let txAssetAmount = Some (ZEN_ASSET, 100UL)
    disp "TEST 8 - no underlying asset in body:" 
    <| order_make_modified_tx txAssetAmount {
        underlyingAsset  = None
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_9 =
    let txAssetAmount = Some (ZEN_ASSET, 100UL)
    disp "TEST 9 - no underlying amount in body:" 
    <| order_make_modified_tx txAssetAmount {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = None
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_10 =
    let txAssetAmount = Some (ZEN_ASSET, 100UL)
    disp "TEST 10 - no underlying asset & amount in body:" 
    <| order_make_modified_tx txAssetAmount {
        underlyingAsset  = None
        underlyingAmount = None
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_11 =
    let txAssetAmount = None
    disp "TEST 11 - empty tx:"
    <| order_make_modified_tx txAssetAmount {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_12 =
    let txAssetAmount = Some (XYZ_ASSET, 100UL)
    disp "TEST 12 - incorrect asset in tx:"
    <| order_make_modified_tx txAssetAmount {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_13 =
    let txAssetAmount = Some (ZEN_ASSET, 99UL)
    disp "TEST 13 - incorrect amount in tx (amount too small):"
    <| order_make_modified_tx txAssetAmount {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL

let test_make_14 =
    let txAssetAmount = Some (ZEN_ASSET, 101UL)
    disp "TEST 14 - incorrect amount in tx (amount too big):"
    <| order_make_modified_tx txAssetAmount {
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL
