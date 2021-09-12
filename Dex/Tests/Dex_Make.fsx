#load "Dex.fsx"

open Dex
open System

let tests = new System.Collections.Generic.Dictionary<int, string * Result<unit,string>>()
let test_counter = ref 1

let test = run_test tests test_counter

test "valid Make order - 100 ZP -> 100 ZP"
    begin valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS
    end

test "valid Make order - 100 ZP -> 5 ZP"
    begin valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 5UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS
    end

test "valid Make order - 100 ZP -> 5 XYZ"
    begin valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 5UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS
    end

test "Valid Make order - 5 XYZ -> 100 ZP" 
    begin valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| XYZ_ASSET
        underlyingAmount = Some <| 5UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS 
    end

test "0 underying amount" 
    begin valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 100UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"
    end

test "0 pair amount" 
    begin valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse OrderTotal, or OrderTotal was 0"
    end

test "0 underlying & pair amount" 
    begin valid_order_make {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| ZEN_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"
    end
 
test "no underlying asset in body" 
    begin order_make_modified_tx [ZEN_ASSET, 100UL] {
    odataDefault with
        underlyingAsset  = None
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAsset"
    end
 
test "no underlying amount in body" 
    begin order_make_modified_tx [ZEN_ASSET, 100UL] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = None
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"
    end

test "no underlying asset & amount in body" 
    begin order_make_modified_tx [ZEN_ASSET, 100UL] {
    odataDefault with
        underlyingAsset  = None
        underlyingAmount = None
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAsset"
    end

test "empty tx"
    begin order_make_modified_tx [] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Incorrect amount of UnderlyingAsset Received"
    end

test "composite tx"
    begin order_make_modified_tx [(XYZ_ASSET, 5UL); (ZEN_ASSET, 50UL); (XYZ_ASSET, 5UL); (ZEN_ASSET, 50UL); (XYZ_ASSET, 5UL)] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS
    end

test "incorrect asset in tx"
    begin order_make_modified_tx [XYZ_ASSET, 100UL] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Incorrect amount of UnderlyingAsset Received"
    end

test "incorrect amount in tx (amount too small)"
    begin order_make_modified_tx [ZEN_ASSET, 99UL] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Incorrect amount of UnderlyingAsset Received"
    end


test "incorrect amount in tx (amount too big)"
    begin order_make_modified_tx [ZEN_ASSET, 101UL] {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Incorrect amount of UnderlyingAsset Received"
    end

test "wrong maker"
    begin order_make_modified_sender (generatePublicKey()) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "SenderPubKey must match MakerPubKey"
    end

test "nonempty wallet"
    begin order_make_modified_wallet (Some 100UL, Some 600UL, Some 10UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_PASS
    end





for test in tests do
   match fst test.Value , snd test.Value with
   | name , Ok _ ->
      ()
   | name , Error err ->
      failwithf "Test %s failed with: %s" name err