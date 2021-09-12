#load "Dex.fsx"

open Dex

let tests = new System.Collections.Generic.Dictionary<int, string * Result<unit,string>>()
let test_counter = ref 1

let test = run_test tests test_counter

test "valid Cancel order - 100 ZP -> 100 ZP"
    begin valid_order_cancel {
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

test "valid Cancel order - 100 ZP -> 600 XYZ"
    begin valid_order_cancel {
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

test "invalid Cancel order - 0 ZP -> 600 XYZ"
    begin valid_order_cancel {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"
    end

test "invalid Cancel order - 100 ZP -> 0 XYZ"
    begin valid_order_cancel {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse OrderTotal, or OrderTotal was 0"
    end
    
test "invalid Cancel order - 0 ZP -> 0 XYZ"
    begin valid_order_cancel {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 0UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 0UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL_with "Could not parse UnderlyingAmount, or UnderlyingAmount was 0"
    end

test "underlying amount is bigger than in wallet"
    begin order_cancel_modified_wallet (Some <| 99UL, Some <| 600UL, Some <| 1UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL
    end

test "pair amount is bigger than in wallet"
    begin order_cancel_modified_wallet (Some <| 100UL, Some <| 599UL, Some <| 1UL) {
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

test "no order token"
    begin order_cancel_modified_wallet (Some <| 100UL, Some <| 600UL, None) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL
    end

test "0 order token"
    begin order_cancel_modified_wallet (Some <| 100UL, Some <| 600UL, Some <| 0UL) {
    odataDefault with
        underlyingAsset  = Some <| ZEN_ASSET
        underlyingAmount = Some <| 100UL
        pairAsset        = Some <| XYZ_ASSET
        orderTotal       = Some <| 600UL
        makerPubKey      = Some <| generatePublicKey()
        nonce            = Some <| 1UL
    }
    |> should_FAIL
    end

test "excess underlying amount"
    begin order_cancel_modified_wallet (Some <| 101UL, Some <| 600UL, Some <| 1UL) {
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

test "excess pair amount"
    begin order_cancel_modified_wallet (Some <| 100UL, Some <| 601UL, Some <| 1UL) {
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

test "excess order token"
    begin order_cancel_modified_wallet (Some <| 100UL, Some <| 600UL, Some <| 2UL) {
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