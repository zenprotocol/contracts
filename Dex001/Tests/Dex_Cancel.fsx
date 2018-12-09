#load "Dex.fsx"

open Dex

let test_cancel_1 = valid_order_cancel {
    underlyingAsset  = ZEN_ASSET
    underlyingAmount = 100UL
    pairAsset        = ZEN_ASSET
    orderTotal       = 100UL
    makerPubKey      = generatePublicKey()
    nonce            = 1UL
}
printfn "test_cancel_1 : %A" test_cancel_1