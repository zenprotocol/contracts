
# ZenDex

## Verify/Build

To verify/record hints, run `zebra  --z3rlimit 7000000 -e Dex001.fst`.  
This command may take a long time to run the first time.
Subsequent runs will be significantly faster.
With an AMD Threadripper 1950x @4.0GHz, recording hints takes ~67s. Subsequent runs take ~11s.

## Usage

ZenDex accepts three commands: `"Make"`, `"Cancel"`, and `"Take"`.

### Making an order

Use the command `"Make"`. You must sign with a public key -
if the sender is `Anonymous` or `Contract contractID`, then the transaction will fail.

The messageBody must consist of a dictionary which includes the following fields:

| Field Name | Type | Description | Notes |
|:----------:|:----:| ----------- | ----- |
| `"UnderlyingAssetVersion"` | `UInt32` | The version number of the underlying asset | |
| `"UnderlyingContractHash"` | `Hash`   | The contract hash for the underlying asset | |
| `"UnderlyingSubIdentifier"`| `Hash`   | The asset sub-identifier for the underlying asset | |
| `"PairAssetVersion"` | `UInt32` | The version number of the pair asset | |
| `"PairContractHash"` | `Hash`   | The contract hash for the pair asset | |
| `"PairSubIdentifier"` | `Hash`   | The asset sub-identifier for the pair asset | |
| `"OrderTotal"` | `UInt64` | The total amount of the pair being ordered | Must be greater than 0 |

The transaction must include a nonzero amount of the underlying asset being locked to ZenDex.

For example, to make an order selling 50ZP for 1234 of some asset α which has asset identifier
`00000000a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3` -

The messageBody would include the field-value pairs

| Field Name | Type | Value | Notes |
|:----------:|:----:| ----------- | ----- |
| `"UnderlyingAssetVersion"` | `UInt32` | 0 | The ZP asset has version 0 |
| `"UnderlyingContractHash"` | `Hash`   | `0000000000000000000000000000000000000000000000000000000000000000` | The ZP asset has the zero hash as it's contract hash |
| `"UnderlyingContractHash"` | `Hash`   | `0000000000000000000000000000000000000000000000000000000000000000` | The ZP asset has the zero hash as it's asset sub-identifier |
| `"PairAssetVersion"` | `UInt32` | `00000000` | The first 4 bytes of an asset identifier are the version number |
| `"PairContractHash"` | `Hash`   | `a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1` | The 5th to 36th bytes are the contract hash of the contract that generated the asset |
| `"PairSubIdentifier"`| `Hash` | `c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3` | The 37th to 68th bytes are the asset sub-identifier |
| `"OrderTotal"` | `UInt64` | `1234` | The order is for 1234 of α |

and the transaction would lock 50ZP to ZenDex.

### Cancelling an order

Use the command `"Cancel"`. You must sign with the public key that was used to create the order.

The messageBody must consist of a dictionary which includes the following fields:

| Field Name | Type | Description |
|:----------:|:----:| ----------- |
| `"UnderlyingAssetVersion"` | `UInt32` | The version number of the underlying asset of your order |
| `"UnderlyingContractHash"` | `Hash`   | The contract hash for the underlying asset of your order |
| `"UnderlyingSubIdentifier"`| `Hash`   | The asset sub-identifier for the underlying asset of your order |
| `"UnderlyingAmount"` | `Hash` | The amount of the underlying in the order |
| `"PairAssetVersion"` | `UInt32` | The version number of the pair asset of your order |
| `"PairContractHash"` | `Hash` | The contract hash for the pair asset of your order |
| `"PairSubIdentifier"` | `Hash` | The asset sub-identifier for the pair asset of your order |
| `"OrderTotal"` | `UInt64` | The total amount of the pair ordered |

the transaction must place the order asset in ZenDex's contract wallet.

### Taking an order

Use the command `"Take"`.
