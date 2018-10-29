
# ZenDex

## Verify/Build

To verify/record hints, run `zebra  --z3rlimit 7000000 -e Dex001.fst`.  
This command may take a long time to run the first time.
Subsequent runs will be significantly faster.
With an AMD Threadripper 1950x @4.0GHz, recording hints can take up to 2 minutes. Subsequent runs take ~11s.

## How it works

ZenDex is a true decentralised exchange, with no operator or fees.

ZenDex stores orders as utxos that it locks to itself.
In this manner, we avoid having a shared state, allowing great parallelism.

When an order is made, the order parameters are hashed together.
ZenDex mints and single token with that hash as it's sub-identifier (the '*order asset*'),
and locks that to itself.

When an order is taken or cancelled, that token is destroyed.
In the case of a partial fill,
ZenDex will create a new order with reduced quantities of the underlying and order total.
The previous order is still destroyed.

When cancelling or taking an order,
one must supply all of the order parameters directly to ZenDex in the messageBody,
because ZenDex does not store this information, and only possesses a hash of the order.

Order books can be constructed by folding over the blockchain,
and looking for transactions involving ZenDex.

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
|:----------:|:----:|:-----------:| ----------------------------------------------------- |
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
| `"UnderlyingAmount"` | `UInt64` | The amount of the underlying in the order |
| `"PairAssetVersion"` | `UInt32` | The version number of the pair asset of your order |
| `"PairContractHash"` | `Hash` | The contract hash for the pair asset of your order |
| `"PairSubIdentifier"` | `Hash` | The asset sub-identifier for the pair asset of your order |
| `"OrderTotal"` | `UInt64` | The total amount of the pair requested in the order |

The transaction must place the order asset in ZenDex's contract wallet,
as well as a sufficient quantity of the underlying.

### Taking an order

Use the command `"Take"`.

The messageBody must consist of a dictionary which includes the following fields:

| Field Name | Type | Description |
|:----------:|:----:| ----------- |
| `"UnderlyingAssetVersion"` | `UInt32` | The version number of the underlying asset of the order being taken |
| `"UnderlyingContractHash"` | `Hash`   | The contract hash for the underlying asset of the order being taken |
| `"UnderlyingSubIdentifier"`| `Hash`   | The asset sub-identifier for the underlying asset of the order being taken |
| `"UnderlyingAmount"` | `UInt64` | The amount of the underlying in the order |
| `"PairAssetVersion"` | `UInt32` | The version number of the pair asset of the order being taken |
| `"PairContractHash"` | `Hash` | The contract hash for the pair asset of the order being taken |
| `"PairSubIdentifier"` | `Hash` | The asset sub-identifier for the pair asset of the order being taken |
| `"OrderTotal"` | `UInt64` | The total amount of the pair requested in the order |
| `"MakerPK"` | `PublicKey` | The public key of the maker of the order being taken |
| `"RequestedPayout"` | `UInt64` | The amount of the underlying to pay out |

The transaction must place the order asset being taken and a sufficient amount of the underlying in ZenDex's contract wallet,
and must lock an amount α of the order's pair asset to the contract, where
<img src="https://latex.codecogs.com/svg.latex?\texttt{RequestedPayout}&space;=&space;\left&space;\lfloor{\texttt{UnderlyingAmount}&space;\times&space;\frac{\alpha}{\texttt{OrderTotal}}}&space;\right&space;\rfloor" title="\texttt{RequestedPayout} = \left \lfloor{\texttt{UnderlyingAmount} \times \frac{\alpha}{\texttt{OrderTotal}}} \right \rfloor" />.

## Notes

Orders are expressed in terms of underlying amount and pair amount to allow for rational price ratios - eg. a trade of 5α for 7β, or 13β for 11γ.
This is not easily expressed as a 'price per' with only integer arithmetic.
The payout for a partial fill should, assuming arbitrarily divisible assets, be calculated as
<img src="https://latex.codecogs.com/svg.latex?\texttt{Payout}&space;=&space;\texttt{UnderlyingAmount}&space;\times&space;\frac{\texttt{PaymentAmount}}{\texttt{OrderTotal}}" title="\texttt{Payout} = \texttt{UnderlyingAmount} \times \frac{\texttt{PaymentAmount}}{\texttt{OrderTotal}}" />.
However, since we do not have arbitrarily divisible assets, we denote orders in the smallest unit of each asset and compute the floor, so that
<img src="https://latex.codecogs.com/svg.latex?\texttt{Payout}&space;=&space;\left&space;\lfloor{\texttt{UnderlyingAmount}&space;\times&space;\frac{\texttt{PaymentAmount}}{\texttt{OrderTotal}}}&space;\right&space;\rfloor" title="\texttt{Payout} = \left \lfloor{\texttt{UnderlyingAmount} \times \frac{\texttt{PaymentAmount}}{\texttt{OrderTotal}}} \right \rfloor" />.

The underlying amount, order total, and payment amount are all 64 bit unsigned integers.
Version 0 ZF* contracts lack integer representations larger than this, and so we are tasked with implementing double-word arithmetic in order to calculate the payoff.
In order to avoid the complexity and potential for error in implementing double-word division,
we instead ask the user to provide the payoff, and validate that it is correct.
Validating that a user's `RequestedPayout` is correct is simpler than computing the payout,
and requires only double-word multiplication and comparison,
both relatively simple compared to double-word division.
