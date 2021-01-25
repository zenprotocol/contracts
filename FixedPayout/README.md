# Fixed Payout

## Verify/Build

To verify/record hints, run `zebra e --z3rlimit 8000000 FixedPayout.fst`.
This command may take a long time to run the first time, subsequent runs will be significantly faster.
The elaborated source file will be created in the `output` directory.

To build, run `zebra c --z3rlimit 8000000 FixedPayout.fst`.
Both the elaborated source file and the compiled binary (`.dll`) file will be created in the `output` directory.

## How it works

This contract assumes there is a working oracle service and an oracle contract which commits on a data set of `<asset, value>` pairs on different times.

The Fixed Payout contract can issue Bull and Bear positions on one of the tickers on which the oracle commits for a future event, and provides the winner with the collateral payed by the issuer.

First - the issuer issues **position tokens** by specifying the future market price, which includes the public key of the oracle service, the contract ID of the oracle contract, the name of the forex ticker, the time frame in which the event will take place, and the price bounds on which the positions diverge.

The contract issues two position tokens, according to the possible positions:

1. Bull - which believes the price will be **within** the specified price bounds during the specified time frame.
2. Bear - which believes the price will be **outside** the specified price bounds during the specified time frame.

Both of those tokens (issued by the same amount as the collateral) are sent to the issuer at the time of the issuing.

The issuer will hold the position they believe in and sell the other tokens, the buyers will hold the opposite position, then they both wait until the event date will occur.

Once the event for which the position was issued has occurred and the oracle has committed on the event data the rightful redeemers must ask the oracle service to provide the proof data (which contains the timestamp, the value of the asset at the time, the root of the Merkle tree of the committed data set, the audit path of the `<asset, value>` pair within the Merkle tree, the index of the `<asset, value>` within the leaves array of the Merkle tree, and the parameters of the Merkle tree), and the oracle contract to attest for the global proof data (the timestamp, the root of the Merkle tree, and the oracle service public key), which would send the fixed payout contract an **attestation token** which when found in the wallet of the Fixed Payout contract will give the redeemer the right for the collateral, since the attestation token is the embodiment of the attestation of the oracle on the occurrence and details of the event.

If the specified redemption data fits both the given attestation token (which guarantees its occurrence) and the position of the given position token (which guarantees the right of the redeemer for the collateral) - the contract then sends the redeemer some of the collateral according to how many position tokens they have provided.

## Usage

The Fixed Payout contract accepts 3 commands:

1. `"Issue"`
2. `"Redeem"`
3. `"Cancel"`

### `"Issue"`

To issue new position tokens - execute the contract with the `"Issue"` command while providing it with a dictionary in the message body which contains the following data:

| Field Name           | Type               | Description
|:--------------------:|:------------------:| -----------
| `"OraclePubKey"`     | `publicKey`        | The public key of the oracle service
| `"OracleContractId"` | `contractId`       | The contract ID of the oracle contract
| `"Ticker"`           | `String`           | The name of the Forex ticker
| `"PriceLow"`         | `UInt64`           | The lowest price for the Bull position
| `"PriceHigh"`        | `UInt64`           | The highest price for the Bull position (optional)
| `"Start"`            | `UInt64`           | The beginning of the time frame in which the asset will be redeemable
| `"Expiry"`           | `UInt64`           | The end of the time frame in which the asset will be redeemable (optional)
| `"Collateral"`       | `String`           | The collateral asset (as an asset string)

The contract will lock to itself all the tokens of the asset defined in the collateral field in the message body which were provided to it in the TX by the issuer and will mint and lock to the issuer the same amount of position tokens of both kinds (so if the issuer supplies the contract with `m` tokens as collateral - the contract will mint and lock to the issuer `m` Bear tokens and `m` Bull tokens based on the future event).

Diagrammatically it looks like this:

```
                                   Collateral × m
Issuer ---------------------------------------------------------------> Contract
   data = <oraclePK, oracleCID, ticker, priceBounds, timeBounds, collateral>

           [[ data ; "Bull" ]] × m     +     [[ data ; "Bear" ]] × m
Contract |------------------------------------------------------------> Issuer

```

### `"Redeem"`

To redeem a position token - execute the contract with the `"Redeem"` command while providing it with a dictionary in the message body which contains the following data:

| Field Name           | Type               | Description
|:--------------------:|:------------------:| -----------
| `"OraclePubKey"`     | `publicKey`        | The public key of the oracle service
| `"OracleContractId"` | `contractId`       | The contract ID of the oracle contract
| `"Ticker"`           | `String`           | The name of the Forex ticker
| `"PriceLow"`         | `UInt64`           | The lowest price for the bull position
| `"PriceHigh"`        | `UInt64`           | The highest price for the bull position (optional)
| `"Start"`            | `UInt64`           | The beginning of the time frame in which the asset will be redeemable
| `"Expiry"`           | `UInt64`           | The end of the time frame in which the asset will be redeemable (optional)
| `"Timestamp"`        | `UInt64`           | Time of the attestation given by the oracle (in milliseconds since Epoch - 00:00:00 UTC, January 1, 1970)
| `"Root"`             | `Hash`             | Root hash of the Merkle tree on which the oracle has committed
| `"Value"`            | `UInt64`           | The attested value of the asset (attested by the oracle)
| `"AuditPath"`        | `list Hash`        | The audit path on the Merkle tree from the leaf of the `<asset, value>` pair to the root of the tree
| `"Index"`            | `UInt64`           | The index of the `<asset, value>` pair in the Merkle tree
| `"Position"`         | `String`           | The position of the redeemer - can be either "Bull" or "Bear"
| `"Collateral"`       | `String`           | The collateral asset (as an asset string)

You'll have to ensure the fixed payout contract has an attestation token from the oracle contract specified in `"OracleContractId"` which commits to the data specified in `"Root"`, `"Timestamp"`, and `"OraclePubKey"`.

You'll also have to provide the contract with the position tokens according to the position specified in `"Position"` - to redeem a Bull position provide the contract with Bull tokens, and make sure the attested value is within the price bounds specified in `"PriceLow"` and `"PriceHigh"`;
to redeem a Bear position provide the contract with Bear tokens, and make sure the attested value is outside the price bounds specified in `"PriceLow"` and `"PriceHigh"`.

For both positions you'll also have to make sure that all the following conditions hold:

1. The specified `"Timestamp"` is within the time bounds specified in `"Start"` and `"Expiry"`.
2. The specified `"AuditPath"` is valid for the Merkle root specified in `"Root"` for the leaf given by the `"Ticker"` key with the specified `"Value"` in the specified `"Index"`.
3. The data hashed in the position tokens is according to the specified `"OraclePubKey"`, `"OracleContractId"`, `"Ticker"`, `"PriceLow"`, `"PriceHigh"`, `"Start"`, `"Expiry"`, and `"Collateral"`.

When all of the conditions hold (including the conditions which are specific for the position) the contract will destroy the provided tokens and lock the collateral asset to the sender of the same amount as the provided position tokens of the specified position.

Diagrammatically it looks like this:

```
                              Postion Token × m
Redeemer -----------------------------------------------------> Contract
               < event data, attestation data, proof data >

                AttestationToken = [[[ attestation data ]]]
Contract ------------------------------------------------------> Contract

(if all the conditions hold):

                     [[event data ; position]] × m
Contract ------------------------------------------------------|

                                 Collateral × m
Contract ------------------------------------------------------> Redeemer

```

The only way for the resulting transaction to be valid is if:
`Position Token` = `[[event data ; position]]`

### `"Cancel"`

To cancel some or all of the issued position tokens you execute the contract with the `"Cancel"`
command and send the contract an equal amount of both position tokens, the same amount of collateral will be sent back to you and the given position tokens will be destroyed.

You'll also need to put in the message body the exact same data that was given at the issuance of the given tokens.

Diagrammatically:

```
                              Collateral × m
Contract --------------------------------------------------------> Canceler
   data = <oraclePK, oracleCID, ticker, priceBounds, timeBounds, collateral>

         [[ data ; "Bull" ]] × m     +     [[ data ; "Bear" ]] × m
Canceler --------------------------------------------------------|

```

As you can see this is the exact dual situation to the `"Issue"` command.

## Problems

If the time frame is too narrow there's a chance the oracle will not commit on the event during that time frame; if the time frame is too wide there's a chance the oracle will commit and attest for both the Bull and the Bear positions, so the collateral will go to whoever redeems it first ("the early bird gets the worm").

## Generated Assets

### Attestation Token

Generated by the **Oracle** contract.

```fsharp
[[[ [[ root ; timestamp ]] ; oraclePublicKey ]]]
```

### Position Token

Generated by the **Fixed Payout** contract.

```fsharp
[[ oracleContractId ; ticker ; priceLow ; priceHigh ; start ; expiry ; collateral ; position ]]
```
