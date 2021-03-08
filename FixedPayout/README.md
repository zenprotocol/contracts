# Fixed Payout

## Verify/Build

To verify/record hints, run `zebra e --z3rlimit 30000000 FixedPayout.fst`.
This command may take a long time to run the first time, subsequent runs will be significantly faster.
The elaborated source file will be created in the `output` directory.

To build, run `zebra c --z3rlimit 30000000 FixedPayout.fst`.
Both the elaborated source file and the compiled binary (`.dll`) file will be created in the `output` directory.

## How it works

This contract assumes there is a working oracle service and an oracle contract which commits on a data set of `<asset, value>` pairs on different times.

The Fixed Payout contract can issue Bull and Bear positions on one of the tickers on which the oracle commits for a future event, and provides the winner with the collateral payed by the issuer.

First - the issuer issues **position tokens** for both positions, derived from the public key of the oracle service, the contract ID of the oracle contract, the name of the forex ticker, the time frame in which the event will take place, the price on which the positions diverge, and the position name.

The contract issues two position tokens, according to the possible positions:

1. Bull - which believes the price will be **above** or **equal to** the specified price during the specified time frame.
2. Bear - which believes the price will be **below** the specified price during the specified time frame.

Both of the tokens (issued by the same amount as the collateral) are sent to the issuer at the time of the issuing.

The issuer will hold the position they believe in and sell the other tokens, the buyers will hold the opposite position, then they will both wait until the event date will occur.

Once the event for which the position was issued has occurred, and the oracle has committed on the event data, the rightful redeemers must ask the oracle service to provide the proof data (which contains the timestamp, the value of the asset at the time of the commitment, the root of the Merkle tree of the committed data set, the audit path of the `<asset, value>` pair within the Merkle tree, the index of the `<asset, value>` within the leaves array of the Merkle tree, and the parameters of the Merkle tree), and the oracle contract has to attest for the global proof data (the timestamp, the root of the Merkle tree, and the oracle service public key), which would send the Fixed Payout contract an **attestation token** which will give the redeemer the right for the collateral when found in the wallet of the Fixed Payout contract, since the attestation token is the embodiment of the attestation of the oracle on the occurrence and details of the event.

If the specified redemption data fits both the given attestation token (which guarantees the occurrence of the event) and the position of the given position token (which guarantees the right of the redeemer for the collateral) - the contract will send the redeemer a portion of the collateral equal to the number of winning position tokens provided.

## Usage

The Fixed Payout contract accepts 3 commands:

1. `"Issue"`
2. `"Redeem"`
3. `"Cancel"`

### `"Issue"`

To issue new position tokens - execute the contract with the `"Issue"` command while providing it with a dictionary in the message body which contains the following data:

| Field Name           | Type               | Description
|:--------------------:|:------------------:| -----------
| `"OraclePubKey"`     | `publicKey`        | Public key of the oracle service
| `"OracleContractId"` | `contractId`       | Contract ID of the oracle contract
| `"Ticker"`           | `String`           | Name of the forex ticker
| `"Price"`            | `UInt64`           | Lowest price for the Bull position and the highest for the Bear position
| `"Start"`            | `UInt64`           | Beginning of the time frame in which the event will take place
| `"Expiry"`           | `UInt64`           | End of the time frame in which the event will take place (optional)
| `"Collateral"`       | `String`           | Collateral asset (as an asset string)

The contract will lock to itself all the collateral tokens (of the asset defined in the `"Collateral"` field in the message body) which were provided to it in the TX by the issuer, and will mint and lock to the issuer the same amount of position tokens of both kinds (so if the issuer supplies the contract with `m` collateral tokens - the contract will mint and lock to the issuer `m` Bear tokens and `m` Bull tokens based on the event).

Diagrammatically it looks like this:

```
                                   Collateral × m
Issuer ---------------------------------------------------------------> Contract
   data = <oraclePK, oracleCID, ticker, price, timeBounds, collateral>

           [[ data ; "Bull" ]] × m     +     [[ data ; "Bear" ]] × m
Contract |------------------------------------------------------------> Issuer

```

### `"Redeem"`

To redeem a position token - execute the contract with the `"Redeem"` command while providing it with a dictionary in the message body which contains the following data:

| Field Name           | Type               | Description
|:--------------------:|:------------------:| -----------
| `"OraclePubKey"`     | `publicKey`        | Public key of the oracle service
| `"OracleContractId"` | `contractId`       | Contract ID of the oracle contract
| `"Ticker"`           | `String`           | Name of the forex ticker
| `"Price"`            | `UInt64`           | Lowest price for the Bull position and the highest for the Bear position
| `"Start"`            | `UInt64`           | Beginning of the time frame in which the event will take place
| `"Expiry"`           | `UInt64`           | End of the time frame in which the event will take place (optional)
| `"Timestamp"`        | `UInt64`           | Time of the commitment made by the oracle (in milliseconds since Epoch - 00:00:00 UTC, January 1, 1970)
| `"Root"`             | `Hash`             | Root hash of the Merkle tree on which the oracle has committed
| `"Value"`            | `UInt64`           | Commited value of the asset (attested by the oracle)
| `"AuditPath"`        | `list Hash`        | Audit path on the Merkle tree from the leaf of the `<asset, value>` pair to the root of the tree
| `"Index"`            | `UInt64`           | Index of the `<asset, value>` pair in the Merkle tree
| `"Position"`         | `String`           | Position of the redeemer - can be either `"Bull"` or `"Bear"`
| `"Collateral"`       | `String`           | Collateral asset (as an asset string)

You'll have to ensure the Fixed Payout contract has an attestation token from the oracle contract specified in `"OracleContractId"` which commits to the data specified in `"Root"`, `"Timestamp"`, and `"OraclePubKey"`.

You'll also have to provide the contract with the position tokens according to the position specified in `"Position"` - to redeem a Bull position provide the contract with Bull tokens, and make sure the attested value is above the price specified in `"Price"` or equal to it; to redeem a Bear position provide the contract with Bear tokens, and make sure the attested value is below the price specified in `"Price"`.

For both positions you'll also have to make sure that all of the following conditions hold:

1. The specified `"Timestamp"` is within the time bounds specified in `"Start"` and `"Expiry"`.
2. The specified `"AuditPath"` is valid for the Merkle root specified in `"Root"` for the leaf given by the `"Ticker"` key with the specified `"Value"` in the specified `"Index"`.
3. The data hashed in the position tokens is according to the specified `"OraclePubKey"`, `"OracleContractId"`, `"Ticker"`, `"Price"`, `"Start"`, `"Expiry"`, and `"Collateral"`.

When all of the conditions hold (including the conditions which are specific for the position) the contract will destroy the provided tokens and will lock to the redeemer collateral tokens of the same amount as the provided position tokens of the specified position.

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
   data = <oraclePK, oracleCID, ticker, price, timeBounds, collateral>

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
[[ oracleContractId ; ticker ; price ; start ; expiry ; collateral ; position ]]
```
