
# FixedPayout

## Verify/Build

To verify/record hints, run `zebra  --z3rlimit 8000000 -e FixedPayout.fst`.
This command may take a long time to run the first time, subsequent runs will be significantly faster.
The elbaorated source file will be created in the `output` directory.

To build, run `zebra --z3rlimit 8000000 -c FixedPayout.fst`
Both the elbaorated source file and the compiled binary (`.dll`) file will be created in the `output` directory.

## How it works

TODO

## Usage

The Fixed Payout contract accepts 2 commands:

1. `Buy`

2. `Redeem`

### `Buy`

To issue a new bet - execute the contract with the `Buy` command while providing it with a dicitionary in the
message body which contains the following data:

| Field Name         | Type               | Description
|:------------------:|:------------------:| -----------
| `OraclePubKey`     | `publicKey`        | The public key of the oracle service
| `OracleContractId` | `contractId`       | The contract ID of the oracle contract
| `Ticker`           | `String`           | The name of the asset on which the bet is based
| `PriceLow`         | `UInt64`           | The lowest price estimate for the Bull position
| `PriceHigh`        | `UInt64`           | The highest price estimate for the Bull position (optional)
| `TimeLow`          | `UInt64`           | The beginning time of the bet
| `TimeHigh`         | `UInt64`           | The ending time of the bet (optional)

The contract will lock to itself all the ZPs supplied to it in the TX by the issuer and will mint and lock to the
issuer the same amount of bet tokens of both types (so if the issuer supplies the contract with `m` ZPs -
the contract will mint and lock to the issuer `m` Bear tokens and `m` Bull tokens).

Diagrammatically it looks like this:

                                             ZP × m
    Issuer ------------------------------------------------------------------------------> Contract
                     data = <oraclePK, oracleCID, ticker, priceBounds, timeBounds>

                     [[ "Bull", data ]] × m     +     [[ "Bear", data ]] × m
    Contract |---------------------------------------------------------------------------> Issuer


### `Redeem`

To redeem a bet token - execute the contract with the `Redeem` command while providing it with a dictionary in the
message body which contrains the following data:

| Field Name         | Type               | Description
|:------------------:|:------------------:| -----------
| `OraclePubKey`     | `publicKey`        | The public key of the oracle service
| `OracleContractId` | `contractId`       | The contract ID of the oracle contract
| `Ticker`           | `String`           | The name of the asset on which the bet is based
| `PriceLow`         | `UInt64`           | The lowest price estimate for the bull position
| `PriceHigh`        | `UInt64`           | The highest price estimate for the bull position (optional)
| `TimeLow`          | `UInt64`           | The beginning time of the bet
| `TimeHigh`         | `UInt64`           | The ending time of the bet (optional)
| `Timestamp`        | `UInt64`           | Time of the attestion given by the oracle
| `Commit`           | `Hash`             | Root hash of the Merkle tree on which the oracle has comitted
| `Value`            | `UInt64`           | The attested value of the asset (attested by the oracle)
| `AuditPath`        | `list Hash`        | The audit path from on the Merkle tree from the leaf of the <asset, value> pair to the root of the tree.
| `CWT`              | `String`           | Salt for the Efficient Sparse Merkle Tree algorithm
| `DefaultHash`      | `Hash`             | Default hash value for the empty leaves in the Merkle tree
| `Position`         | `String`           | The position of the redeemer - can be either "Bull" or "Bear"

You'll have to provide the contract with an attestation token given by the oracle contract specified in `OracleContractId` which commits to the data specified in `Commit`, `Timestamp`, and `OraclePubKey`.

You'll also have to provide the contract with bet tokens according to the position specified in `Position` -
to redeem a Bull position provide the contract with Bull tokens ([["Bull", data]]),
and make sure the attested value is within the price bounds specified in `PriceLow` and `PriceHigh`;
to redeem a Bear position provide the contract with Bear tokens ([["Bear", data]]),
and make sure the attested value is outside the price bounds specified in `PriceLow` and `PriceHigh`.

For both the Bull and the Bear position you'll also have to make sure that the all the following conditions hold:

1. The specified `Timestamp` is within the time bounds specified in `TimeLow` and `TimeHigh`.
2. The specified `AuditPath` is valid for the Merkle root specified in `Commit` for the leaf given by the `Ticker` key with the specified `Value`, using `CWT` and `DefaultHash` as the tree parameters.
3. The data in the bet tokens is according to the specified `OraclePubKey`, `OracleContractId`, `Ticker`, `PriceLow`, `PriceHigh`, `TimeLow`, and `TimeHigh`.

When all of those conditions hold (including the conditions which are specific for the position) the contract will
destroy the provided tokens and lock ZPs to the sender of the same amount as the provided bet tokens of
the specified position.

Diagrammatically it looks like this:

                             BetToken × m          +       AttestationToken
    Redeemer ------------------------------------------------------------------------------> Contract
                          < bet data, attestation data, proof data >

    (if all the conditions hold):

                    [[Position, bet data]] x m     +     [[[ attestation data ]]]
    Contract -------------------------------------------------------------------------------|


                                                ZP × m
    Contract ------------------------------------------------------------------------------> Redeemer

The only way for the resulting TX to be valid is if:
    `BetToken` = `[[Position, bet data]]`
and:
    `AttestationToken` = `[[[ attestation data ]]]`
