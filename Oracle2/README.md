# Fee Oracle

## Verify/Build

To verify/record hints, run `zebra e Oracle.fst`.

The elaborated source file will be created in the `output` directory.

To build, run `zebra c Oracle.fst`.

Both the elaborated source file and the compiled binary (`.dll`) file will be created in the `output` directory.

## How it works

The *oracle contract* allows a *data provider* to make a *commitment* (with the `"Commit"` command) in the form of a hash, which will be recorded on the blockchain, and can provide *attestation* tokens (with the `"Attest"` command) to prove that a commitment was recorded on the blockchain.

The *commitment token* is minted by the *contract using a signed transaction (the contract witness is signed by the data provider)* and is derived from the committed hash and the *public key* of the *provider* who sent the data, optionally along with the required attestation fee asset identifier and fee amount (if they are required by the committing oracle).

The *token* is then locked to the *oracle contract* and stays in the possession of the *contract* indefinitely.

Whenever a proof of a commitment is needed, an attestation token can be minted (with the `"Attest"` command) by providing the commitment data (i.e - the committed hash, the *public key* of the *data provider*, and optionally the fee asset and fee amount if there is an attestation fee*)*, from which the *contract* derives once again a *commitment token* which is taken from the *contract wallet* and then locked back to the *contract*, and mints from the same data an *attestation token* which is sent to the *recipient* specified by the *sender*.

If the commitment required an attestation fee, the contract also locks the required attestation fee to the *public key* of the *data provider*, which means the attestation transaction will only be valid if the required attestation fee was provided to the contract as inputs in the transaction (any provided amount of the attestation fee asset behind the required amount will be locked back to the provided *return address* or to the sender if no was *return address* provided).

Since the *contract* tries to take the *commitment token* from its *wallet* and lock it to itself - the only way for the *attestation transaction* to be valid is if the *contract* has the *commitment token* to begin with (otherwise it would create an invalid execution), meaning that the data provider must have first executed the contract with the `"Commit"` command.

## Usage

The Oracle contract has 2 commands:

1. `"Commit"`
2. `"Attest"`

### `"Commit"`

To commit a hash execute the Oracle contract with the `"Commit"` command and provide the hash in the message body as a dictionary with the field name `"Commit"`.

If you want to require an attestation fee to be payed to the committing oracle in return for attestation tokens, you'll need to add a `"FeeAmount"` to specify the required fee, and a `"FeeAsset"` to specify the asset of the fee (if you only provide `"FeeAmount"` without a `"FeeAsset"` the fee asset will automatically be ZP).
**Note:** The transaction must be signed, the provider who executed this command **must authenticate the transaction**.

| Field Name         | Type               | Description                        | Comments
|:------------------:|:------------------:|:----------------------------------:|:--------------------
| `"Commit"`         | `hash`             | The committed hash                 |
| `"FeeAsset"`	     | `asset`	          | Attestation fee asset (optional)   | Defaults to ZP when omitted
| `"FeeAmount"`	     | `uint64`	          | Attestation fee amount (optional)  | Omitting it or setting it to 0 means no fee will be required

The contract will then take the *public key* of the *provider*, concatenate it to the provided hash, concatenate the *fee asset* and *fee amount* (if they are provided) to the result, mint a *commitment token* from the hash of the concatenation, and lock it to itself.

```
                       commitment token = [[ Commit ; public key ]]
Contract |------------------------------------------------------------> Contract
```

(or with attestation fee):

```
        commitment token = [[ Commit ; public key ; FeeAsset ; FeeAmount ]]
Contract |------------------------------------------------------------> Contract
```

From now on the contract will hold the *commitment token* indefinitely.

### `"Attest"`

To ask for an *attestation* on a committed hash by a specific *provider* execute the contract with the `"Attest"` command and provide a message body as a dictionary with the following data:

| Field Name         | Type               | Description                                        | Comments
|:------------------:|:------------------:|:--------------------------------------------------:|:-----------------------------------------------
| `"Commit"`         | `hash`             | The committed hash                                 |
| `"OraclePubKey"`   | `publicKey`        | The public key of the provider                     |
| `"Recipient"`      | `lock`             | The recipient lock of the attestation token        |
| `"FeeAsset"`	     | `asset`	          | Attestation fee asset (optional)	               | Defaults to ZP when omitted
| `"FeeAmount"`	     | `uint64`	          | Attestation fee amount (optional)	               | Omit it or set it to 0 when no fee is required
| `"ReturnAddress"`  | `lock`	          | Return address for change from the attestation fee |

To ensure that the specified hash was indeed committed the contract will take the *public key* of the *provider*, concatenate it to the provided *commitment hash*, concatenate the *fee asset* and *fee amount* (if they are provided) to the result, look up in its *wallet* for a *commitment token* derived from the hash of the concatenation, and lock it to itself.

The contract will also take the **double hash** (the hash of the hash) of the concatenation of the *commitment hash* to the *public key*, mint an *attestation token* out of it, and lock it to the *recipient* - this will provide the *recipient* with a concrete and exchangeable evidence that the specified hash was indeed committed to.

```
                commitment token = [[ Commit ; OraclePubKey ]]
Contract ------------------------------------------------------------> Contract

                attestation token = [[[ Commit ; OraclePubKey ]]]
Contract |------------------------------------------------------------> Recipient
```

(or with attestation fee):

```
        commitment token = [[ Commit ; OraclePubKey ; FeeAsset ; FeeAmount ]]
Contract ------------------------------------------------------------> Contract

                        FeeAsset x FeeAmount
Sender ------------------------------------------------------------> Contract

                attestation token = [[[ Commit ; OraclePubKey ]]]
Contract |------------------------------------------------------------> Recipient
```

## Generated Assets

### Commitment Token

Without attestation fee:

```
[[ hash ; oraclePublicKey ]]
```

with attestation fee:

```
[[ hash ; oraclePublicKey ; feeAsset ; feeAmount ]]
```

### Attestation Token

```
[[[ hash ; oraclePublicKey ]]]
```