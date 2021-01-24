# Oracle

## Verify/Build

To verify/record hints, run `zebra e Oracle.fst`. The elaborated source file will be created in the `output` directory.

To build, run `zebra c Oracle.fst`. Both the elaborated source file and the compiled binary (`.dll`) file will be created in the `output` directory.

## How it works

The *oracle contract* allows a *data provider* to make a *commitment* (with the `"Commit"` command) in the form of a hash, which will be recorded on the blockchain, and can provide *attestation* tokens (with the `"Attest"` command) to prove that a commitment was recorded in the blockchain.

The *commitment token* is minted by the *contract using a signed transaction (the contract witness is signed by the data provider)* and is derived from the committed hash and the *public key* of the *provider* who sent the data. The *token* is then locked to the *oracle contract* and stays in the possession of the *contract* indefinitely.

Whenever a proof of a commitment is needed, an attestation token can be minted (with the `"Attest"` command) by providing the committed hash and the *public key* of the *data provider*, from which the *contract* derives once again a *commitment token* which is taken from the *contract wallet* and then locked back to the *contract*, then mints from the same data an *attestation token* which is sent to the *recipient* specified by the *sender*.

Since the *contract* tries to take the *commitment token* from its *wallet* and lock it to itself - the only way for the *attestation transaction* to be valid is if the *contract* has the *commitment token* to begin with (otherwise it would create an invalid execution), meaning that the data provider must have first executed the contract with the `"Commit"` command.

## Usage

The Oracle contract has 2 commands:

1. `"Commit"`
2. `"Attest"`

### `"Commit"`

To commit a hash execute the Oracle contract with the `"Commit"` command and provide the hash in the message body as a dictionary with the field name `"Commit"`.

Note: the execution must be signed, who execute this command must authenticate the transaction.

| Field Name         | Type               | Description
|:------------------:|:------------------:| -----------
| `"Commit"`         | `hash`             | The committed hash

The contract will then take the *public key* of the *provider*, concatenate it to the provided hash, mint a *commitment token* from the hash of the concatenation, and lock it to itself.

```
                       commitment token = [[ Commit ; public key ]]
Contract |------------------------------------------------------------> Contract
```

From now on the contract will hold the *commitment token* indefinitely.

### `"Attest"`

To ask for an *attestation* on a committed hash by a specific *provider* execute the contract with the `"Attest"` command and provide a message body as a dictionary with the following data:

| Field Name         | Type               | Description
|:------------------:|:------------------:| -----------
| `"Commit"`         | `hash`             | The committed hash
| `"OraclePubKey"`   | `publicKey`        | The public key of the provider
| `"Recipient"`      | `lock`             | The recipient lock of the attestation token

To ensure that the specified hash was indeed committed to the contract will take the *public key* of the *provider*, concatenate it to the provided hash, look up in its *wallet* for a *commitment token* derived from the hash of the concatenation, and lock it to itself. The contract will also take the **double hash** (the hash of the hash) of the concatenation, mint an *attestation token* out of it, and lock it to the *recipient* - this will provide the *recipient* with a concrete and exchangeable evidence that the specified hash was indeed committed to.

```
                       commitment token = [[ Commit ; public key ]]
Contract ------------------------------------------------------------> Contract

                       attestation token = [[[ Commit ; public key ]]]
Contract |------------------------------------------------------------> Recipient
```

## Generated Assets

### Commitment Token

```
[[ hash ; oraclePublicKey ]]
```

### Attestation Token

```
[[[ hash ; oraclePublicKey ]]]
```
