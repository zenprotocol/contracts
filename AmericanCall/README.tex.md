
# American Call Option Contract

This contract makes it possible for a user with public key $\pi$ issue an amount $\alpha$ of an asset $\omega$ representing the right to trade a strike amount $K$ of base asset $\beta$ for a payout amount $U$ of pair asset $\phi$ before a time $\tau$.

## Verify/Build

To verify/record hints, run `zebra --z3rlimit 7000000 -e American Call.fst`

To build, run `zebra --z3rlimit 7000000 -c American Call.fst`


## Operation

The contract requires a message structured as a dictionary with the following fields:

| Field Name | Type               | Description
|:----------:|:------------------:| -----------
| `Base`     | `String`           | String encoding of $\beta$
| `Pair`     | `String`           | String encoding of $\phi$
| `Strike`   | `UInt64`           | $K$
| `Payout`   | `UInt64`           | $U$
| `Expiry`   | `UInt64`           | $\tau$
| `Issuer`   | `option publicKey` | $\pi$
| `Options`  | `UInt64`           | The number of options involved in the current transaction

These parameters determine the following asset subidentifiers:

| Symbol    | Definition
|:---------:| ----------
| $\omega$  | $H(\beta || \phi || K || U ||\tau || \pi )$
| $\lambda$ | $H(\omega)$

The contract accepts the following commands:

### `Issue`

In this case, the `Options` field in the message must be $\alpha$. The contract must be provided with an amount $U \alpha$ of asset $\phi$.

The contract will mint an amount $\alpha$ of assets $\omega$ and $\lambda$.
It will lock the $\omega$ assets to $\pi$, and both the
$\lambda$ and $\phi$ assets to itself.

### `Exercise`


In this case, the `Options` field in the message must be the number of options being exercised, $\epsilon$.

The contract must be invoked before network time $\tau$.

The contract must be provided with an amount $K \epsilon$ of asset $\beta$, and an amount $\epsilon$ of asset $\omega$.

The contract wallet must be populated with an amount $\epsilon$ of asset $\lambda$, and an amount $U \epsilon$ of asset $\phi$.

The contract will lock the $\phi$ assets to the user, and the $\beta$ assets to $\pi$.

The contract will destroy the $\omega$ and $\lambda$ assets.

### `Expire`

In this case, the `Options` field in the message must be the number of options being expired, $\epsilon$.

The contract must be invoked after network time $\tau$.

The contract wallet must be populated with an amount $\epsilon$ of asset $\lambda$, and an amount $U \epsilon$ of asset $\phi$.

The contract will lock the $\phi$ assets to $\pi$.

The contract will destroy the $\lambda$ assets.
