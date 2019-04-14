
# American Call Option Contract

This contract makes it possible for a user with public key <img src="/AmericanCall/tex/f30fdded685c83b0e7b446aa9c9aa120.svg?invert_in_darkmode&sanitize=true" align=middle width=9.96010619999999pt height=14.15524440000002pt/> issue an amount <img src="/AmericanCall/tex/c745b9b57c145ec5577b82542b2df546.svg?invert_in_darkmode&sanitize=true" align=middle width=10.57650494999999pt height=14.15524440000002pt/> of an asset <img src="/AmericanCall/tex/ae4fb5973f393577570881fc24fc2054.svg?invert_in_darkmode&sanitize=true" align=middle width=10.82192594999999pt height=14.15524440000002pt/> representing the right to trade a strike amount <img src="/AmericanCall/tex/d6328eaebbcd5c358f426dbea4bdbf70.svg?invert_in_darkmode&sanitize=true" align=middle width=15.13700594999999pt height=22.465723500000017pt/> of base asset <img src="/AmericanCall/tex/8217ed3c32a785f0b5aad4055f432ad8.svg?invert_in_darkmode&sanitize=true" align=middle width=10.16555099999999pt height=22.831056599999986pt/> for a payout amount <img src="/AmericanCall/tex/6bac6ec50c01592407695ef84f457232.svg?invert_in_darkmode&sanitize=true" align=middle width=13.01596064999999pt height=22.465723500000017pt/> of pair asset <img src="/AmericanCall/tex/f50853d41be7d55874e952eb0d80c53e.svg?invert_in_darkmode&sanitize=true" align=middle width=9.794543549999991pt height=22.831056599999986pt/> before a time <img src="/AmericanCall/tex/0fe1677705e987cac4f589ed600aa6b3.svg?invert_in_darkmode&sanitize=true" align=middle width=9.046852649999991pt height=14.15524440000002pt/>.

## Verify/Build

To verify/record hints, run `zebra --z3rlimit 7000000 -e American Call.fst`

To build, run `zebra --z3rlimit 7000000 -c American Call.fst`


## Operation

The contract requires a message structured as a dictionary with the following fields:

| Field Name | Type               | Description
|:----------:|:------------------:| -----------
| `Base`     | `String`           | String encoding of <img src="/AmericanCall/tex/8217ed3c32a785f0b5aad4055f432ad8.svg?invert_in_darkmode&sanitize=true" align=middle width=10.16555099999999pt height=22.831056599999986pt/>
| `Pair`     | `String`           | String encoding of <img src="/AmericanCall/tex/f50853d41be7d55874e952eb0d80c53e.svg?invert_in_darkmode&sanitize=true" align=middle width=9.794543549999991pt height=22.831056599999986pt/>
| `Strike`   | `UInt64`           | <img src="/AmericanCall/tex/d6328eaebbcd5c358f426dbea4bdbf70.svg?invert_in_darkmode&sanitize=true" align=middle width=15.13700594999999pt height=22.465723500000017pt/>
| `Payout`   | `UInt64`           | <img src="/AmericanCall/tex/6bac6ec50c01592407695ef84f457232.svg?invert_in_darkmode&sanitize=true" align=middle width=13.01596064999999pt height=22.465723500000017pt/>
| `Expiry`   | `UInt64`           | <img src="/AmericanCall/tex/0fe1677705e987cac4f589ed600aa6b3.svg?invert_in_darkmode&sanitize=true" align=middle width=9.046852649999991pt height=14.15524440000002pt/>
| `Issuer`   | `option publicKey` | <img src="/AmericanCall/tex/f30fdded685c83b0e7b446aa9c9aa120.svg?invert_in_darkmode&sanitize=true" align=middle width=9.96010619999999pt height=14.15524440000002pt/>
| `Options`  | `UInt64`           | The number of options involved in the current transaction

These parameters determine the following asset subidentifiers:

| Symbol    | Definition
|:---------:| ----------
| <img src="/AmericanCall/tex/ae4fb5973f393577570881fc24fc2054.svg?invert_in_darkmode&sanitize=true" align=middle width=10.82192594999999pt height=14.15524440000002pt/>  | <img src="/AmericanCall/tex/69d67f5991498e2b25528958b4666f93.svg?invert_in_darkmode&sanitize=true" align=middle width=140.56757879999998pt height=24.65753399999998pt/>
| <img src="/AmericanCall/tex/fd8be73b54f5436a5cd2e73ba9b6bfa9.svg?invert_in_darkmode&sanitize=true" align=middle width=9.58908224999999pt height=22.831056599999986pt/> | <img src="/AmericanCall/tex/059a1046e91eb20fe51efb568bb9d5fe.svg?invert_in_darkmode&sanitize=true" align=middle width=38.60730884999999pt height=24.65753399999998pt/>

The contract accepts the following commands:

### `Issue`

In this case, the `Options` field in the message must be <img src="/AmericanCall/tex/c745b9b57c145ec5577b82542b2df546.svg?invert_in_darkmode&sanitize=true" align=middle width=10.57650494999999pt height=14.15524440000002pt/>. The contract must be provided with an amount <img src="/AmericanCall/tex/5e020b49a17311284541888331866bf8.svg?invert_in_darkmode&sanitize=true" align=middle width=23.592444149999988pt height=22.465723500000017pt/> of asset <img src="/AmericanCall/tex/f50853d41be7d55874e952eb0d80c53e.svg?invert_in_darkmode&sanitize=true" align=middle width=9.794543549999991pt height=22.831056599999986pt/>.

The contract will mint an amount <img src="/AmericanCall/tex/c745b9b57c145ec5577b82542b2df546.svg?invert_in_darkmode&sanitize=true" align=middle width=10.57650494999999pt height=14.15524440000002pt/> of assets <img src="/AmericanCall/tex/ae4fb5973f393577570881fc24fc2054.svg?invert_in_darkmode&sanitize=true" align=middle width=10.82192594999999pt height=14.15524440000002pt/> and <img src="/AmericanCall/tex/fd8be73b54f5436a5cd2e73ba9b6bfa9.svg?invert_in_darkmode&sanitize=true" align=middle width=9.58908224999999pt height=22.831056599999986pt/>.
It will lock the <img src="/AmericanCall/tex/ae4fb5973f393577570881fc24fc2054.svg?invert_in_darkmode&sanitize=true" align=middle width=10.82192594999999pt height=14.15524440000002pt/> assets to <img src="/AmericanCall/tex/f30fdded685c83b0e7b446aa9c9aa120.svg?invert_in_darkmode&sanitize=true" align=middle width=9.96010619999999pt height=14.15524440000002pt/>, and both the
<img src="/AmericanCall/tex/fd8be73b54f5436a5cd2e73ba9b6bfa9.svg?invert_in_darkmode&sanitize=true" align=middle width=9.58908224999999pt height=22.831056599999986pt/> and <img src="/AmericanCall/tex/f50853d41be7d55874e952eb0d80c53e.svg?invert_in_darkmode&sanitize=true" align=middle width=9.794543549999991pt height=22.831056599999986pt/> assets to itself.

### `Exercise`


In this case, the `Options` field in the message must be the number of options being exercised, <img src="/AmericanCall/tex/7ccca27b5ccc533a2dd72dc6fa28ed84.svg?invert_in_darkmode&sanitize=true" align=middle width=6.672392099999992pt height=14.15524440000002pt/>.

The contract must be invoked before network time <img src="/AmericanCall/tex/0fe1677705e987cac4f589ed600aa6b3.svg?invert_in_darkmode&sanitize=true" align=middle width=9.046852649999991pt height=14.15524440000002pt/>.

The contract must be provided with an amount <img src="/AmericanCall/tex/dba4a743b28f2e711f83d9bbd0c71508.svg?invert_in_darkmode&sanitize=true" align=middle width=21.809391449999993pt height=22.465723500000017pt/> of asset <img src="/AmericanCall/tex/8217ed3c32a785f0b5aad4055f432ad8.svg?invert_in_darkmode&sanitize=true" align=middle width=10.16555099999999pt height=22.831056599999986pt/>, and an amount <img src="/AmericanCall/tex/7ccca27b5ccc533a2dd72dc6fa28ed84.svg?invert_in_darkmode&sanitize=true" align=middle width=6.672392099999992pt height=14.15524440000002pt/> of asset <img src="/AmericanCall/tex/ae4fb5973f393577570881fc24fc2054.svg?invert_in_darkmode&sanitize=true" align=middle width=10.82192594999999pt height=14.15524440000002pt/>.

The contract wallet must be populated with an amount <img src="/AmericanCall/tex/7ccca27b5ccc533a2dd72dc6fa28ed84.svg?invert_in_darkmode&sanitize=true" align=middle width=6.672392099999992pt height=14.15524440000002pt/> of asset <img src="/AmericanCall/tex/fd8be73b54f5436a5cd2e73ba9b6bfa9.svg?invert_in_darkmode&sanitize=true" align=middle width=9.58908224999999pt height=22.831056599999986pt/>, and an amount <img src="/AmericanCall/tex/8eba8aec1dc67099db9582352aff7ec7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.68833129999999pt height=22.465723500000017pt/> of asset <img src="/AmericanCall/tex/f50853d41be7d55874e952eb0d80c53e.svg?invert_in_darkmode&sanitize=true" align=middle width=9.794543549999991pt height=22.831056599999986pt/>.

The contract will lock the <img src="/AmericanCall/tex/f50853d41be7d55874e952eb0d80c53e.svg?invert_in_darkmode&sanitize=true" align=middle width=9.794543549999991pt height=22.831056599999986pt/> assets to the user, and the <img src="/AmericanCall/tex/8217ed3c32a785f0b5aad4055f432ad8.svg?invert_in_darkmode&sanitize=true" align=middle width=10.16555099999999pt height=22.831056599999986pt/> assets to <img src="/AmericanCall/tex/f30fdded685c83b0e7b446aa9c9aa120.svg?invert_in_darkmode&sanitize=true" align=middle width=9.96010619999999pt height=14.15524440000002pt/>.

The contract will destroy the <img src="/AmericanCall/tex/ae4fb5973f393577570881fc24fc2054.svg?invert_in_darkmode&sanitize=true" align=middle width=10.82192594999999pt height=14.15524440000002pt/> and <img src="/AmericanCall/tex/fd8be73b54f5436a5cd2e73ba9b6bfa9.svg?invert_in_darkmode&sanitize=true" align=middle width=9.58908224999999pt height=22.831056599999986pt/> assets.

### `Expire`

In this case, the `Options` field in the message must be the number of options being expired, <img src="/AmericanCall/tex/7ccca27b5ccc533a2dd72dc6fa28ed84.svg?invert_in_darkmode&sanitize=true" align=middle width=6.672392099999992pt height=14.15524440000002pt/>.

The contract must be invoked after network time <img src="/AmericanCall/tex/0fe1677705e987cac4f589ed600aa6b3.svg?invert_in_darkmode&sanitize=true" align=middle width=9.046852649999991pt height=14.15524440000002pt/>.

The contract wallet must be populated with an amount <img src="/AmericanCall/tex/7ccca27b5ccc533a2dd72dc6fa28ed84.svg?invert_in_darkmode&sanitize=true" align=middle width=6.672392099999992pt height=14.15524440000002pt/> of asset <img src="/AmericanCall/tex/fd8be73b54f5436a5cd2e73ba9b6bfa9.svg?invert_in_darkmode&sanitize=true" align=middle width=9.58908224999999pt height=22.831056599999986pt/>, and an amount <img src="/AmericanCall/tex/8eba8aec1dc67099db9582352aff7ec7.svg?invert_in_darkmode&sanitize=true" align=middle width=19.68833129999999pt height=22.465723500000017pt/> of asset <img src="/AmericanCall/tex/f50853d41be7d55874e952eb0d80c53e.svg?invert_in_darkmode&sanitize=true" align=middle width=9.794543549999991pt height=22.831056599999986pt/>.

The contract will lock the <img src="/AmericanCall/tex/f50853d41be7d55874e952eb0d80c53e.svg?invert_in_darkmode&sanitize=true" align=middle width=9.794543549999991pt height=22.831056599999986pt/> assets to <img src="/AmericanCall/tex/f30fdded685c83b0e7b446aa9c9aa120.svg?invert_in_darkmode&sanitize=true" align=middle width=9.96010619999999pt height=14.15524440000002pt/>.

The contract will destroy the <img src="/AmericanCall/tex/fd8be73b54f5436a5cd2e73ba9b6bfa9.svg?invert_in_darkmode&sanitize=true" align=middle width=9.58908224999999pt height=22.831056599999986pt/> assets.
