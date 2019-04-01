=============================
American Call Option Contract
=============================

This contract makes it possible for a user with public key :math:`\pi` issue an amount :math:`\alpha` of an asset :math:`\omega` representing the right to trade a strike amount :math:`K` of base asset :math:`\beta` for a payout amount :math:`U` of pair asset :math:`\phi` before a time :math:`\tau`.

------------
Verify/Build
------------

To verify/record hints, run ``zebra --z3rlimit 7000000 -e American Call.fst``

To build, run ``zebra --z3rlimit 7000000 -c American Call.fst``

---------
Operation
---------

The contract requires a message structured as a dictionary with the following fields:

=========== =================    ===========
Field Name  Type                 Description
----------- -----------------    -----------
``Base``    ``String``           String encoding of :math:`\beta`
``Pair``    ``String``           String encoding of :math:`\phi`
``Strike``  ``UInt64``           :math:`K`
``Payout``  ``UInt64``           :math:`U`
``Expiry``  ``UInt64``           :math:`\tau`
``Issuer``  ``option publicKey`` :math:`\pi`
``Options`` ``UInt64``           The number of options involved in the current transaction
=========== ==================   ===========

These parameters determine the following asset subidentifiers:

=============== ==========
Symbol          Definition
--------------- ----------
:math:`\omega`  :math:`H(\beta || \phi || K || U ||\tau || \pi )`
:math:`\lambda` :math:`H(\omega)`
=============== ==========

The contract accepts the following commands:

``Issue``
~~~~~~~~~

In this case, the ``Options`` field in the message must be :math:`\alpha`. The contract must be provided with an amount :math:`U \alpha` of asset :math:`\phi`.

The contract will mint an amount :math:`\alpha` of assets :math:`\omega` and :math:`\lambda`.
It will lock the :math:`\omega` assets to :math:`\pi`, and both the
:math:`\lambda` and :math:`\phi` assets to itself.

``Exercise``
~~~~~~~~~~~~

In this case, the ``Options`` field in the message must be the number of options being exercised, :math:`\epsilon`.

The contract must be invoked before network time :math:`\tau`.

The contract must be provided with an amount :math:`K \epsilon` of asset :math:`\beta`, and an amount :math:`\epsilon` of asset :math:`\omega`.

The contract wallet must be populated with an amount :math:`\epsilon` of asset :math:`\lambda`, and an amount :math:`U \epsilon` of asset :math:`\phi`.

The contract will lock the :math:`\phi` assets to the user, and the :math:`\beta` assets to :math:`\pi`.

The contract will destroy the :math:`\omega` and :math:`\lambda` assets.

``Expire``
~~~~~~~~~~~~

In this case, the ``Options`` field in the message must be the number of options being expired, :math:`\epsilon`.

The contract must be invoked after network time :math:`\tau`.

The contract wallet must be populated with an amount :math:`\epsilon` of asset :math:`\lambda`, and an amount :math:`U \epsilon` of asset :math:`\phi`.

The contract will lock the :math:`\phi` assets to :math:`\pi`.

The contract will destroy the :math:`\lambda` assets.
