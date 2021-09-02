#!/bin/bash

RLIMIT=800000000

cd AuthenticatedSupply
zebra c -z $RLIMIT AuthenticatedSupply.fst
cd ..

cd Bet
zebra c -z $RLIMIT Bet.fst
zebra r Tests/Buy.fsx
zebra r Tests/Redeem.fsx
cd ..

cd CGP
zebra c -z $RLIMIT CGP.fst
zebra r tests/CGP.fsx
cd ..

cd Dex
zebra c -z $RLIMIT Dex.fst
zebra r Tests/Dex_Make.fsx
zebra r Tests/Dex_Take.fsx
zebra r Tests/Dex_Cancel.fsx
cd ..

cd FixedPayout
zebra c -z $RLIMIT FixedPayout.fst
zebra r tests/FPC3.fsx
cd ..

cd MOfN
zebra c -z $RLIMIT MOfN.fst
cd ..

cd NamedToken
zebra c -z $RLIMIT NamedToken.fst
cd ..

cd Oracle
zebra c -z $RLIMIT Oracle.fst
cd Tests
source run
cd ..
cd ..

cd Oracle2
zebra c -z $RLIMIT Oracle2.fst
zebra r tests/OracleTests.fsx
cd ..

