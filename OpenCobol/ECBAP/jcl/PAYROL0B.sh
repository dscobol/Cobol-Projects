#!/bin/bash

PGM=PAYROL0B

cobc -x ../cbl/$PGM.cbl -I ../cpy -o ../bin/$PGM

if [ "$?" -eq 0 ]; then
    ../bin/$PGM
else
    echo "Complier Return code not ZERO."
fi
