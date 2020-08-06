#!/bin/bash

PGM=ws532
SYSLIB="$HOME/dev/cobol/common/cpy"

cobc -x ../cbl/$PGM.cbl -I $SYSLIB -o ../bin/$PGM

if [ "$?" -eq 0 ]; then
    ../bin/$PGM
else
    echo "Complier Return code not ZERO."
fi
