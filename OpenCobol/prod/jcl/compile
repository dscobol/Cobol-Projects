#!/bin/bash

if [ -n "$1" ]; then # If first parameter passed then print Hi

SYSLIB="$DHOME/dev/cobol/Cobol-Projects/common/cpy"

    cobc -x ../cbl/$PGM.cbl -I $SYSLIB -o ../bin/$1

else

    echo "Must provide name of program. "

fi
