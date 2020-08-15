#!/bin/bash

LLM=SUB01
PGM=MAINPGM

SYSLIB="$DHOME/dev/cobol/Cobol-Projects/common/cpy"
LOADLIB="../bin"

# clean up
rm $LOADLIB/$LLM.dll
rm $LOADLIB/$PGM.exe


cobc -m -std=ibm ../cbl/$LLM.cbl -I $SYSLIB -o ../bin/$LLM.dll
cobc -x -std=ibm ../cbl/$PGM.cbl -I $SYSLIB -o ../bin/$PGM

if [ "$?" -eq 0 ]; then
    export COB_LIBRARY_PATH="$LOADLIB"
    ../bin/$PGM
else
    echo "Complier Return code not ZERO."
fi
