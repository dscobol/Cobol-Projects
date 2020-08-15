#!/bin/bash

LLM=SUB01
PGM=MAINPGM
RPGM=CALLME

SYSLIB="$DHOME/dev/cobol/Cobol-Projects/common/cpy"
LOADLIB="../bin"

# clean up
#rm $LOADLIB/$LLM3
rm $LOADLIB/$LLM
rm $LOADLIB/$PGM

#cobc -m -std=ibm ../cbl/$LLM3.cbl -I $SYSLIB -o ../bin/$LLM3.dll
cobc -c ../cbl/$LLM.cbl \
    -I $SYSLIB \
    -L $LOADLIB \
    -o ../bin/$LLM

cobc -c -x ../cbl/$PGM.cbl \
    -I $SYSLIB \
    -L $LOADLIB \
    -o ../bin/$PGM

cobc -x ../cbl/$RPGM $PGM $LLM  \
     -I $SYSLIB \
     -L $LOADLIB \
     -o ../bin/$RPGM

if [ "$?" -eq 0 ]; then
    export COB_LIBRARY_PATH="$LOADLIB"
    export LD_LIBRARY_PATH="$LOADLIB"
    ../bin/$PGM.exe
else
    echo "Complier Return code not ZERO."
fi
