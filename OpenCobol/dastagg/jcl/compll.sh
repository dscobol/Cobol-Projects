#!/bin/bash

# Some notes:
# Must change name of the file name to match
# what the calling program will call it by.
# The file name will be used by the compiler to create the name of the dll.

LLM=ValidateCheckDigit
SYSLIB="$DHOME/dev/cobol/Cobol-Projects/common/cpy"
LOADLIB="../bin"

# clean up
rm $LOADLIB/$LLM.dll

# compile module and put it in /bin directory
cobc -m -std=ibm ../cbl/$LLM.cbl -I $SYSLIB -o ../bin/$LLM.dll