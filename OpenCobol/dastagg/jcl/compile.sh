#!/bin/bash

PGM=Listing16-2sub
SYSLIB="$DHOME/dev/cobol/Cobol-Projects/common/cpy"

cobc -x ../cbl/$PGM.cbl -I $SYSLIB -o ../bin/$PGM