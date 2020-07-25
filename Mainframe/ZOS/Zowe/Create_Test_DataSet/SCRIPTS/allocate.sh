#!/bin/sh
################################################################
# Allocate and upload the test dataset.
################################################################
DATANAME='c10-1testdata.dat.txt'

PROJECT='C10'
MEMBNAME='SALEFILE'

HLC="Z81187"
DATADIR='C:\Users\x-pla\dev\cobol\common\data'
#DATADIR='/home/dastagg/dev/cobol/common/data'
FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs

echo "Submit the JCL to allocate the test dataset"
zowe ${JOBS_CMD} submit local-file ../JCL/allocate.jcl
sleep 3s

echo "Copy the test data to the created dataset.."
zowe ${FILES_CMD} upload file-to-data-set ${DATADIR}/${DATANAME} ${HLC}.${PROJECT}.${MEMBNAME}
