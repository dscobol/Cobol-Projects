#!/bin/sh
################################################################
# Allocate and upload the test dataset.
################################################################
DATANAME='hinstype.dat.txt'

PROJECT='LEARN'
MEMBNAME='INSTYPE'

HLC="Z81187"
DATADIR='C:\Users\dastagg\dev\cobol\Cobol-Projects\common\data\ECBAP'
#DATADIR='/home/dastagg/dev/cobol/Cobol-Projects/common/data'

FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs

echo "Submit the JCL to allocate the test dataset"
zowe ${JOBS_CMD} submit local-file ../JCL/allocate.jcl
sleep 3s

echo "Copy the test data to the created dataset.."
zowe ${FILES_CMD} upload file-to-data-set ${DATADIR}/${DATANAME} ${HLC}.${PROJECT}.${MEMBNAME}
