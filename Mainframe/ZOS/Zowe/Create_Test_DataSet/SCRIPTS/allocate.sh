#!/bin/sh
################################################################
# Allocate and upload the test dataset.
################################################################
DATANAME='members.dat.txt'

PROJECT='TEST'
MEMBNAME='MEMBERS'

HLC="Z81187"
DATADIR='..\DATA'
#DATADIR='../DATA'

FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs

echo "Submit the JCL to allocate the test dataset"
zowe ${JOBS_CMD} submit local-file ../JCL/allocate.jcl
sleep 3s

echo "Copy the test data to the created dataset.."
zowe ${FILES_CMD} upload file-to-data-set ${DATADIR}/${DATANAME} ${HLC}.${PROJECT}.${MEMBNAME}
