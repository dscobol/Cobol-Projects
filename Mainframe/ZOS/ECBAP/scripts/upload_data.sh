#!/bin/sh
################################################################
# Allocate and upload all the members and datasets for the
# Enterprise COBOL for Business Application Programming class.
################################################################

PROJECT='LEARN'

HLC="Z81187"
DATADIR='../data'
FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs


echo "Create and upload the CBL PDS"
zowe ${FILES_CMD} create pds ${HLC}.${PROJECT}.CBL --size 3CYL --db 10
sleep 2s
zowe ${FILES_CMD} ul dir-to-pds ../cbl ${HLC}.${PROJECT}.CBL
sleep 2s

echo "Create and upload the JCL PDS"
zowe ${FILES_CMD} create pds ${HLC}.${PROJECT}.JCL
sleep 2s
zowe ${FILES_CMD} ul dir-to-pds ../jcl ${HLC}.${PROJECT}.JCL
sleep 2s


echo "Create the CPY, OBJ, and LOAD PDS"
zowe ${FILES_CMD} create pds ${HLC}.${PROJECT}.CPY
zowe ${FILES_CMD} create pds ${HLC}.${PROJECT}.OBJ
zowe ${FILES_CMD} create pds ${HLC}.${PROJECT}.LOAD --rf U --rl 0 --bs 4096
sleep 2s

echo "Create and upload the test datasets"
zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.INSCLAIM --rl 90 --bs 9000
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.INSCLAIM ${HLC}.${PROJECT}.INSCLAIM

zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.ACCT.DATA --rl 170 --bs 1700
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.ACCT.DATA ${HLC}.${PROJECT}.ACCT.DATA --binary

zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.OUTFILE --rl 178 --bs 1780
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.OUTFILE ${HLC}.${PROJECT}.OUTFILE

zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.PAYROL01
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.PAYROL01 ${HLC}.${PROJECT}.PAYROL01

zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.INVALS
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.INVALS ${HLC}.${PROJECT}.INVALS

zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.FAVIN
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.FAVIN ${HLC}.${PROJECT}.FAVIN

zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.B37
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.B37 ${HLC}.${PROJECT}.B37

zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.PAYCHECK.DATA
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.PAYCHECK.DATA ${HLC}.${PROJECT}.PAYCHECK.DATA

zowe ${FILES_CMD} create ps ${HLC}.${PROJECT}.PAYROL3A --rl 90 --bs 9000
sleep 2s
zowe ${FILES_CMD} ul ftds ${DATADIR}/DDS0001.LEARN.PAYROL3A ${HLC}.${PROJECT}.PAYROL3A
