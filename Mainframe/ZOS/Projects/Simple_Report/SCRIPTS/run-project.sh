#!/bin/sh
################################################################
# LICENSED MATERIALS - PROPERTY OF IBM
# "RESTRICTED MATERIALS OF IBM"
# (C) COPYRIGHT IBM CORPORATION 2020. ALL RIGHTS RESERVED
# US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,
# OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE
# CONTRACT WITH IBM CORPORATION
################################################################

HLC="Z81187"
PROJECT='SIMRPT'
FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs

echo "Submitted job to allocate data sets.."
zowe ${JOBS_CMD} submit local-file ../JCL/ALLOCATE.jcl
sleep 3s

echo "Copy my app to the created PDS.."
zowe ${FILES_CMD} upload dir-to-pds ../COBOL ${HLC}.${PROJECT}.COBOL
zowe ${FILES_CMD} upload dir-to-pds ../COPYBOOK ${HLC}.${PROJECT}.COBCOPY
zowe ${FILES_CMD} upload file-to-data-set ../RESOURCES/MEMBERS ${HLC}.${PROJECT}.MEMBERS

echo "Compile and Run my app"
zowe ${JOBS_CMD} submit local-file ../JCL/RUN.jcl

#echo "Downloading $HLQ.$PROJECT.CUSTRPT"
#zowe $FILES_CMD download data-set "$HLQ.$PROJECT.CUSTRPT"
#echo "Finished."
