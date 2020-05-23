#!/bin/sh
################################################################
# Delete all the Zowe project datasets
################################################################

HLC="Z81187"
PROJECT="ZOWE"
FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs

echo "Deleting data sets for ZOWE projects environment.."
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.CBL -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.CPY -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.LOAD -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.OBJ -f
