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

echo "Deleting data sets for SAM app.."
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.COBOL -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.COBCOPY -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.MEMBERS -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.LOAD -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.OBJ -f
