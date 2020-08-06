#!/bin/sh
################################################################
# Allocate the Zowe project datasets
################################################################

JOBS_CMD="zos-jobs" # zos-jobs

echo "Submit the JCL to allocate the Zowe project datasets"
zowe ${JOBS_CMD} submit local-file ../JCL/allocate.jcl
