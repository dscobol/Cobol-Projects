#!/bin/sh
################################################################
# Just run the job.
JOBS_CMD="zos-jobs" # zos-jobs

echo "Compile and Run my app"
zowe ${JOBS_CMD} submit local-file ../JCL/RUN.jcl
