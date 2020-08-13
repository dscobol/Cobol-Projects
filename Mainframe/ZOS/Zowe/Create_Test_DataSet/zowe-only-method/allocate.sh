#!/bin/sh
################################################################
# Allocate and upload a dataset.
#
# This will use the Zowe only method to upload the dataset.
#
# To Use: In all these examples, Z81187 is used. Change that to your TSO user id.
#
# Update the parameters in this file.
# 
# HLQ="Z81187"
# PROJECT='TEST'
# MEMBNAME='MEMBERS'
# LRECL="28"
# 
# Copy the file you want to upload into this directory and update the name.
#
# DATANAME='members.dat.txt'
#
# run with ./allocate.sh
#
# Note: Some editors will automatically put an extra LF at the end
# of a file when you open it, edit and save.
# Double Check the dataset on the Mainframe to make sure that it doesn't
# have that extra blank line from the uploaded file.
#
################################################################
#
# file to be uploaded
DATANAME='members.dat.txt'

# Update these parameters to the new name of the dataset.
PROJECT='TEST'
MEMBNAME='MEMBERS'
LRECL='27'
BLKSIZE='2700'

# These are defaults.  
HLQ='Z81187'

# If you are just uploading test datasets to play with, these are good.
SIZE='3TRK'
RECFM='FB'


FILES_CMD='zos-files' # files
JOBS_CMD='zos-jobs' # zos-jobs

echo 'Zowe: delete the dataset'
zowe ${FILES_CMD} delete data-set \
   ${HLQ}.${PROJECT}.${MEMBNAME} -f
sleep 1s

echo 'Zowe: create the dataset'
zowe ${FILES_CMD} create data-set-sequential \
   ${HLQ}.${PROJECT}.${MEMBNAME} \
   --block-size ${BLKSIZE} \
   --record-format ${RECFM} \
   --record-length ${LRECL} \
   --size ${SIZE}
sleep 1s

echo 'Copy the file to the created dataset..'
zowe ${FILES_CMD} upload file-to-data-set \
   ${DATANAME} ${HLQ}.${PROJECT}.${MEMBNAME}
