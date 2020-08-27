#!/bin/sh
################################################################
# Allocate and upload a dataset.
#
# This will use a hybrid method to upload the dataset.
# First, it will use a JCL member to delete then create the dataset,
# then it will upload the file to the created dataset.
#
# To Use: In all these examples, Z81187 is used. Change that to your TSO user id.
# Update the parameters in allocate.jcl:
# Change the JOB Name to your TSO user id + a letter or number.
#//    SET HLQ='Z81187'       <*TSO USER ID
#//    SET PROJECT='TEST'
#//    SET FILENAME='MEMBERS'
#//    SET LENGTH='28'
#//    SET FORMAT='FB'
#
# then update those same parameters in this file to match those
# 
#HLQ="Z81187"
#PROJECT='TEST'
#MEMBNAME='MEMBERS'
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
DATANAME='psap.dat.txt'

HLQ="Z81187"
PROJECT='TEST'
MEMBNAME='PSAP'

FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs

echo "Zowe, submit the JCL to allocate the dataset."
zowe ${JOBS_CMD} submit local-file allocate.jcl
sleep 2s

echo "Zowe, upload the file."
zowe ${FILES_CMD} upload file-to-data-set ${DATANAME} ${HLQ}.${PROJECT}.${MEMBNAME}
