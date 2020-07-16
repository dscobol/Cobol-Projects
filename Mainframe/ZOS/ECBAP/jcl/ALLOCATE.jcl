//USER21A JOB NOTIFY=&SYSUID,
// MSGCLASS=H,MSGLEVEL=(1,1),REGION=144M
//****************************************************************
//* THE FOLLOWING HLQ SYMBOLIC MUST CONTAIN THE HIGH LEVEL
//* QUALIFIER UNDER WHICH THE &PROJECT. DATASETS MAY RESIDE.
//*
//    SET HLQ='USER21'                        *TSO USER ID
//    SET PROJECT='LEARN'
//    SET FILENAME='FAVIN'
//    SET LENGTH=58
//*************************
//* CLEAN UP DATASETS
//*************************
//DELETE   EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//DD1      DD DSN=&HLQ..&PROJECT..&FILENAME.,
//            DISP=(MOD,DELETE,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(0))
//*
//*************************
//* ALLOCATE DATASETS
//*************************
//ALLOCAT EXEC PGM=IEFBR14,COND=(8,LT)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//DD1      DD DSN=&HLQ..&PROJECT..&FILENAME.,
//            DCB=(BLKSIZE=0,LRECL=&LENGTH.,RECFM=FB,DSORG=PS),
//            DISP=(NEW,CATLG),
//            SPACE=(TRK,(10,2),RLSE)
//*
