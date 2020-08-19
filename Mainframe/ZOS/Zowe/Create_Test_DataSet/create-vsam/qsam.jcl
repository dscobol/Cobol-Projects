//Z81187Q JOB NOTIFY=&SYSUID,
// MSGCLASS=H,MSGLEVEL=(1,1),REGION=144M
//****************************************************************
//* THE FOLLOWING HLQ SYMBOLIC MUST CONTAIN THE HIGH LEVEL
//* QUALIFIER UNDER WHICH THE &PROJECT. DATASETS MAY RESIDE.
//*
//    SET HLQ='Z81187'                        *TSO USER ID
//    SET PROJECT='TEST'
//    SET FILE1='CUSTDATA'
//    SET LENGTH='157'
//    SET FORMAT='FB'
//*************************
//* CLEAN UP DATASETS
//*************************
//DELETE   EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//DD1      DD DSN=&HLQ..&PROJECT..&FILE1.,
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
//DD1      DD DSN=&HLQ..&PROJECT..&FILE1.,
//            DCB=(BLKSIZE=0,LRECL=&LENGTH.,RECFM=&FORMAT.,DSORG=PS),
//            DISP=(NEW,CATLG),
//            SPACE=(TRK,(3,1),RLSE)
//*
