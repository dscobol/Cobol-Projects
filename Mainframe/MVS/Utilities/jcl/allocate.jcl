//HERC03A JOB NOTIFY=HERC03,
//        MSGCLASS=H,MSGLEVEL=(1,1),REGION=144M,
//        USER=HERC01,PASSWORD=CUL8TR
//* CLEAN UP DATASETS
//*************************
//DELETE   EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//DD1      DD DSN=HERC03.CH7.GADGETS,
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
//DD1      DD DSN=HERC03.CH7.GADGETS,
//            DCB=(BLKSIZE=0,LRECL=46,RECFM=FB,DSORG=PS),
//            DISP=(NEW,CATLG),
//            UNIT=SYSDA,
//            VOL=SER=PUB013,
//            SPACE=(TRK,(100,10),RLSE)
//*
