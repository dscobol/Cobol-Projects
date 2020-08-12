//USER21D JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
// SET COBPGM='WRITEFIL'
//**** Compile JCL ******
//STP0000 EXEC PROC=ELAXFCOC,
// CICS=,
// DB2=,
// COMP=
//COBOL.SYSPRINT DD SYSOUT=*
//SYSLIN DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ(&COBPGM.)
//COBOL.SYSLIB DD DISP=SHR,
//        DSN=&SYSUID..COBOL.COPYLIB
//COBOL.SYSXMLSD DD DUMMY
//COBOL.SYSIN DD DISP=SHR,
//        DSN=&SYSUID..LEARN.COBOL(&COBPGM.)
//****Link/Edit Step ******
//LKED EXEC PROC=ELAXFLNK
//LINK.SYSLIB DD DSN=CEE.SCEELKED,
//        DISP=SHR
//        DD DSN=&SYSUID..LEARN.LOAD,
//        DISP=SHR
//LINK.OBJ0000 DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ(&COBPGM.)
//LINK.SYSLIN DD *
     INCLUDE OBJ0000
/*
//LINK.SYSLMOD   DD  DISP=SHR,
//        DSN=&SYSUID..LEARN.LOAD(&COBPGM.)
//*
//** Go (Run) Step. Add //DD cards when needed ******
//GO    EXEC   PROC=ELAXFGO,GO=&COBPGM.,
//        LOADDSN=&SYSUID..LEARN.LOAD
//CEEOPTS DD *
  TEST(,,,DBMDT%USER21:*)
/*
//******* ADDITIONAL RUNTIME JCL HERE ******
//ACCTREC  DD DSN=DDS0001.LEARN.ACCT.DATA,DISP=SHR
//PRTLINE  DD SYSOUT=*
//PAYROLL  DD DSN=DDS0001.LEARN.PAYROL01,DISP=SHR
//PAYROL3A  DD DSN=DDS0001.LEARN.PAYROL3A,DISP=SHR
//INFILE   DD DSN=DDS0001.LEARN.ACCT.DATA,DISP=SHR
//OUTFILE  DD DSN=DDS0001.LEARN.OUTFILE,DISP=SHR
//INVALS   DD DSN=DDS0001.LEARN.INVALS,DISP=SHR
//PAYCHECK DD SYSOUT=*
//FAVIN    DD DSN=DDS0001.LEARN.FAVIN,DISP=SHR
//RFPIN    DD DSN=DDS0001.LEARN.FAVRFP,DISP=SHR
//PROPOSAL DD SYSOUT=*
//B37      DD DSN=DDS0001.LEARN.B37,DISP=SHR
//CLAIM    DD DSN=DDS0001.LEARN.INSCLAIM,DISP=SHR
//CLAIMRPT DD SYSOUT=*