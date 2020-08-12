//EXE4 JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(EXE4),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(EXE4),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//RUN     EXEC PGM=EXE4
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//WEATIN   DD DSN=&SYSUID..WEATIN,DISP=SHR
//WEATOUT      DD DSN=&SYSUID..WEAT,
//            DCB=(BLKSIZE=0,LRECL=110,RECFM=FB,DSORG=PS),
//            DISP=(NEW,CATLG),SPACE=(TRK,(100,10),RLSE) 
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF