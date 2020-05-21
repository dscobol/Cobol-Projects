//Z81187P JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN   DD DSN=&SYSUID..CBL(PRTRPT1),DISP=SHR
//COBOL.SYSLIB  DD DSN=&SYSUID..CBL,DISP=SHR
//LKED.SYSLMOD  DD DSN=&SYSUID..LOAD(PRTRPT1),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//RUN     EXEC PGM=PRTRPT1
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//INFILE    DD DSN=&SYSUID..SIMRPT.MEMBERS,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
