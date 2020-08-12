//Z81187B JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DISP=SHR,DSN=&SYSUID..CBL(BDS0704)
//COBOL.SYSLIB DD DISP=SHR,DSN=&SYSUID..CPY
//LKED.SYSLMOD DD DISP=SHR,DSN=&SYSUID..LOAD(BDS0704)
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=BDS0704
//STEPLIB   DD DISP=SHR,DSN=&SYSUID..LOAD
//GADGETS   DD DISP=SHR,DSN=&SYSUID..CH7.GADGETS
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF