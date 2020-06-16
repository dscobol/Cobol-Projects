//Z81187B JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DISP=SHR,DSN=&SYSUID..CBL(BDS0803)
//COBOL.SYSLIB DD DISP=SHR,DSN=&SYSUID..CPY
//LKED.SYSLMOD DD DISP=SHR,DSN=&SYSUID..LOAD(BDS0803)
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=BDS0803
//STEPLIB   DD DISP=SHR,DSN=&SYSUID..LOAD
//MEMBERS   DD DISP=SHR,DSN=&SYSUID..CH8.MEMBERS
//MEMRPT    DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
