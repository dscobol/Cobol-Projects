//Z81187B JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(BDS0702),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(BDS0702),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//RUN     EXEC PGM=BDS0702
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//EMPFILE   DD DSN=&SYSUID..CH7.EMPLOYEE,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF