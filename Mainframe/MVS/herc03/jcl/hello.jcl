//HERC03H  JOB  1,MSGCLASS=H,NOTIFY=HERC03
//COBRUN   EXEC COBUCG
//COB.SYSIN    DD DSNAME=HERC03.CBL(HELLO),DISP=SHR
//COB.SYSLIB   DD DSNAME=SYS1.COBLIB,DISP=SHR
//COB.SYSPUNCH DD DUMMY
//GO.SYSOUT    DD SYSOUT=*
