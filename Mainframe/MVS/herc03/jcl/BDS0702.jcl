//HERC0372 JOB  1,MSGCLASS=H,NOTIFY=HERC03
//COBRUN   EXEC COBUCG
//COB.SYSIN    DD DISP=SHR,DSNAME=HERC03.CBL(BDS0702)
//COB.SYSLIB   DD DISP=SHR,DSNAME=SYS1.COBLIB
//COB.SYSOUT   DD SYSOUT=*
//COB.SYSPUNCH DD SYSOUT=*
//GO.EMPFILE   DD DISP=SHR,DSNAME=HERC03.CH7.EMPLOYEE
//GO.SYSOUT    DD SYSOUT=*
