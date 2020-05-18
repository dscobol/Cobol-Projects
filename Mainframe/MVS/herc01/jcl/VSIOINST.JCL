//HERC01I1  JOB MSGCLASS=H,NOTIFY=HERC01,
//            USER=HERC01,PASSWORD=CUL8TR
//*                                                                     00020000
//********************************************************************* 00030000
//* DELETE PRIOR VERSIONS OF SOURCE AND OBJECT DATASETS               * 00040000
//********************************************************************* 00050000
//*                                                                     00060000
//IDCAMS  EXEC PGM=IDCAMS,REGION=1024K                                  00070000
//SYSPRINT DD  SYSOUT=*                                                 00080000
//SYSIN    DD  *                                                        00090000
    DELETE SYS2.VSAMIO.SOURCE NONVSAM SCRATCH PURGE                     00100000
    DELETE SYS2.VSAMIO.OBJECT NONVSAM SCRATCH PURGE                     00110000
/*                                                                      00120000
//*                                                                     00130000
//********************************************************************* 00140000
//* CREATE A PDS WITH PROGRAM SOURCE AND COPY BOOK                    * 00150000
//********************************************************************* 00160000
//*                                                                     00170000
//IEBUPDTE EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      00180000
//SYSPRINT DD  SYSOUT=*                                                 00190000
//*                                                                     00200000
//SYSUT2   DD  DSN=SYS2.VSAMIO.SOURCE,DISP=(,CATLG,DELETE),             00210000
//             UNIT=3380,VOL=SER=PUB012,SPACE=(TRK,(15,,2)),            00220000
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)                      00230000
//SYSPRINT DD  SYSOUT=*                                                 00240000
//SYSIN    DD  DATA,DLM='><'                                            00250000
./ ADD NAME=VSAMIOS,LIST=ALL                                            00260000
         PRINT ON,NOGEN,NODATA                                          00010000
*---------------------------------------------------------------------* 00020000
*                                                                     * 00030000
* THIS ROUTINE IS DESIGNED TO PROVIDE VSAM DATASET INPUT/OUTPUT       * 00040000
* CAPABILITY TO PROGRAMS WRITTEN WITH THE MVT COBOL COMPILER, AND     * 00050000
* EXECUTED UNDER THE MVS 3.8J OPERATING SYSTEM RUNNING ON EMULATED    * 00060000
* HARDWARE UNDER THE HERCULES EMULATOR.                               * 00070000
*                                                                     * 00080000
* THE ROUTINE IS DESIGNED TO PROVIDE ACCESS TO ONE OR MORE VSAM       * 00090000
* DATASETS (CLUSTERS).  THE REQUIRED ACB AND RPL (VSAM DATASET        * 00100000
* CONTROL BLOCKS) ARE GENERATED AND STORED IN MEMORY ALLOCATED BY     * 00110000
* THE CALLING COBOL PROGRAM.  DURING THE CALLS TO OPEN AND ACCESS A   * 00120000
* PARTICULAR DATASET, THE ACB AND RPL ARE MODIFIED AS REQUIRED TO     * 00130000
* SATISFY THE REQUESTS AGAINST THAT PARTICULAR DATASET.   THERE IS    * 00140000
* NO NECESSITY TO MODIFY THE ALC CODE AND REASSEMBLE PRIOR TO CALLING * 00150000
* THE ROUTINE.                                                        * 00160000
*                                                                     * 00170000
* BECAUSE THE MVT COBOL COMPILER IS UNABLE TO UTILIZE DYNAMIC CALLING * 00180000
* OF SUBROUTINES, IT IS NECESSARY TO LINK THE OBJECT MODULE FOR THIS  * 00190000
* ROUTINE TO EACH CALLING COBOL PROGRAM. AS THIS IS THE CASE, IT IS   * 00200000
* NOT NECESSARY FOR THE ROUTINE TO BE SERIALLY REENTRANT, AND I HAVE  * 00210000
* NOT DEVOTED THE EFFORT TO MAKE IT SO.                               * 00220000
*                                                                     * 00230000
* THIS CODE IS PLACED IN THE PUBLIC DOMAIN AND MAY BE FREELY USED     * 00240000
* AND INCORPORATED INTO DERIVED WORKS AS LONG AS ATTRIBUTION TO THE   * 00250000
* ORIGINAL AUTHORSHIP REMAINS IN ANY DISTRIBUTED COPIES OF THE ALC    * 00260000
* SOURCE.                                                             * 00270000
*                                                                     * 00280000
* WRITTEN NOVEMBER, 2001 BY JAY MOSELEY, CCP                          * 00290000
* HTTP://WWW.JAYMOSELEY.COM/HERCULES/                                 * 00300000
*                                                                     * 00310000
*---------------------------------------------------------------------* 00320000
         EJECT                                                          00330000
*                                                                       00340000
         LCLC  &SYSECT                                                  00350000
&SYSECT  SETC  'VSAMIO'            ROUTINE IDENTIFICATION               00360000
*                                                                       00370000
&SYSECT  CSECT                                                          00380000
*                                                                       00390000
@IDENT01 B     @IDENT04(R15)       BRANCH AROUND IDENT CONSTANTS        00400000
         DC    AL1(@IDENT03-@IDENT02)                                   00410000
@IDENT02 DC    C'&SYSECT V2'                                            00420000
         DC    C'&SYSDATE &SYSTIME - '                                  00430000
         DC    C'VSAM INPUT/OUTPUT ROUTINE FOR MVS 3.8J'                00440000
@IDENT03 DS    0H                                                       00450000
@IDENT04 EQU   *-@IDENT01                                               00460000
*                                                                       00470000
         SAVE  (14,12)                                                  00480000
*                                                                       00490000
         BALR  R12,0               BASE REGISTERS ARE 12 AND 11         00500000
         USING *,R12,R11                                                00510000
         LA    R11,4095(,R12)                                           00520000
         LA    R11,1(,R11)                                              00530000
*                                                                       00540000
         LR    R5,R13              COPY CALLER'S SAVEAREA ADDR          00550000
         LA    R13,SAVEAREA        ESTABLISH MY SAVEAREA                00560000
         ST    R5,4(,R13)          BACK CHAIN SAVE AREAS                00570000
         ST    R13,8(,R5)          FORWARD CHAIN SAVE AREAS             00580000
*                                                                       00590000
*---------------------------------------------------------------------* 00600000
* THREE BLOCKS OF STORAGE ARE PASSED AS PARAMETERS FROM THE CALLER.   * 00610000
* THE FIRST CONTAINS THE COMMAND TO BE PROCESSED AND RETURN CODE/     * 00620000
* FEEDBACK FIELDS.  THE SECOND CONTAINS DATASET SPECIFICATION FIELDS  * 00630000
* AND CONTROL BLOCKS MAINTAINED BY THIS ROUTINE ON SUBSEQUENT CALLS.  * 00640000
* THE THIRD IS THE RECORD AREA USED FOR INPUT/OUTPUT FOR THE DATASET. * 00650000
*---------------------------------------------------------------------* 00660000
         LM    R8,R10,0(R1)        ADDRESS PASSED PARAMETER BLOCKS      00670000
         USING $CCOMM,R8                                                00680000
         USING $FCOMM,R9                                                00690000
         USING $RECORD,R10                                              00700000
*                                                                       00710000
*---------------------------------------------------------------------* 00720000
* RESET FIELDS USED TO RETURN STATUS INFORMATION TO THE CALLER        * 00730000
*---------------------------------------------------------------------* 00740000
         XC    $RC,$RC             CLEAR RETURN CODE                    00750000
         XC    $VSRC,$VSRC         CLEAR VSAM RETURN CODE               00760000
         XC    $VSFUNC,$VSFUNC     CLEAR VSAM FUNCTION CODE             00770000
         XC    $VSREAS,$VSREAS     CLEAR VSAM REASON CODE               00780000
*                                                                       00790000
*---------------------------------------------------------------------* 00800000
* PERFORM REQUESTED FUNCTION                                          * 00810000
*---------------------------------------------------------------------* 00820000
         CLC   $COMMAND,$OPEN                                           00830000
         BE    @OPEN                                                    00840000
*                                                                       00850000
         CLI   $OPENSW,C'O'        IS DATASET OPEN?                     00860000
         BE    @COMM02               YES, OTHER COMMANDS ARE VALID      00870000
*                                                                       00880000
         MVC   $RC,=H'22'          INDICATE CLOSED DATASET              00890000
         B     GOBACK                                                   00900000
*                                                                       00910000
@COMM02  DS    0H                                                       00920000
         CLC   $COMMAND,$CLOSE                                          00930000
         BE    @CLOSE                                                   00940000
         CLC   $COMMAND,$READ                                           00950000
         BE    @READ                                                    00960000
         CLC   $COMMAND,$WRITE                                          00970000
         BE    @WRITE                                                   00980000
         CLC   $COMMAND,$REWRITE                                        00990000
         BE    @REWRITE                                                 01000000
         CLC   $COMMAND,$DELETE                                         01010000
         BE    @DELETE                                                  01020000
         CLC   $COMMAND,$STARTEQ                                        01030000
         BE    @START                                                   01040000
         CLC   $COMMAND,$STARTGE                                        01050000
         BE    @START                                                   01060000
*                                                                       01070000
         MVC   $RC,=H'20'          INDICATE PARAMETER ERROR             01080000
*                                                                       01090000
GOBACK   DS    0H                                                       01100000
*---------------------------------------------------------------------* 01110000
* RETURN TO CALLING PROGRAM WITH ZERO RETURN CODE IN R15              * 01120000
*---------------------------------------------------------------------* 01130000
         L     R13,4(,R13)         RETRIEVE CALLER'S SAVE AREA          01140000
         RETURN (14,12),RC=0                                            01150000
*                                                                       01160000
@OPEN    DS    0H                                                       01170000
*---------------------------------------------------------------------* 01180000
* PROCESSING "OPEN" COMMAND FROM CALLER                               * 01190000
*---------------------------------------------------------------------* 01200000
         CLI   $OPENSW,C'C'        IS DATASET CLOSED?                   01210000
         BE    @OPEN02               YES, OPEN IS VALID                 01220000
*                                                                       01230000
         MVC   $RC,=H'21'          ERROR: OPEN DATASET                  01240000
         B     GOBACK                                                   01250000
*                                                                       01260000
@OPEN02  DS    0H                                                       01270000
         CLC   $DDNAME,$BLANK      IS DDNAME BLANK?                     01280000
         BNE   @OPEN03               NO, CONTINUE                       01290000
*                                                                       01300000
         MVC   $RC,=H'28'          ERROR: NO DDNAME SUPPLIED            01310000
         B     GOBACK                                                   01320000
*                                                                       01330000
@OPEN03  DS    0H                                                       01340000
*---------------------------------------------------------------------* 01350000
* BUILD ACCESS CONTROL BLOCK IN CALLER'S STORAGE USING MODEL.         * 01360000
*---------------------------------------------------------------------* 01370000
         MVC   IFGACB($ACBLEN),ACBMODEL                                 01380000
         LA    R2,IFGACB                                                01390000
         MODCB ACB=(R2),DDNAME=(*,$DDNAME)                              01400000
*                                                                       01410000
*---------------------------------------------------------------------* 01420000
* BUILD REQUEST PARAMETER LIST BLOCK IN CALLER'S STORAGE USING MODEL. * 01430000
*---------------------------------------------------------------------* 01440000
         MVC   IFGRPL($RPLLEN),RPLMODEL                                 01450000
         LA    R2,IFGRPL                                                01460000
         MODCB RPL=(R2),ACB=(S,IFGACB)                                  01470000
*                                                                       01480000
*---------------------------------------------------------------------* 01490000
* THE COMBINATION OF ORGANIZATION, ACCESS, AND MODE WILL BE USED TO   * 01500000
* DERIVE A NUMERIC VALUE WHICH IS USED TO ENSURE VALID PERMUTATIONS.  * 01510000
*---------------------------------------------------------------------* 01520000
         SR    R7,R7               CLEAR TO COMPUTE MODE KEY VALUE      01530000
*                                                                       01540000
*---------------------------------------------------------------------* 01550000
* PARSE ORGANIZATION KEYWORD                                          * 01560000
*---------------------------------------------------------------------* 01570000
         CLC   $ORG,$ESDS                                               01580000
         BE    @OES                                                     01590000
         CLC   $ORG,$RRDS                                               01600000
         BE    @ORR                                                     01610000
         CLC   $ORG,$KSDS                                               01620000
         BE    @OKS                                                     01630000
*                                                                       01640000
         MVC   $RC,=H'23'          ERROR: UNKNOWN ORGANIZATION          01650000
*                                                                       01660000
         B     GOBACK                                                   01670000
*                                                                       01680000
@OES     DS    0H                                                       01690000
*---------------------------------------------------------------------* 01700000
* ORGANIZATION = ESDS                                                 * 01710000
*---------------------------------------------------------------------* 01720000
***>     LA    R7,0                                                     01730000
         LA    R2,IFGACB                                                01740000
         MODCB ACB=(R2),MACRF=(ADR)                                     01750000
         LA    R2,IFGRPL                                                01760000
         MODCB RPL=(R2),OPTCD=(ADR)                                     01770000
*                                                                       01780000
         B     @OPEN10                                                  01790000
*                                                                       01800000
@ORR     DS    0H                                                       01810000
*---------------------------------------------------------------------* 01820000
* ORGANIZATION = RRDS                                                 * 01830000
*---------------------------------------------------------------------* 01840000
         LA    R7,9                                                     01850000
         LA    R2,IFGACB                                                01860000
         MODCB ACB=(R2),MACRF=(KEY)                                     01870000
         LA    R2,IFGRPL                                                01880000
         MODCB RPL=(R2),OPTCD=(KEY)                                     01890000
*                                                                       01900000
         B     @OPEN10                                                  01910000
*                                                                       01920000
@OKS     DS    0H                                                       01930000
*---------------------------------------------------------------------* 01940000
* ORGANIZATION = KSDS                                                 * 01950000
*---------------------------------------------------------------------* 01960000
         LA    R7,18                                                    01970000
         LA    R2,IFGACB                                                01980000
         MODCB ACB=(R2),MACRF=(KEY)                                     01990000
         LA    R2,IFGRPL                                                02000000
         MODCB RPL=(R2),OPTCD=(KEY)                                     02010000
*                                                                       02020000
@OPEN10  DS    0H                                                       02030000
*---------------------------------------------------------------------* 02040000
* PARSE ACCESS KEYWORD                                                * 02050000
*---------------------------------------------------------------------* 02060000
         CLC   $ACCESS,$SEQ                                             02070000
         BE    @OSEQ                                                    02080000
         CLC   $ACCESS,$DIR                                             02090000
         BE    @ODIR                                                    02100000
         CLC   $ACCESS,$DYN                                             02110000
         BE    @ODYN                                                    02120000
*                                                                       02130000
         MVC   $RC,=H'24'          ERROR: UNKNOWN ACCESS                02140000
*                                                                       02150000
         B     GOBACK                                                   02160000
*                                                                       02170000
@OSEQ    DS    0H                                                       02180000
*---------------------------------------------------------------------* 02190000
* ACCESS = SEQUENTIAL                                                 * 02200000
*---------------------------------------------------------------------* 02210000
***>     LA    R7,0(,R7)                                                02220000
         LA    R2,IFGACB                                                02230000
         MODCB ACB=(R2),MACRF=(SEQ)                                     02240000
         LA    R2,IFGRPL                                                02250000
         MODCB RPL=(R2),OPTCD=(SEQ)                                     02260000
*                                                                       02270000
         B     @OPEN20                                                  02280000
*                                                                       02290000
@ODIR    DS    0H                                                       02300000
*---------------------------------------------------------------------* 02310000
* ACCESS = DIRECT                                                     * 02320000
*---------------------------------------------------------------------* 02330000
         LA    R7,3(,R7)                                                02340000
*                                                                       02350000
         CLC   $ORG,$ESDS                                               02360000
         BNE   @ODIROK                                                  02370000
*                                                                       02380000
         MVC   $RC,=H'25'          ERROR: BAD ACCESS FOR ESDS           02390000
*                                                                       02400000
         B     GOBACK                                                   02410000
*                                                                       02420000
@ODIROK  DS    0H                                                       02430000
         LA    R2,IFGACB                                                02440000
         MODCB ACB=(R2),MACRF=(DIR)                                     02450000
         LA    R2,IFGRPL                                                02460000
         MODCB RPL=(R2),OPTCD=(DIR)                                     02470000
*                                                                       02480000
         B     @OPEN20                                                  02490000
*                                                                       02500000
@ODYN    DS    0H                                                       02510000
*---------------------------------------------------------------------* 02520000
* ACCESS = DYNAMIC                                                    * 02530000
*---------------------------------------------------------------------* 02540000
         LA    R7,6(,R7)                                                02550000
*                                                                       02560000
         CLC   $ORG,$ESDS                                               02570000
         BNE   @ODYNOK                                                  02580000
*                                                                       02590000
         MVC   $RC,=H'25'          ERROR: BAD ACCESS FOR ESDS           02600000
*                                                                       02610000
         B     GOBACK                                                   02620000
*                                                                       02630000
@ODYNOK  DS    0H                                                       02640000
         LA    R2,IFGACB                                                02650000
         MODCB ACB=(R2),MACRF=(SEQ,DIR)                                 02660000
         LA    R2,IFGRPL                                                02670000
         MODCB RPL=(R2),OPTCD=(SEQ)                                     02680000
*                                                                       02690000
@OPEN20  DS    0H                                                       02700000
*---------------------------------------------------------------------* 02710000
* PARSE MODE KEYWORD                                                  * 02720000
*---------------------------------------------------------------------* 02730000
         CLC   $MODE,$INPUT                                             02740000
         BE    @OIN                                                     02750000
         CLC   $MODE,$OUTPUT                                            02760000
         BE    @OOUT                                                    02770000
         CLC   $MODE,$UPDATE                                            02780000
         BE    @OBOTH                                                   02790000
*                                                                       02800000
         MVC   $RC,=H'26'          ERROR: BAD MODE                      02810000
*                                                                       02820000
         B     GOBACK                                                   02830000
*                                                                       02840000
@OIN     DS    0H                                                       02850000
*---------------------------------------------------------------------* 02860000
* MODE = INPUT                                                        * 02870000
*---------------------------------------------------------------------* 02880000
***>     LA    R7,0(,R7)                                                02890000
         LA    R2,IFGACB                                                02900000
         MODCB ACB=(R2),MACRF=(IN)                                      02910000
         LA    R2,IFGRPL                                                02920000
         MODCB RPL=(R2),OPTCD=(NUP)                                     02930000
*                                                                       02940000
         B     @OPEN30                                                  02950000
*                                                                       02960000
@OOUT    DS    0H                                                       02970000
*---------------------------------------------------------------------* 02980000
* MODE = OUTPUT                                                       * 02990000
*---------------------------------------------------------------------* 03000000
         LA    R7,1(,R7)                                                03010000
*                                                                       03020000
         C     R7,=F'16'           ORG = RRDS AND ACCESS = DYNAMIC      03030000
         BE    @OOUTNO               OUTPUT NOT PERMITTED               03040000
         C     R7,=F'22'           ORG = KSDS AND ACCESS = DIRECT       03050000
         BE    @OOUTNO               OUTPUT NOT PERMITTED               03060000
         C     R7,=F'25'           ORG = KSDS AND ACCESS = DYNAMIC      03070000
         BE    @OOUTNO               OUTPUT NOT PERMITTED               03080000
*                                                                       03090000
         LA    R2,IFGACB                                                03100000
         MODCB ACB=(R2),MACRF=(OUT)                                     03110000
         LA    R2,IFGRPL                                                03120000
         MODCB RPL=(R2),OPTCD=(NUP)                                     03130000
*                                                                       03140000
         B     @OPEN30                                                  03150000
*                                                                       03160000
@OOUTNO  DS    0H                                                       03170000
         MVC   $RC,=H'27'          ERROR: BAD MODE FOR ORG/ACCESS       03180000
*                                                                       03190000
         B     GOBACK                                                   03200000
*                                                                       03210000
@OBOTH   DS    0H                                                       03220000
*---------------------------------------------------------------------* 03230000
* MODE = INPUT/OUTPUT                                                 * 03240000
*---------------------------------------------------------------------* 03250000
         LA    R7,2(,R7)                                                03260000
         LA    R2,IFGACB                                                03270000
         MODCB ACB=(R2),MACRF=(OUT)                                     03280000
         LA    R2,IFGRPL                                                03290000
         MODCB RPL=(R2),OPTCD=(UPD)                                     03300000
*                                                                       03310000
@OPEN30  DS    0H                                                       03320000
*---------------------------------------------------------------------* 03330000
* FINISH MODIFICATIONS AND OPEN DATASET                               * 03340000
*---------------------------------------------------------------------* 03350000
         BAL   R14,MODIFY          MODIFY COMMON RPL PARAMETERS         03360000
*                                                                       03370000
         LA    R2,IFGACB                                                03380000
         OPEN  ((R2))                                                   03390000
         MVI   $OPENSW,C'O'                                             03400000
*                                                                       03410000
         B     GOBACK                                                   03420000
*                                                                       03430000
@CLOSE   DS    0H                                                       03440000
*---------------------------------------------------------------------* 03450000
* CLOSE THE DATASET.                                                  * 03460000
*---------------------------------------------------------------------* 03470000
*                                                                       03480000
         LA    R2,IFGACB                                                03490000
         CLOSE ((R2))                                                   03500000
         MVI   $OPENSW,C'C'                                             03510000
*                                                                       03520000
         B     GOBACK                                                   03530000
*                                                                       03540000
@READ    DS    0H                                                       03550000
*---------------------------------------------------------------------* 03560000
* READ RECORD USING KEY/RRN FROM THE DATASET                          * 03570000
*---------------------------------------------------------------------* 03580000
         BAL   R14,MODIFY          MODIFY COMMON RPL PARAMETERS         03590000
*                                                                       03600000
         GET   RPL=(R2)                                                 03610000
*                                                                       03620000
         SHOWCB RPL=(R2),FIELDS=RECLEN,AREA=FEEDBACK,LENGTH=4           03630000
         L     R7,FEEDBACK                                              03640000
         STH   R7,$RECLEN                                               03650000
*                                                                       03660000
         B     GOBACK                                                   03670000
*                                                                       03680000
@WRITE   DS    0H                                                       03690000
*---------------------------------------------------------------------* 03700000
* WRITE PASSED RECORD TO THE DATASET (LOAD OR INSERTION)              * 03710000
*---------------------------------------------------------------------* 03720000
         BAL   R14,MODIFY          MODIFY COMMON RPL PARAMETERS         03730000
*                                                                       03740000
         SR    R7,R7                                                    03750000
         CLC   $MODE,$UPDATE       IF ACB IS OPEN FOR UPDATE            03760000
         BE    @WRITE20              MUST MODIFY FOR THIS INSERTION     03770000
*                                                                       03780000
         B     @WRITE50            NO NEED TO MODIFY                    03790000
*                                                                       03800000
@WRITE20 DS    0H                                                       03810000
         MODCB RPL=(R2),OPTCD=(NUP)                                     03820000
         L     R7,=A(@WRITERS)     ADDRESS TO RESTORE DEFAULT           03830000
*                                                                       03840000
@WRITE50 DS    0H                                                       03850000
         PUT   RPL=(R2)                                                 03860000
*                                                                       03870000
         LTR   R7,R7               WAS RPL MODIFIED FOR THIS WRITE?     03880000
         BZ    GOBACK                NO, JUST RETURN                    03890000
*                                                                       03900000
@WRITERS DS    0H                                                       03910000
*                                                                       03920000
         SR    R7,R7               TO AVOID LOOPING IF ERROR ON MODCB   03930000
         MODCB RPL=(R2),OPTCD=(UPD)                                     03940000
*                                                                       03950000
         B     GOBACK                                                   03960000
*                                                                       03970000
@REWRITE DS    0H                                                       03980000
*---------------------------------------------------------------------* 03990000
* REWRITE THE RECORD PREVIOUSLY READ FROM THE DATASET                 * 04000000
*---------------------------------------------------------------------* 04010000
         LA    R2,IFGRPL                                                04020000
*                                                                       04030000
         SR    R3,R3               CLEAR R4 TO LOAD                     04040000
         LH    R3,$RECLEN            LENGTH OF RECORD                   04050000
*                                                                       04060000
         MODCB RPL=(R2),RECLEN=(R3),AREALEN=(R3),AREA=(R10)             04070000
*                                                                       04080000
         PUT   RPL=(R2)                                                 04090000
*                                                                       04100000
         B     GOBACK                                                   04110000
*                                                                       04120000
@DELETE  DS    0H                                                       04130000
*---------------------------------------------------------------------* 04140000
* DELETE THE PREVIOUSLY READ RECORD FROM THE DATASET                  * 04150000
*---------------------------------------------------------------------* 04160000
         LA    R2,IFGRPL                                                04170000
*                                                                       04180000
         SR    R4,R4               CLEAR R4 TO LOAD                     04190000
         LH    R4,$RECLEN            LENGTH OF RECORD                   04200000
*                                                                       04210000
         MODCB RPL=(R2),RECLEN=(R3),AREALEN=(R3),AREA=(R10)             04220000
*                                                                       04230000
         ERASE RPL=(R2)                                                 04240000
*                                                                       04250000
         B     GOBACK                                                   04260000
*                                                                       04270000
@START   DS    0H                                                       04280000
*---------------------------------------------------------------------* 04290000
* POSITION RECORD POINTER BASED UPON PROVIDED ARGUMENT                * 04300000
*---------------------------------------------------------------------* 04310000
         BAL   R14,MODIFY          MODIFY COMMON RPL PARAMETERS         04320000
*                                                                       04330000
         CLI   $COMMAND+5,C'E'     KEY 'EQ' REQUESTED                   04340000
         BE    @STARTEQ                                                 04350000
*                                                                       04360000
         B     @STARTGE                                                 04370000
*                                                                       04380000
@STARTEQ DS    0H                                                       04390000
*                                                                       04400000
         MODCB RPL=(R2),OPTCD=(KEQ)                                     04410000
         SR    R7,R7               NO NEED TO RESTORE IF ERROR          04420000
*                                                                       04430000
         B     @START50                                                 04440000
*                                                                       04450000
@STARTGE DS    0H                                                       04460000
*                                                                       04470000
         MODCB RPL=(R2),OPTCD=(KGE)                                     04480000
         L     R7,=A(@STARTRS)     ADDRESS TO RESTORE DEFAULT           04490000
*                                                                       04500000
@START50 DS    0H                                                       04510000
*                                                                       04520000
         POINT RPL=(R2)                                                 04530000
*                                                                       04540000
@STARTRS DS    0H                                                       04550000
*                                                                       04560000
         SR    R7,R7               TO AVOID LOOPING IF ERROR ON MODCB   04570000
         MODCB RPL=(R2),OPTCD=(KEQ)                                     04580000
*                                                                       04590000
         B     GOBACK                                                   04600000
*                                                                       04610000
MODIFY   DS    0H                                                       04620000
*---------------------------------------------------------------------* 04630000
* THIS ROUTINE IS PERFORMED FOR EACH COMMAND TO MODIFY THE RPL        * 04640000
* CONTROL BLOCK BASED UPON THE ORGANIZATION, ACCESS, AND OPEN MODE.   * 04650000
*---------------------------------------------------------------------* 04660000
         ST    R14,SAVER14         SAVE RETURN ADDRESS                  04670000
*                                                                       04680000
         LA    R2,IFGRPL           ADDRESS GENERATED RPL                04690000
*                                                                       04700000
         SR    R3,R3               CLEAR R3 TO LOAD                     04710000
         LH    R3,$RECLEN            LENGTH OF RECORD                   04720000
*                                                                       04730000
         SR    R4,R4               CLEAR R4 TO CONTAIN KEY ADDRESS      04740000
         SR    R5,R5               CLEAR R5 TO CONTAIN KEY LENGTH       04750000
*                                                                       04760000
         CLC   $ORG,$ESDS          IF ORGANIZATION NOT ESDS             04770000
         BNE   MOD20                 MORE MODCB PARMS TO LOAD           04780000
*                                                                       04790000
         MODCB RPL=(R2),RECLEN=(R3),AREALEN=(R3),AREA=(R10)             04800000
*                                                                       04810000
         B     MOD80                                                    04820000
*                                                                       04830000
MOD20    DS    0H                                                       04840000
         LA    R4,$RRN             ADDRESS RELATIVE RECORD NUMBER       04850000
*                                                                       04860000
         CLC   $ORG,$RRDS          IF ORGANIZATION NOT RRDS             04870000
         BNE   MOD40                 MORE MODCB PARMS TO LOAD           04880000
*                                                                       04890000
         MODCB RPL=(R2),RECLEN=(R3),AREALEN=(R3),AREA=(R10),ARG=(R4)    04900000
*                                                                       04910000
         B     MOD80                                                    04920000
*                                                                       04930000
MOD40    DS    0H                                                       04940000
         LH    R4,$RKP             LOAD KEY OFFSET                      04950000
         AR    R4,R10              ADD RECORD ADDRESS                   04960000
*                                                                       04970000
         LH    R5,$KEYLEN          LOAD KEY LENGTH                      04980000
*                                                                       04990000
         MODCB RPL=(R2),RECLEN=(R3),AREALEN=(R3),AREA=(R10),           C05000000
               ARG=(R4),KEYLEN=(R5)                                     05010000
*                                                                       05020000
MOD80    DS    0H                                                       05030000
         L     R14,SAVER14         RELOAD RETURN ADDRESS                05040000
         BR    R14                                                      05050000
*                                                                       05060000
SAVER14  DS    F                   SAVE LOCAL RETURN ADDRESS            05070000
*                                                                       05080000
VSERROR  DS    0H                                                       05090000
*---------------------------------------------------------------------* 05100000
* ERROR OCCURED DURING VSAM PROCESSING AND EXIT LIST BROUGHT US HERE. * 05110000
* RETRIEVE FEEDBACK INFORMATION AND PLACE IN THE PARAMETER BLOCK TO   * 05120000
* RETURN TO CALLER.                                                   * 05130000
*---------------------------------------------------------------------* 05140000
         MVC   $RC,=H'8'           INDICATE LOGICAL OR PHYSICAL ERROR   05150000
*                                                                       05160000
         LA    R2,IFGRPL                                                05170000
         SHOWCB RPL=(R2),FIELDS=FDBK,AREA=FEEDBACK,LENGTH=4             05180000
*                                                                       05190000
         ICM   R5,B'1111',FEEDBACK RETRIEVE FEEDBACK CODES              05200000
         STCM  R5,B'0100',$VSRC+1      AND VSAM RETURN CODE             05210000
         STCM  R5,B'0010',$VSFUNC+1    AND VSAM FUNCTION CODE           05220000
         STCM  R5,B'0001',$VSREAS+1    AND VSAM FEEDBACK CODE           05230000
*                                                                       05240000
*---------------------------------------------------------------------* 05250000
* STARTGE AND WRITE COMMANDS MODIFY THE RPL, IF AN ERROR OCCURRED     * 05260000
* THOSE MODIFICATIONS NEED TO BE RESET.  IF R7 IS NOT ZERO, A BRANCH  * 05270000
* TO R7 WILL GO TO THE APPROPRIATE RESET CODE.                        * 05280000
*---------------------------------------------------------------------* 05290000
         CLC   $COMMAND,$STARTGE                                        05300000
         BE    VSERR20                                                  05310000
         CLC   $COMMAND,$WRITE                                          05320000
         BNE   GOBACK                                                   05330000
*                                                                       05340000
VSERR20  DS    0H                                                       05350000
         LTR   R7,R7               IF R7=0 THEN NO RESET NECESSARY      05360000
         BZ    GOBACK                                                   05370000
*                                                                       05380000
         BR    R7                  GO TO RESET MODCB                    05390000
*                                                                       05400000
VSEOF    DS    0H                                                       05410000
*---------------------------------------------------------------------* 05420000
* END OF INPUT OCCURED DURING READ AND THE EXIT LIST BROUGHT US HERE. * 05430000
* PLACE SPECIAL VALUE OF 9999 IN $RC TO INDICATE END OF FILE TO THE   * 05440000
* CALLING PROGRAM.                                                    * 05450000
*---------------------------------------------------------------------* 05460000
         MVC   $RC,=H'9999'                                             05470000
*                                                                       05480000
         B     GOBACK                                                   05490000
*                                                                       05500000
*---------------------------------------------------------------------* 05510000
* THE ACCESS CONTROL BLOCK BELOW IS USED AS A MODEL TO BUILD ACB'S    * 05520000
* DYNAMICALLY AS REQUIRED TO OPEN FILES FOR THE CALLER.               * 05530000
*---------------------------------------------------------------------* 05540000
ACBMODEL ACB   DDNAME=VSAMDD,EXLST=EXL001                               05550000
*                                                                       05560000
*---------------------------------------------------------------------* 05570000
* THE REQUEST PARAMETER BLOCK BELOW IS USED AS A MODEL TO BUILD RPL'S * 05580000
* DYNAMICALLY AS REQUIRED TO PROCESS FILES FOR THE CALLER.            * 05590000
*---------------------------------------------------------------------* 05600000
RPLMODEL RPL   ACB=ACBMODEL                                             05610000
*                                                                       05620000
*---------------------------------------------------------------------* 05630000
* THE EXIT ROUTINE LIST BLOCK PROVIDES THE ADDRESSES FOR ERROR        * 05640000
* HANDLING AND END OF DATASET CONDITIONS.                             * 05650000
*---------------------------------------------------------------------* 05660000
EXL001   EXLST LERAD=VSERROR,SYNAD=VSERROR,EODAD=VSEOF                  05670000
*                                                                       05680000
*---------------------------------------------------------------------* 05690000
* FEEDBACK AFTER LOGIC OR I/O ERROR WILL BE RETRIEVED IN THE FULLWORD * 05700000
* BELOW TO BE RETURNED TO THE CALLER.                                 * 05710000
*---------------------------------------------------------------------* 05720000
FEEDBACK DS    F                                                        05730000
*                                                                       05740000
*---------------------------------------------------------------------* 05750000
* MY REGISTER SAVE AREA                                               * 05760000
*---------------------------------------------------------------------* 05770000
SAVEAREA DS    18F                                                      05780000
*                                                                       05790000
*---------------------------------------------------------------------* 05800000
* THE FOLLOWING CONSTANTS ARE USED TO PARSE PARAMETERS PASSED FROM    * 05810000
* THE CALLER.                                                         * 05820000
*---------------------------------------------------------------------* 05830000
$BLANK   DC    CL08'        '                                           05840000
*                                                                       05850000
$ESDS    DC    CL04'ESDS'                                               05860000
$KSDS    DC    CL04'KSDS'                                               05870000
$RRDS    DC    CL04'RRDS'                                               05880000
*                                                                       05890000
$DIR     DC    CL10'DIRECT    '                                         05900000
$SEQ     DC    CL10'SEQUENTIAL'                                         05910000
$DYN     DC    CL10'DYNAMIC   '                                         05920000
*                                                                       05930000
$INPUT   DC    CL06'INPUT '                                             05940000
$OUTPUT  DC    CL06'OUTPUT'                                             05950000
$UPDATE  DC    CL06'UPDATE'                                             05960000
*                                                                       05970000
$OPEN    DC    CL08'OPEN    '                                           05980000
$CLOSE   DC    CL08'CLOSE   '                                           05990000
$READ    DC    CL08'READ    '                                           06000000
$WRITE   DC    CL08'WRITE   '                                           06010000
$REWRITE DC    CL08'REWRITE '                                           06020000
$DELETE  DC    CL08'DELETE  '                                           06030000
$STARTEQ DC    CL08'STARTEQ '                                           06040000
$STARTGE DC    CL08'STARTGE '                                           06050000
*                                                                       06060000
         LTORG                                                          06070000
*                                                                       06080000
$CCOMM   DSECT                     ACCESSED VIA R8                      06090000
*---------------------------------------------------------------------* 06100000
* THE DUMMY SECTION BELOW MAPS THE COMMAND COMMUNICATION PARAMETER    * 06110000
* BLOCK PASSED FROM THE CALLING PROGRAM.                              * 06120000
*---------------------------------------------------------------------* 06130000
$COMMAND DS    CL08                COMMAND FUNCTION                     06140000
$RC      DS    H                   RETURN CODE                          06150000
$VSRC    DS    H                   VSAM RETURN CODE                     06160000
$VSFUNC  DS    H                   VSAM FUNCTION CODE                   06170000
$VSREAS  DS    H                   VSAM REASON CODE                     06180000
*                                                                       06190000
$FCOMM   DSECT                     ACCESSED VIA R9                      06200000
*---------------------------------------------------------------------* 06210000
* THE DUMMY SECTION BELOW MAPS THE DATASET CONTROL PARAMETER BLOCK    * 06220000
* PASSED FROM THE CALLING PROGRAM.                                    * 06230000
*---------------------------------------------------------------------* 06240000
* THE PARAMETERS BELOW MUST BE SET BEFORE THE DATASET IS OPENED AND   * 06250000
* (GENERALLY) SHOULD NOT BE MODIFIED WHILE THE DATASET IS OPEN.       * 06260000
*---------------------------------------------------------------------* 06270000
$DDNAME  DS    CL08                DD NAME                              06280000
$ORG     DS    CL04                ORGANIZATION (KS/ES/RR)              06290000
$ACCESS  DS    CL10                ACCESS MODE (DIR/SEQ/DYNAM)          06300000
$MODE    DS    CL06                MODE (INPUT/OUTPUT/UPDATE)           06310000
$RECLEN  DS    H                   RECORD LENGTH                        06320000
$RKP     DS    H                   KEY OFFSET FROM BEGINNING OF RECORD  06330000
$KEYLEN  DS    H                   KEY LENGTH                           06340000
$RRN     EQU   $RKP                RELATIVE RECORD NUMBER               06350000
$OPENSW  DS    CL1                 OPEN FLAG (O=OPEN/C=CLOSED)          06360000
         DS    CL1                 FORCE ALIGNMENT                      06370000
         IFGACB DSECT=NO           AREA IN WHICH ACB WILL BE BUILT      06380000
$ACBLEN  EQU   (*-IFGACB)                                               06390000
         IFGRPL DSECT=NO           AREA IN WHICH RPL WILL BE BUILT      06400000
$RPLLEN  EQU   (*-IFGRPL)                                               06410000
*                                                                       06420000
$RECORD  DSECT                     ACCESSED VIA R10                     06430000
*---------------------------------------------------------------------* 06440000
* THE FOLLOWING AREA IS MAPPED TO THE CALLER'S STORAGE WHICH CONTAINS * 06450000
* THE DATA RECORD.  THE STORAGE ALLOCATED MUST MATCH THE RECORD       * 06460000
* LENGTH SET IN THE FILE PARAMETER BLOCK ABOVE OR IT MAY RESULT IN    * 06470000
* DESTRUCTIVE OVERLAP OF OTHER STORAGE IN THE CALLING PROGRAM WHICH   * 06480000
* COULD RESULT IN AN ABEND.                                           * 06490000
*---------------------------------------------------------------------* 06500000
$IOAREA  DS    0CL500                                                   06510000
*                                                                       06520000
R0       EQU   0                                                        06530000
R1       EQU   1                                                        06540000
R2       EQU   2                                                        06550000
R3       EQU   3                                                        06560000
R4       EQU   4                                                        06570000
R5       EQU   5                                                        06580000
R6       EQU   6                                                        06590000
R7       EQU   7                                                        06600000
R8       EQU   8                                                        06610000
R9       EQU   9                                                        06620000
R10      EQU   10                                                       06630000
R11      EQU   11                                                       06640000
R12      EQU   12                                                       06650000
R13      EQU   13                                                       06660000
R14      EQU   14                                                       06670000
R15      EQU   15                                                       06680000
*                                                                       06690000
         END                                                            06700000
./ ADD NAME=VSAMIO,LIST=ALL                                             00280000
000100* ************************************************************** *
000200*                                                                *
000300*        VV   VV  SSSSS     A    M     M  IIII    OOOOO          *
000400*        VV   VV SS   SS   AAA   MM   MM   II    OO   OO         *
000500*        VV   VV SS       AA AA  MMM MMM   II    OO   OO         *
000600*        VV   VV  SSSSS  AA   AA MMMMMMM   II    OO   OO         *
000700*        VV   VV      SS AA   AA MM M MM   II    OO   OO         *
000800*         VV VV  SS   SS AAAAAAA MM   MM   II    OO   OO         *
000900*          VVV   SS   SS AA   AA MM   MM   II    OO   OO         *
001000*           V     SSSSS  AA   AA MM   MM  IIII    OOOOO          *
001100*                                                                *
001200* ************************************************************** *
001300*                                                                *
001400* THESE PARAMETERS ARE USED TO INTERFACE WITH THE VSAM DATASET   *
001500* ACCESS ROUTINE.                                                *
001600*                                                                *
001700* THE VSIO-PARAMETER-VALUES SUPPLY THE VALUES USED TO MOVE INTO  *
001800* PARAMETER ENTRIES TO TAILOR THE ROUTINE TO A SPECIFIC DATASET  *
001900* AND TO PROVIDE COMMANDS TO DRIVE THE ROUTINE.                  *
002000* ************************************************************** *
002100 01  VSIO-PARAMETER-VALUES.
002200     02  VSIO-OPEN               PIC  X(08)  VALUE 'OPEN    '.
002300     02  VSIO-CLOSE              PIC  X(08)  VALUE 'CLOSE   '.
002400     02  VSIO-READ               PIC  X(08)  VALUE 'READ    '.
002500     02  VSIO-WRITE              PIC  X(08)  VALUE 'WRITE   '.
002600     02  VSIO-REWRITE            PIC  X(08)  VALUE 'REWRITE '.
002700     02  VSIO-DELETE             PIC  X(08)  VALUE 'DELETE  '.
002800     02  VSIO-START-KEY-EQUAL    PIC  X(08)  VALUE 'STARTEQ '.
002900     02  VSIO-START-KEY-NOTLESS  PIC  X(08)  VALUE 'STARTGE '.
003000     02  VSIO-KSDS               PIC  X(04)  VALUE 'KSDS'.
003100     02  VSIO-ESDS               PIC  X(04)  VALUE 'ESDS'.
003200     02  VSIO-RRDS               PIC  X(04)  VALUE 'RRDS'.
003300     02  VSIO-SEQUENTIAL         PIC  X(10)  VALUE 'SEQUENTIAL'.
003400     02  VSIO-DIRECT             PIC  X(10)  VALUE 'DIRECT    '.
003500     02  VSIO-DYNAMIC            PIC  X(10)  VALUE 'DYNAMIC   '.
003600     02  VSIO-INPUT              PIC  X(06)  VALUE 'INPUT '.
003700     02  VSIO-OUTPUT             PIC  X(06)  VALUE 'OUTPUT'.
003800     02  VSIO-INPUT-OUTPUT       PIC  X(06)  VALUE 'UPDATE'.
003900
004000* ************************************************************** *
004100* THE VSIO-PARAMETER-BLOCK IS THE COMMUNICATION INTERFACE TO     *
004200* THE ROUTINE.                                                   *
004300* ************************************************************** *
004400 01  VSIO-PARAMETER-BLOCK.
004500     02  VSIO-COMMAND            PIC  X(08).
004600     02  VSIO-RETURN-CODE        PIC S9(04)  COMP.
004700         88  VSIO-SUCCESS                    VALUE +0.
004800         88  VSIO-LOGIC-ERROR                VALUE +8.
004900         88  VSIO-END-OF-FILE                VALUE +9999.
005000         88  VSIO-PARAMETER-ERROR            VALUE +20 THRU +28.
005100         88  VSIO-COMMAND-UNKNOWN            VALUE +20.
005200         88  VSIO-DATASET-ALREADY-OPEN       VALUE +21.
005300         88  VSIO-DATASET-NOT-OPEN           VALUE +22.
005400         88  VSIO-ORGANIZATION-KEYWORD       VALUE +23.
005500         88  VSIO-ACCESS-KEYWORD             VALUE +24.
005600         88  VSIO-ACCESS-UNSUPPORTED         VALUE +25.
005700         88  VSIO-MODE-KEYWORD               VALUE +26.
005800         88  VSIO-MODE-UNSUPPORTED           VALUE +27.
005900         88  VSIO-DDNAME-BLANK               VALUE +28.
006000     02  VSIO-VSAM-RETURN-CODE   PIC S9(04)  COMP.
006100     02  VSIO-VSAM-FUNCTION-CODE PIC S9(04)  COMP.
006200     02  VSIO-VSAM-FEEDBACK-CODE PIC S9(04)  COMP.
006300         88  VSIO-DUPLICATE-RECORD           VALUE +8.
006400         88  VSIO-SEQUENCE-ERROR             VALUE +12.
006500         88  VSIO-RECORD-NOT-FOUND           VALUE +16.
006600         88  VSIO-NO-MORE-SPACE              VALUE +28.
006700         88  VSIO-READ-WITHOUT-START         VALUE +88.
006800* ************************************************************** *
006900*                    END OF VSAMIO COPY BOOK                     *
007000* ************************************************************** *
./ ADD NAME=VSAMIOFB,LIST=ALL                                           00300000
000100* ************************************************************** *
000200*                                                                *
000300*  VV   VV  SSSSS     A    M     M IIII  OOOOO  FFFFFFF BBBBBB   *
000400*  VV   VV SS   SS   AAA   MM   MM  II  OO   OO FF      BB   BB  *
000500*  VV   VV SS       AA AA  MMM MMM  II  OO   OO FF      BB   BB  *
000600*  VV   VV  SSSSS  AA   AA MMMMMMM  II  OO   OO FFFFF   BBBBBB   *
000700*  VV   VV      SS AA   AA MM M MM  II  OO   OO FF      BB   BB  *
000800*   VV VV  SS   SS AAAAAAA MM   MM  II  OO   OO FF      BB   BB  *
000900*    VVV   SS   SS AA   AA MM   MM  II  OO   OO FF      BB   BB  *
001000*     V     SSSSS  AA   AA MM   MM IIII  OOOOO  FF      BBBBBB   *
001100*                                                                *
001200* ************************************************************** *
001300* THESE PARAMETERS ARE USED TO INTERFACE WITH THE VSAM DATASET   *
001400* ACCESS ROUTINE, AND ARE USED TO COMMUNICATE CHARACTERISTICS    *
001500* FOR A SINGLE VSAM DATASET.                                     *
001600*                                                                *
001700* WITH THE 2 EXCEPTIONS FOR RECORD LENGTH (TO ACCOMODATE         *
001800* VARIABLE LENGTH RECORDS) AND RELATIVE RECORD (TO ACCOMODATE    *
001900* RELATIVE RECORD DATASETS) THESE DATA NAMES MUST BE POPULATED   *
002000* PRIOR TO CALLING THE ROUTINE TO OPEN THE DATASET AND MUST NOT  *
002100* THEN BE CHANGED UNTIL THE DATASET HAS BEEN CLOSED.             *
002200* ************************************************************** *
002300 01  VSIO-FILE-BLOCK.
002400     02  VSIO-DDNAME             PIC  X(08)  VALUE SPACES.
002500     02  VSIO-ORGANIZATION       PIC  X(04)  VALUE SPACES.
002600     02  VSIO-ACCESS             PIC  X(10)  VALUE SPACES.
002700     02  VSIO-MODE               PIC  X(06)  VALUE SPACES.
002800     02  VSIO-RECORD-LENGTH      PIC S9(04)  COMP VALUE +0.
002900     02  VSIO-KEY-ARGUMENT.
003000         03  VSIO-KEY-POSITION   PIC S9(04)  COMP VALUE +0.
003100         03  VSIO-KEY-LENGTH     PIC S9(04)  COMP VALUE +0.
003200     02  VSIO-RELATIVE-RECORD    REDEFINES VSIO-KEY-ARGUMENT
003300                                 PIC S9(08)  COMP.
003400     02  FILLER                  PIC  X(01)  VALUE 'C'.
003500         88  VSIO-FILE-OPEN                  VALUE 'O'.
003600         88  VSIO-FILE-CLOSED                VALUE 'C'.
003700     02  FILLER                  PIC  X(153).
003800* ************************************************************** *
003900*                   END OF VSAMIOFB COPY BOOK                    *
004000* ************************************************************** *
./ ADD NAME=ESDSLOAD,LIST=ALL                                           00320000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. ESDSLOAD.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY LOADING AN ESDS      *
000900* CLUSTER WITH RECORDS FROM A SEQUENTIAL DATASET.               *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000     SELECT RECORD-IMAGES
002100         ASSIGN TO DA-2314-S-IMAGES.
002200
002300 DATA DIVISION.
002400 FILE SECTION.
002500 FD  RECORD-IMAGES
002600     LABEL RECORDS ARE STANDARD
002700     BLOCK CONTAINS 10 RECORDS
002800     DATA RECORD IS RECORD-IMAGE.
002900
003000 01  RECORD-IMAGE                PIC X(80).
003100
003200 WORKING-STORAGE SECTION.
003300 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
003400     88  END-OF-FILE                         VALUE 'Y'.
003500
003600 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
003700 77  COUNTER-EDIT                PIC  ZZ,ZZZ,ZZ9.
003800
003900 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
004000 01  ESDSF01                     COPY VSAMIOFB.
004100 01  ESDS-RECORD                 PIC X(80).
004200
004300 PROCEDURE DIVISION.
004400
004500 000-INITIATE.
004600
004700     DISPLAY 'ESDSLOAD: WRITE ESDS SEQUENTIALLY'.
004800     DISPLAY '---------------------------------'.
004900     DISPLAY ' '.
005000
005100     OPEN INPUT RECORD-IMAGES.
005200
005300     MOVE 'ESDSF01' TO VSIO-DDNAME.
005400     MOVE VSIO-ESDS TO VSIO-ORGANIZATION.
005500     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
005600     MOVE VSIO-OUTPUT TO VSIO-MODE.
005700     MOVE +80 TO VSIO-RECORD-LENGTH.
005800     MOVE +0 TO VSIO-KEY-LENGTH, VSIO-KEY-POSITION.
005900     MOVE VSIO-OPEN TO VSIO-COMMAND.
006000     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
006100                         ESDS-RECORD.
006200*    END-CALL.
006300     IF NOT VSIO-SUCCESS
006400         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
006500                 VSIO-COMMAND
006600         EXHIBIT NAMED VSIO-RETURN-CODE,
006700         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
006800                       VSIO-VSAM-FUNCTION-CODE,
006900                       VSIO-VSAM-FEEDBACK-CODE
007000         STOP RUN.
007100*    END-IF.
007200
007300 010-PROCESS.
007400
007500     PERFORM 110-PROCESS-DATA
007600        THRU 119-EXIT
007700       UNTIL END-OF-FILE
007800          OR (NOT VSIO-SUCCESS).
007900*    END-PERFORM.
008000
008100 020-TERMINATE.
008200
008300     CLOSE RECORD-IMAGES.
008400
008500     MOVE VSIO-CLOSE TO VSIO-COMMAND.
008600     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
008700                         ESDS-RECORD.
008800*    END-CALL.
008900     IF NOT VSIO-SUCCESS
009000         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009100                 VSIO-COMMAND
009200         EXHIBIT NAMED VSIO-RETURN-CODE,
009300         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
009400                       VSIO-VSAM-FUNCTION-CODE,
009500                       VSIO-VSAM-FEEDBACK-CODE.
009600*    END-IF.
009700
009800     MOVE RECORD-COUNTER TO COUNTER-EDIT.
009900     DISPLAY COUNTER-EDIT ' RECORDS WERE LOADED SUCCESSFULLY'.
010000
010100     STOP RUN.
010200
010300 110-PROCESS-DATA.
010400     READ RECORD-IMAGES
010500         AT END
010600             MOVE 'Y' TO END-OF-FILE-SWITCH.
010700*    END-READ.
010800
010900     IF NOT END-OF-FILE
011000
011100         MOVE RECORD-IMAGE TO ESDS-RECORD
011200
011300         MOVE VSIO-WRITE TO VSIO-COMMAND
011400         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
011500                         ESDS-RECORD
011600*        END-CALL
011700
011800         IF VSIO-SUCCESS
011900             ADD +1 TO RECORD-COUNTER
012000         ELSE
012100             IF VSIO-LOGIC-ERROR
012200            AND VSIO-NO-MORE-SPACE
012300                DISPLAY 'INSUFFICIENT SPACE DEFINED IN CLUSTER'
012400                        'TO CONTAIN ALL RECORDS - LOADING '
012500                        'TERMINATED'
012600             ELSE
012700                 DISPLAY 'VSAMIO ERROR OCCURRED DURING '
012800                         VSIO-COMMAND
012900                 EXHIBIT NAMED VSIO-RETURN-CODE,
013000                 EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
013100                               VSIO-VSAM-FUNCTION-CODE,
013200                               VSIO-VSAM-FEEDBACK-CODE.
013300*            END-IF
013400*        END-IF
013500*    END-IF.
013600
013700 119-EXIT.
013800     EXIT.
013900
014000
./ ADD NAME=ESDSREAD,LIST=ALL                                           00340000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. ESDSREAD.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY READING RECORDS      *
000900* FROM AN ESDS CLUSTER AND DISPLAYING THEIR CONTENTS ON SYSOUT. *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000 DATA DIVISION.
002100 FILE SECTION.
002200
002300 WORKING-STORAGE SECTION.
002400 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
002500     88  END-OF-FILE                         VALUE 'Y'.
002600
002700 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
002800 77  COUNTER-EDIT                PIC ZZ,ZZZ,ZZ9.
002900
003000 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
003100 01  ESDSF01                     COPY VSAMIOFB.
003200 01  ESDS-RECORD                 PIC X(80).
003300
003400 PROCEDURE DIVISION.
003500
003600 000-INITIATE.
003700
003800     DISPLAY 'ESDSREAD: READ ESDS SEQUENTIALLY'.
003900     DISPLAY '--------------------------------'.
004000     DISPLAY ' '.
004100
004200     MOVE 'ESDSF01' TO VSIO-DDNAME.
004300     MOVE VSIO-ESDS TO VSIO-ORGANIZATION.
004400     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
004500     MOVE VSIO-INPUT TO VSIO-MODE.
004600     MOVE +80 TO VSIO-RECORD-LENGTH.
004700     MOVE +0 TO VSIO-KEY-LENGTH, VSIO-KEY-POSITION.
004800     MOVE VSIO-OPEN TO VSIO-COMMAND.
004900     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
005000                         ESDS-RECORD.
005100*    END-CALL.
005200     IF NOT VSIO-SUCCESS
005300         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
005400                 VSIO-COMMAND
005500         EXHIBIT NAMED VSIO-RETURN-CODE,
005600         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
005700                       VSIO-VSAM-FUNCTION-CODE,
005800                       VSIO-VSAM-FEEDBACK-CODE
005900         STOP RUN.
006000*    END-IF.
006100
006200 010-PROCESS.
006300
006400     PERFORM 110-PROCESS-DATA
006500        THRU 119-EXIT
006600       UNTIL END-OF-FILE.
006700*    END-PERFORM.
006800
006900 020-TERMINATE.
007000
007100     MOVE VSIO-CLOSE TO VSIO-COMMAND.
007200     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
007300                         ESDS-RECORD.
007400*    END-CALL.
007500     IF NOT VSIO-SUCCESS
007600         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
007700                 VSIO-COMMAND
007800         EXHIBIT NAMED VSIO-RETURN-CODE,
007900         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
008000                       VSIO-VSAM-FUNCTION-CODE,
008100                       VSIO-VSAM-FEEDBACK-CODE.
008200*    END-IF.
008300
008400     STOP RUN.
008500
008600 110-PROCESS-DATA.
008700
008800     MOVE VSIO-READ TO VSIO-COMMAND.
008900     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
009000                         ESDS-RECORD.
009100*    END-CALL.
009200
009300     IF NOT VSIO-SUCCESS
009400         IF VSIO-END-OF-FILE
009500             MOVE 'Y' TO END-OF-FILE-SWITCH
009600         ELSE
009700             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009800                     VSIO-COMMAND
009900             EXHIBIT NAMED VSIO-RETURN-CODE,
010000             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
010100                           VSIO-VSAM-FUNCTION-CODE,
010200                           VSIO-VSAM-FEEDBACK-CODE
010300             MOVE 'Y' TO END-OF-FILE-SWITCH.
010400*        END-IF
010500*    END-IF.
010600
010700     IF NOT END-OF-FILE
010800         ADD +1 TO RECORD-COUNTER
010900         MOVE RECORD-COUNTER TO COUNTER-EDIT
011000         DISPLAY COUNTER-EDIT ': ' ESDS-RECORD.
011100*    END-IF.
011200
011300 119-EXIT.
011400     EXIT.
011500
011600
./ ADD NAME=ESDSUPDT,LIST=ALL                                           00360000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. ESDSUPDT.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY READING RECORDS      *
000900* FROM AN ESDS CLUSTER AND THEN REWRITING SOME OF THE RESORDS   *
001000* WITH CHANGED RECORD CONTENTS.                                 *
001100* ************************************************************* *
001200
001300 ENVIRONMENT DIVISION.
001400 CONFIGURATION SECTION.
001500 SOURCE-COMPUTER. IBM-370.
001600 OBJECT-COMPUTER. IBM-370.
001700
001800 INPUT-OUTPUT SECTION.
001900 FILE-CONTROL.
002000
002100 DATA DIVISION.
002200 FILE SECTION.
002300
002400 WORKING-STORAGE SECTION.
002500 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
002600     88  END-OF-FILE                         VALUE 'Y'.
002700
002800 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
002900 77  COUNTER-EDIT                PIC ZZ,ZZZ,ZZ9.
003000
003100 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
003200 01  ESDSF01                     COPY VSAMIOFB.
003300 01  ESDS-RECORD.
003400     02  ESR-ID.
003500         03  ESRI-HIGH           PIC 9(2).
003600         03  FILLER              PIC 9(6).
003700         03  ESRI-LOW            PIC 9(2).
003800     02  FILLER                  PIC X(70).
003900
004000 PROCEDURE DIVISION.
004100
004200     DISPLAY 'ESDSUPDT: READ/REWRITE ESDS SEQUENTIALLY'.
004300     DISPLAY '----------------------------------------'.
004400     DISPLAY ' '.
004500
004600 000-INITIATE.
004700
004800     MOVE 'ESDSF01' TO VSIO-DDNAME.
004900     MOVE VSIO-ESDS TO VSIO-ORGANIZATION.
005000     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
005100     MOVE VSIO-INPUT-OUTPUT TO VSIO-MODE.
005200     MOVE +80 TO VSIO-RECORD-LENGTH.
005300     MOVE +0 TO VSIO-KEY-LENGTH, VSIO-KEY-POSITION.
005400     MOVE VSIO-OPEN TO VSIO-COMMAND.
005500     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
005600                         ESDS-RECORD.
005700*    END-CALL.
005800     IF NOT VSIO-SUCCESS
005900         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
006000                 VSIO-COMMAND
006100         EXHIBIT NAMED VSIO-RETURN-CODE,
006200         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
006300                       VSIO-VSAM-FUNCTION-CODE,
006400                       VSIO-VSAM-FEEDBACK-CODE
006500         STOP RUN.
006600*    END-IF.
006700
006800 010-PROCESS.
006900
007000     PERFORM 110-PROCESS-DATA
007100        THRU 119-EXIT
007200       UNTIL END-OF-FILE
007300          OR (NOT VSIO-SUCCESS).
007400*    END-PERFORM.
007500
007600 020-TERMINATE.
007700
007800     MOVE VSIO-CLOSE TO VSIO-COMMAND.
007900     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
008000                         ESDS-RECORD.
008100*    END-CALL.
008200     IF NOT VSIO-SUCCESS
008300         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
008400                 VSIO-COMMAND
008500         EXHIBIT NAMED VSIO-RETURN-CODE,
008600         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
008700                       VSIO-VSAM-FUNCTION-CODE,
008800                       VSIO-VSAM-FEEDBACK-CODE.
008900*    END-IF.
009000
009100     STOP RUN.
009200
009300 110-PROCESS-DATA.
009400
009500     MOVE VSIO-READ TO VSIO-COMMAND.
009600     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
009700                         ESDS-RECORD.
009800*    END-CALL.
009900
010000     IF NOT VSIO-SUCCESS
010100         IF VSIO-END-OF-FILE
010200             MOVE 'Y' TO END-OF-FILE-SWITCH
010300         ELSE
010400             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
010500                     VSIO-COMMAND
010600             EXHIBIT NAMED VSIO-RETURN-CODE,
010700             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
010800                           VSIO-VSAM-FUNCTION-CODE,
010900                           VSIO-VSAM-FEEDBACK-CODE
011000             MOVE 'Y' TO END-OF-FILE-SWITCH.
011100*        END-IF
011200*    END-IF.
011300
011400     IF NOT END-OF-FILE
011500         ADD +1 TO RECORD-COUNTER
011600         PERFORM 120-UPDATE-PROCESS
011700            THRU 129-EXIT.
011800*    END-IF.
011900
012000 119-EXIT.
012100     EXIT.
012200
012300 120-UPDATE-PROCESS.
012400
012500     IF ESRI-LOW EQUAL 2
012600     OR ESRI-LOW EQUAL 6
012700     OR ESRI-LOW EQUAL 7
012800         MOVE RECORD-COUNTER TO COUNTER-EDIT
012900         DISPLAY COUNTER-EDIT ': ' ESDS-RECORD
013000         MOVE '99' TO ESRI-HIGH
013100         MOVE VSIO-REWRITE TO VSIO-COMMAND
013200         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
013300                         ESDS-RECORD
013400*        END-CALL
013500         IF VSIO-SUCCESS
013600             DISPLAY '   UPDATED: ' ESDS-RECORD
013700         ELSE
013800             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
013900                     VSIO-COMMAND
014000             EXHIBIT NAMED VSIO-RETURN-CODE,
014100             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
014200                           VSIO-VSAM-FUNCTION-CODE,
014300                           VSIO-VSAM-FEEDBACK-CODE.
014400*        END-IF
014500*    END-IF.
014600
014700 129-EXIT.
014800     EXIT.
014900
./ ADD NAME=ESDSADDT,LIST=ALL                                           00380000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. ESDSADDT.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY ADDING RECORDS TO AN *
000900* EXISTING ENTRY SEQUENCED DATASET.                             *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000     SELECT RECORD-IMAGES
002100         ASSIGN TO UR-2540R-S-SYSIN.
002200
002300 DATA DIVISION.
002400 FILE SECTION.
002500 FD  RECORD-IMAGES
002600     LABEL RECORDS ARE OMITTED
002700     BLOCK CONTAINS 0 RECORDS
002800     DATA RECORD IS RECORD-IMAGE.
002900
003000 01  RECORD-IMAGE                PIC X(80).
003100
003200 WORKING-STORAGE SECTION.
003300 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
003400     88  END-OF-FILE                         VALUE 'Y'.
003500
003600 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
003700 77  COUNTER-EDIT                PIC  ZZ,ZZZ,ZZ9.
003800
003900 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
004000 01  ESDSF01                     COPY VSAMIOFB.
004100 01  ESDS-RECORD                 PIC X(80).
004200
004300 PROCEDURE DIVISION.
004400
004500     DISPLAY 'ESDSADDT: WRITE (EXTEND) ESDS SEQUENTIALLY'.
004600     DISPLAY '------------------------------------------'.
004700     DISPLAY ' '.
004800
004900 000-INITIATE.
005000
005100     OPEN INPUT RECORD-IMAGES.
005200
005300     MOVE 'ESDSF01' TO VSIO-DDNAME.
005400     MOVE VSIO-ESDS TO VSIO-ORGANIZATION.
005500     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
005600     MOVE VSIO-OUTPUT TO VSIO-MODE.
005700     MOVE +80 TO VSIO-RECORD-LENGTH.
005800     MOVE +0 TO VSIO-KEY-LENGTH, VSIO-KEY-POSITION.
005900     MOVE VSIO-OPEN TO VSIO-COMMAND.
006000     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
006100                         ESDS-RECORD.
006200*    END-CALL.
006300     IF NOT VSIO-SUCCESS
006400         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
006500                 VSIO-COMMAND
006600         EXHIBIT NAMED VSIO-RETURN-CODE,
006700         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
006800                       VSIO-VSAM-FUNCTION-CODE,
006900                       VSIO-VSAM-FEEDBACK-CODE
007000         STOP RUN.
007100*    END-IF.
007200
007300 010-PROCESS.
007400
007500     PERFORM 110-PROCESS-DATA
007600        THRU 119-EXIT
007700       UNTIL END-OF-FILE
007800          OR (NOT VSIO-SUCCESS).
007900*    END-PERFORM.
008000
008100 020-TERMINATE.
008200
008300     CLOSE RECORD-IMAGES.
008400
008500     MOVE VSIO-CLOSE TO VSIO-COMMAND.
008600     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
008700                         ESDS-RECORD.
008800*    END-CALL.
008900     IF NOT VSIO-SUCCESS
009000         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009100                 VSIO-COMMAND
009200         EXHIBIT NAMED VSIO-RETURN-CODE,
009300         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
009400                       VSIO-VSAM-FUNCTION-CODE,
009500                       VSIO-VSAM-FEEDBACK-CODE.
009600*    END-IF.
009700
009800     MOVE RECORD-COUNTER TO COUNTER-EDIT.
009900     DISPLAY COUNTER-EDIT ' RECORDS WERE ADDED SUCCESSFULLY'.
010000
010100     STOP RUN.
010200
010300 110-PROCESS-DATA.
010400     READ RECORD-IMAGES
010500         AT END
010600             MOVE 'Y' TO END-OF-FILE-SWITCH.
010700*    END-READ.
010800
010900     IF NOT END-OF-FILE
011000
011100         MOVE RECORD-IMAGE TO ESDS-RECORD
011200
011300         MOVE VSIO-WRITE TO VSIO-COMMAND
011400         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, ESDSF01,
011500                         ESDS-RECORD
011600*        END-CALL
011700
011800         IF VSIO-SUCCESS
011900             ADD +1 TO RECORD-COUNTER
012000         ELSE
012100             IF VSIO-LOGIC-ERROR
012200            AND VSIO-NO-MORE-SPACE
012300                DISPLAY 'INSUFFICIENT SPACE DEFINED IN CLUSTER'
012400                        'TO CONTAIN NEW RECORDS - PROCESSING '
012500                        'TERMINATED'
012600             ELSE
012700                 DISPLAY 'VSAMIO ERROR OCCURRED DURING '
012800                         VSIO-COMMAND
012900                 EXHIBIT NAMED VSIO-RETURN-CODE,
013000                 EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
013100                               VSIO-VSAM-FUNCTION-CODE,
013200                               VSIO-VSAM-FEEDBACK-CODE.
013300*            END-IF
013400*        END-IF
013500*    END-IF.
013600
013700 119-EXIT.
013800     EXIT.
013900
014000
./ ADD NAME=KSDSLOAD,LIST=ALL                                           00400000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. KSDSLOAD.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY LOADING A KSDS       *
000900* CLUSTER SEQUENTIALLY WITH RECORDS FROM A SEQUENTIAL DATASET.  *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000     SELECT RECORD-IMAGES
002100         ASSIGN TO DA-2314-S-IMAGES.
002200
002300 DATA DIVISION.
002400 FILE SECTION.
002500 FD  RECORD-IMAGES
002600     LABEL RECORDS ARE STANDARD
002700     BLOCK CONTAINS 10 RECORDS
002800     DATA RECORD IS RECORD-IMAGE.
002900
003000 01  RECORD-IMAGE                PIC X(80).
003100
003200 WORKING-STORAGE SECTION.
003300 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
003400     88  END-OF-FILE                         VALUE 'Y'.
003500
003600 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
003700 77  COUNTER-EDIT                PIC  ZZ,ZZZ,ZZ9.
003800
003900 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
004000 01  KSDSF01                     COPY VSAMIOFB.
004100 01  KSDS-RECORD                 PIC X(80).
004200
004300 PROCEDURE DIVISION.
004400
004500 000-INITIATE.
004600
004700     DISPLAY 'KSDSLOAD: WRITE KSDS SEQUENTIALLY'.
004800     DISPLAY '---------------------------------'.
004900     DISPLAY ' '.
005000
005100     OPEN INPUT RECORD-IMAGES.
005200
005300     MOVE 'KSDSF01' TO VSIO-DDNAME.
005400     MOVE VSIO-KSDS TO VSIO-ORGANIZATION.
005500     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
005600     MOVE VSIO-OUTPUT TO VSIO-MODE.
005700     MOVE +80 TO VSIO-RECORD-LENGTH.
005800     MOVE +0 TO VSIO-KEY-POSITION.
005900     MOVE +10 TO VSIO-KEY-LENGTH.
006000     MOVE VSIO-OPEN TO VSIO-COMMAND.
006100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
006200                         KSDS-RECORD.
006300*    END-CALL.
006400     IF NOT VSIO-SUCCESS
006500         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
006600                 VSIO-COMMAND
006700         EXHIBIT NAMED VSIO-RETURN-CODE,
006800         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
006900                       VSIO-VSAM-FUNCTION-CODE,
007000                       VSIO-VSAM-FEEDBACK-CODE
007100         STOP RUN.
007200*    END-IF.
007300
007400 010-PROCESS.
007500
007600     PERFORM 110-PROCESS-DATA
007700        THRU 119-EXIT
007800       UNTIL END-OF-FILE
007900          OR (NOT VSIO-SUCCESS).
008000*    END-PERFORM.
008100
008200 020-TERMINATE.
008300
008400     CLOSE RECORD-IMAGES.
008500
008600     MOVE VSIO-CLOSE TO VSIO-COMMAND.
008700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
008800                         KSDS-RECORD.
008900*    END-CALL.
009000     IF NOT VSIO-SUCCESS
009100         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009200                 VSIO-COMMAND
009300         EXHIBIT NAMED VSIO-RETURN-CODE,
009400         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
009500                       VSIO-VSAM-FUNCTION-CODE,
009600                       VSIO-VSAM-FEEDBACK-CODE.
009700*    END-IF.
009800
009900     MOVE RECORD-COUNTER TO COUNTER-EDIT.
010000     DISPLAY COUNTER-EDIT ' RECORDS WERE LOADED SUCCESSFULLY'.
010100
010200     STOP RUN.
010300
010400 110-PROCESS-DATA.
010500     READ RECORD-IMAGES
010600         AT END
010700             MOVE 'Y' TO END-OF-FILE-SWITCH.
010800*    END-READ.
010900
011000     IF NOT END-OF-FILE
011100         PERFORM 120-ADD-RECORD THRU 129-EXIT.
011200*    END-IF.
011300
011400 119-EXIT.
011500     EXIT.
011600
011700 120-ADD-RECORD.
011800
011900     MOVE RECORD-IMAGE TO KSDS-RECORD.
012000
012100     MOVE VSIO-WRITE TO VSIO-COMMAND.
012200     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
012300                         KSDS-RECORD.
012400*    END-CALL.
012500
012600     IF VSIO-SUCCESS
012700         ADD +1 TO RECORD-COUNTER
012800         GO TO 129-EXIT.
012900*    END-IF.
013000
013100     IF VSIO-LOGIC-ERROR
013200     AND VSIO-NO-MORE-SPACE
013300         DISPLAY 'INSUFFICIENT SPACE DEFINED IN '
013400                 'CLUSTER TO CONTAIN ALL RECORDS - '
013500                 'LOADING TERMINATED'
013600         GO TO 129-EXIT.
013700*    END-IF.
013800
013900     IF VSIO-LOGIC-ERROR
014000     AND VSIO-SEQUENCE-ERROR
014100         DISPLAY 'KEY SEQUENCE ERROR DURING LOAD'
014200         DISPLAY 'RECORD BYPASSED: ' RECORD-IMAGE
014300         MOVE +0 TO VSIO-RETURN-CODE
014400         GO TO 129-EXIT.
014500*    END-IF.
014600
014700     DISPLAY 'VSAMIO ERROR OCCURRED DURING '
014800             VSIO-COMMAND.
014900     EXHIBIT NAMED VSIO-RETURN-CODE.
015000     EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
015100                   VSIO-VSAM-FUNCTION-CODE,
015200                   VSIO-VSAM-FEEDBACK-CODE.
015300*    END-IF.
015400
015500 129-EXIT.
015600     EXIT.
015700
015800
./ ADD NAME=KSDSLVAR,LIST=ALL                                           00420000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. KSDSLVAR.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY LOADING A KSDS       *
000900* CONTAINING VARIABLE LENGTH RECORDS.                           *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000     SELECT RECORD-IMAGES
002100         ASSIGN TO UR-2540R-S-SYSIN.
002200
002300 DATA DIVISION.
002400 FILE SECTION.
002500 FD  RECORD-IMAGES
002600     LABEL RECORDS ARE OMITTED
002700     BLOCK CONTAINS 0 RECORDS
002800     DATA RECORDS IS RECORD-IMAGE.
002900
003000 01  RECORD-IMAGE.
003100     02  FILLER                  PIC X(07).
003200     02  RI-COLUMN-8             PIC X(01).
003300     02  FILLER                  PIC X(72).
003400
003500 WORKING-STORAGE SECTION.
003600 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
003700     88  END-OF-FILE                         VALUE 'Y'.
003800
003900* ************************************************************** *
004000* TWO ALTERNATING CARD FORMATS ARE READ FROM SYSIN, WITH A PAIR  *
004100* PROVIDING THE DATA TO ADD A STUDENT AND ALL COURSES ENROLLED.  *
004200* BASED ON THE FORMAT, THE CARD READ IS MOVED TO THE APPROPRIATE *
004300* GROUP LEVEL ITEM BELOW FOR PROCESSING.                         *
004400* ************************************************************** *
004500 01  CARD-IMAGES.
004600     02  CI-STUDENT-INFO.
004700         03  CISI-STUDENT-ID     PIC X(07).
004800         03  CISI-NAME           PIC X(22).
004900         03  CISI-ADDRESS        PIC X(25).
005000         03  CISI-CITY           PIC X(15).
005100         03  CISI-STATE          PIC X(02).
005200         03  CISI-ZIPCODE        PIC 9(05).
005300         03  CISI-GENDER         PIC X(01).
005400         03  CISI-MAJOR          PIC X(03).
005500     02  CI-COURSE-INFO.
005600         03  CICI-STUDENT-ID     PIC X(07).
005700         03  CICI-COURSE-TABLE   OCCURS 8 TIMES
005800                                 INDEXED BY CICI-COURSE-IX.
005900             04  FILLER          PIC X(01).
006000             04  CICI-COURSE-ID  PIC 9(06).
006100         03  FILLER              PIC X(17).
006200
006300 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
006400 01  STUDENT-MASTER-FILE         COPY VSAMIOFB.
006500* ************************************************************** *
006600* SINCE THE STUDENT DATASET CONTAINS VARIABLE LENGTH RECORDS,    *
006700* TWO FORMATS ARE DEFINED BELOW, ONE FOR THE 83 BYTE HEADER      *
006800* RECORD AND ONE FOR THE 17 BYTE TRAILER SEGMENTS.               *
006900* ************************************************************** *
007000 01  STUDENT-INFO-RECORD.
007100     02  SIR-STUDENT-ID          PIC X(07).
007200     02  SIR-KEY-ID              PIC 9(03).
007300     02  SIR-NAME                PIC X(22).
007400     02  SIR-ADDRESS             PIC X(25).
007500     02  SIR-CITY                PIC X(15).
007600     02  SIR-STATE               PIC X(02).
007700     02  SIR-ZIPCODE             PIC 9(05).
007800     02  SIR-GENDER              PIC X(01).
007900     02  SIR-MAJOR               PIC X(03).
008000 01  COURSE-INFO-RECORD.
008100     02  CIR-STUDENT-ID          PIC X(07).
008200     02  CIR-KEY-ID              PIC 9(03).
008300     02  CIR-COURSE-ID           PIC 9(06).
008400     02  CIR-GRADE               PIC S9(3)V99 COMP-3.
008500
008600 PROCEDURE DIVISION.
008700
008800 000-INITIATE.
008900
009000     DISPLAY 'KSDSLVAR: LOAD VARIABLE LENGTH KSDS'.
009100     DISPLAY '-----------------------------------'.
009200     DISPLAY ' '.
009300
009400     OPEN INPUT RECORD-IMAGES.
009500
009600     MOVE 'STMASTR' TO VSIO-DDNAME.
009700     MOVE VSIO-KSDS TO VSIO-ORGANIZATION.
009800     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
009900     MOVE VSIO-OUTPUT TO VSIO-MODE.
010000     MOVE +83 TO VSIO-RECORD-LENGTH.
010100     MOVE +0 TO VSIO-KEY-POSITION.
010200     MOVE +10 TO VSIO-KEY-LENGTH.
010300     MOVE VSIO-OPEN TO VSIO-COMMAND.
010400     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
010500                         STUDENT-MASTER-FILE,
010600                         STUDENT-INFO-RECORD.
010700*    END-CALL.
010800     IF NOT VSIO-SUCCESS
010900         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
011000         STOP RUN.
011100*    END-IF.
011200
011300 010-PROCESS.
011400
011500     PERFORM 110-PROCESS-DATA
011600        THRU 119-EXIT
011700       UNTIL END-OF-FILE
011800          OR (NOT VSIO-SUCCESS).
011900*    END-PERFORM.
012000
012100 020-TERMINATE.
012200
012300     CLOSE RECORD-IMAGES.
012400
012500     MOVE VSIO-CLOSE TO VSIO-COMMAND.
012600     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
012700                         STUDENT-MASTER-FILE,
012800                         STUDENT-INFO-RECORD.
012900*    END-CALL.
013000     IF NOT VSIO-SUCCESS
013100         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT.
013200*    END-IF.
013300
013400     STOP RUN.
013500
013600 110-PROCESS-DATA.
013700     READ RECORD-IMAGES
013800         AT END
013900             MOVE 'Y' TO END-OF-FILE-SWITCH.
014000*    END-READ.
014100
014200     IF NOT END-OF-FILE
014300         PERFORM 120-ADD-RECORD THRU 129-EXIT.
014400*    END-IF.
014500
014600 119-EXIT.
014700     EXIT.
014800
014900 120-ADD-RECORD.
015000
015100     IF RI-COLUMN-8 NOT EQUAL SPACE
015200         MOVE RECORD-IMAGE TO CI-STUDENT-INFO
015300         PERFORM 130-ADD-STUDENT THRU 139-EXIT
015400     ELSE
015500         MOVE RECORD-IMAGE TO CI-COURSE-INFO
015600         PERFORM 140-ADD-COURSES THRU 149-EXIT.
015700*    END-IF.
015800
015900 129-EXIT.
016000     EXIT.
016100
016200 130-ADD-STUDENT.
016300
016400     MOVE CISI-STUDENT-ID TO SIR-STUDENT-ID.
016500     MOVE ZERO            TO SIR-KEY-ID.
016600     MOVE CISI-NAME       TO SIR-NAME.
016700     MOVE CISI-ADDRESS    TO SIR-ADDRESS.
016800     MOVE CISI-CITY       TO SIR-CITY.
016900     MOVE CISI-STATE      TO SIR-STATE.
017000     MOVE CISI-ZIPCODE    TO SIR-ZIPCODE.
017100     MOVE CISI-GENDER     TO SIR-GENDER.
017200     MOVE CISI-MAJOR      TO SIR-MAJOR.
017300
017400     MOVE +83 TO VSIO-RECORD-LENGTH.
017500     MOVE VSIO-WRITE TO VSIO-COMMAND.
017600     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
017700                         STUDENT-MASTER-FILE,
017800                         STUDENT-INFO-RECORD.
017900*    END-CALL.
018000
018100     IF VSIO-SUCCESS
018200         GO TO 139-EXIT.
018300*    END-IF.
018400
018500     IF VSIO-LOGIC-ERROR
018600     AND VSIO-NO-MORE-SPACE
018700         DISPLAY 'INSUFFICIENT SPACE DEFINED IN '
018800                 'CLUSTER TO CONTAIN ALL RECORDS - '
018900                 'LOADING TERMINATED'
019000         GO TO 139-EXIT.
019100*    END-IF.
019200
019300     IF VSIO-LOGIC-ERROR
019400     AND VSIO-SEQUENCE-ERROR
019500         DISPLAY 'KEY SEQUENCE ERROR DURING LOAD'
019600         DISPLAY 'RECORD BYPASSED: (STUDENT) '
019700                 SIR-STUDENT-ID '-' SIR-KEY-ID ' ' SIR-NAME
019800         MOVE +0 TO VSIO-RETURN-CODE
019900         GO TO 139-EXIT.
020000*    END-IF.
020100
020200     PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT.
020300
020400 139-EXIT.
020500     EXIT.
020600
020700 140-ADD-COURSES.
020800
020900     MOVE ZERO TO CIR-KEY-ID.
021000     PERFORM 150-ADD-A-COURSE THRU 159-EXIT
021100       VARYING CICI-COURSE-IX
021200         FROM +1 BY +1
021300         UNTIL CICI-COURSE-IX > +8.
021400*    END-PERFORM.
021500
021600 149-EXIT.
021700     EXIT.
021800
021900 150-ADD-A-COURSE.
022000
022100     IF CICI-COURSE-ID (CICI-COURSE-IX) EQUAL ZEROS
022200         GO TO 159-EXIT.
022300*    END-IF.
022400
022500     MOVE CICI-STUDENT-ID TO CIR-STUDENT-ID.
022600     ADD 1 TO CIR-KEY-ID.
022700     MOVE CICI-COURSE-ID (CICI-COURSE-IX) TO CIR-COURSE-ID.
022800     MOVE +0 TO CIR-GRADE.
022900
023000     MOVE +19 TO VSIO-RECORD-LENGTH.
023100     MOVE VSIO-WRITE TO VSIO-COMMAND.
023200     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
023300                         STUDENT-MASTER-FILE,
023400                         COURSE-INFO-RECORD.
023500*    END-CALL.
023600
023700     IF VSIO-SUCCESS
023800         GO TO 159-EXIT.
023900*    END-IF.
024000
024100     IF VSIO-LOGIC-ERROR
024200     AND VSIO-NO-MORE-SPACE
024300         DISPLAY 'INSUFFICIENT SPACE DEFINED IN '
024400                 'CLUSTER TO CONTAIN ALL RECORDS - '
024500                 'LOADING TERMINATED'
024600         GO TO 159-EXIT.
024700*    END-IF.
024800
024900     IF VSIO-LOGIC-ERROR
025000     AND VSIO-SEQUENCE-ERROR
025100         DISPLAY 'KEY SEQUENCE ERROR DURING LOAD'
025200         DISPLAY 'RECORD BYPASSED: (COURSE) '
025300                 CIR-STUDENT-ID '-' CIR-KEY-ID '-' CIR-COURSE-ID
025400         MOVE +0 TO VSIO-RETURN-CODE
025500         GO TO 159-EXIT.
025600*    END-IF.
025700
025800     PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT.
025900
026000 159-EXIT.
026100     EXIT.
026200
026300 500-DISPLAY-UNKNOWN-ERROR.
026400
026500     DISPLAY 'UNEXPECTED VSAMIO ERROR OCCURRED DURING '
026600             VSIO-COMMAND.
026700     EXHIBIT NAMED VSIO-RETURN-CODE.
026800     EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
026900                   VSIO-VSAM-FUNCTION-CODE,
027000                   VSIO-VSAM-FEEDBACK-CODE.
027100
027200 509-EXIT.
027300     EXIT.
./ ADD NAME=KSDSMULT,LIST=ALL                                           00440000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. KSDSMULT
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY ACCESSING FOUR VSAM  *
000900* DATASETS SIMULTANEOUSLY TO PRODUCE A REPORT.                  *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000     SELECT REPORT-FILE
002100         ASSIGN TO UR-1403-S-SYSPRINT.
002200
002300 DATA DIVISION.
002400 FILE SECTION.
002500 FD  REPORT-FILE
002600     LABEL RECORDS ARE OMITTED
002700     BLOCK CONTAINS 0 RECORDS
002800     REPORT IS STUDENT-REPORT.
002900
003000 WORKING-STORAGE SECTION.
003100
003200 01  REPORTING-FIELDS.
003300     02  WS-GENDER               PIC X(06).
003400
003500 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
003600
003700* ************************************************************** *
003800* COMMUNICATION TO VSAMIO FOR STUDENT MASTER DATASET             *
003900* ************************************************************** *
004000 01  STUDENT-MASTER-FILE         COPY VSAMIOFB.
004100 01  STUDENT-RECORD-AREA         PIC X(83).
004200 01  STUDENT-INFO-RECORD.
004300     02  SIR-STUDENT-ID          PIC X(07).
004400     02  SIR-KEY-ID              PIC 9(03).
004500     02  SIR-NAME                PIC X(22).
004600     02  SIR-ADDRESS             PIC X(25).
004700     02  SIR-CITY                PIC X(15).
004800     02  SIR-STATE               PIC X(02).
004900     02  SIR-ZIPCODE             PIC 9(05).
005000     02  SIR-GENDER              PIC X(01).
005100     02  SIR-MAJOR               PIC X(03).
005200 01  COURSE-INFO-RECORD.
005300     02  CIR-STUDENT-ID          PIC X(07).
005400     02  CIR-KEY-ID              PIC 9(03).
005500     02  CIR-COURSE-ID           PIC 9(06).
005600     02  FILLER                  REDEFINES CIR-COURSE-ID.
005700         03  CIR-DEPARTMENT      PIC 9(02).
005800         03  CIR-COURSE          PIC 9(03).
005900         03  CIR-SECTION         PIC 9(01).
006000     02  CIR-GRADE               PIC S9(3)V99 COMP-3.
006100
006200* ************************************************************** *
006300* COMMUNICATION TO VSAMIO FOR DEPARTMENT TABLE DATASET           *
006400* ************************************************************** *
006500 01  DEPARTMENT-TABLE-FILE       COPY VSAMIOFB.
006600 01  DEPARTMENT-TABLE-RECORD.
006700     02  DTR-NUMBER              PIC 9(02).
006800     02  DTR-NAME                PIC X(30).
006900
007000* ************************************************************** *
007100* COMMUNICATION TO VSAMIO FOR COURSE TABLE DATASET               *
007200* ************************************************************** *
007300 01  COURSE-TABLE-FILE           COPY VSAMIOFB.
007400 01  COURSE-TABLE-RECORD.
007500     02  CTR-DEPARTMENT          PIC 9(02).
007600     02  CTR-NUMBER              PIC 9(03).
007700     02  CTR-DESCRIPTION         PIC X(50).
007800     02  CTR-CREDIT-HOURS        PIC 9(01).
007900
008000* ************************************************************** *
008100* COMMUNICATION TO VSAMIO FOR SECTION TABLE DATASET              *
008200* ************************************************************** *
008300 01  SECTION-TABLE-FILE          COPY VSAMIOFB.
008400 01  SECTION-TABLE-RECORD.
008500     02  STR-DEPARTMENT          PIC 9(02).
008600     02  STR-COURSE              PIC 9(03).
008700     02  STR-NUMBER              PIC 9(01).
008800     02  STR-DESCRIPTION         PIC X(11).
008900
009000 REPORT SECTION.
009100 RD  STUDENT-REPORT
009200     CONTROLS IS SIR-STUDENT-ID, DTR-NUMBER
009300     PAGE LIMIT IS 60 LINES
009400     FIRST DETAIL 3
009500     LAST DETAIL 55.
009600
009700 01  HEADING-ON-PAGE             TYPE IS PAGE HEADING.
009800     02  LINE 1.
009900         03  COLUMN 01           PIC X(08)
010000                                 SOURCE IS CURRENT-DATE.
010100         03  COLUMN 28           PIC X(25)   VALUE IS
010200             'STUDENT ENROLLMENT REPORT'.
010300         03  COLUMN 72           PIC X(04)   VALUE 'PAGE'.
010400         03  COLUMN 77           PIC ZZZ9
010500                                 SOURCE IS PAGE-COUNTER.
010600
010700 01  STUDENT-SUBHEAD             TYPE IS CONTROL HEADING
010800                                 SIR-STUDENT-ID.
010900     02  LINE PLUS 2.
011000         03  COLUMN 01           PIC 9(07)
011100                                 SOURCE IS SIR-STUDENT-ID.
011200         03  COLUMN 10           PIC X(22)
011300                                 SOURCE IS SIR-NAME.
011400         03  COLUMN 53           PIC X(06)
011500                                 SOURCE IS WS-GENDER.
011600     02  LINE PLUS 1.
011700         03  COLUMN 10           PIC X(25)
011800                                 SOURCE IS SIR-ADDRESS.
011900     02  LINE PLUS 1.
012000         03  COLUMN 10           PIC X(15)
012100                                 SOURCE IS SIR-CITY.
012200         03  COLUMN 36           PIC X(02)
012300                                 SOURCE IS SIR-STATE.
012400         03  COLUMN 39           PIC 9(05)
012500                                 SOURCE IS SIR-ZIPCODE.
012600         03  COLUMN 53           PIC X(06) VALUE 'MAJOR:'.
012700         03  COLUMN 60           PIC X(03)
012800                                 SOURCE IS SIR-MAJOR.
012900 01  DEPARTMENT-SUBHEAD          TYPE IS CONTROL HEADING
013000                                 DTR-NUMBER.
013100     02  LINE PLUS 2.
013200         03  COLUMN 10           PIC X(30)
013300                                 SOURCE IS DTR-NAME.
013400     02  LINE PLUS 1.
013500
013600 01  COURSE-DETAIL               TYPE IS DETAIL.
013700     02  LINE PLUS 1.
013800         03  COLUMN 10           PIC 999
013900                                 SOURCE IS CIR-COURSE.
014000         03  COLUMN 13           PIC X VALUE '-'.
014100         03  COLUMN 14           PIC 9
014200                                 SOURCE IS CIR-SECTION.
014300         03  COLUMN 16           PIC X VALUE '('.
014400         03  COLUMN 17           PIC 9
014500                                 SOURCE IS CTR-CREDIT-HOURS.
014600         03  COLUMN 18           PIC X VALUE ')'.
014700         03  COLUMN 20           PIC X(46)
014800                                 SOURCE IS CTR-DESCRIPTION.
014900         03  COLUMN 67           PIC X(11)
015000                                 SOURCE IS STR-DESCRIPTION.
015100
015200 01  STUDENT-FOOTING             TYPE IS CONTROL FOOTING
015300                                 SIR-STUDENT-ID
015400                                 NEXT GROUP IS NEXT PAGE.
015500     02  LINE PLUS 2.
015600         03  COLUMN 10           PIC X(21) VALUE
015700             'TOTAL HOURS ENROLLED:'.
015800         03  COLUMN 32           PIC ZZ9
015900                                 SUM CTR-CREDIT-HOURS.
016000
016100 PROCEDURE DIVISION.
016200
016300 000-INITIATE.
016400
016500     MOVE 'STMASTR' TO VSIO-DDNAME OF STUDENT-MASTER-FILE.
016600     MOVE VSIO-KSDS TO VSIO-ORGANIZATION OF STUDENT-MASTER-FILE.
016700     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS OF STUDENT-MASTER-FILE.
016800     MOVE VSIO-INPUT TO VSIO-MODE OF STUDENT-MASTER-FILE.
016900     MOVE +83 TO VSIO-RECORD-LENGTH OF STUDENT-MASTER-FILE.
017000     MOVE +0 TO VSIO-KEY-POSITION OF STUDENT-MASTER-FILE.
017100     MOVE +10 TO VSIO-KEY-LENGTH OF STUDENT-MASTER-FILE.
017200     MOVE VSIO-OPEN TO VSIO-COMMAND.
017300     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
017400                         STUDENT-MASTER-FILE,
017500                         STUDENT-RECORD-AREA.
017600*    END-CALL.
017700     IF NOT VSIO-SUCCESS
017800         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
017900         STOP RUN.
018000*    END-IF.
018100
018200     MOVE 'DEPTTBL' TO VSIO-DDNAME OF DEPARTMENT-TABLE-FILE.
018300     MOVE VSIO-KSDS TO VSIO-ORGANIZATION OF DEPARTMENT-TABLE-FILE
018400     MOVE VSIO-DIRECT TO VSIO-ACCESS OF DEPARTMENT-TABLE-FILE.
018500     MOVE VSIO-INPUT TO VSIO-MODE OF DEPARTMENT-TABLE-FILE.
018600     MOVE +32 TO VSIO-RECORD-LENGTH OF DEPARTMENT-TABLE-FILE.
018700     MOVE +0 TO VSIO-KEY-POSITION OF DEPARTMENT-TABLE-FILE.
018800     MOVE +2 TO VSIO-KEY-LENGTH OF DEPARTMENT-TABLE-FILE.
018900     MOVE VSIO-OPEN TO VSIO-COMMAND.
019000     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
019100                         DEPARTMENT-TABLE-FILE,
019200                         DEPARTMENT-TABLE-RECORD.
019300*    END-CALL.
019400     IF NOT VSIO-SUCCESS
019500         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
019600         STOP RUN.
019700*    END-IF.
019800
019900     MOVE 'CRSETBL' TO VSIO-DDNAME OF COURSE-TABLE-FILE.
020000     MOVE VSIO-KSDS TO VSIO-ORGANIZATION OF COURSE-TABLE-FILE.
020100     MOVE VSIO-DIRECT TO VSIO-ACCESS OF COURSE-TABLE-FILE.
020200     MOVE VSIO-INPUT TO VSIO-MODE OF COURSE-TABLE-FILE.
020300     MOVE +56 TO VSIO-RECORD-LENGTH OF COURSE-TABLE-FILE.
020400     MOVE +0 TO VSIO-KEY-POSITION OF COURSE-TABLE-FILE.
020500     MOVE +5 TO VSIO-KEY-LENGTH OF COURSE-TABLE-FILE.
020600     MOVE VSIO-OPEN TO VSIO-COMMAND.
020700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
020800                         COURSE-TABLE-FILE,
020900                         COURSE-TABLE-RECORD.
021000*    END-CALL.
021100     IF NOT VSIO-SUCCESS
021200         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
021300         STOP RUN.
021400*    END-IF.
021500
021600     MOVE 'SECTTBL' TO VSIO-DDNAME OF SECTION-TABLE-FILE.
021700     MOVE VSIO-KSDS TO VSIO-ORGANIZATION OF SECTION-TABLE-FILE.
021800     MOVE VSIO-DIRECT TO VSIO-ACCESS OF SECTION-TABLE-FILE.
021900     MOVE VSIO-INPUT TO VSIO-MODE OF SECTION-TABLE-FILE.
022000     MOVE +17 TO VSIO-RECORD-LENGTH OF SECTION-TABLE-FILE.
022100     MOVE +0 TO VSIO-KEY-POSITION  OF SECTION-TABLE-FILE.
022200     MOVE +6 TO VSIO-KEY-LENGTH OF SECTION-TABLE-FILE.
022300     MOVE VSIO-OPEN TO VSIO-COMMAND.
022400     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
022500                         SECTION-TABLE-FILE,
022600                         SECTION-TABLE-RECORD.
022700*    END-CALL.
022800     IF NOT VSIO-SUCCESS
022900         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
023000         STOP RUN.
023100*    END-IF.
023200
023300     OPEN OUTPUT REPORT-FILE.
023400
023500     INITIATE STUDENT-REPORT.
023600
023700 010-PROCESS.
023800
023900     PERFORM 110-PROCESS-STUDENTS
024000        THRU 119-EXIT
024100       UNTIL NOT VSIO-SUCCESS.
024200*    END-PERFORM.
024300
024400 020-TERMINATE.
024500
024600     TERMINATE STUDENT-REPORT.
024700
024800     CLOSE REPORT-FILE.
024900
025000     MOVE VSIO-CLOSE TO VSIO-COMMAND.
025100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
025200                         STUDENT-MASTER-FILE,
025300                         STUDENT-RECORD-AREA.
025400*    END-CALL.
025500     IF NOT VSIO-SUCCESS
025600         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT.
025700*    END-IF.
025800
025900     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
026000                         DEPARTMENT-TABLE-FILE,
026100                         DEPARTMENT-TABLE-RECORD.
026200*    END-CALL.
026300     IF NOT VSIO-SUCCESS
026400         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT.
026500*    END-IF.
026600
026700     MOVE VSIO-CLOSE TO VSIO-COMMAND.
026800     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
026900                         COURSE-TABLE-FILE,
027000                         COURSE-TABLE-RECORD.
027100*    END-CALL.
027200     IF NOT VSIO-SUCCESS
027300         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT.
027400*    END-IF.
027500
027600     MOVE VSIO-CLOSE TO VSIO-COMMAND.
027700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
027800                         SECTION-TABLE-FILE,
027900                         SECTION-TABLE-RECORD.
028000*    END-CALL.
028100     IF NOT VSIO-SUCCESS
028200         PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT.
028300*    END-IF.
028400
028500     STOP RUN.
028600
028700 110-PROCESS-STUDENTS.
028800
028900     MOVE +83 TO VSIO-RECORD-LENGTH OF STUDENT-MASTER-FILE.
029000     MOVE VSIO-READ TO VSIO-COMMAND.
029100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
029200                         STUDENT-MASTER-FILE,
029300                         STUDENT-RECORD-AREA.
029400*    END-CALL.
029500     IF NOT VSIO-SUCCESS
029600         IF VSIO-END-OF-FILE
029700             GO TO 119-EXIT
029800         ELSE
029900             PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
030000             GO TO 119-EXIT.
030100*        END-IF
030200*    END-IF.
030300
030400     IF VSIO-RECORD-LENGTH OF STUDENT-MASTER-FILE EQUAL +83
030500         MOVE STUDENT-RECORD-AREA TO STUDENT-INFO-RECORD
030600         PERFORM 120-STUDENT-FIELDS THRU 129-EXIT
030700     ELSE
030800         MOVE STUDENT-RECORD-AREA TO COURSE-INFO-RECORD
030900         PERFORM 130-COURSE-LOOKUP THRU 139-EXIT
031000         GENERATE COURSE-DETAIL.
031100*    END-IF.
031200
031300 119-EXIT.
031400     EXIT.
031500
031600 120-STUDENT-FIELDS.
031700
031800     IF SIR-GENDER EQUAL 'M'
031900         MOVE 'MALE' TO WS-GENDER
032000     ELSE
032100         MOVE 'FEMALE' TO WS-GENDER.
032200*    END-IF.
032300
032400 129-EXIT.
032500     EXIT.
032600
032700 130-COURSE-LOOKUP.
032800
032900     MOVE CIR-DEPARTMENT TO DTR-NUMBER,
033000                            CTR-DEPARTMENT,
033100                            STR-DEPARTMENT.
033200     MOVE CIR-COURSE TO CTR-NUMBER,
033300                        STR-COURSE.
033400     MOVE CIR-SECTION TO STR-NUMBER.
033500
033600     MOVE VSIO-READ TO VSIO-COMMAND.
033700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
033800                         DEPARTMENT-TABLE-FILE,
033900                         DEPARTMENT-TABLE-RECORD.
034000*    END-CALL.
034100     IF NOT VSIO-SUCCESS
034200         IF VSIO-LOGIC-ERROR
034300         AND VSIO-RECORD-NOT-FOUND
034400             MOVE '*** NOT IN TABLE FILE ***' TO DTR-NAME
034500             MOVE +0 TO VSIO-RETURN-CODE
034600         ELSE
034700             PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
034800             GO TO 139-EXIT.
034900*        END-IF
035000*    END-IF.
035100
035200     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
035300                         COURSE-TABLE-FILE,
035400                         COURSE-TABLE-RECORD.
035500*    END-CALL.
035600     IF NOT VSIO-SUCCESS
035700         IF VSIO-LOGIC-ERROR
035800         AND VSIO-RECORD-NOT-FOUND
035900             MOVE '*** NOT IN TABLE FILE ***' TO CTR-DESCRIPTION
036000             MOVE ZERO TO CTR-CREDIT-HOURS
036100             MOVE +0 TO VSIO-RETURN-CODE
036200         ELSE
036300             PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
036400             GO TO 139-EXIT.
036500*        END-IF
036600*    END-IF.
036700
036800     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK,
036900                         SECTION-TABLE-FILE,
037000                         SECTION-TABLE-RECORD.
037100*    END-CALL.
037200     IF NOT VSIO-SUCCESS
037300         IF VSIO-LOGIC-ERROR
037400         AND VSIO-RECORD-NOT-FOUND
037500             MOVE '*NOT/TABLE*' TO STR-DESCRIPTION
037600             MOVE +0 TO VSIO-RETURN-CODE
037700         ELSE
037800             PERFORM 500-DISPLAY-UNKNOWN-ERROR THRU 509-EXIT
037900             GO TO 139-EXIT.
038000*        END-IF
038100*    END-IF.
038200
038300 139-EXIT.
038400     EXIT.
038500
038600 500-DISPLAY-UNKNOWN-ERROR.
038700
038800     DISPLAY 'UNEXPECTED VSAMIO ERROR OCCURRED DURING '
038900             VSIO-COMMAND.
039000     EXHIBIT NAMED VSIO-RETURN-CODE.
039100     EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
039200                   VSIO-VSAM-FUNCTION-CODE,
039300                   VSIO-VSAM-FEEDBACK-CODE.
039400
039500 509-EXIT.
039600     EXIT.
./ ADD NAME=KSDSREAD,LIST=ALL                                           00460000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. KSDSREAD.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY READING RECORDS FROM *
000900* A KSDS CLUSTER AND DISPLAYING THEIR CONTENTS ON SYSOUT.       *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000 DATA DIVISION.
002100 FILE SECTION.
002200
002300 WORKING-STORAGE SECTION.
002400 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
002500     88  END-OF-FILE                         VALUE 'Y'.
002600
002700 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
002800 01  KSDSF01                     COPY VSAMIOFB.
002900 01  KSDS-RECORD.
003000     02  KSDS-KEY                PIC X(10).
003100     02  FILLER                  PIC X(70).
003200
003300 PROCEDURE DIVISION.
003400
003500     DISPLAY 'KSDSREAD: READ KSDS SEQUENTIALLY'.
003600     DISPLAY '--------------------------------'.
003700     DISPLAY ' '.
003800
003900 000-INITIATE.
004000
004100     MOVE 'KSDSF01' TO VSIO-DDNAME.
004200     MOVE VSIO-KSDS TO VSIO-ORGANIZATION.
004300     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
004400     MOVE VSIO-INPUT TO VSIO-MODE.
004500     MOVE +80 TO VSIO-RECORD-LENGTH.
004600     MOVE +0 TO VSIO-KEY-POSITION.
004700     MOVE +10 TO VSIO-KEY-LENGTH.
004800     MOVE VSIO-OPEN TO VSIO-COMMAND.
004900     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
005000                         KSDS-RECORD.
005100*    END-CALL.
005200     IF NOT VSIO-SUCCESS
005300         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
005400                 VSIO-COMMAND
005500         EXHIBIT NAMED VSIO-RETURN-CODE,
005600         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
005700                       VSIO-VSAM-FUNCTION-CODE,
005800                       VSIO-VSAM-FEEDBACK-CODE
005900         STOP RUN.
006000*    END-IF.
006100
006200 010-PROCESS.
006300
006400     PERFORM 110-PROCESS-DATA
006500        THRU 119-EXIT
006600       UNTIL END-OF-FILE.
006700*    END-PERFORM.
006800
006900 020-TERMINATE.
007000
007100     MOVE VSIO-CLOSE TO VSIO-COMMAND.
007200     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
007300                         KSDS-RECORD.
007400*    END-CALL.
007500     IF NOT VSIO-SUCCESS
007600         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
007700                 VSIO-COMMAND
007800         EXHIBIT NAMED VSIO-RETURN-CODE,
007900         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
008000                       VSIO-VSAM-FUNCTION-CODE,
008100                       VSIO-VSAM-FEEDBACK-CODE.
008200*    END-IF.
008300
008400     STOP RUN.
008500
008600 110-PROCESS-DATA.
008700
008800     MOVE VSIO-READ TO VSIO-COMMAND.
008900     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
009000                         KSDS-RECORD.
009100*    END-CALL.
009200
009300     IF NOT VSIO-SUCCESS
009400         IF VSIO-END-OF-FILE
009500             MOVE 'Y' TO END-OF-FILE-SWITCH
009600         ELSE
009700             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009800                     VSIO-COMMAND
009900             EXHIBIT NAMED VSIO-RETURN-CODE,
010000             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
010100                           VSIO-VSAM-FUNCTION-CODE,
010200                           VSIO-VSAM-FEEDBACK-CODE
010300             MOVE 'Y' TO END-OF-FILE-SWITCH.
010400*        END-IF
010500*    END-IF.
010600
010700     IF NOT END-OF-FILE
010800         DISPLAY 'KEY: ' KSDS-KEY
010900                 '  RECORD: ' KSDS-RECORD.
011000*    END-IF.
011100
011200 119-EXIT.
011300     EXIT.
011400
011500
./ ADD NAME=KSDSRAND,LIST=ALL                                           00480000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. KSDSRAND.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY ACCESSING A KSDS     *
000900* CLUSTER RANDOMLY AND ADDING/UPDATING/DELETING RECORDS.        *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000     SELECT RECORD-IMAGES
002100         ASSIGN TO UR-2540R-S-SYSIN.
002200
002300 DATA DIVISION.
002400 FILE SECTION.
002500
002600 FD  RECORD-IMAGES
002700     LABEL RECORDS ARE OMITTED
002800     BLOCK CONTAINS 0 RECORDS
002900     DATA RECORD IS RECORD-IMAGE.
003000
003100 01  RECORD-IMAGE.
003200     02  RI-ACTION               PIC X(01).
003300         88  ACTION-IS-ADD                   VALUE 'A'.
003400         88  ACTION-IS-CHANGE                VALUE 'C'.
003500         88  ACTION-IS-DELETE                VALUE 'D'.
003600         88  ACTION-IS-VALID                 VALUE 'A', 'C', 'D'.
003700     02  FILLER                  PIC X(01).
003800     02  RI-IMAGE                PIC X(78).
003900
004000 WORKING-STORAGE SECTION.
004100 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
004200     88  END-OF-FILE                         VALUE 'Y'.
004300
004400 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
004500 77  COUNTER-EDIT                PIC ZZ,ZZZ,ZZ9.
004600
004700 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
004800 01  KSDSF01                     COPY VSAMIOFB.
004900 01  KSDS-RECORD.
005000     02  KR-KEY                  PIC X(10).
005100     02  FILLER                  PIC X(70).
005200
005300 PROCEDURE DIVISION.
005400
005500 000-INITIATE.
005600
005700     DISPLAY 'KSDSRAND: READ/REWRITE KSDS DIRECT'.
005800     DISPLAY '----------------------------------'.
005900     DISPLAY ' '.
006000
006100     OPEN INPUT RECORD-IMAGES.
006200
006300     MOVE 'KSDSF01' TO VSIO-DDNAME.
006400     MOVE VSIO-KSDS TO VSIO-ORGANIZATION.
006500     MOVE VSIO-DIRECT TO VSIO-ACCESS.
006600     MOVE VSIO-INPUT-OUTPUT TO VSIO-MODE.
006700     MOVE +80 TO VSIO-RECORD-LENGTH.
006800     MOVE +0 TO VSIO-KEY-POSITION.
006900     MOVE +10 TO VSIO-KEY-LENGTH.
007000     MOVE VSIO-OPEN TO VSIO-COMMAND.
007100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
007200                         KSDS-RECORD.
007300*    END-CALL.
007400     IF NOT VSIO-SUCCESS
007500         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
007600                 VSIO-COMMAND
007700         EXHIBIT NAMED VSIO-RETURN-CODE,
007800         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
007900                       VSIO-VSAM-FUNCTION-CODE,
008000                       VSIO-VSAM-FEEDBACK-CODE
008100         STOP RUN.
008200*    END-IF.
008300
008400 010-PROCESS.
008500
008600     PERFORM 110-PROCESS-UPDATES
008700        THRU 119-EXIT
008800       UNTIL END-OF-FILE.
008900*    END-PERFORM.
009000
009100 020-TERMINATE.
009200
009300     MOVE VSIO-CLOSE TO VSIO-COMMAND.
009400     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
009500                         KSDS-RECORD.
009600*    END-CALL.
009700     IF NOT VSIO-SUCCESS
009800         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009900                 VSIO-COMMAND
010000         EXHIBIT NAMED VSIO-RETURN-CODE,
010100         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
010200                       VSIO-VSAM-FUNCTION-CODE,
010300                       VSIO-VSAM-FEEDBACK-CODE.
010400*    END-IF.
010500
010600     STOP RUN.
010700
010800 110-PROCESS-UPDATES.
010900
011000     READ RECORD-IMAGES
011100         AT END
011200             MOVE 'Y' TO END-OF-FILE-SWITCH.
011300*    END-READ.
011400
011500     IF END-OF-FILE
011600         GO TO 119-EXIT.
011700*    END-IF.
011800
011900     ADD +1 TO RECORD-COUNTER.
012000
012100     IF NOT ACTION-IS-VALID
012200         MOVE RECORD-COUNTER TO COUNTER-EDIT
012300         DISPLAY COUNTER-EDIT ': ' RECORD-IMAGE
012400         DISPLAY '            *** ACTION INVALID: IGNORED'
012500         DISPLAY ' '
012600         GO TO 119-EXIT.
012700*    END-IF.
012800
012900     IF ACTION-IS-ADD
013000         PERFORM 120-ADD-PROCESS THRU 129-EXIT.
013100*    END-IF.
013200
013300     IF ACTION-IS-CHANGE
013400         PERFORM 130-CHANGE-PROCESS THRU 139-EXIT.
013500*    END-IF.
013600
013700     IF ACTION-IS-DELETE
013800         PERFORM 140-DELETE-PROCESS THRU 149-EXIT.
013900*    END-IF.
014000
014100 119-EXIT.
014200     EXIT.
014300
014400 120-ADD-PROCESS.
014500
014600     MOVE RI-IMAGE TO KSDS-RECORD.
014700     MOVE VSIO-WRITE TO VSIO-COMMAND.
014800     DISPLAY  'ADDING: ' KSDS-RECORD.
014900     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
015000                         KSDS-RECORD.
015100*    END-CALL.
015200
015300     IF VSIO-SUCCESS
015400         DISPLAY '               RECORD ADDED'
015500     ELSE
015600         IF VSIO-LOGIC-ERROR
015700        AND VSIO-DUPLICATE-RECORD
015800             MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
015900             DISPLAY '            *** DUPLICATE RECORD ON FILE'
016000         ELSE
016100             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
016200                     VSIO-COMMAND
016300             EXHIBIT NAMED VSIO-RETURN-CODE,
016400             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
016500                           VSIO-VSAM-FUNCTION-CODE,
016600                           VSIO-VSAM-FEEDBACK-CODE.
016700*        END-IF
016800*    END-IF.
016900
017000     DISPLAY ' '.
017100
017200 129-EXIT.
017300     EXIT.
017400
017500 130-CHANGE-PROCESS.
017600
017700     PERFORM 150-READ-RECORD THRU 159-EXIT.
017800
017900     IF VSIO-LOGIC-ERROR
018000     AND VSIO-RECORD-NOT-FOUND
018100         DISPLAY '          *** RECORD NOT FOUND'
018200         DISPLAY ' '
018300         GO TO 139-EXIT.
018400*    END-IF.
018500
018600     DISPLAY KSDS-RECORD.
018700     DISPLAY '               RECORD BEFORE CHANGE'.
018800
018900     MOVE RI-IMAGE TO KSDS-RECORD.
019000     MOVE VSIO-REWRITE TO VSIO-COMMAND.
019100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
019200                         KSDS-RECORD.
019300*    END-CALL.
019400
019500     IF VSIO-SUCCESS
019600         DISPLAY KSDS-RECORD
019700         DISPLAY '               RECORD AFTER CHANGE'
019800     ELSE
019900         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
020000                 VSIO-COMMAND
020100         EXHIBIT NAMED VSIO-RETURN-CODE,
020200         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
020300                       VSIO-VSAM-FUNCTION-CODE,
020400                       VSIO-VSAM-FEEDBACK-CODE
020500*    END-IF
020600
020700     DISPLAY ' '.
020800
020900 139-EXIT.
021000     EXIT.
021100
021200 140-DELETE-PROCESS.
021300
021400     PERFORM 150-READ-RECORD THRU 159-EXIT.
021500
021600     IF VSIO-LOGIC-ERROR
021700     AND VSIO-RECORD-NOT-FOUND
021800         DISPLAY '          *** RECORD NOT FOUND'
021900         DISPLAY ' '
022000         GO TO 149-EXIT.
022100*    END-IF.
022200
022300     DISPLAY KSDS-RECORD.
022400     DISPLAY '               RECORD BEFORE DELETE'.
022500
022600     MOVE VSIO-DELETE TO VSIO-COMMAND.
022700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
022800                         KSDS-RECORD.
022900*    END-CALL.
023000
023100     IF VSIO-SUCCESS
023200         DISPLAY '             DELETED '
023300     ELSE
023400         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
023500                 VSIO-COMMAND
023600         EXHIBIT NAMED VSIO-RETURN-CODE,
023700         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
023800                       VSIO-VSAM-FUNCTION-CODE,
023900                       VSIO-VSAM-FEEDBACK-CODE.
024000*    END-IF.
024100
024200     DISPLAY ' '.
024300
024400 149-EXIT.
024500     EXIT.
024600
024700 150-READ-RECORD.
024800
024900     MOVE RI-IMAGE TO KSDS-RECORD.
025000     DISPLAY 'ATTEMPTING TO READ: ' KR-KEY.
025100     MOVE VSIO-READ TO VSIO-COMMAND.
025200     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
025300                         KSDS-RECORD.
025400*    END-CALL.
025500
025600     IF NOT VSIO-SUCCESS
025700         IF VSIO-LOGIC-ERROR
025800        AND VSIO-RECORD-NOT-FOUND
025900            NEXT SENTENCE
026000         ELSE
026100             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
026200                     VSIO-COMMAND
026300             EXHIBIT NAMED VSIO-RETURN-CODE,
026400             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
026500                           VSIO-VSAM-FUNCTION-CODE,
026600                           VSIO-VSAM-FEEDBACK-CODE.
026700*        END-IF
026800*    END-IF.
026900
027000 159-EXIT.
027100     EXIT.
027200
./ ADD NAME=KSDSSSEQ,LIST=ALL                                           00500000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. KSDSSSEQ.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY USING START AND READ *
000900* NEXT COMMANDS ON A KSDS CLUSTER (SKIP-SEQUENTIAL PROCESSING). *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000 DATA DIVISION.
002100 FILE SECTION.
002200
002300 WORKING-STORAGE SECTION.
002400 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
002500     88  END-OF-FILE                         VALUE 'Y'.
002600
002700 77  RECORD-COUNTER              PIC S9(8).
002800
002900 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
003000 01  KSDSF01                     COPY VSAMIOFB.
003100 01  KSDS-RECORD.
003200     02  KR-KEY                  PIC X(10).
003300     02  FILLER                  PIC X(70).
003400
003500 PROCEDURE DIVISION.
003600
003700 000-INITIATE.
003800
003900     DISPLAY 'KSDSSSEQ: READ KSDS SEQUENTIALLY (W/START)'.
004000     DISPLAY '------------------------------------------'.
004100     DISPLAY ' '.
004200
004300     MOVE 'KSDSF01' TO VSIO-DDNAME.
004400     MOVE VSIO-KSDS TO VSIO-ORGANIZATION.
004500     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
004600     MOVE VSIO-INPUT TO VSIO-MODE.
004700     MOVE +80 TO VSIO-RECORD-LENGTH.
004800     MOVE +0 TO VSIO-KEY-POSITION.
004900     MOVE +10 TO VSIO-KEY-LENGTH.
005000     MOVE VSIO-OPEN TO VSIO-COMMAND.
005100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
005200                         KSDS-RECORD.
005300*    END-CALL.
005400     IF NOT VSIO-SUCCESS
005500         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
005600                 VSIO-COMMAND
005700         EXHIBIT NAMED VSIO-RETURN-CODE,
005800         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
005900                       VSIO-VSAM-FUNCTION-CODE,
006000                       VSIO-VSAM-FEEDBACK-CODE
006100         STOP RUN.
006200*    END-IF.
006300
006400 010-PROCESS.
006500
006600     PERFORM 110-PROCESS-DATA
006700        THRU 119-EXIT
006800       UNTIL END-OF-FILE.
006900*    END-PERFORM.
007000
007100 020-TERMINATE.
007200
007300     MOVE VSIO-CLOSE TO VSIO-COMMAND.
007400     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
007500                         KSDS-RECORD.
007600*    END-CALL.
007700     IF NOT VSIO-SUCCESS
007800         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
007900                 VSIO-COMMAND
008000         EXHIBIT NAMED VSIO-RETURN-CODE,
008100         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
008200                       VSIO-VSAM-FUNCTION-CODE,
008300                       VSIO-VSAM-FEEDBACK-CODE.
008400*    END-IF.
008500
008600     STOP RUN.
008700
008800 110-PROCESS-DATA.
008900
009000     MOVE '1033846021' TO KR-KEY.
009100     DISPLAY 'START KEY EQUAL TO ' KR-KEY.
009200     MOVE VSIO-START-KEY-EQUAL TO VSIO-COMMAND.
009300     PERFORM 120-START-AND-READ THRU 129-EXIT.
009400
009500     MOVE '2534789096' TO KR-KEY.
009600     DISPLAY 'START KEY EQUAL TO ' KR-KEY.
009700     MOVE VSIO-START-KEY-EQUAL TO VSIO-COMMAND.
009800     PERFORM 120-START-AND-READ THRU 129-EXIT.
009900
010000     MOVE '3284189067' TO KR-KEY.
010100     DISPLAY 'START KEY GREATER THAN OR EQUAL TO ' KR-KEY.
010200     MOVE VSIO-START-KEY-NOTLESS TO VSIO-COMMAND.
010300     PERFORM 120-START-AND-READ THRU 129-EXIT.
010400
010500     MOVE '3860000000' TO KR-KEY.
010600     DISPLAY 'START KEY GREATER THAN OR EQUAL TO ' KR-KEY.
010700     MOVE VSIO-START-KEY-NOTLESS TO VSIO-COMMAND.
010800     PERFORM 120-START-AND-READ THRU 129-EXIT.
010900
011000     MOVE 'Y' TO END-OF-FILE-SWITCH.
011100
011200 119-EXIT.
011300     EXIT.
011400
011500 120-START-AND-READ.
011600
011700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
011800                         KSDS-RECORD.
011900*    END-CALL.
012000
012100     IF NOT VSIO-SUCCESS
012200         IF VSIO-RECORD-NOT-FOUND
012300             DISPLAY '*** RECORD NOT FOUND ***'
012400         ELSE
012500             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
012600                     VSIO-COMMAND
012700             EXHIBIT NAMED VSIO-RETURN-CODE,
012800             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
012900                           VSIO-VSAM-FUNCTION-CODE,
013000                           VSIO-VSAM-FEEDBACK-CODE.
013100*        END-IF
013200*    END-IF.
013300
013400     IF NOT VSIO-SUCCESS
013500         GO TO 129-EXIT.
013600*    END-IF.
013700
013800     MOVE +0 TO RECORD-COUNTER.
013900     PERFORM 130-READ-AND-DISPLAY THRU 139-EXIT
014000       UNTIL END-OF-FILE
014100          OR RECORD-COUNTER > +4.
014200*    END-PERFORM.
014300     MOVE 'N' TO END-OF-FILE-SWITCH.
014400
014500 129-EXIT.
014600     EXIT.
014700
014800 130-READ-AND-DISPLAY.
014900
015000     MOVE VSIO-READ TO VSIO-COMMAND.
015100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
015200                         KSDS-RECORD.
015300*    END-CALL.
015400
015500     IF VSIO-SUCCESS
015600         DISPLAY 'KEY: ' KR-KEY '  RECORD: ' KSDS-RECORD
015700     ELSE
015800         IF VSIO-END-OF-FILE
015900             MOVE 'Y' TO END-OF-FILE-SWITCH
016000         ELSE
016100             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
016200                     VSIO-COMMAND
016300             EXHIBIT NAMED VSIO-RETURN-CODE,
016400             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
016500                           VSIO-VSAM-FUNCTION-CODE,
016600                           VSIO-VSAM-FEEDBACK-CODE.
016700*        END-IF
016800*    END-IF.
016900
017000     ADD +1 TO RECORD-COUNTER.
017100
017200 139-EXIT.
017300     EXIT.
017400
017500
./ ADD NAME=KSDSUPDT,LIST=ALL                                           00520000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. KSDSUPDT.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY READING RECORDS FROM *
000900* A KSDS CLUSTER SEQUENTAILLY AND UPDATING SELECTED RECORDS.    *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000 DATA DIVISION.
002100 FILE SECTION.
002200
002300 WORKING-STORAGE SECTION.
002400 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
002500     88  END-OF-FILE                         VALUE 'Y'.
002600
002700 77  COUNTER-EDIT                PIC ZZ,ZZZ,ZZ9.
002800
002900 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
003000 01  KSDSF01                     COPY VSAMIOFB.
003100 01  KSDS-RECORD.
003200     02  KR-KEY                  PIC X(10).
003300     02  FILLER                  PIC X(49).
003400     02  KR-CITY                 PIC X(15).
003500     02  FILLER                  PIC X(06).
003600
003700 PROCEDURE DIVISION.
003800
003900 000-INITIATE.
004000
004100     DISPLAY 'KSDSUPDT: READ/REWRITE KSDS SEQUENTIALLY'.
004200     DISPLAY '----------------------------------------'.
004300     DISPLAY ' '.
004400
004500     MOVE 'KSDSF01' TO VSIO-DDNAME.
004600     MOVE VSIO-KSDS TO VSIO-ORGANIZATION.
004700     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
004800     MOVE VSIO-INPUT-OUTPUT TO VSIO-MODE.
004900     MOVE +80 TO VSIO-RECORD-LENGTH.
005000     MOVE +0 TO VSIO-KEY-POSITION.
005100     MOVE +10 TO VSIO-KEY-LENGTH.
005200     MOVE VSIO-OPEN TO VSIO-COMMAND.
005300     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
005400                         KSDS-RECORD.
005500*    END-CALL.
005600     IF NOT VSIO-SUCCESS
005700         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
005800                 VSIO-COMMAND
005900         EXHIBIT NAMED VSIO-RETURN-CODE,
006000         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
006100                       VSIO-VSAM-FUNCTION-CODE,
006200                       VSIO-VSAM-FEEDBACK-CODE
006300         STOP RUN.
006400*    END-IF.
006500
006600 010-PROCESS.
006700
006800     PERFORM 110-PROCESS-DATA
006900        THRU 119-EXIT
007000       UNTIL END-OF-FILE.
007100*    END-PERFORM.
007200
007300 020-TERMINATE.
007400
007500     MOVE VSIO-CLOSE TO VSIO-COMMAND.
007600     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
007700                         KSDS-RECORD.
007800*    END-CALL.
007900     IF NOT VSIO-SUCCESS
008000         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
008100                 VSIO-COMMAND
008200         EXHIBIT NAMED VSIO-RETURN-CODE,
008300         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
008400                       VSIO-VSAM-FUNCTION-CODE,
008500                       VSIO-VSAM-FEEDBACK-CODE.
008600*    END-IF.
008700
008800     STOP RUN.
008900
009000 110-PROCESS-DATA.
009100
009200     MOVE VSIO-READ TO VSIO-COMMAND.
009300     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
009400                         KSDS-RECORD.
009500*    END-CALL.
009600
009700     IF NOT VSIO-SUCCESS
009800         IF VSIO-END-OF-FILE
009900             MOVE 'Y' TO END-OF-FILE-SWITCH
010000         ELSE
010100             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
010200                     VSIO-COMMAND
010300             EXHIBIT NAMED VSIO-RETURN-CODE,
010400             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
010500                           VSIO-VSAM-FUNCTION-CODE,
010600                           VSIO-VSAM-FEEDBACK-CODE
010700             MOVE 'Y' TO END-OF-FILE-SWITCH.
010800*        END-IF
010900*    END-IF.
011000
011100     IF NOT END-OF-FILE
011200         PERFORM 120-UPDATE-PROCESS
011300            THRU 129-EXIT.
011400*    END-IF.
011500
011600 119-EXIT.
011700     EXIT.
011800
011900 120-UPDATE-PROCESS.
012000
012100     IF KR-CITY EQUAL 'SAN ANTONIO'
012200         DISPLAY KSDS-RECORD
012300         DISPLAY '           BEFORE UPDATE'
012400         MOVE 'AUSTIN' TO KR-CITY
012500         MOVE VSIO-REWRITE TO VSIO-COMMAND
012600         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
012700                         KSDS-RECORD
012800*        END-CALL
012900         IF VSIO-SUCCESS
013000             DISPLAY KSDS-RECORD
013100             DISPLAY '           AFTER UPDATE'
013200         ELSE
013300             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
013400                     VSIO-COMMAND
013500             EXHIBIT NAMED VSIO-RETURN-CODE,
013600             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
013700                           VSIO-VSAM-FUNCTION-CODE,
013800                           VSIO-VSAM-FEEDBACK-CODE.
013900*        END-IF
014000*    END-IF.
014100
014200     IF KR-CITY EQUAL 'DETROIT'
014300         DISPLAY KSDS-RECORD
014400         DISPLAY '          BEFORE DELETE'
014500         MOVE VSIO-DELETE TO VSIO-COMMAND
014600         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, KSDSF01,
014700                         KSDS-RECORD
014800*        END-CALL
014900         IF VSIO-SUCCESS
015000             DISPLAY '   DELETED  '
015100         ELSE
015200             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
015300                     VSIO-COMMAND
015400             EXHIBIT NAMED VSIO-RETURN-CODE,
015500             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
015600                           VSIO-VSAM-FUNCTION-CODE,
015700                           VSIO-VSAM-FEEDBACK-CODE.
015800*        END-IF
015900*    END-IF.
016000
016100 129-EXIT.
016200     EXIT.
016300
./ ADD NAME=RRDSLODR,LIST=ALL                                           00540000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. RRDSLODR.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY LOADING AN RRDS      *
000900* CLUSTER DIRECTLY WITH RECORDS FROM A SEQUENTIAL DATASET. THE  *
001000* RRN IS TAKEN FROM THE SEQUENCE PART OF THE RECORD AND THERE   *
001100* ARE GAPS IN THE TEST DATA.                                    *
001200* ************************************************************* *
001300
001400 ENVIRONMENT DIVISION.
001500 CONFIGURATION SECTION.
001600 SOURCE-COMPUTER. IBM-370.
001700 OBJECT-COMPUTER. IBM-370.
001800
001900 INPUT-OUTPUT SECTION.
002000 FILE-CONTROL.
002100
002200     SELECT RECORD-IMAGES
002300         ASSIGN TO DA-2314-S-IMAGES.
002400
002500 DATA DIVISION.
002600 FILE SECTION.
002700 FD  RECORD-IMAGES
002800     LABEL RECORDS ARE STANDARD
002900     BLOCK CONTAINS 10 RECORDS
003000     DATA RECORD IS RECORD-IMAGE.
003100
003200 01  RECORD-IMAGE                PIC X(80).
003300
003400 WORKING-STORAGE SECTION.
003500 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
003600     88  END-OF-FILE                         VALUE 'Y'.
003700
003800 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
003900 77  COUNTER-EDIT                PIC  ZZ,ZZZ,ZZ9.
004000
004100 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
004200 01  RRDSF01                     COPY VSAMIOFB.
004300 01  RRDS-RECORD.
004400     02  FILLER                  PIC X(07).
004500     02  RRDS-RRN                PIC 9(03).
004600     02  FILLER                  PIC X(70).
004700
004800 PROCEDURE DIVISION.
004900
005000 000-INITIATE.
005100
005200     DISPLAY 'RRDSLODR: WRITE RRDS DIRECT (VACANT SLOTS)'.
005300     DISPLAY '------------------------------------------'.
005400     DISPLAY ' '.
005500
005600     OPEN INPUT RECORD-IMAGES.
005700
005800     MOVE 'RRDSF01' TO VSIO-DDNAME.
005900     MOVE VSIO-RRDS TO VSIO-ORGANIZATION.
006000     MOVE VSIO-DIRECT TO VSIO-ACCESS.
006100     MOVE VSIO-OUTPUT TO VSIO-MODE.
006200     MOVE +80 TO VSIO-RECORD-LENGTH.
006300     MOVE +0 TO VSIO-KEY-LENGTH, VSIO-KEY-POSITION.
006400     MOVE VSIO-OPEN TO VSIO-COMMAND.
006500     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
006600                         RRDS-RECORD.
006700*    END-CALL.
006800     IF NOT VSIO-SUCCESS
006900         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
007000                 VSIO-COMMAND
007100         EXHIBIT NAMED VSIO-RETURN-CODE,
007200         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
007300                       VSIO-VSAM-FUNCTION-CODE,
007400                       VSIO-VSAM-FEEDBACK-CODE
007500         STOP RUN.
007600*    END-IF.
007700
007800 010-PROCESS.
007900
008000     PERFORM 110-PROCESS-DATA
008100        THRU 119-EXIT
008200       UNTIL END-OF-FILE
008300          OR (NOT VSIO-SUCCESS).
008400*    END-PERFORM.
008500
008600 020-TERMINATE.
008700
008800     CLOSE RECORD-IMAGES.
008900
009000     MOVE VSIO-CLOSE TO VSIO-COMMAND.
009100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
009200                         RRDS-RECORD.
009300*    END-CALL.
009400     IF NOT VSIO-SUCCESS
009500         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009600                 VSIO-COMMAND
009700         EXHIBIT NAMED VSIO-RETURN-CODE,
009800         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
009900                       VSIO-VSAM-FUNCTION-CODE,
010000                       VSIO-VSAM-FEEDBACK-CODE.
010100*    END-IF.
010200
010300     MOVE RECORD-COUNTER TO COUNTER-EDIT.
010400     DISPLAY COUNTER-EDIT ' RECORDS WERE LOADED SUCCESSFULLY'.
010500
010600     STOP RUN.
010700
010800 110-PROCESS-DATA.
010900     READ RECORD-IMAGES
011000         AT END
011100             MOVE 'Y' TO END-OF-FILE-SWITCH.
011200*    END-READ.
011300
011400     IF NOT END-OF-FILE
011500
011600         MOVE RECORD-IMAGE TO RRDS-RECORD
011700         MOVE RRDS-RRN TO VSIO-RELATIVE-RECORD
011800
011900         MOVE VSIO-WRITE TO VSIO-COMMAND
012000         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
012100                         RRDS-RECORD
012200*        END-CALL
012300
012400         IF VSIO-SUCCESS
012500             ADD +1 TO RECORD-COUNTER
012600         ELSE
012700             IF VSIO-LOGIC-ERROR
012800            AND VSIO-NO-MORE-SPACE
012900                DISPLAY 'INSUFFICIENT SPACE DEFINED IN CLUSTER'
013000                        'TO CONTAIN ALL RECORDS - LOADING '
013100                        'TERMINATED'
013200             ELSE
013300                 DISPLAY 'VSAMIO ERROR OCCURRED DURING '
013400                         VSIO-COMMAND
013500                 EXHIBIT NAMED VSIO-RETURN-CODE,
013600                 EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
013700                               VSIO-VSAM-FUNCTION-CODE,
013800                               VSIO-VSAM-FEEDBACK-CODE.
013900*            END-IF
014000*        END-IF
014100*    END-IF.
014200
014300 119-EXIT.
014400     EXIT.
014500
014600
./ ADD NAME=RRDSLODS,LIST=ALL                                           00560000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. RRDSLODS.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY LOADING AN RRDS      *
000900* CLUSTER SEQUENTIALLY WITH RECORDS FROM A SEQUENTIAL DATASET.  *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000     SELECT RECORD-IMAGES
002100         ASSIGN TO DA-2314-S-IMAGES.
002200
002300 DATA DIVISION.
002400 FILE SECTION.
002500 FD  RECORD-IMAGES
002600     LABEL RECORDS ARE STANDARD
002700     BLOCK CONTAINS 10 RECORDS
002800     DATA RECORD IS RECORD-IMAGE.
002900
003000 01  RECORD-IMAGE                PIC X(80).
003100
003200 WORKING-STORAGE SECTION.
003300 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
003400     88  END-OF-FILE                         VALUE 'Y'.
003500
003600 77  RELATIVE-RECORD-NUMBER      PIC S9(8)   VALUE +0.
003700 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
003800 77  COUNTER-EDIT                PIC  ZZ,ZZZ,ZZ9.
003900
004000 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
004100 01  RRDSF01                     COPY VSAMIOFB.
004200 01  RRDS-RECORD                 PIC X(80).
004300
004400 PROCEDURE DIVISION.
004500
004600 000-INITIATE.
004700
004800     DISPLAY 'RRDSLODS: WRITE RRDS SEQUENTIALLY'.
004900     DISPLAY '---------------------------------'.
005000     DISPLAY ' '.
005100
005200     OPEN INPUT RECORD-IMAGES.
005300
005400     MOVE 'RRDSF01' TO VSIO-DDNAME.
005500     MOVE VSIO-RRDS TO VSIO-ORGANIZATION.
005600     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
005700     MOVE VSIO-OUTPUT TO VSIO-MODE.
005800     MOVE +80 TO VSIO-RECORD-LENGTH.
005900     MOVE +0 TO VSIO-KEY-LENGTH, VSIO-KEY-POSITION.
006000     MOVE VSIO-OPEN TO VSIO-COMMAND.
006100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
006200                         RRDS-RECORD.
006300*    END-CALL.
006400     IF NOT VSIO-SUCCESS
006500         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
006600                 VSIO-COMMAND
006700         EXHIBIT NAMED VSIO-RETURN-CODE,
006800         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
006900                       VSIO-VSAM-FUNCTION-CODE,
007000                       VSIO-VSAM-FEEDBACK-CODE
007100         STOP RUN.
007200*    END-IF.
007300
007400 010-PROCESS.
007500
007600     PERFORM 110-PROCESS-DATA
007700        THRU 119-EXIT
007800       UNTIL END-OF-FILE
007900          OR (NOT VSIO-SUCCESS).
008000*    END-PERFORM.
008100
008200 020-TERMINATE.
008300
008400     CLOSE RECORD-IMAGES.
008500
008600     MOVE VSIO-CLOSE TO VSIO-COMMAND.
008700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
008800                         RRDS-RECORD.
008900*    END-CALL.
009000     IF NOT VSIO-SUCCESS
009100         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009200                 VSIO-COMMAND
009300         EXHIBIT NAMED VSIO-RETURN-CODE,
009400         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
009500                       VSIO-VSAM-FUNCTION-CODE,
009600                       VSIO-VSAM-FEEDBACK-CODE.
009700*    END-IF.
009800
009900     MOVE RECORD-COUNTER TO COUNTER-EDIT.
010000     DISPLAY COUNTER-EDIT ' RECORDS WERE LOADED SUCCESSFULLY'.
010100
010200     STOP RUN.
010300
010400 110-PROCESS-DATA.
010500     READ RECORD-IMAGES
010600         AT END
010700             MOVE 'Y' TO END-OF-FILE-SWITCH.
010800*    END-READ.
010900
011000     IF NOT END-OF-FILE
011100
011200         ADD +1 TO RELATIVE-RECORD-NUMBER
011300         MOVE RELATIVE-RECORD-NUMBER TO VSIO-RELATIVE-RECORD
011400         MOVE RECORD-IMAGE TO RRDS-RECORD
011500
011600         MOVE VSIO-WRITE TO VSIO-COMMAND
011700         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
011800                         RRDS-RECORD
011900*        END-CALL
012000
012100         IF VSIO-SUCCESS
012200             ADD +1 TO RECORD-COUNTER
012300         ELSE
012400             IF VSIO-LOGIC-ERROR
012500            AND VSIO-NO-MORE-SPACE
012600                DISPLAY 'INSUFFICIENT SPACE DEFINED IN CLUSTER'
012700                        'TO CONTAIN ALL RECORDS - LOADING '
012800                        'TERMINATED'
012900             ELSE
013000                 DISPLAY 'VSAMIO ERROR OCCURRED DURING '
013100                         VSIO-COMMAND
013200                 EXHIBIT NAMED VSIO-RETURN-CODE,
013300                 EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
013400                               VSIO-VSAM-FUNCTION-CODE,
013500                               VSIO-VSAM-FEEDBACK-CODE.
013600*            END-IF
013700*        END-IF
013800*    END-IF.
013900
014000 119-EXIT.
014100     EXIT.
014200
014300
./ ADD NAME=RRDSRAND,LIST=ALL                                           00580000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. RRDSRAND.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSIO ROUTINE BY ACCESSING AN RRDS      *
000900* CLUSTER RANDOMLY AND ADDING/UPDATING/DELETING RECORDS.        *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000     SELECT RECORD-IMAGES
002100         ASSIGN TO UR-2540R-S-SYSIN.
002200
002300 DATA DIVISION.
002400 FILE SECTION.
002500
002600 FD  RECORD-IMAGES
002700     LABEL RECORDS ARE OMITTED
002800     BLOCK CONTAINS 0 RECORDS
002900     DATA RECORD IS RECORD-IMAGE.
003000
003100 01  RECORD-IMAGE.
003200     02  RI-ACTION               PIC X(01).
003300         88  ACTION-IS-ADD                   VALUE 'A'.
003400         88  ACTION-IS-CHANGE                VALUE 'C'.
003500         88  ACTION-IS-DELETE                VALUE 'D'.
003600         88  ACTION-IS-VALID                 VALUE 'A', 'C', 'D'.
003700     02  FILLER                  PIC X(01).
003800     02  RI-IMAGE                PIC X(78).
003900
004000 WORKING-STORAGE SECTION.
004100 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
004200     88  END-OF-FILE                         VALUE 'Y'.
004300
004400 77  RECORD-COUNTER              PIC S9(8)   VALUE +0.
004500 77  COUNTER-EDIT                PIC ZZ,ZZZ,ZZ9.
004600
004700 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
004800 01  RRDSF01                     COPY VSAMIOFB.
004900 01  RRDS-RECORD.
005000     02  RR-KEY.
005100         03  RRK-HIGH            PIC 9(7).
005200         03  RRK-LOW             PIC 9(3).
005300     02  FILLER                  PIC X(70).
005400
005500 PROCEDURE DIVISION.
005600
005700 000-INITIATE.
005800
005900     DISPLAY 'RRDSRAND: READ/REWRITE RRDS DIRECT'.
006000     DISPLAY '----------------------------------'.
006100     DISPLAY ' '.
006200
006300     OPEN INPUT RECORD-IMAGES.
006400
006500     MOVE 'RRDSF01' TO VSIO-DDNAME.
006600     MOVE VSIO-RRDS TO VSIO-ORGANIZATION.
006700     MOVE VSIO-DIRECT TO VSIO-ACCESS.
006800     MOVE VSIO-INPUT-OUTPUT TO VSIO-MODE.
006900     MOVE +80 TO VSIO-RECORD-LENGTH.
007000     MOVE VSIO-OPEN TO VSIO-COMMAND.
007100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
007200                         RRDS-RECORD.
007300*    END-CALL.
007400     IF NOT VSIO-SUCCESS
007500         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
007600                 VSIO-COMMAND
007700         EXHIBIT NAMED VSIO-RETURN-CODE,
007800         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
007900                       VSIO-VSAM-FUNCTION-CODE,
008000                       VSIO-VSAM-FEEDBACK-CODE
008100         STOP RUN.
008200*    END-IF.
008300
008400 010-PROCESS.
008500
008600     PERFORM 110-PROCESS-UPDATES
008700        THRU 119-EXIT
008800       UNTIL END-OF-FILE.
008900*    END-PERFORM.
009000
009100 020-TERMINATE.
009200
009300     MOVE VSIO-CLOSE TO VSIO-COMMAND.
009400     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
009500                         RRDS-RECORD.
009600*    END-CALL.
009700     IF NOT VSIO-SUCCESS
009800         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009900                 VSIO-COMMAND
010000         EXHIBIT NAMED VSIO-RETURN-CODE,
010100         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
010200                       VSIO-VSAM-FUNCTION-CODE,
010300                       VSIO-VSAM-FEEDBACK-CODE.
010400*    END-IF.
010500
010600     STOP RUN.
010700
010800 110-PROCESS-UPDATES.
010900
011000     READ RECORD-IMAGES
011100         AT END
011200             MOVE 'Y' TO END-OF-FILE-SWITCH.
011300*    END-READ.
011400
011500     IF END-OF-FILE
011600         GO TO 119-EXIT.
011700*    END-IF.
011800
011900     ADD +1 TO RECORD-COUNTER.
012000
012100     IF NOT ACTION-IS-VALID
012200         MOVE RECORD-COUNTER TO COUNTER-EDIT
012300         DISPLAY COUNTER-EDIT ': ' RECORD-IMAGE
012400         DISPLAY '            *** ACTION INVALID: IGNORED'
012500         DISPLAY ' '
012600         GO TO 119-EXIT.
012700*    END-IF.
012800
012900     IF ACTION-IS-ADD
013000         PERFORM 120-ADD-PROCESS THRU 129-EXIT.
013100*    END-IF.
013200
013300     IF ACTION-IS-CHANGE
013400         PERFORM 130-CHANGE-PROCESS THRU 139-EXIT.
013500*    END-IF.
013600
013700     IF ACTION-IS-DELETE
013800         PERFORM 140-DELETE-PROCESS THRU 149-EXIT.
013900*    END-IF.
014000
014100 119-EXIT.
014200     EXIT.
014300
014400 120-ADD-PROCESS.
014500
014600     MOVE RI-IMAGE TO RRDS-RECORD.
014700     MOVE RRK-LOW TO VSIO-RELATIVE-RECORD.
014800     MOVE VSIO-WRITE TO VSIO-COMMAND.
014900     MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT.
015000     DISPLAY COUNTER-EDIT ': ADDING: ' RRDS-RECORD.
015100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
015200                         RRDS-RECORD.
015300*    END-CALL.
015400
015500     IF VSIO-SUCCESS
015600         DISPLAY '               RECORD ADDED'
015700     ELSE
015800         IF VSIO-LOGIC-ERROR
015900        AND VSIO-DUPLICATE-RECORD
016000             MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
016100             DISPLAY '            *** DUPLICATE RECORD ON FILE'
016200         ELSE
016300             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
016400                     VSIO-COMMAND
016500             EXHIBIT NAMED VSIO-RETURN-CODE,
016600             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
016700                           VSIO-VSAM-FUNCTION-CODE,
016800                           VSIO-VSAM-FEEDBACK-CODE.
016900*        END-IF
017000*    END-IF.
017100
017200     DISPLAY ' '.
017300
017400 129-EXIT.
017500     EXIT.
017600
017700 130-CHANGE-PROCESS.
017800
017900     PERFORM 150-READ-RECORD THRU 159-EXIT.
018000
018100     IF VSIO-LOGIC-ERROR
018200     AND VSIO-RECORD-NOT-FOUND
018300         MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
018400         DISPLAY COUNTER-EDIT ' *** RECORD NOT FOUND'
018500         DISPLAY ' '
018600         GO TO 139-EXIT.
018700*    END-IF.
018800
018900     MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT.
019000     DISPLAY COUNTER-EDIT ': ' RRDS-RECORD.
019100     DISPLAY '               RECORD BEFORE CHANGE'.
019200
019300     MOVE RI-IMAGE TO RRDS-RECORD.
019400     MOVE VSIO-REWRITE TO VSIO-COMMAND.
019500     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
019600                         RRDS-RECORD.
019700*    END-CALL.
019800
019900     IF VSIO-SUCCESS
020000         MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
020100         DISPLAY COUNTER-EDIT ': ' RRDS-RECORD
020200         DISPLAY '               RECORD AFTER CHANGE'
020300     ELSE
020400         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
020500                 VSIO-COMMAND
020600         EXHIBIT NAMED VSIO-RETURN-CODE,
020700         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
020800                       VSIO-VSAM-FUNCTION-CODE,
020900                       VSIO-VSAM-FEEDBACK-CODE
021000*    END-IF
021100
021200     DISPLAY ' '.
021300
021400 139-EXIT.
021500     EXIT.
021600
021700 140-DELETE-PROCESS.
021800
021900     PERFORM 150-READ-RECORD THRU 159-EXIT.
022000
022100     IF VSIO-LOGIC-ERROR
022200     AND VSIO-RECORD-NOT-FOUND
022300         MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
022400         DISPLAY COUNTER-EDIT ' *** RECORD NOT FOUND'
022500         DISPLAY ' '
022600         GO TO 149-EXIT.
022700*    END-IF.
022800
022900     MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT.
023000     DISPLAY COUNTER-EDIT ': ' RRDS-RECORD.
023100     DISPLAY '               RECORD BEFORE DELETE'.
023200
023300     MOVE VSIO-DELETE TO VSIO-COMMAND.
023400     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
023500                         RRDS-RECORD.
023600*    END-CALL.
023700
023800     IF VSIO-SUCCESS
023900         DISPLAY '             DELETED '
024000     ELSE
024100         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
024200                 VSIO-COMMAND
024300         EXHIBIT NAMED VSIO-RETURN-CODE,
024400         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
024500                       VSIO-VSAM-FUNCTION-CODE,
024600                       VSIO-VSAM-FEEDBACK-CODE.
024700*    END-IF.
024800
024900     DISPLAY ' '.
025000
025100 149-EXIT.
025200     EXIT.
025300
025400 150-READ-RECORD.
025500
025600     MOVE RI-IMAGE TO RRDS-RECORD.
025700     MOVE RRK-LOW TO VSIO-RELATIVE-RECORD.
025800     MOVE VSIO-READ TO VSIO-COMMAND.
025900     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
026000                         RRDS-RECORD.
026100*    END-CALL.
026200
026300     IF NOT VSIO-SUCCESS
026400         IF VSIO-LOGIC-ERROR
026500        AND VSIO-RECORD-NOT-FOUND
026600            NEXT SENTENCE
026700         ELSE
026800             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
026900                     VSIO-COMMAND
027000             EXHIBIT NAMED VSIO-RETURN-CODE,
027100             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
027200                           VSIO-VSAM-FUNCTION-CODE,
027300                           VSIO-VSAM-FEEDBACK-CODE.
027400*        END-IF
027500*    END-IF.
027600
027700 159-EXIT.
027800     EXIT.
027900
./ ADD NAME=RRDSREAD,LIST=ALL                                           00600000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. RRDSREAD.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY READING RECORDS FROM *
000900* AN RRDS CLUSTER AND DISPLAYING THEIR CONTENTS ON SYSOUT.      *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000 DATA DIVISION.
002100 FILE SECTION.
002200
002300 WORKING-STORAGE SECTION.
002400 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
002500     88  END-OF-FILE                         VALUE 'Y'.
002600
002700 77  COUNTER-EDIT                PIC ZZ,ZZZ,ZZ9.
002800
002900 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
003000 01  RRDSF01                     COPY VSAMIOFB.
003100 01  RRDS-RECORD                 PIC X(80).
003200
003300 PROCEDURE DIVISION.
003400
003500 000-INITIATE.
003600
003700     DISPLAY 'RRDSREAD: READ RRDS SEQUENTIALLY'.
003800     DISPLAY '--------------------------------'.
003900     DISPLAY ' '.
004000
004100     MOVE 'RRDSF01' TO VSIO-DDNAME.
004200     MOVE VSIO-RRDS TO VSIO-ORGANIZATION.
004300     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
004400     MOVE VSIO-INPUT TO VSIO-MODE.
004500     MOVE +80 TO VSIO-RECORD-LENGTH.
004600     MOVE VSIO-OPEN TO VSIO-COMMAND.
004700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
004800                         RRDS-RECORD.
004900*    END-CALL.
005000     IF NOT VSIO-SUCCESS
005100         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
005200                 VSIO-COMMAND
005300         EXHIBIT NAMED VSIO-RETURN-CODE,
005400         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
005500                       VSIO-VSAM-FUNCTION-CODE,
005600                       VSIO-VSAM-FEEDBACK-CODE
005700         STOP RUN.
005800*    END-IF.
005900
006000 010-PROCESS.
006100
006200     PERFORM 110-PROCESS-DATA
006300        THRU 119-EXIT
006400       UNTIL END-OF-FILE.
006500*    END-PERFORM.
006600
006700 020-TERMINATE.
006800
006900     MOVE VSIO-CLOSE TO VSIO-COMMAND.
007000     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
007100                         RRDS-RECORD.
007200*    END-CALL.
007300     IF NOT VSIO-SUCCESS
007400         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
007500                 VSIO-COMMAND
007600         EXHIBIT NAMED VSIO-RETURN-CODE,
007700         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
007800                       VSIO-VSAM-FUNCTION-CODE,
007900                       VSIO-VSAM-FEEDBACK-CODE.
008000*    END-IF.
008100
008200     STOP RUN.
008300
008400 110-PROCESS-DATA.
008500
008600     MOVE VSIO-READ TO VSIO-COMMAND.
008700     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
008800                         RRDS-RECORD.
008900*    END-CALL.
009000
009100     IF NOT VSIO-SUCCESS
009200         IF VSIO-END-OF-FILE
009300             MOVE 'Y' TO END-OF-FILE-SWITCH
009400         ELSE
009500             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
009600                     VSIO-COMMAND
009700             EXHIBIT NAMED VSIO-RETURN-CODE,
009800             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
009900                           VSIO-VSAM-FUNCTION-CODE,
010000                           VSIO-VSAM-FEEDBACK-CODE
010100             MOVE 'Y' TO END-OF-FILE-SWITCH.
010200*        END-IF
010300*    END-IF.
010400
010500     IF NOT END-OF-FILE
010600         MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
010700         DISPLAY COUNTER-EDIT ': ' RRDS-RECORD.
010800*    END-IF.
010900
011000 119-EXIT.
011100     EXIT.
011200
011300
./ ADD NAME=RRDSSSEQ,LIST=ALL                                           00620000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. RRDSSSEQ.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY USING START AND READ *
000900* NEXT COMMANDS ON AN RRDS CLUSTER (SKIP-SEQ PROCESSING).       *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000 DATA DIVISION.
002100 FILE SECTION.
002200
002300 WORKING-STORAGE SECTION.
002400 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
002500     88  END-OF-FILE                         VALUE 'Y'.
002600
002700 77  RECORD-COUNTER              PIC S9(8).
002800 77  COUNTER-EDIT                PIC ZZ,ZZZ,ZZ9.
002900
003000 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
003100 01  RRDSF01                     COPY VSAMIOFB.
003200 01  RRDS-RECORD                 PIC X(80).
003300
003400 PROCEDURE DIVISION.
003500
003600 000-INITIATE.
003700
003800     DISPLAY 'RRDSSSEQ: READ RRDS SEQUENTIALLY (W/START)'.
003900     DISPLAY '------------------------------------------'.
004000     DISPLAY ' '.
004100
004200     MOVE 'RRDSF01' TO VSIO-DDNAME.
004300     MOVE VSIO-RRDS TO VSIO-ORGANIZATION.
004400     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
004500     MOVE VSIO-INPUT TO VSIO-MODE.
004600     MOVE +80 TO VSIO-RECORD-LENGTH.
004700     MOVE VSIO-OPEN TO VSIO-COMMAND.
004800     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
004900                         RRDS-RECORD.
005000*    END-CALL.
005100     IF NOT VSIO-SUCCESS
005200         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
005300                 VSIO-COMMAND
005400         EXHIBIT NAMED VSIO-RETURN-CODE,
005500         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
005600                       VSIO-VSAM-FUNCTION-CODE,
005700                       VSIO-VSAM-FEEDBACK-CODE
005800         STOP RUN.
005900*    END-IF.
006000
006100 010-PROCESS.
006200
006300     PERFORM 110-PROCESS-DATA
006400        THRU 119-EXIT
006500       UNTIL END-OF-FILE.
006600*    END-PERFORM.
006700
006800 020-TERMINATE.
006900
007000     MOVE VSIO-CLOSE TO VSIO-COMMAND.
007100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
007200                         RRDS-RECORD.
007300*    END-CALL.
007400     IF NOT VSIO-SUCCESS
007500         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
007600                 VSIO-COMMAND
007700         EXHIBIT NAMED VSIO-RETURN-CODE,
007800         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
007900                       VSIO-VSAM-FUNCTION-CODE,
008000                       VSIO-VSAM-FEEDBACK-CODE.
008100*    END-IF.
008200
008300     STOP RUN.
008400
008500 110-PROCESS-DATA.
008600
008700     DISPLAY 'START RRN EQUAL TO 21'.
008800     MOVE 21 TO VSIO-RELATIVE-RECORD.
008900     MOVE VSIO-START-KEY-EQUAL TO VSIO-COMMAND.
009000     PERFORM 120-START-AND-READ THRU 129-EXIT.
009100     DISPLAY 'START RRN EQUAL TO 51'.
009200     MOVE 51 TO VSIO-RELATIVE-RECORD.
009300     MOVE VSIO-START-KEY-EQUAL TO VSIO-COMMAND.
009400     PERFORM 120-START-AND-READ THRU 129-EXIT.
009500     DISPLAY 'START RRN GREATER THAN OR EQUAL TO 81'.
009600     MOVE 81 TO VSIO-RELATIVE-RECORD.
009700     MOVE VSIO-START-KEY-NOTLESS TO VSIO-COMMAND.
009800     PERFORM 120-START-AND-READ THRU 129-EXIT.
009900     DISPLAY 'START RRN GREATER THAN OR EQUAL TO 111'.
010000     MOVE 111 TO VSIO-RELATIVE-RECORD.
010100     MOVE VSIO-START-KEY-NOTLESS TO VSIO-COMMAND.
010200     PERFORM 120-START-AND-READ THRU 129-EXIT.
010300
010400     MOVE 'Y' TO END-OF-FILE-SWITCH.
010500
010600 119-EXIT.
010700     EXIT.
010800
010900 120-START-AND-READ.
011000
011100     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
011200                         RRDS-RECORD.
011300*    END-CALL.
011400
011500     IF NOT VSIO-SUCCESS
011600         IF VSIO-RECORD-NOT-FOUND
011700             DISPLAY '*** RECORD NOT FOUND ***'
011800         ELSE
011900             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
012000                     VSIO-COMMAND
012100             EXHIBIT NAMED VSIO-RETURN-CODE,
012200             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
012300                           VSIO-VSAM-FUNCTION-CODE,
012400                           VSIO-VSAM-FEEDBACK-CODE.
012500*        END-IF
012600*    END-IF.
012700
012800     IF NOT VSIO-SUCCESS
012900         GO TO 129-EXIT.
013000
013100     MOVE +0 TO RECORD-COUNTER.
013200     PERFORM 130-READ-AND-DISPLAY THRU 139-EXIT
013300       UNTIL END-OF-FILE
013400          OR RECORD-COUNTER > +4.
013500*    END-PERFORM.
013600     MOVE 'N' TO END-OF-FILE-SWITCH.
013700
013800 129-EXIT.
013900     EXIT.
014000
014100 130-READ-AND-DISPLAY.
014200
014300     MOVE VSIO-READ TO VSIO-COMMAND.
014400     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
014500                         RRDS-RECORD.
014600*    END-CALL.
014700
014800     IF VSIO-SUCCESS
014900         MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
015000         DISPLAY COUNTER-EDIT ': ' RRDS-RECORD
015100     ELSE
015200         IF VSIO-END-OF-FILE
015300             MOVE 'Y' TO END-OF-FILE-SWITCH
015400         ELSE
015500             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
015600                     VSIO-COMMAND
015700             EXHIBIT NAMED VSIO-RETURN-CODE,
015800             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
015900                           VSIO-VSAM-FUNCTION-CODE,
016000                           VSIO-VSAM-FEEDBACK-CODE.
016100*        END-IF
016200*    END-IF.
016300
016400     ADD +1 TO RECORD-COUNTER.
016500
016600 139-EXIT.
016700     EXIT.
016800
016900
./ ADD NAME=RRDSUPDT,LIST=ALL                                           00640000
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. RRDSUPDT.
000300 AUTHOR. JAY MOSELEY.
000400 DATE-WRITTEN. NOVEMBER, 2001.
000500 DATE-COMPILED.
000600
000700* ************************************************************* *
000800* THIS PROGRAM TESTS THE VSAMIO ROUTINE BY READING RECORDS FROM *
000900* AN RRDS CLUSTER SEQUENTIALLY AND UPDATING SELECTED RECORDS.   *
001000* ************************************************************* *
001100
001200 ENVIRONMENT DIVISION.
001300 CONFIGURATION SECTION.
001400 SOURCE-COMPUTER. IBM-370.
001500 OBJECT-COMPUTER. IBM-370.
001600
001700 INPUT-OUTPUT SECTION.
001800 FILE-CONTROL.
001900
002000 DATA DIVISION.
002100 FILE SECTION.
002200
002300 WORKING-STORAGE SECTION.
002400 77  END-OF-FILE-SWITCH          PIC X(1)    VALUE 'N'.
002500     88  END-OF-FILE                         VALUE 'Y'.
002600
002700 77  COUNTER-EDIT                PIC ZZ,ZZZ,ZZ9.
002800
002900 01  VSIO-PARAMETER-VALUES       COPY VSAMIO.
003000 01  RRDSF01                     COPY VSAMIOFB.
003100 01  RRDS-RECORD.
003200     02  RRR-ID.
003300         03  RRRI-HIGH           PIC 9(2).
003400         03  FILLER              PIC 9(7).
003500         03  RRRI-LOW            PIC 9(1).
003600     02  FILLER                  PIC X(70).
003700
003800 PROCEDURE DIVISION.
003900
004000 000-INITIATE.
004100
004200     DISPLAY 'RRDSUPDT: READ/REWRITE RRDS SEQUENTIALLY'.
004300     DISPLAY '----------------------------------------'.
004400     DISPLAY ' '.
004500
004600     MOVE 'RRDSF01' TO VSIO-DDNAME.
004700     MOVE VSIO-RRDS TO VSIO-ORGANIZATION.
004800     MOVE VSIO-SEQUENTIAL TO VSIO-ACCESS.
004900     MOVE VSIO-INPUT-OUTPUT TO VSIO-MODE.
005000     MOVE +80 TO VSIO-RECORD-LENGTH.
005100     MOVE VSIO-OPEN TO VSIO-COMMAND.
005200     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
005300                         RRDS-RECORD.
005400*    END-CALL.
005500     IF NOT VSIO-SUCCESS
005600         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
005700                 VSIO-COMMAND
005800         EXHIBIT NAMED VSIO-RETURN-CODE,
005900         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
006000                       VSIO-VSAM-FUNCTION-CODE,
006100                       VSIO-VSAM-FEEDBACK-CODE
006200         STOP RUN.
006300*    END-IF.
006400
006500 010-PROCESS.
006600
006700     PERFORM 110-PROCESS-DATA
006800        THRU 119-EXIT
006900       UNTIL END-OF-FILE.
007000*    END-PERFORM.
007100
007200 020-TERMINATE.
007300
007400     MOVE VSIO-CLOSE TO VSIO-COMMAND.
007500     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
007600                         RRDS-RECORD.
007700*    END-CALL.
007800     IF NOT VSIO-SUCCESS
007900         DISPLAY 'VSAMIO ERROR OCCURRED DURING '
008000                 VSIO-COMMAND
008100         EXHIBIT NAMED VSIO-RETURN-CODE,
008200         EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
008300                       VSIO-VSAM-FUNCTION-CODE,
008400                       VSIO-VSAM-FEEDBACK-CODE.
008500*    END-IF.
008600
008700     STOP RUN.
008800
008900 110-PROCESS-DATA.
009000
009100     MOVE VSIO-READ TO VSIO-COMMAND.
009200     CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
009300                         RRDS-RECORD.
009400*    END-CALL.
009500
009600     IF NOT VSIO-SUCCESS
009700         IF VSIO-END-OF-FILE
009800             MOVE 'Y' TO END-OF-FILE-SWITCH
009900         ELSE
010000             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
010100                     VSIO-COMMAND
010200             EXHIBIT NAMED VSIO-RETURN-CODE,
010300             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
010400                           VSIO-VSAM-FUNCTION-CODE,
010500                           VSIO-VSAM-FEEDBACK-CODE
010600             MOVE 'Y' TO END-OF-FILE-SWITCH.
010700*        END-IF
010800*    END-IF.
010900
011000     IF NOT END-OF-FILE
011100         PERFORM 120-UPDATE-PROCESS
011200            THRU 129-EXIT.
011300*    END-IF.
011400
011500 119-EXIT.
011600     EXIT.
011700
011800 120-UPDATE-PROCESS.
011900
012000     IF RRRI-LOW EQUAL 2
012100     OR RRRI-LOW EQUAL 4
012200     OR RRRI-LOW EQUAL 8
012300         MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
012400         DISPLAY COUNTER-EDIT ': ' RRDS-RECORD
012500         MOVE '99' TO RRRI-HIGH
012600         MOVE VSIO-REWRITE TO VSIO-COMMAND
012700         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
012800                         RRDS-RECORD
012900*        END-CALL
013000         IF VSIO-SUCCESS
013100             DISPLAY '   UPDATED: ' RRDS-RECORD
013200         ELSE
013300             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
013400                     VSIO-COMMAND
013500             EXHIBIT NAMED VSIO-RETURN-CODE,
013600             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
013700                           VSIO-VSAM-FUNCTION-CODE,
013800                           VSIO-VSAM-FEEDBACK-CODE.
013900*        END-IF
014000*    END-IF.
014100
014200     IF RRRI-LOW EQUAL 5
014300     OR RRRI-LOW EQUAL 7
014400         MOVE VSIO-RELATIVE-RECORD TO COUNTER-EDIT
014500         DISPLAY COUNTER-EDIT ': ' RRDS-RECORD
014600         MOVE VSIO-DELETE TO VSIO-COMMAND
014700         CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, RRDSF01,
014800                         RRDS-RECORD
014900*        END-CALL
015000         IF VSIO-SUCCESS
015100             DISPLAY '   DELETED  '
015200         ELSE
015300             DISPLAY 'VSAMIO ERROR OCCURRED DURING '
015400                     VSIO-COMMAND
015500             EXHIBIT NAMED VSIO-RETURN-CODE,
015600             EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
015700                           VSIO-VSAM-FUNCTION-CODE,
015800                           VSIO-VSAM-FEEDBACK-CODE.
015900*        END-IF
016000*    END-IF.
016100
016200 129-EXIT.
016300     EXIT.
016400
./ ENDUP                                                                00660000
><                                                                      00670000
/*                                                                      00680000
//*                                                                     00690000
//********************************************************************* 00700000
//* ASSEMBLE VSAMIO INTO OBJECT DATASET                               * 00710000
//********************************************************************* 00720000
//*                                                                     00730000
//ASM     EXEC PGM=IEUASM,REGION=1024K,                                 00740000
//             PARM='LIST,NODECK,OBJECT'                                00750000
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR                                 00760000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(600,100))                        00770000
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(300,50))                         00780000
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(300,50))                         00790000
//SYSPRINT DD  SYSOUT=*                                                 00800000
//SYSPUNCH DD  DUMMY                                                    00810000
//SYSGO    DD  DSN=SYS2.VSAMIO.OBJECT(VSAMIO),DISP=(,CATLG),            00820000
//             UNIT=3380,VOL=SER=PUB012,SPACE=(TRK,(5,,1))              00830000
//SYSIN    DD  DSN=SYS2.VSAMIO.SOURCE(VSAMIOS),DISP=SHR                 00840000
//                                                                      00850000
