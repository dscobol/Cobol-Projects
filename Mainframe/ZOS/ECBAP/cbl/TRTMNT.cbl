000100 IDENTIFICATION DIVISION.                                         00010001
000200 PROGRAM-ID.  TRTMNT.                                             00020001
000300 AUTHOR. JON SAYLES.                                              00030001
000400 INSTALLATION. COBOL DEV Center.                                  00040001
000500 DATE-WRITTEN. 01/23/05.                                          00050001
000600 DATE-COMPILED. 01/23/99.                                         00060067
000700 SECURITY. CONFIDENTIAL PATIENT DATA.                             00070001
000800                                                                  00080001
000900******************************************************************00090001
001000*REMARKS.                                                         00100008
001100*                                                                 00110001
001200*          THIS PROGRAM EDITS A DAILY TREATMENT TRANSACTION FILE  00120001
001300*          PRODUCED BY DATA ENTRY OPERATORS FROM CICS SCREENS     00130001
001400*                                                                 00140001
001500*          IT CONTAINS EVERY TREATMENT FOR EVERY PATIENT IN THE   00150001
001600*          HOSPITAL.                                              00160001
001700*                                                                 00170001
001800*          THE PROGRAM EDITS EACH RECORD AGAINST A NUMBER OF      00180001
001900*          CRITERIA, BALANCES FINAL TOTALS AND WRITES GOOD        00190001
002000*          RECORDS TO AN OUTPUT FILE                              00200001
002100*                                                                 00210001
002100*          NOTE: Y2K WINDOWING USED ON ALL DATE FIELDS            00210101
002100*                                                                 00210201
002200******************************************************************00220001
002300                                                                  00230001
002400         INPUT FILE              -   DDS0001.TRMTDATA             00240001
002500                                                                  00250001
002600         VSAM MASTER FILE        -   DDS0001.PATMASTR             00260001
002700                                                                  00270001
002800         INPUT ERROR FILE        -   DDS0001.TRMTERR              00280001
002900                                                                  00290001
003000         OUTPUT FILE PRODUCED    -   DDS001.TRMTEDIT              00300001
003100                                                                  00310001
003200         DUMP FILE               -   SYSOUT                       00320001
003300                                                                  00330001
003400******************************************************************00340001
003500                                                                  00350001
003600 ENVIRONMENT DIVISION.                                            00360001
003700 CONFIGURATION SECTION.                                           00370001
003800 SOURCE-COMPUTER. IBM-390.                                        00380001
003900 OBJECT-COMPUTER. IBM-390.                                        00390001
004000 INPUT-OUTPUT SECTION.                                            00400001
004100 FILE-CONTROL.                                                    00410001
004200     SELECT SYSOUT                                                00420001
004300     ASSIGN TO UT-S-SYSOUT                                        00430001
004400       ORGANIZATION IS SEQUENTIAL.                                00440001
004500                                                                  00450001
004600     SELECT TRMTDATA                                              00460001
004700     ASSIGN TO UT-S-TRMTDATA                                      00470001
004800       ACCESS MODE IS SEQUENTIAL                                  00480001
004900       FILE STATUS IS OFCODE.                                     00490001
005000                                                                  00500001
005100     SELECT TRMTEDIT                                              00510001
005200     ASSIGN TO UT-S-TRMTEDIT                                      00520001
005300       ACCESS MODE IS SEQUENTIAL                                  00530001
005400       FILE STATUS IS OFCODE.                                     00540001
005500                                                                  00550001
005600     SELECT TRMTERR                                               00560001
005700     ASSIGN TO UT-S-TRMTERR                                       00570001
005800       ACCESS MODE IS SEQUENTIAL                                  00580001
005900       FILE STATUS IS OFCODE.                                     00590001
006000                                                                  00600001
006100     SELECT PATMSTR                                               00610001
006200            ASSIGN       TO PATMSTR                               00620061
006300            ORGANIZATION IS INDEXED                               00630061
006400            ACCESS MODE  IS RANDOM                                00640061
006500            RECORD KEY   IS PATIENT-KEY                           00650061
006600            FILE STATUS  IS PATMSTR-STATUS.                       00660061
006700                                                                  00670001
006800 DATA DIVISION.                                                   00680001
006900 FILE SECTION.                                                    00690001
007000 FD  SYSOUT                                                       00700001
007100     RECORDING MODE IS F                                          00710001
007200     LABEL RECORDS ARE STANDARD                                   00720001
007300     RECORD CONTAINS 130 CHARACTERS                               00730001
007400     BLOCK CONTAINS 0 RECORDS                                     00740001
007500     DATA RECORD IS SYSOUT-REC.                                   00750061
007600 01  SYSOUT-REC  PIC X(130).                                      00760001
007700                                                                  00770001
007800****** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM    00780001
007900****** IT CONSISTS OF ALL PATIENT TREATMENTS ENTERED              00790001
008000****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS     00800001
008100****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND    00810001
008200 FD  TRMTDATA                                                     00820001
008300     RECORDING MODE IS F                                          00830001
008400     LABEL RECORDS ARE STANDARD                                   00840001
008500     RECORD CONTAINS 1101 CHARACTERS                              00850001
008600     BLOCK CONTAINS 0 RECORDS                                     00860001
008700     DATA RECORD IS INPATIENT-TREATMENT-REC-DATA.                 00870001
008800 01  INPATIENT-TREATMENT-REC-DATA PIC X(1101).                    00880001
008900                                                                  00890001
009000****** THIS FILE IS WRITTEN FOR ALL TREATMENT RECORDS THAT PASS   00900001
009100****** THE PROGRAM'S EDIT ROUTINES                                00910001
009200****** THE TRAILER RECORD SHOULD ONLY CARRY THE NUMBER OF         00920001
009300****** RECORDS IN THE FILE ON TO THE NEXT JOB STEP                00930001
009400 FD  TRMTEDIT                                                     00940001
009500     RECORDING MODE IS F                                          00950001
009600     LABEL RECORDS ARE STANDARD                                   00960001
009700     BLOCK CONTAINS 0 RECORDS                                     00970001
009800     RECORD CONTAINS 1101 CHARACTERS                              00980001
009900     DATA RECORD IS INPATIENT-TREATMENT-REC-EDIT.                 00990001
010000 01  INPATIENT-TREATMENT-REC-EDIT PIC X(1101).                    01000001
010100                                                                  01010001
010200 FD  TRMTERR                                                      01020001
010300     RECORDING MODE IS F                                          01030001
010400     LABEL RECORDS ARE STANDARD                                   01040001
010500     RECORD CONTAINS 1141 CHARACTERS                              01050001
010600     BLOCK CONTAINS 0 RECORDS                                     01060001
010700     DATA RECORD IS INPATIENT-TREATMENT-REC-ERR.                  01070001
010800 01  INPATIENT-TREATMENT-REC-ERR.                                 01080001
010900     05  ERR-MSG                     PIC X(40).                   01090001
011000     05  REST-OF-REC                 PIC X(1101).                 01100001
011100                                                                  01110001
011200 FD  PATMSTR                                                      01120001
011300     RECORD CONTAINS 2964 CHARACTERS                              01130001
011400     DATA RECORD IS PATIENT-MASTER-REC.                           01140001
011500 01  PATMSTR-REC.                                                 01150001
011600     05 PATIENT-KEY      PIC X(06).                               01160001
011700     05 FILLER           PIC X(2958).                             01170001
011800                                                                  01180001
011900** QSAM FILE                                                      01190001
012000 WORKING-STORAGE SECTION.                                         01200001
012100 01  FILLER                     PIC X(33) VALUE                   01210001
012200        '* WORKING STORAGE BEGINS HERE *'.                        01220001
012300                                                                  01230001
012400 01  FILLER                     PIC X(33) VALUE                   01240001
012500             '****** DUMP MSG ****************'.                  01250001
012600***************************************************************** 01260001
012700*    DUMP POINTER AREA2s                                          01270001
012800*        PARA POINTER- MOVE PARAGRAPH NUMBER TO THIS POINTER    * 01280001
012900*                      AS EACH PARAGRAPH IS ENTERED. DO NOT     * 01290001
013000*                      MOVE PARAGRAPH NUMBERS OF COMMON           01300001
013100*                      PARAGRAPHS (USE COMM POINTER).             01310001
013200*                                                               * 01320001
013300*        COMM POINTER - EACH COMMON PARAGRAPH SHOULD MOVE       * 01330001
013400*                       ITS PARAGRAPH NUMBER TO THIS POINTER    * 01340001
013500*                       AT IT INCEPTION.                          01350001
013600*                                                               * 01360001
013700***************************************************************** 01370001
013800 01  DUMP-LOCATOR.                                                01380001
013900     05 FILLER             PIC X(32)                              01390001
014000         VALUE '>>>>>>> WS DUMP POINTERS >>>>>>>'.                01400001
014100     05 FILLER             PIC X(16)   VALUE 'Z PARA POINTER'.    01410001
014200     05 PARA-POINTER       PIC X(8)    VALUE SPACES.              01420001
014300     05 FILLER             PIC X(8)    VALUE '       Z'.          01430001
014400     05 FILLER             PIC X(16)   VALUE 'Z COMM POINTER'.    01440001
014500     05 COMM-POINTER       PIC X(8)    VALUE SPACES.              01450001
014600     05 FILLER             PIC X(32)                              01460001
014700             VALUE '<<<<<<< WS DUMP POINTERS <<<<<<<'.            01470001
014800                                                                  01480001
014900 01  WS-CALLED-MODULE   PIC X(8) VALUE "DTEVAL".                  01490001
015000 01  DUMP-DISPLAY.                                                01500001
015100     05 DUMP-STATUS               PIC X(3)  VALUE SPACES.         01510001
015200     05 DUMP-MESSAGE              PIC X(61) VALUE 'NO MSG'.       01520001
           05 WS-CICS-XFER              PIC X(8)  VALUE SPACES.
015300                                                                  01530001
015400 01  FILE-STATUS-CODES.                                           01540001
015500     05  PATMSTR-STATUS          PIC X(2).                        01550001
015600         88 RECORD-FOUND         VALUE "00".                      01560001
015700         88 PATMSTR-NOT-FOUND    VALUE "23".                      01570001
015800     05  OFCODE                  PIC X(2).                        01580001
015900         88 CODE-WRITE    VALUE SPACES.                           01590001
016000                                                                  01600001
      *016100 COPY TREATMNT.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST.COPYLIB\TREATMNT.cpy

      ** DAILY PATIENT/TREATMENTS FILE
       01  INPATIENT-TREATMENT-REC.
           05  RECORD-TYPE             PIC X(01).
               88  TRAILER-REC        VALUE "T".
           05  PATIENT-ID              PIC 9(6).
           05  TREATMENT-DATE-TIME.
               10 TREATMENT-DATE       PIC X(08).
               10 FILLER               PIC X.
               10 TREATMENT-TIME       PIC X(08).
               10 FILLER               PIC X(09).
           05  BED-IDENTITY            PIC X(4).
           05  PRIMARY-DIAGNOSTIC-CODE PIC X(5).
           05  MEDICATION-ID           PIC X(8).
           05  TREATMENT-MODE          PIC X(03).
               88  ORAL-ADMIN          VALUE "0RA".
               88  INTRAVENOUS-ADMIN   VALUE "INV".
               88  INJECTION           VALUE "INJ".
               88  MRI                 VALUE "MRI".
               88  CAT                 VALUE "CAT".
               88  CHEMO-THERAPY       VALUE "CHM".
               88  RADIATION-THERAPY   VALUE "RAD".
               88  SURGERY             VALUE "SUR".
               88  PHYSIO-THERAPY      VALUE "PHY".
               88  EQUIPMENT           VALUE "EQP".
               88  LAB-TESTS           VALUE "LAB".
               88  VENIPUNCTURE        VALUE "VEN".                     022904MN
               88  OTHER-TREATMENT     VALUE "OTH".
               88  VALID-TRTMNT-MODES VALUES ARE
                  "ORA", "INV", "INJ", "MRI", "CAT"
                  "SUR", "PHY", "EQP", "LAB", "VEN"
                  "MRI", "CAT", "CHM", "RAD", "OTH".
           05  BILLABLE-TREATMENT-IND   PIC X(01).
               88  NON-BILLABLE         VALUE "N".
               88  BILLABLE             VALUE "B".
               88  DEFERRED             VALUE "D".
               88 VALID-BILLABLE-TYPES
                   VALUES ARE "N", "B", "G", "D".
           05  MEDICATION-COST         PIC 9(5)V99.
           05  ATTENDING-PHYS-ID       PIC X(08).
           05  PRESCRIBING-PHYS-ID     PIC X(08).
           05  SUPERVISOR-NURSE-ID     PIC X(08).
           05  TREATMENT-NURSE-ID      PIC X(08).
           05  PHARMACY-COST           PIC 9(3)V99.
           05  ANCILLARY-CHARGE        PIC 9(3)V99.
           05  LAB-CHARGES OCCURS 12 TIMES.
               10  LAB-TEST-ID         PIC X(08).
               10  TEST-CATEGORY       PIC X(04).
                   88 PULMINARY           VALUE "PULM".
                   88 BLOOD               VALUE "BLOD".
                   88 SPINAL              VALUE "SPNL".
                   88 H1N1                VALUE "H1N1".
                   88 GASTRO              VALUE "GAST".
                   88 LUNG                VALUE "LUNG".
                   88 NUCLEAR-MEDICINE    VALUE "NUCL".
                   88 RENAL               VALUE "RNAL".
                   88 MISCELLANEOUS      VALUE "MISC".
                   88 VALID-CATEGORY VALUES ARE "PULM", "BLOD", "NUCL",
                      "GAST", "SPNL", "LUNG", "RNAL", "H1N1", "MISC".
      ****** FOR PERFORMANCE, MOVE H1N1 TO THE TOP OF THE LIST
               10  TEST-SHORT-DESC         PIC X(25).
               10  TEST-COST               PIC 9(5)V99.
               10  VENIPUNCTURE-COST       PIC 9(3)V99.
               10  PRESCRIBING-PHYS        PIC X(08).
               10  DIAG-CDE                PIC X(05).
           05  TREATMENT-COMMENTS      PIC X(254).
           05   NEW-VAR                PIC X(23).
                                                                        01610001
016200                                                                  01620001
016300 01  WS-TRAILER-REC.                                              01630001
016400     05  FILLER                  PIC X(1).                        01640001
016500     05  IN-RECORD-COUNT         PIC 9(9).                        01650001
016600     05  FILLER                  PIC X(1).                        01660001
016700     05  IN-MEDICATION-CHARGES   PIC S9(9)V99.                    01670001
016800     05  IN-PHARMACY-CHARGES     PIC S9(7)V99.                    01680001
016900     05  IN-ANCILLARY-CHARGES    PIC S9(5)V99.                    01690001
017000                                                                  01700001
017100 01  WS-OUTPUT-REC.                                               01710001
017200     05  PATIENT-NBR-O           PIC 9(6).                        01720001
017300     05  FILLER                  PIC X(2) VALUE SPACES.           01730001
017400     05  PATIENT-NAME-O          PIC X(20).                       01740001
017500     05  PATIENT-PHONE-O         PIC X(10).                       01750001
017600     05  FILLER                  PIC X(2) VALUE SPACES.           01760001
017700     05  PATIENT-TYPE-O          PIC X(2).                        01770001
017800     05  BED-IDENTITY-O          PIC ZZZ9.                        01780001
017900     05  FILLER                  PIC X(2) VALUE SPACES.           01790001
018000     05  CURR-DATE-O             PIC X(6).                        01800001
018100     05  FILLER                  PIC X(2) VALUE SPACES.           01810001
018200     05  PATIENT-AMT-PER-DAY-O   PIC $$,$$9.99.                   01820001
018300     05  FILLER                  PIC X(2) VALUE SPACES.           01830001
018400     05  INS-COVERAGE-PERC-O     PIC 999.                         01840001
018500     05  FILLER                  PIC X(2) VALUE SPACES.           01850001
018600     05  INS-TYPE-O              PIC X(4).                        01860001
018700     05  HOSPITAL-STAY-LTH-O     PIC 999.                         01870001
018800     05  FILLER                  PIC X(7) VALUE SPACES.           01880001
018900                                                                  01890001
      *019000     COPY PATMSTR.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST.COPYLIB\PATMSTR.cpy

      * COPY PTMSTR.
       01  PATIENT-MASTER-REC.
           05  PATIENT-ID                      PIC X(6).
           05  PATIENT-TYPE                    PIC X(1).
               88 INPATIENT   VALUE "I".
               88 OUTPATIENT  VALUE "0".
               88 VALID-TYPE  VALUES ARE "I", "O".
           05  PREVIOUS-PATIENT-IND            PIC X(01).
               88 PREV-PATIENT         VALUE "Y".
               88 NOT-PREVE-PATIENT    VALUE "N".
               88 VALID-PREV-IND  VALUES ARE "Y", "N".
           05  PRIMARY-STAY-WARD-NBR           PIC X(4).
               88  INTENSIVE-CARE  VALUE "0010".
               88  OBSTETRICS      VALUE "2010".
               88  PEDIATRICS      VALUE "1010".
               88  ONCOLOGY        VALUE "0011".
               88  CARDIO-THORACIC VALUE "0110".
               88  GENERAL         VALUE "0000".
               88  VALID-WARD VALUES ARE
                   "0010", "2010", "1010", "0011", "0110", "0000".
           05  BED-IDENTITY-PRIMARY            PIC 9(4).
           05  DATE-ADMIT                      PIC X(10).
           05  DATE-DISCHARGE                  PIC X(10).
           05  ATTENDING-PHYSICIAN             PIC X(08).
           05  DIAGNOSTIC-CODE-PRIMARY         PIC X(05).
           05  DIAGNOSTIC-CODE-SECONDARY       PIC X(05).
           05  DIAGNOSTIC-CODE-TERTIARY        PIC X(05).
           05  INS-TYPE                        PIC X(3).
               88 VALID-INS-TYPE VALUES ARE "HMO", "PPO", "POS" "MAN".
               88 MANAGED-CARE VALUE "MAN".
           05  HOSPITAL-STAY-LTH               PIC 999.
           05  PATIENT-TOT-X                   PIC X(9).
           05  PATIENT-TOT-AMT REDEFINES PATIENT-TOT-X
                                               PIC 9(7)V99.
           05  PRIMARY-CARE-PHYSICIAN-ID       PIC X(8).
           05  IN-OUT-NETWORK                  PIC X(1).
               88 IN-NETWORK       VALUE "N".
               88 OUT-OF-NETWORK   VALUE "O".
           05  COPAY                           PIC S9(3).
           05  REMAINING-DEDUCTIBLE            PIC S9(4).
           05  HIPAA-FORM-SIGNED-IND           PIC X(01).
               88 HIPAA-SIGNED       VALUE "Y".
               88 HIPAA-UNSIGNED     VALUE "N".
           05  PATIENT-ADMIT-COMMENTS          PIC X(254).
           05  DAILY-LAB-CHARGES-SUMMARY OCCURS 20 TIMES.
               10  LAB-TEST-S-ID             PIC X(08).
               10  LAB-TEST-DATE             PIC X(08).
               10  TEST-SHORT-S-DESC         PIC X(25).
               10  TEST-DIAG-CODE            PIC X(5).
               10  TEST-CHARGES              PIC 9(7)V99.
               10  PRESCRIBING-S-PHYS-ID     PIC X(08).
           05  EQUIPMENT-CHARGES-SUMMARY OCCURS 20 TIMES.
               10  EQUIPMENT-S-ID            PIC X(08).
               10  EQUIPMENT-CHARGE-DATE     PIC X(08).
               10  EQUIP-DIAG-CODE           PIC X(5).
               10  EQUIPMENT-S-SHORT-DESC    PIC X(30).
               10  EQUIPMENT-CHARGES         PIC 9(7)V99.
               10  EQUIPMENT-PRES-PHYS-ID    PIC X(08).
                                                                        01900061
019100** VSAM FILE                                                      01910001
019200                                                                  01920001
019300 01  WS-SYSOUT-REC.                                               01930001
019400     05  MSG                     PIC X(80).                       01940001
019500                                                                  01950001
019600 77  WS-DATE                     PIC 9(6) COMP-3.                 01960001
019700                                                                  01970001
019800 01  COUNTERS-AND-ACCUMULATORS.                                   01980001
019900     05 RECORDS-WRITTEN          PIC 9(7) COMP.                   01990001
020000     05 RECORDS-IN-ERROR         PIC 9(7) COMP.                   02000001
020100     05 RECORDS-READ             PIC 9(7) COMP.                   02010001
020200     05 WS-MEDICATION-CHARGES    PIC S9(8)V99 COMP-3.             02020009
020300     05 WS-PHARMACY-CHARGES      PIC S9(9)V99 COMP-3.             02030009
020400     05 WS-ANCILLARY-CHARGES     PIC S9(6)V99 COMP-3.             02040009
           05 WS-DIAGCODE-TEMP         PIC  9(6).                       02040171
020500                                                                  02050001
020600 01  MISC-WS-FLDS.                                                02060001
020700     05 STR-LTH                  PIC 9(04) VALUE 0.               02070004
020800     05 RETURN-CD                PIC S9(04) VALUE 0.              02080001
020900     05 ROW-SUB                  PIC S9(04) COMP-3.               02090009
021000                                                                  02100001
021100 01  FLAGS-AND-SWITCHES.                                          02110001
021200     05 MORE-DATA-SW             PIC X(01) VALUE "Y".             02120001
021300         88 NO-MORE-DATA VALUE "N".                               02130001
021400     05 ERROR-FOUND-SW           PIC X(01) VALUE "N".             02140001
021500         88 RECORD-ERROR-FOUND VALUE "Y".                         02150001
021600         88 VALID-RECORD  VALUE "N".                              02160001
021600         88 VALID-ROW  VALUE    "R".                              02160153
021700     05  MORE-TABLE-ROWS         PIC X(01) VALUE "Y".             02170001
021800         88 NO-MORE-TABLE-ROWS VALUE "N".                         02180001
021900                                                                  02190001
022000* COPY ABENDREC.                                                  02200031
022100** QSAM FILE                                                      02210001
      *022200 COPY ABENDREC.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST.COPYLIB\ABENDREC.cpy

      ** QSAM FILE   - 09/09/2013
       01  ABEND-REC.
           05  FILLER             PIC X(13) VALUE "ABEND PARA: ".
           05  PARA-NAME          PIC X(20).
           05  ABEND-REASON       PIC X(40).
           05  FILLER             PIC X(10) VALUE " EXPECTED:".
           05  EXPECTED-VAL       PIC 9(6).
           05  FILLER             PIC X(8) VALUE " ACTUAL:".
           05  ACTUAL-VAL         PIC 9(6).
           05  FILLER             PIC X(9) VALUE " VALUE-3:".
           05  VALUE-3            PIC 9(6).
           05  FILLER             PIC X(9) VALUE "S0CB VALS".
           05  ONE-VAL            PIC 9 VALUE 1.
           05  ZERO-VAL           PIC 9 VALUE 0.
                                                                        02220001
      *022300 COPY BADD400.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST.COPYLIB\BADD400.cpy

          SKIP1
      * ***************************************************************
      * Created: May 29, 2012 7:39:43 AM America/New_York
      * Generated by: IBM Rational Developer for System z with EGL
      * ***************************************************************
       01 BADD400I.
          02 FILLER                    PIC X(12).
          02 MyArray1D                 OCCURS 8 TIMES.
      *
             03 MyArray1L              PIC S9(4) COMP.
             03 MyArray1F              PIC X.
             03 FILLER       REDEFINES MyArray1F.
                04 MyArray1A           PIC X.
             03 MyArray1I              PIC X(20).
      *
          02 DATEL                     PIC S9(4) COMP.
          02 DATEF                     PIC X.
          02 FILLER          REDEFINES DATEF.
             03 DATEA                  PIC X.
          02 DATEI                     PIC X(8).
      *
          02 TIMEL                     PIC S9(4) COMP.
          02 TIMEF                     PIC X.
          02 FILLER          REDEFINES TIMEF.
             03 TIMEA                  PIC X.
          02 TIMEI                     PIC X(8).
      *
          02 PERSONNL                  PIC S9(4) COMP.
          02 PERSONNF                  PIC X.
          02 FILLER          REDEFINES PERSONNF.
             03 PERSONNA               PIC X.
          02 PERSONNI                  PIC X(10).
      *
          02 FNAMEL                    PIC S9(4) COMP.
          02 FNAMEF                    PIC X.
          02 FILLER          REDEFINES FNAMEF.
             03 FNAMEA                 PIC X.
          02 FNAMEI                    PIC X(12).
      *
          02 LNAMEL                    PIC S9(4) COMP.
          02 LNAMEF                    PIC X.
          02 FILLER          REDEFINES LNAMEF.
             03 LNAMEA                 PIC X.
          02 LNAMEI                    PIC X(15).
      *
          02 STREETL                   PIC S9(4) COMP.
          02 STREETF                   PIC X.
          02 FILLER          REDEFINES STREETF.
             03 STREETA                PIC X.
          02 STREETI                   PIC X(16).
      *
          02 CITYL                     PIC S9(4) COMP.
          02 CITYF                     PIC X.
          02 FILLER          REDEFINES CITYF.
             03 CITYA                  PIC X.
          02 CITYI                     PIC X(12).
      *
          02 STATEL                    PIC S9(4) COMP.
          02 STATEF                    PIC X.
          02 FILLER          REDEFINES STATEF.
             03 STATEA                 PIC X.
          02 STATEI                    PIC X(2).
      * Phone # input
          02 PHONEL                    PIC S9(4) COMP.
          02 PHONEF                    PIC X.
          02 FILLER          REDEFINES PHONEF.
             03 PHONEA                 PIC X.
          02 PHONEI                    PIC X(14).
      * Phone # input
          02 PHONEL                    PIC S9(4) COMP.
          02 PHONEF                    PIC X.
          02 FILLER          REDEFINES PHONEF.
             03 PHONEA                 PIC X.
          02 PHONEI                    PIC X(14).
      *
          02 SALARYL                   PIC S9(4) COMP.
          02 SALARYF                   PIC X.
          02 FILLER          REDEFINES SALARYF.
             03 SALARYA                PIC X.
          02 SALARYI                   PIC 9(7).
      *
          02 INSTR1L                   PIC S9(4) COMP.
          02 INSTR1F                   PIC X.
          02 FILLER          REDEFINES INSTR1F.
             03 INSTR1A                PIC X.
          02 INSTR1I                   PIC X(40).
      *
          02 INSTRL                    PIC S9(4) COMP.
          02 INSTRF                    PIC X.
          02 FILLER          REDEFINES INSTRF.
             03 INSTRA                 PIC X.
          02 INSTRI                    PIC X(40).
      *
          02 MSGL                      PIC S9(4) COMP.
          02 MSGF                      PIC X.
          02 FILLER          REDEFINES MSGF.
             03 MSGA                   PIC X.
          02 MSGI                      PIC X(40).
      * *******************************************
       01 BADD400O REDEFINES BADD400I.
          02 FILLER                    PIC X(12).
          02 MyArray1                  OCCURS 8 TIMES.
      *
             03 FILLER                 PIC X(3).
             03 MyArray1O              PIC X(20).
      *
          02 FILLER                    PIC X(3).
          02 DATEO                     PIC X(8).
      *
          02 FILLER                    PIC X(3).
          02 TIMEO                     PIC X(8).
      *
          02 FILLER                    PIC X(3).
          02 PERSONNO                  PIC X(10).
      *
          02 FILLER                    PIC X(3).
          02 FNAMEO                    PIC X(12).
      *
          02 FILLER                    PIC X(3).
          02 LNAMEO                    PIC X(15).
      *
          02 FILLER                    PIC X(3).
          02 STREETO                   PIC X(16).
      *
          02 FILLER                    PIC X(3).
          02 CITYO                     PIC X(12).
      *
          02 FILLER                    PIC X(3).
          02 STATEO                    PIC X(2).
      * Phone # input
          02 FILLER                    PIC X(3).
          02 PHONEO                    PIC X(14).
      * Phone # input
          02 FILLER                    PIC X(3).
          02 PHONEO                    PIC X(14).
      *
          02 FILLER                    PIC X(3).
          02 SALARYO                   PIC 9(7).
      *
          02 FILLER                    PIC X(3).
          02 INSTR1O                   PIC X(40).
      *
          02 FILLER                    PIC X(3).
          02 INSTRO                    PIC X(40).
      *
          02 FILLER                    PIC X(3).
          02 MSGO                      PIC X(40).
                                                                        02230001
      *022300 COPY CUST.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST.COPYLIB\CUST.cpy

      ******************************************************************
      * DCLGEN TABLE(DDS0001.CUST)                                     *
      *        LIBRARY(DDS0001.TEST.COPYLIB(CUST))                     *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CUST)                                         *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      *        DBCSDELIM(NO)                                           *
      *        COLSUFFIX(YES)                                          *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.CUST TABLE
           ( ID                             CHAR(2),
             LNAME                          CHAR(12),
             FNAME                          CHAR(6),
             CITY                           CHAR(15),
             ST                             CHAR(2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.CUST                       *
      ******************************************************************
       01  CUST.
      *    *************************************************************
           10 ID1                   PIC X(2).
      *    *************************************************************
           10 LNAME                PIC X(12).
      *    *************************************************************
           10 FNAME                PIC X(6).
      *    *************************************************************
           10 CITY                 PIC X(15).
      *    *************************************************************
           10 ST                   PIC X(2).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  ICUST.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 5 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
                                                                        02230001
      *022300 COPY CDAT3L.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST.COPYLIB\CDAT3L.cpy

       01 W-CDAT3-LINKAGE-AREA.
           10 W-CDAT3-DATE-IN.
             15 W-CDAT3-RET-YYYY              PIC X(4).
             15 W-CDAT3-RET-MM                PIC X(2).
             15 W-CDAT3-RET-DD                PIC X(2).
           10 W-CDAT3-RETIRE-DATE              PIC X(80).
           10 W-CDAT3-PROGRAM-RETCODE          PIC 9(4) VALUE 0.
              88 W-CDAT3-REQUEST-SUCCESS          VALUE 0.
           10 W-CDAT3-RETIRE-ERRMSG            PIC X(30).
                                                                        02230001
      *022300 COPY EMPL.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST.COPYLIB\EMPL.cpy

      ******************************************************************
      * DCLGEN TABLE(DDS0001.EMPL)                                     *
      *        LIBRARY(DDS0001.TEST.COPYLIB(EMPL))                     *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(EMPL)                                         *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      *        DBCSDELIM(NO)                                           *
      *        COLSUFFIX(YES)                                          *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DDS0001.EMPL TABLE
           ( NBR                            CHAR(2),
             LNAME                          CHAR(10),
             FNAME                          CHAR(8),
             DOB                            INTEGER,
             HIREDTE                        INTEGER,
             PERF                           SMALLINT,
             JOB                            CHAR(4),
             DEPT                           CHAR(3),
             PROJ                           CHAR(2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DDS0001.EMPL                       *
      ******************************************************************
       01  EMPL.
      *    *************************************************************
           10 NBR                  PIC X(2).
      *    *************************************************************
           10 LNAME                PIC X(10).
      *    *************************************************************
           10 FNAME                PIC X(8).
      *    *************************************************************
           10 DOB                  PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 HIREDTE              PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 PERF                 PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 JOB                  PIC X(4).
      *    *************************************************************
           10 DEPT                 PIC X(3).
      *    *************************************************************
           10 PROJ                 PIC X(2).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  IEMPL.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 9 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
                                                                        02230001
      *022300 COPY FUNDFILE.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST.COPYLIB\FUNDFILE.cpy

       01  FUND-FILE-LAYOUT.
          03  FUND-FILE-ID                 PIC X(8).
          03  FUND-FILE-NAME               PIC X(50).
          03  FUND-FILE-RATING             PIC X.
          03  FUND-FILE-PRICE              PIC X(15).
                                                                        02230001
022400                                                                  02240001
022500* COPY DIAGCODE.                                                  02250001
022600******************************************************************02260001
022700***** DB2 TABLE DCLGENS                                           02270001
022800 01  DCLDIAG-CODES.                                               02280001
022900     10 DIAG-CODE                      PIC X(05).                 02290001
023000     10 INS-TYPE                       PIC X(03).                 02300001
023100     10 COPAY                          PIC S9(4) COMP.            02310001
023200     10 DEDUCTIBLE                     PIC S9(4) COMP.            02320001
023300                                                                  02330001
023400 01  DCLWARD-CODES.                                               02340001
023500     10 WARD-ID                        PIC X(04).                 02350001
023600     10 PRIMARY-PHYSICIAN-ID           PIC X(08).                 02360001
023700     10 SUPERVISE-NURSE-ID             PIC X(08).                 02370001
023800     10 LOCATION                       PIC X(08).                 02380001
023900     10 NUMBER-OF-BEDS                 PIC S9(4) COMP.            02390001
024000     10 BASE-ROOM-CHARGE               PIC S9(5)V99 COMP-3.       02400001
024100                                                                  02410001
024200 01  DCLHOSP-BED.                                                 02420001
024300     10 BED-ID                         PIC X(04).                 02430001
024400     10 ROOM-ID                        PIC X(08).                 02440001
024500     10 WARD-ID                        PIC X(08).                 02450001
024600     10 SPECIAL-CHARGES                PIC S9(5)V99 COMP-3.       02460001
024700                                                                  02470001
024800 01  DCLMEDICATION.                                               02480001
024900     10 MEDICATION-ID                  PIC X(08).                 02490001
025000     10 MED-NAME                       PIC X(08).                 02500001
025100     10 SHORT-DESCRIPTION              PIC X(08).                 02510001
025200     10 COST                           PIC S9(5)V99 COMP-3.       02520001
025300     10 PHARMACY-COST                  PIC S9(3)V99 COMP-3.       02530001
025400                                                                  02540001
      *025500     EXEC SQL INCLUDE  SQLCA END-EXEC.

      * Copybook Location:
      * zserveros\DDS0001\DDS0001.TEST1.COPYLIB\SQLCA.cpy

       01 SQLCA.
           03 SQLCAID          PIC X(8).
           03 SQLCABC          PIC S9(9) COMP.
           03 SQLCODE          PIC S9(9) COMP.
           03 SQLERRM.
               49 SQLERRML     PIC S9(4) COMP.
               49 SQLERRMC     PIC X(70).
           03 SQLERRP          PIC X(8).
           03 SQLERRD OCCURS 6  PIC S9(9) COMP.
           03 SQLWARN.
               05 SQLWARN0       PIC X.
               05 SQLWARN1       PIC X.
               05 SQLWARN2       PIC X.
               05 SQLWARN3       PIC X.
               05 SQLWARN4       PIC X.
               05 SQLWARN5       PIC X.
               05 SQLWARN6       PIC X.
               05 SQLWARN7       PIC X.
           03 SQLEXT             PIC X(8).
                                                                        02550001
025600                                                                  02560001
025700 PROCEDURE DIVISION.                                              02570001
025800     PERFORM 000-HOUSEKEEPING THRU 000-EXIT.                      02580001
025900     PERFORM 100-MAINLINE THRU 100-EXIT                           02590001
026000             UNTIL NO-MORE-DATA OR                                02600001
026100******* Balancing logic put in by TGD 02/12/92                    02610001
026200             TRAILER-REC.                                         02620001
026300     PERFORM 999-CLEANUP THRU 999-EXIT.                           02630001
026400     MOVE +0 TO RETURN-CODE.                                      02640001
026500     GOBACK.                                                      02650001
026600                                                                  02660001
026700 000-HOUSEKEEPING.                                                02670001
026800     MOVE "000-HOUSEKEEPING" TO PARA-NAME.                        02680001
026900     DISPLAY "HOUSEKEEPING".                                      02690001
027000*  Code your statement here to OPEN files                         02700001
027100     ACCEPT  WS-DATE FROM DATE.                                   02710001
027200     INITIALIZE COUNTERS-AND-ACCUMULATORS.                        02720001
027300     PERFORM 800-OPEN-FILES THRU 800-EXIT.                        02730001
027400     PERFORM 900-READ-TRMTDATA THRU 900-EXIT.                     02740001
027500     DISPLAY 'MORE-DATA-SW ' MORE-DATA-SW.                        02750001
027600     IF NO-MORE-DATA                                              02760001
027700         MOVE "EMPTY INPUT FILE" TO ABEND-REASON                  02770001
027800         GO TO 1000-ABEND-RTN.                                    02780001
027900 000-EXIT.                                                        02790001
028000     EXIT.                                                        02800001
028100                                                                  02810001
028200 100-MAINLINE.                                                    02820001
028300     DISPLAY '100-MAINLINE...JS'.                                 02830055
           DISPLAY 'TRMT REC ' INPATIENT-TREATMENT-REC.                 02830155
028400     MOVE "100-MAINLINE" TO PARA-NAME.                            02840001
028500*  Validate patient type and insurance coverage                   02850001
028600     PERFORM 300-FIELD-EDITS THRU 300-EXIT.                       02860001
028700                                                                  02870001
028800     IF RECORD-ERROR-FOUND                                        02880001
028900         ADD +1 TO RECORDS-IN-ERROR                               02890001
029000         PERFORM 710-WRITE-TRMTERR THRU 710-EXIT                  02900001
029100     ELSE                                                         02910001
029200         PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.                02920001
029300     PERFORM 900-READ-TRMTDATA THRU 900-EXIT.                     02930001
029400 100-EXIT.                                                        02940001
029500     EXIT.                                                        02950001
029600                                                                  02960001
029700 300-FIELD-EDITS.                                                 02970001
029800     MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.            02980001
029900     MOVE "300-FIELD-EDITS" TO PARA-NAME.                         02990001
030000******** non-numeric fields                                       03000001
030100     IF NOT VALID-BILLABLE-TYPES IN BILLABLE-TREATMENT-IND        03010001
030200        MOVE "*** INVALID BILLABLE TYPE" TO                       03020001
030300        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03030001
030400        MOVE "Y" TO ERROR-FOUND-SW                                03040001
030500        GO TO 300-EXIT.                                           03050001
030600                                                                  03060001
030700     IF NOT VALID-TRTMNT-MODES IN TREATMENT-MODE                  03070001
030800        MOVE "*** INVALID TREATMENT MODE" TO                      03080001
030900        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03090001
031000        MOVE "Y" TO ERROR-FOUND-SW                                03100001
031100        GO TO 300-EXIT.                                           03110001
031200                                                                  03120001
031300     IF PATIENT-ID IN INPATIENT-TREATMENT-REC NOT NUMERIC         03130001
031400        MOVE "*** NON-NUMERIC PATIENT-ID" TO                      03140001
031500        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03150001
031600        MOVE "Y" TO ERROR-FOUND-SW                                03160001
031700        GO TO 300-EXIT.                                           03170001
031800                                                                  03180001
031900     IF PATIENT-ID IN INPATIENT-TREATMENT-REC = ZERO              03190001
032000        MOVE "*** INVALID (000000) PATIENT-ID" TO                 03200001
032100        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03210001
032200        MOVE "Y" TO ERROR-FOUND-SW                                03220001
032300        GO TO 300-EXIT.                                           03230001
032400                                                                  03240001
032500     IF BED-IDENTITY IN INPATIENT-TREATMENT-REC NOT NUMERIC       03250001
032600        MOVE "*** NON-NUMERIC BED-IDENTITY" TO                    03260001
032700        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03270001
032800        MOVE "Y" TO ERROR-FOUND-SW                                03280001
032900        GO TO 300-EXIT.                                           03290001
033000                                                                  03300001
033100     IF MEDICATION-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC    03310001
033200        MOVE "*** NON-NUMERIC MEDICATION-COST" TO                 03320001
033300        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03330001
033400        MOVE "Y" TO ERROR-FOUND-SW                                03340001
033500        GO TO 300-EXIT.                                           03350001
033600                                                                  03360001
033700     IF PHARMACY-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC      03370001
033800        MOVE "*** NON-NUMERIC PHARMACY COSTS" TO                  03380001
033900        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03390001
034000        MOVE "Y" TO ERROR-FOUND-SW                                03400049
034100        GO TO 300-EXIT.                                           03410001
034200                                                                  03420001
034300     IF ANCILLARY-CHARGE IN INPATIENT-TREATMENT-REC NOT NUMERIC   03430001
034400        MOVE "*** NON-NUMERIC ANCILLARY-CHARGES" TO               03440001
034500        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03450001
034600        MOVE "Y" TO ERROR-FOUND-SW                                03460049
034700        GO TO 300-EXIT.                                           03470001
034800********************                                              03480001
034900     IF ATTENDING-PHYS-ID = SPACES                                03490001
035000        MOVE "*** BLANK ATTENDING PHYSICIAN-ID" TO                03500001
035100        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03510001
035200        MOVE "Y" TO ERROR-FOUND-SW                                03520001
035300        GO TO 300-EXIT.                                           03530001
035400                                                                  03540001
035500     IF PRESCRIBING-PHYS-ID = SPACES                              03550001
035600        MOVE "*** BLANK PRESCRIBING PHYSICIAN-ID" TO              03560001
035700        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03570001
035800        MOVE "Y" TO ERROR-FOUND-SW                                03580001
035900        GO TO 300-EXIT.                                           03590001
036000                                                                  03600001
036100*    CALL WS-CALLED-MODULE USING TREATMENT-DATE, RETURN-CD.       03610026
036200*     CALL 'DTEVAL' USING TREATMENT-DATE, RETURN-CD.              03620060
036300     CALL 'DATEVAL' USING TREATMENT-DATE, RETURN-CD.              03630060
036400     IF RETURN-CD < 0                                             03640001
036500        MOVE "*** BAD DATE PORTION OF DATE-TIME" TO               03650001
036600        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03660001
036700        MOVE "Y" TO ERROR-FOUND-SW                                03670001
036800        GO TO 300-EXIT.                                           03680001
036900                                                                  03690001
037000     MOVE "Y" TO MORE-TABLE-ROWS.                                 03700001
037100     PERFORM 350-CHECK-LAB-TABLE THRU 350-EXIT VARYING ROW-SUB    03710001
037200          FROM 1 BY 1 UNTIL NO-MORE-TABLE-ROWS OR ROW-SUB = 8.    03720034
037300                                                                  03730001
037400     IF VALID-RECORD                                              03740001
037500         PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-EXIT.           03750001
037600                                                                  03760001
037700****** VERIFY TABLE (JUST TYPES AND LAB-TEST-ID)                  03770001
037800                                                                  03780001
037900 300-EXIT.                                                        03790001
038000     EXIT.                                                        03800001
038100                                                                  03810001
038200 350-CHECK-LAB-TABLE.                                             03820001
038300     IF LAB-TEST-ID(ROW-SUB) = SPACES                             03830001
038400        MOVE "N" TO MORE-TABLE-ROWS                               03840001
038500        GO TO 350-EXIT.                                           03850001
038600                                                                  03860001
038700     IF NOT VALID-CATEGORY(ROW-SUB)                               03870001
038800        MOVE "*** INVALID LAB-TEST CATEGORY" TO                   03880001
038900        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03890001
039000        MOVE "Y" TO ERROR-FOUND-SW                                03900001
039100        GO TO 350-EXIT.                                           03910001
039200                                                                  03920001
039300 350-EXIT.                                                        03930001
039400     EXIT.                                                        03940001
039500                                                                  03950001
039600                                                                  03960001
039700 400-NUMERIC-RANGE-EDITS.                                         03970001
039800     MOVE "400-NUMERIC-RANGE-EDITS" TO PARA-NAME.                 03980001
039900******** Call to VSAM file to read record                         03990001
040000     IF  (MEDICATION-COST > 99000                                 04000001
040100     OR  MEDICATION-COST < 1.01)                                  04010001
040200         MOVE "*** INVALID MEDICATION COST" TO                    04020001
040300         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04030001
040400         MOVE "Y" TO ERROR-FOUND-SW                               04040001
040500         GO TO 400-EXIT.                                          04050001
040600                                                                  04060001
040700     IF  (PHARMACY-COST IN INPATIENT-TREATMENT-REC > 990          04070001
040800     OR  PHARMACY-COST IN INPATIENT-TREATMENT-REC < .99)          04080001
040900         MOVE "*** INVALID PHARMACY COSTS" TO                     04090001
041000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04100001
041100         MOVE "Y" TO ERROR-FOUND-SW                               04110001
041200         GO TO 400-EXIT.                                          04120004
041300                                                                  04130001
041400     IF  (ANCILLARY-CHARGE > 900                                  04140001
041500     OR  ANCILLARY-CHARGE < 1.01)                                 04150001
041600         MOVE "*** INVALID ANCILLARY CHARGES" TO                  04160001
041700         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04170001
041800         MOVE "Y" TO ERROR-FOUND-SW                               04180001
041900         GO TO 400-EXIT.                                          04190001
042000                                                                  04200001
042100     IF VALID-RECORD                                              04210001
042200         PERFORM 450-CROSS-FIELD-EDITS THRU 450-EXIT.             04220001
042300                                                                  04230001
042400 400-EXIT.                                                        04240001
042500     EXIT.                                                        04250001
042600                                                                  04260001
042700 450-CROSS-FIELD-EDITS.                                           04270001
042800     MOVE "450-CROSS-FIELD-EDITS" TO PARA-NAME.                   04280001
042900******** Specific requirements for certain frocedures             04290001
043000     IF  MRI OR CAT OR CHEMO-THERAPY OR RADIATION-THERAPY         04300001
043100         OR SURGERY OR LAB-TESTS                                  04310001
043200         IF MEDICATION-COST = ZERO OR                             04320001
043300            ANCILLARY-CHARGE = ZERO                               04330001
043400         MOVE "*** INVALID $$ AMOUNTS FOR fROCEDURES" TO          04340001
043500         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04350001
043600         MOVE "Y" TO ERROR-FOUND-SW                               04360001
043700         GO TO 450-EXIT.                                          04370001
043800                                                                  04380001
043900     IF  ORAL-ADMIN OR INTRAVENOUS-ADMIN OR INJECTION             04390001
044000         IF PHARMACY-COST IN INPATIENT-TREATMENT-REC = ZERO OR    04400001
044100            ANCILLARY-CHARGE = ZERO                               04410001
044200         MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO          04420001
044300         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04430001
044400         MOVE "Y" TO ERROR-FOUND-SW                               04440001
044500         GO TO 450-EXIT.                                          04450001
044600                                                                  04460001
044700     IF  NOT OTHER-TREATMENT                                      04470032
044800         IF TREATMENT-NURSE-ID = SPACES OR                        04480001
044900            SUPERVISOR-NURSE-ID = SPACES                          04490001
045000         MOVE "*** INVALID NURSING ENTRIES" TO                    04500001
045100         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04510001
045200         MOVE "Y" TO ERROR-FOUND-SW                               04520001
045300         GO TO 450-EXIT.                                          04530001
045400                                                                  04540001
045500     IF  NOT (OTHER-TREATMENT AND LAB-TESTS)                      04550001
045600         IF TREATMENT-COMMENTS = SPACES                           04560001
045700         MOVE "*** INVALID TREATMENT COMMENTS" TO                 04570001
045800         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04580001
045900         MOVE "Y" TO ERROR-FOUND-SW                               04590001
046000         GO TO 450-EXIT.                                          04600001
046100                                                                  04610001
046200     IF  CHEMO-THERAPY OR RADIATION-THERAPY OR SURGERY            04620001
046300        MOVE +0 TO STR-LTH                                        04630001
046400*        CALL 'LENPGM' USING TREATMENT-COMMENTS, STR-LTH          04640026
046500        IF STR-LTH < 25                                           04650001
046600         MOVE "*** INVALID TREATMENT COMMENT LENGTH" TO           04660001
046700         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04670001
046800         MOVE "Y" TO ERROR-FOUND-SW                               04680001
046900         GO TO 450-EXIT.                                          04690001
047000                                                                  04700001
047100     IF VALID-RECORD                                              04710001
047200         PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.              04720001
047300                                                                  04730001
047400 450-EXIT.                                                        04740001
047500     EXIT.                                                        04750001
047600                                                                  04760001
047700 500-CROSS-FILE-EDITS.                                            04770001
047800     MOVE "500-CROSS-FILE-EDITS" TO PARA-NAME.                    04780010
048800     MOVE 'DTEVAL' TO WS-CALLED-MODULE.                           04780145
047900******** Call to VSAM file to read record                         04790001
048000     MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO                04800001
048100            PATIENT-KEY.                                          04810001
048200     READ PATMSTR INTO PATIENT-MASTER-REC.                        04820001
048300     IF  NOT RECORD-FOUND                                         04830001
048400         MOVE "*** PATIENT NOT-FOUND ON MASTER FILE" TO           04840001
048500         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04850001
048600         MOVE "Y" TO ERROR-FOUND-SW                               04860001
048700         GO TO 500-EXIT.                                          04870001
           MOVE 'ABCDEF' TO WS-CALLED-MODULE.                           04870107
036100     CALL WS-CALLED-MODULE USING TREATMENT-DATE, RETURN-CD.       04880145
                                                                        04880245
048900     IF VALID-RECORD                                              04890001
049000        PERFORM 600-DB2-TABLE-EDITS THRU 600-EXIT.                04900001
049100                                                                  04910001
049200 500-EXIT.                                                        04920001
049300     EXIT.                                                        04930001
049400                                                                  04940001
049500 600-DB2-TABLE-EDITS.                                             04950001
049600     MOVE "600-DB2-TABLE-EDITS" TO PARA-NAME.                     04960001
049700******** EXEC SQL to get info from DB2                            04970001
049800     MOVE DIAGNOSTIC-CODE-PRIMARY IN PATIENT-MASTER-REC TO        04980001
049900          DIAG-CODE IN DCLDIAG-CODES                              04990002
050000                                                                  05000001
050100****** CHECK FOR VALID DIAGNOSTIC CODE                            05010001
050200     EXEC SQL                                                     05020001
050300        SELECT DIAG_CODE INTO :DIAG-CODE                          05030001
050400        FROM DDS0001.DIAG_CODES                                   05040001
050500        WHERE DIAG_CODE = :DCLDIAG-CODES.DIAG-CODE                05050001
050600     END-EXEC.                                                    05060001
050700                                                                  05070001

050800     IF SQLCODE = -811 OR 0                                       05080001
050900         NEXT SENTENCE                                            05090001
051000     ELSE                                                         05100001
051100     IF SQLCODE = +100                                            05110001
051200         MOVE "*** DIAGNOSTIC CODE NOT-FOUND IN DIAG_CODES" TO    05120001
051300         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05130001
051400         MOVE "Y" TO ERROR-FOUND-SW                               05140001
051500         MOVE SQLCODE TO  PATIENT-ID IN INPATIENT-TREATMENT-REC   05150061
051600         MOVE DIAG-CODE IN DCLDIAG-CODES                          05160001
051700                            TO PRIMARY-DIAGNOSTIC-CODE            05170001
051800         MOVE SQLCODE TO  EXPECTED-VAL                            05180061
051900         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               05190061
052000                         TO ACTUAL-VAL                            05200061
052100         WRITE SYSOUT-REC FROM ABEND-REC                          05210001
052200         GO TO 600-EXIT                                           05220001
052300     ELSE                                                         05230001
052400     IF SQLCODE < 0                                               05240001
052500         MOVE "***  FATAL DB2 ERROR HERE" TO                      05250001
052600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05260001
052700         MOVE "Y" TO ERROR-FOUND-SW                               05270001
052800         MOVE SQLCODE TO  PATIENT-ID IN INPATIENT-TREATMENT-REC   05280061
052900         MOVE DIAG-CODE IN DCLDIAG-CODES                          05290001
053000                            TO PRIMARY-DIAGNOSTIC-CODE            05300001
053100         MOVE SQLCODE TO  EXPECTED-VAL                            05310061
053200         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               05320061
053300                         TO ACTUAL-VAL                            05330061
053400         WRITE SYSOUT-REC FROM ABEND-REC                          05340001
      *** Error to show Unreachable code                                05340171
               MOVE WS-DIAGCODE-TEMP TO DIAG-CODE IN DCLDIAG-CODES      05340273
053500         GO TO 1000-DB2-ERROR-RTN.                                05350001
053600                                                                  05360001
053700****** CHECK FOR VALID BED IDENTITY                               05370001
053800     MOVE BED-IDENTITY TO BED-ID.                                 05380001
053900     EXEC SQL                                                     05390001
054000        SELECT BED_ID INTO :BED-ID                                05400001
054100        FROM DDS0001.HOSP_BED                                     05410001
054200        WHERE BED_ID = :BED-ID                                    05420001
054300     END-EXEC.                                                    05430001

           EXEC SQL
            DECLARE CURSOR_NAME CURSOR FOR
              SELECT D.COPAY, D.DIAG_CODE FROM DIAG_CODES D
                WHERE D.COPAY > 100
           END-EXEC.
054400                                                                  05440001
054500     IF SQLCODE = -811 OR 0                                       05450001
054600         NEXT SENTENCE                                            05460001
054700     ELSE                                                         05470001
054800     IF SQLCODE = +100                                            05480001
054900         MOVE "*** BED IDENT NOT-FOUND IN HOSP_BED" TO            05490001
055000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05500001
055100         MOVE "Y" TO ERROR-FOUND-SW                               05510001
055200         MOVE SQLCODE TO  EXPECTED-VAL                            05520061
055300         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               05530061
055400                         TO ACTUAL-VAL                            05540061
055500         WRITE SYSOUT-REC FROM ABEND-REC                          05550001
055600         GO TO 600-EXIT                                           05560001
055700     ELSE                                                         05570001
055800     IF SQLCODE < 0                                               05580001
055900         MOVE "***  FATAL DB2 ERROR" TO                           05590001
056000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05600001
056100         MOVE "Y" TO ERROR-FOUND-SW                               05610001
056200         MOVE SQLCODE TO  EXPECTED-VAL                            05620061
056300         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               05630061
056400                         TO ACTUAL-VAL                            05640061
056500         WRITE SYSOUT-REC FROM ABEND-REC                          05650001
056600         GO TO 1000-DB2-ERROR-RTN                                 05660036
056700     ELSE                                                         05670036
           IF SQLCODE = -803 PERFORM 852-WRITE-DUP-IDX-ERR              05670136
                   THRU 852-EXIT.                                       05670236
056800****** CHECK FOR VALID PHYSICIAN-ID                               05680001
056900     MOVE ATTENDING-PHYS-ID TO PRIMARY-PHYSICIAN-ID.              05690001
057000     EXEC SQL                                                     05700001
057100        SELECT PRIMARY_PHYSICIAN_ID INTO :PRIMARY-PHYSICIAN-ID    05710029
057200        FROM DDS0001.WARD_DATA                                    05720029
057300        WHERE PRIMARY_PHYSICIAN_ID = :PRIMARY-PHYSICIAN-ID        05730029
057500     END-EXEC.                                                    05750001
057600                                                                  05760001
057700     IF SQLCODE = -811 OR 0                                       05770001
057800         NEXT SENTENCE                                            05780001
057900     ELSE                                                         05790001
058000     IF SQLCODE = +100                                            05800001
058100         MOVE "*** ATTENDING PHYSICIAN NOT FOUND IN TABLE" TO     05810001
058200         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05820001
058300         MOVE "Y" TO ERROR-FOUND-SW                               05830001
058400         MOVE SQLCODE TO  EXPECTED-VAL                            05840061
058500         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               05850061
058600                         TO ACTUAL-VAL                            05860061
058700         WRITE SYSOUT-REC FROM ABEND-REC                          05870001
058800         GO TO 600-EXIT                                           05880001
058900     ELSE                                                         05890001
059000         MOVE "***  FATAL DB2 ERROR" TO                           05900001
059100         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05910001
059200         MOVE "Y" TO ERROR-FOUND-SW                               05920001
059300         MOVE SQLCODE TO  EXPECTED-VAL                            05930061
059400         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               05940061
059500                         TO ACTUAL-VAL                            05950061
059600         WRITE SYSOUT-REC FROM ABEND-REC                          05960001
059700         GO TO 1000-DB2-ERROR-RTN.                                05970001
059800                                                                  05980001
059900****** CHECK FOR VALID MEDICATION-ID                              05990001
060000     MOVE MEDICATION-ID IN INPATIENT-TREATMENT-REC TO             06000001
060100            MEDICATION-ID IN DCLMEDICATION.                       06010001
060200                                                                  06020001
060300     EXEC SQL                                                     06030001
060400        SELECT MEDICATION_ID                                      06040001
060500                       INTO :DCLMEDICATION.MEDICATION-ID          06050001
060600        FROM DDS0001.MEDICATION                                   06060001
060700        WHERE MEDICATION_ID = :DCLMEDICATION.MEDICATION-ID        06070001
060800     END-EXEC.                                                    06080001
060900***********************                                           06090001
061000     IF SQLCODE = -811 OR 0                                       06100001
061100         NEXT SENTENCE                                            06110001
061200     ELSE                                                         06120001
061300     IF SQLCODE = +100                                            06130001
061400         MOVE "*** MEDICATION-ID NOT FOUND IN TABLE" TO           06140001
061500         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06150001
061600         MOVE "Y" TO ERROR-FOUND-SW                               06160001
061700         MOVE SQLCODE TO  EXPECTED-VAL                            06170061
061800         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               06180061
061900                         TO ACTUAL-VAL                            06190061
062000         WRITE SYSOUT-REC FROM ABEND-REC                          06200001
062100         GO TO 600-EXIT                                           06210001
062200     ELSE                                                         06220001
062300     IF SQLCODE < 0                                               06230001
062400         MOVE "***  FATAL DB2 ERROR" TO                           06240001
062500         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06250001
062600         MOVE "Y" TO ERROR-FOUND-SW                               06260001
062700         MOVE SQLCODE TO  EXPECTED-VAL                            06270061
062800         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               06280061
062900                         TO ACTUAL-VAL                            06290061
063000         WRITE SYSOUT-REC FROM ABEND-REC                          06300001
063100         GO TO 1000-DB2-ERROR-RTN.                                06310001
063200                                                                  06320001
063300****** CHECK FOR VALID SUPERVISOR NURSE-ID                        06330001
063400     MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.              06340001
063500     EXEC SQL                                                     06350001
063600        SELECT SUPERVISE_NURSE_ID                                 06360001
063700                       INTO :SUPERVISE-NURSE-ID                   06370001
063800        FROM DDS0001.WARD_DATA                                    06380001
063900        WHERE SUPERVISE_NURSE_ID = :SUPERVISE-NURSE-ID            06390001
064000     END-EXEC.                                                    06400001
064100                                                                  06410001
064200     IF SQLCODE = -811 OR 0                                       06420001
064300         NEXT SENTENCE                                            06430001
064400     ELSE                                                         06440001
064500     IF SQLCODE = +100                                            06450001
064600         MOVE "*** SUPERVISOR NURSE NOT FOUND" TO                 06460001
064700         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06470001
064800         MOVE "Y" TO ERROR-FOUND-SW                               06480001
064900         MOVE SQLCODE TO  EXPECTED-VAL                            06490061
065000         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               06500061
065100                         TO ACTUAL-VAL                            06510061
065200         WRITE SYSOUT-REC FROM ABEND-REC                          06520001
065300         GO TO 600-EXIT                                           06530001
065400     ELSE                                                         06540001
065500     IF SQLCODE < 0                                               06550001
065600         MOVE "*** FATAL DB2 ERROR" TO                            06560001
065700         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06570001
065800         MOVE "Y" TO ERROR-FOUND-SW                               06580001
065900         MOVE SQLCODE TO  EXPECTED-VAL                            06590061
066000         MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC               06600061
066100                         TO ACTUAL-VAL                            06610061
066200         WRITE SYSOUT-REC FROM ABEND-REC                          06620001
066300         GO TO 1000-DB2-ERROR-RTN.                                06630001
066400 600-EXIT.                                                        06640001
066500     EXIT.                                                        06650001
066600                                                                  06660001
066700 700-WRITE-TRMTEDIT.                                              06670001
066800     MOVE "700-WRITE-TRMTEDIT" TO PARA-NAME.                      06680001
066900                                                                  06690001
067000     WRITE INPATIENT-TREATMENT-REC-EDIT                           06700001
067100         FROM INPATIENT-TREATMENT-REC.                            06710001
067200     ADD MEDICATION-COST  TO WS-MEDICATION-CHARGES.               06720001
067300     ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.                06730001
067400     ADD PHARMACY-COST IN INPATIENT-TREATMENT-REC                 06740001
067500                          TO WS-PHARMACY-CHARGES.                 06750001
067600     ADD +1 TO RECORDS-WRITTEN.                                   06760001
067700 700-EXIT.                                                        06770001
067800     EXIT.                                                        06780001
067900                                                                  06790001
068000 710-WRITE-TRMTERR.                                               06800001
068100     MOVE INPATIENT-TREATMENT-REC TO REST-OF-REC.                 06810001
068200     WRITE INPATIENT-TREATMENT-REC-ERR.                           06820001
068300     ADD +1 TO RECORDS-IN-ERROR.                                  06830001
068400 710-EXIT.                                                        06840001
068500     EXIT.                                                        06850001
068600                                                                  06860001
068700 800-OPEN-FILES.                                                  06870001
068800     DISPLAY '800-OPEN-FILES..'.                                  06880074
068900     MOVE "800-OPEN-FILES" TO PARA-NAME.                          06890001
069000     OPEN INPUT TRMTDATA.                                         06900001
069100     OPEN OUTPUT TRMTEDIT, SYSOUT, TRMTERR.                       06910001
069200     OPEN I-O PATMSTR.                                            06920001
069300 800-EXIT.                                                        06930001
069400     EXIT.                                                        06940001
069500                                                                  06950001
069600 850-CLOSE-FILES.                                                 06960001
069700     MOVE "850-CLOSE-FILES" TO PARA-NAME.                         06970001
069800     CLOSE TRMTDATA,                                              06980001
069900           TRMTEDIT, SYSOUT, TRMTERR,                             06990001
070000           PATMSTR.                                               07000001
070100 850-EXIT.                                                        07010001
070200     EXIT.                                                        07020001
070300                                                                  07030001
069600 851-WRITE-AUDIT-LOGS.                                            07030135
069700     MOVE "851-CLOSE-FILES" TO PARA-NAME.                         07030235
           WRITE SYSOUT-REC AFTER ADVANCING 3.                          07030336
      ***** END LOG/AUDIT TRAIL                                         07030535
070100 851-EXIT.                                                        07030635
070200     EXIT.                                                        07030735
070300                                                                  07030835
069600 852-WRITE-DUP-IDX-ERR.                                           07030936
      *    IF SQLCODE = -803 GO TO 852-EXIT.                            07031036
069700*    MOVE "852-WRITE-DUP-IDX-ERR" TO PARA-NAME.                   07031136
      *    WRITE SYSOUT-REC AFTER ADVANCING 1.                          07031236
      ***** END LOG/AUDIT TRAIL                                         07031336
070100 852-EXIT.                                                        07031436
070200     EXIT.                                                        07031536
070300                                                                  07031636
069600 853-CLOSE-PGM.                                                   07031736
      *    IF SQLCODE = -803 GO TO 852-WRITE-DUP-IDX-ERR.               07031836
069700     MOVE "853-WRITE-DUP-IDX-ERR" TO PARA-NAME.                   07031936
      *    GOBACK.                                                      07032036
           WRITE SYSOUT-REC AFTER ADVANCING 1.                          07032136
           EXEC CICS
             LINK PROGRAM ('CDAT1')
           END-EXEC.
           EXEC CICS
             XCTL PROGRAM ('CDAT2')
           END-EXEC.
      ***** END LOG/AUDIT TRAIL                                         07032236
070100 853-EXIT.                                                        07032336
070200     EXIT.                                                        07032436
070300                                                                  07032536
070400 900-READ-TRMTDATA.                                               07040001
070500     DISPLAY '900-READ-TRMTDATA...'.                              07050001
070600*  Code your statements here to read the input file               07060001
070700*  Remember to move "NO" to IFCODE if the input file is AT END    07070001
070800     READ TRMTDATA  INTO INPATIENT-TREATMENT-REC                  07080001
070900         AT END MOVE "N" TO MORE-DATA-SW                          07090001
071000         GO TO 900-EXIT                                           07100001
071100     END-READ                                                     07110001
071200     MOVE "N" TO ERROR-FOUND-SW.                                  07120001
071300     ADD +1 TO RECORDS-READ.                                      07130001
071400     DISPLAY '900.1 RECORDS-READ = ' RECORDS-READ.                07140001
071500 900-EXIT.                                                        07150001
071600     EXIT.                                                        07160001
071700                                                                  07170001
071800 999-CLEANUP.                                                     07180001
071900     MOVE "999-CLEANUP" TO PARA-NAME.                             07190001
072000*  Final file-handling edits and trailer record handling          07200001
072100     IF NOT TRAILER-REC                                           07210001
072200         MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON  07220001
072300         GO TO 1000-ABEND-RTN.                                    07230001
072400                                                                  07240001
072500     MOVE INPATIENT-TREATMENT-REC-DATA TO WS-TRAILER-REC.         07250001
072600                                                                  07260001
072700     IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT                 07270001
072800         MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"        07280001
072900                               TO ABEND-REASON                    07290001
073000         GO TO 1000-ABEND-RTN.                                    07300001
073100                                                                  07310001
073200                                                                  07320001
073300     IF WS-ANCILLARY-CHARGES NOT EQUAL TO IN-ANCILLARY-CHARGES    07330001
073400         MOVE "** ANCILLARY CHARGES OUT OF BALANCE"               07340001
073500                               TO ABEND-REASON                    07350001
073600         MOVE WS-ANCILLARY-CHARGES TO EXPECTED-VAL                07360001
073700         MOVE IN-ANCILLARY-CHARGES                                07370001
073800                TO ACTUAL-VAL                                     07380001
073900         DISPLAY "** ANCILLARY CHARGES IN **"                     07390001
074000         DISPLAY WS-ANCILLARY-CHARGES                             07400001
074100         DISPLAY "** ANCILLARY CHARGES EXPECTED **"               07410001
074200         DISPLAY  IN-ANCILLARY-CHARGES.                           07420001
074300                                                                  07430001
074400     IF WS-MEDICATION-CHARGES  NOT EQUAL TO IN-MEDICATION-CHARGES 07440001
074500         MOVE "** MEDICATION CHARGES OUT OF BALANCE"              07450001
074600                               TO ABEND-REASON                    07460001
074700         DISPLAY "** MEDICATION CHARGES IN **"                    07470001
074800         DISPLAY WS-MEDICATION-CHARGES                            07480001
074900         DISPLAY "** MEDICATION CHARGES EXPECTED **"              07490001
075000         DISPLAY  IN-MEDICATION-CHARGES.                          07500001
075100                                                                  07510001
075200     IF WS-PHARMACY-CHARGES  NOT EQUAL TO IN-PHARMACY-CHARGES     07520001
075300         MOVE "** PHARMACY CHARGES OUT OF BALANCE"                07530001
075400                               TO ABEND-REASON                    07540001
075500         DISPLAY "** PHARMACY CHARGES IN **"                      07550001
075600         DISPLAY WS-PHARMACY-CHARGES                              07560001
075700         DISPLAY "** PHARMACY CHARGES EXPECTED **"                07570001
075800         DISPLAY  IN-PHARMACY-CHARGES.                            07580001
075900                                                                  07590001
076000     MOVE "T" TO RECORD-TYPE.                                     07600001
076100     ADD +1 TO RECORDS-WRITTEN.                                   07610001
076200     MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.                     07620001
076300     MOVE WS-ANCILLARY-CHARGES TO IN-ANCILLARY-CHARGES.           07630001
076400     MOVE WS-MEDICATION-CHARGES TO IN-MEDICATION-CHARGES.         07640001
076500     MOVE WS-PHARMACY-CHARGES TO IN-PHARMACY-CHARGES.             07650001
076600     WRITE INPATIENT-TREATMENT-REC-EDIT FROM WS-TRAILER-REC.      07660001
076700                                                                  07670001
076800*  Code the statement to close all files                          07680001
076900     PERFORM 850-CLOSE-FILES THRU 850-EXIT.                       07690001
077000                                                                  07700001
077100     DISPLAY "** RECORDS READ **".                                07710001
077200     DISPLAY RECORDS-READ.                                        07720001
077300     DISPLAY "** RECORD-IN EXPECTED **".                          07730001
077400     DISPLAY  IN-RECORD-COUNT.                                    07740001
077500     DISPLAY "** RECORDS WRITTEN **".                             07750001
077600     DISPLAY  RECORDS-WRITTEN.                                    07760001
077700     DISPLAY "** ERROR RECORDS FOUND **".                         07770001
077800     DISPLAY  RECORDS-IN-ERROR.                                   07780001
077900                                                                  07790001
078000*  Code the statement to Display a successful end-of-job msg      07800001
078100     DISPLAY "******** NORMAL END OF JOB TRTMNT ********".        07810001
078200 999-EXIT.                                                        07820001
078300     EXIT.                                                        07830001
078400                                                                  07840001
078500 1000-ABEND-RTN.                                                  07850001
078600     WRITE SYSOUT-REC FROM ABEND-REC.                             07860001
078700     PERFORM 850-CLOSE-FILES THRU 850-EXIT.                       07870001
078800     DISPLAY "*** ABNORMAL END OF JOB - TRTMNT ***" UPON CONSOLE. 07880001
078900     DIVIDE ZERO-VAL INTO ONE-VAL.                                07890059
079000                                                                  07900001
079100 1000-DB2-ERROR-RTN.                                              07910001
079200************************************************************      07920001
079300*       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *      07930001
079400************************************************************      07940001
079500                                                                  07950001
079600      DISPLAY '**** WE HAVE A SERIOUS PROBLEM HERE *****'.        07960001
079700      DISPLAY '999-ERROR-TRAP-RTN '.                              07970001
079800      MULTIPLY SQLCODE BY -1 GIVING SQLCODE.                      07980001
079900      DISPLAY 'SQLCODE ==> ' SQLCODE.                             07990001
080000      DISPLAY SQLCA.                                              08000001
080100      DISPLAY SQLERRM.                                            08010001
080200      EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.               08020001
080300      EXEC SQL ROLLBACK WORK END-EXEC.                            08030001
080400      GO TO 1000-ABEND-RTN.                                       08040001