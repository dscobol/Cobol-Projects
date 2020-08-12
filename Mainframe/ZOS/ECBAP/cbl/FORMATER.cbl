       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FORMATER.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV CENTER.
       DATE-WRITTEN. 01/23/88.
       DATE-COMPILED. 01/23/88.
       SECURITY. CONFIDENTIAL PATIENT DATA.

      ******************************************************************
      ******************************************************************
      *REMARKS.
      *
      *          THIS PROGRAM EDITS A DAILY TREATMENT TRANSACTION FILE
      *          PRODUCED BY DATA ENTRY OPERATORS FROM CICS SCREENS
      *
      *          IT CONTAINS EVERY TREATMENT FOR EVERY PATIENT IN THE
      *          HOSPITAL.
      *
      *          THE PROGRAM EDITS EACH RECORD AGAINST A NUMBER OF
      *          CRITERIA, BALANCES FINAL TOTALS AND WRITES GOOD
      *          RECORDS TO AN OUTPUT FILE
      *
      ******************************************************************

               INPUT FILE              -   DDS0001.TRMTDATA

               VSAM MASTER FILE        -   DDS0001.PATMASTR

               INPUT ERROR FILE        -   DDS0001.TRMTERR

               OUTPUT FILE PRODUCED    -   DDS001.TRMTEDIT

               DUMP FILE               -   SYSOUT

      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSOUT
           ASSIGN TO UT-S-SYSOUT
             ORGANIZATION IS SEQUENTIAL.

           SELECT TRMTDATA
           ASSIGN TO UT-S-TRMTDATA
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT TRMTEDIT
           ASSIGN TO UT-S-TRMTEDIT
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT TRMTERR
           ASSIGN TO UT-S-TRMTERR
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATMSTR
                  ASSIGN       TO PATMSTR
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS PATIENT-KEY
                  FILE STATUS  IS PATMSTR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SYSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 130 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SYSOUT-REC.
       01  SYSOUT-REC  PIC X(130).

      ****** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM
      ****** IT CONSISTS OF ALL PATIENT TREATMENTS ENTERED
      ****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS
      ****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND
       FD  TRMTDATA
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1101 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-TREATMENT-REC-DATA.
       01  INPATIENT-TREATMENT-REC-DATA PIC X(1101).

      ****** THIS FILE IS WRITTEN FOR ALL TREATMENT RECORDS THAT PASS
      ****** THE PROGRAM'S EDIT ROUTINES
      ****** THE TRAILER RECORD SHOULD ONLY CARRY THE NUMBER OF
      ****** RECORDS IN THE FILE ON TO THE NEXT JOB STEP
       FD  TRMTEDIT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 1101 CHARACTERS
           DATA RECORD IS INPATIENT-TREATMENT-REC-EDIT.
       01  INPATIENT-TREATMENT-REC-EDIT PIC X(1101).

       FD  TRMTERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1141 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-TREATMENT-REC-ERR.
       01  INPATIENT-TREATMENT-REC-ERR.
          05  ERR-MSG                     PIC X(40).
          05  REST-OF-REC                 PIC X(1101).

       FD  PATMSTR
           RECORD CONTAINS 2964 CHARACTERS
           DATA RECORD IS PATIENT-MASTER-REC.
       01  PATMSTR-REC.
          05 PATIENT-KEY      PIC X(06).
          05 FILLER           PIC X(2958).

      ** QSAM FILE
       WORKING-STORAGE SECTION.
       01  FILLER                     PIC X(32) VALUE
              '* WORKING STORAGE BEGINS HERE *'.

       01  FILLER                     PIC X(32) VALUE
                   '****** DUMP MSG ****************'.
      *****************************************************************
      *    DUMP POINTER AREA
      *        PARA POINTER- MOVE PARAGRAPH NUMBER TO THIS POINTER    *
      *                      AS EACH PARAGRAPH IS ENTERED. DO NOT     *
      *                      MOVE PARAGRAPH NUMBERS OF COMMON
      *                      PARAGRAPHS (USE COMM POINTER).
      *                                                               *
      *        COMM POINTER - EACH COMMON PARAGRAPH SHOULD MOVE       *
      *                       ITS PARAGRAPH NUMBER TO THIS POINTER    *
      *                       AT IT INCEPTION.
      *                                                               *
      *****************************************************************
       01  DUMP-LOCATOR.
          05 FILLER             PIC X(32)
               VALUE '>>>>>>> WS DUMP POINTERS >>>>>>>'.
          05 FILLER             PIC X(16)   VALUE 'Z PARA POINTER'.
          05 PARA-POINTER       PIC X(8)    VALUE SPACES.
          05 FILLER             PIC X(8)    VALUE '       Z'.
          05 FILLER             PIC X(16)   VALUE 'Z COMM POINTER'.
          05 COMM-POINTER       PIC X(8)    VALUE SPACES.
          05 FILLER             PIC X(32)
                   VALUE '<<<<<<< WS DUMP POINTERS <<<<<<<'.

       01  DUMP-DISPLAY.
          05 DUMP-STATUS               PIC X(3)  VALUE SPACES.
          05 DUMP-MESSAGE              PIC X(61) VALUE 'NO MSG'.

       01  FILE-STATUS-CODES.
          05  PATMSTR-STATUS          PIC X(2).
             88 RECORD-FOUND         VALUE "00".
             88 PATMSTR-NOT-FOUND    VALUE "23".
          05  OFCODE                  PIC X(2).
             88 CODE-WRITE    VALUE SPACES.

       COPY TREATMNT.

       01  WS-TRAILER-REC.
          05  FILLER                  PIC X(1).
          05  IN-RECORD-COUNT         PIC 9(9).
          05  FILLER                  PIC X(1).
          05  IN-MEDICATION-CHARGES   PIC S9(9)V99.
          05  IN-PHARMACY-CHARGES     PIC S9(7)V99.
          05  IN-ANCILLARY-CHARGES    PIC S9(5)V99.

       01  WS-OUTPUT-REC.
          05  PATIENT-NBR-O           PIC 9(6).
          05  FILLER                  PIC X(2) VALUE SPACES.
          05  PATIENT-NAME-O          PIC X(20).
          05  PATIENT-PHONE-O         PIC X(10).
          05  FILLER                  PIC X(2) VALUE SPACES.
          05  PATIENT-TYPE-O          PIC X(2).
          05  BED-IDENTITY-O          PIC ZZZ9.
          05  FILLER                  PIC X(2) VALUE SPACES.
          05  CURR-DATE-O             PIC X(6).
          05  FILLER                  PIC X(2) VALUE SPACES.
          05  PATIENT-AMT-PER-DAY-O   PIC $$,$$9.99.
          05  FILLER                  PIC X(2) VALUE SPACES.
          05  INS-COVERAGE-PERC-O     PIC 999.
          05  FILLER                  PIC X(2) VALUE SPACES.
          05  INS-TYPE-O              PIC X(4).
          05  HOSPITAL-STAY-LTH-O     PIC 999.
          05  FILLER                  PIC X(7) VALUE SPACES.

       COPY PATMSTR.
      ** VSAM FILE

       01  WS-SYSOUT-REC.
          05  MSG                     PIC X(80).

       77  WS-DATE                     PIC 9(6).

       01  COUNTERS-AND-ACCUMULATORS.
          05 RECORDS-WRITTEN          PIC 9(7) COMP.
          05 RECORDS-IN-ERROR         PIC 9(7) COMP.
          05 RECORDS-READ             PIC 9(7) COMP.
          05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.
          05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.
          05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.

       01  MISC-WS-FLDS.
          05 STR-LTH                  PIC 9(04) VALUE 0.
          05 RETURN-CD                PIC S9(04) VALUE 0.
          05 ROW-SUB                  PIC 9(02).

       01  FLAGS-AND-SWITCHES.
          05 MORE-DATA-SW             PIC X(01) VALUE "Y".
             88 NO-MORE-DATA VALUE "N".
          05 ERROR-FOUND-SW           PIC X(01) VALUE "N".
             88 RECORD-ERROR-FOUND VALUE "Y".
             88 VALID-RECORD  VALUE "N".
          05  MORE-TABLE-ROWS         PIC X(01) VALUE "Y".
             88 NO-MORE-TABLE-ROWS VALUE "N".

      * COPY ABENDREC.
      ** QSAM FILE
       COPY ABENDREC.

      * COPY DIAGCODE.
      ******************************************************************
      ***** DB2 TABLE DCLGENS
       01  DCLDIAG-CODES.
          10 DIAG-CODE                   PIC X(05).
          10 INS-TYPE                    PIC X(03).
          10 COPAY                       PIC S9(4) COMP.
          05 OTHER-VARS.
          10 DEDUCTIBLE                  PIC S9(4) COMP.
          05 VAR2                           PIC X(40).
          15 VAR3                        PIC X(40).
          05 VAR4                           PIC X(40).
          20 VAR5                        PIC X(40).
          10 VAR6                           PIC X(8).
          88 MOOSE-AND-SQUIRREL    VALUE 'ROCKY   '.


       01  DCLWARD-CODES.
          10 WARD-ID                        PIC X(04).
          10 PRIMARY-PHYSICIAN-ID           PIC X(08).
          10 SUPERVISE-NURSE-ID             PIC X(08).
          10 LOCATION                       PIC X(08).
          10 NUMBER-OF-BEDS                 PIC S9(4) COMP.
          10 BASE-ROOM-CHARGE               PIC S9(5)V99 COMP-3.

       01  DCLHOSP-BED.
          10 BED-ID                         PIC X(04).
          10 ROOM-ID                        PIC X(08).
          10 WARD-ID                        PIC X(08).
          10 SPECIAL-CHARGES                PIC S9(5)V99 COMP-3.

       01  DCLMEDICATION.
          10 MEDICATION-ID                  PIC X(08).
          10 MED-NAME                       PIC X(08).
          10 SHORT-DESCRIPTION              PIC X(08).
          10 COST                           PIC S9(5)V99 COMP-3.
          10 PHARMACY-COST                  PIC S9(3)V99 COMP-3.

       COPY SQLCA.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
           UNTIL NO-MORE-DATA OR
      ******* Balancing logic put in by TGD 02/12/92
           TRAILER-REC.
           PERFORM 999-CLEANUP THRU 999-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE "000-HOUSEKEEPING" TO PARA-NAME.
           DISPLAY "HOUSEKEEPING".
      *  Code your statement here to OPEN files
      *    ACCEPT  WS-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.
           INITIALIZE COUNTERS-AND-ACCUMULATORS.
           PERFORM 800-OPEN-FILES THRU 800-EXIT.
           PERFORM 900-READ-TRMTDATA THRU 900-EXIT.
           IF NO-MORE-DATA
              MOVE "EMPTY INPUT FILE" TO ABEND-REASON
              GO TO 1000-ABEND-RTN.
       000-EXIT.
           EXIT.

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
      *     DISPLAY "100-MAINLINE".
      *  Validate patient type and insurance coverage
           PERFORM 300-FIELD-EDITS THRU 300-EXIT.
      *     GO TO 300-FIELD-EDITS.

           IF RECORD-ERROR-FOUND
              ADD +1 TO RECORDS-IN-ERROR
              PERFORM 710-WRITE-TRMTERR THRU 710-EXIT
           ELSE
              PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.
           PERFORM 900-READ-TRMTDATA THRU 900-EXIT.
       100-EXIT.
           EXIT.

       300-FIELD-EDITS.
           MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.
           MOVE "300-FIELD-EDITS" TO PARA-NAME.

           IF PRESCRIBING-PHYS-ID = SPACES
              MOVE "*** BLANK PRESCRIBING PHYSICIAN-ID" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           CALL 'DATEVAL' USING TREATMENT-DATE, RETURN-CD.
           IF RETURN-CD < 0
              MOVE "*** BAD DATE PORTION OF DATE-TIME" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 300-EXIT.

           MOVE "Y" TO MORE-TABLE-ROWS.
           PERFORM 350-CHECK-LAB-TABLE THRU 350-EXIT VARYING ROW-SUB
           FROM 1 BY 1 UNTIL NO-MORE-TABLE-ROWS OR ROW-SUB = 12.

           IF VALID-RECORD
              PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-EXIT.

       300-EXIT.
           EXIT.

       350-CHECK-LAB-TABLE.
           IF LAB-TEST-ID(ROW-SUB) = SPACES
              MOVE "N" TO MORE-TABLE-ROWS
              GO TO 350-EXIT.

           IF NOT VALID-CATEGORY(ROW-SUB)
              MOVE "*** INVALID LAB-TEST CATEGORY" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              GO TO 350-EXIT.

       350-EXIT.
           EXIT.


       400-NUMERIC-RANGE-EDITS.
           MOVE "400-NUMERIC-RANGE-EDITS" TO PARA-NAME.
      ******** CALL TO VSAM FILE TO READ DIAG RECORD
           IF (PHARMACY-COST IN INPATIENT-TREATMENT-REC > 990)
               IF (MEDICATION-COST > 9900.0
                  OR MEDICATION-COST < 1.01)
                   MOVE "*** INVALID MEDICATION COST" TO
                      ERR-MSG IN INPATIENT-TREATMENT-REC-ERR.
           MOVE "Y" TO ERROR-FOUND-SW
           PERFORM 710-WRITE-TRMTERR THRU 710-EXIT
           GO TO 400-EXIT
           IF (PHARMACY-COST IN INPATIENT-TREATMENT-REC > 880)
               IF (ANCILLARY-CHARGE > 900 AND ERROR-FOUND-SW = 'N')
                   IF LAB-TEST-ID(ROW-SUB) AND NOT VALID-CATEGORY
                      (ROW-SUB)
                      OR PHARMACY-COST IN INPATIENT-TREATMENT-REC < .88
                       MOVE "*** INVALID PHARMACY COSTS" TO
                          ERR-MSG IN INPATIENT-TREATMENT-REC-ERR.
           IF (ANCILLARY-CHARGE > 100)
           NEXT SENTENCE
           ELSE
               IF (ANCILLARY-CHARGE > 1000
                  OR ANCILLARY-CHARGE < 1.01)
                   MOVE "Y" TO ERROR-FOUND-SW
                   GO TO 400-EXIT.
           IF VALID-RECORD
               PERFORM 450-CROSS-FIELD-EDITS THRU 450-EXIT.

       400-EXIT.
           EXIT.

       450-CROSS-FIELD-EDITS.
           MOVE "450-CROSS-FIELD-EDITS" TO PARA-NAME.
      ******** Specific requirements for certain frocedures
           IF  MRI OR CAT OR CHEMO-THERAPY OR RADIATION-THERAPY
           OR SURGERY OR LAB-TESTS
              IF MEDICATION-COST = ZERO OR
              ANCILLARY-CHARGE = ZERO
                 MOVE "*** INVALID $$ AMOUNTS FOR fROCEDURES" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 GO TO 450-EXIT.

           IF  ORAL-ADMIN OR INTRAVENOUS-ADMIN OR INJECTION
              IF PHARMACY-COST IN INPATIENT-TREATMENT-REC = ZERO OR
              ANCILLARY-CHARGE = ZERO
                 MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 GO TO 450-EXIT.

           IF  NOT OTHER-TREATMENT
              IF TREATMENT-NURSE-ID = SPACES OR
              SUPERVISOR-NURSE-ID = SPACES
                 MOVE "*** INVALID NURSING ENTRIES" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 GO TO 450-EXIT.

           IF  NOT (OTHER-TREATMENT AND LAB-TESTS)
              IF TREATMENT-COMMENTS = SPACES
                 MOVE "*** INVALID TREATMENT COMMENTS" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 GO TO 450-EXIT.

           IF  CHEMO-THERAPY OR RADIATION-THERAPY OR SURGERY
              MOVE +0 TO STR-LTH
              CALL 'STRLTH' USING TREATMENT-COMMENTS, STR-LTH
              IF STR-LTH < 25
                 MOVE "*** INVALID TREATMENT COMMENT LENGTH" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 PERFORM 710-WRITE-TRMTERR THRU 710-EXIT
                 GO TO 450-EXIT.

           IF VALID-RECORD
              PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.

       450-EXIT.
           EXIT.

       500-CROSS-FILE-EDITS.
           MOVE "500-CROSS-FILE-EDITS" TO PARA-NAME.
      ******** Call to VSAM file to read record
           MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO
           PATIENT-KEY.
           READ PATMSTR INTO PATIENT-MASTER-REC.
           IF  NOT RECORD-FOUND
              MOVE "*** PATIENT NOT-FOUND ON MASTER FILE" TO
              ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
              MOVE "Y" TO ERROR-FOUND-SW
              PERFORM 710-WRITE-TRMTERR THRU 700-EXIT
              GO TO 500-EXIT.

           IF VALID-RECORD
              PERFORM 600-DB2-TABLE-EDITS THRU 600-EXIT.

       500-EXIT.
           EXIT.

       600-DB2-TABLE-EDITS.
           MOVE "600-DB2-TABLE-EDITS" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE DIAGNOSTIC-CODE-PRIMARY IN PATIENT-MASTER-REC TO
           DIAG-CODE IN DCLDIAG-CODES.

      ****** CHECK FOR VALID DIAGNOSTIC CODE
           EXEC SQL
                SELECT DIAG_CODE INTO :DIAG-CODE
                FROM DDS0001.DIAG_CODES
                WHERE DIAG_CODE = :DIAG-CODE
                END-EXEC.

           IF SQLCODE = -811 OR 0
              NEXT SENTENCE
           ELSE
              IF SQLCODE = +100
                 MOVE "*** DIAGNOSTIC CODE NOT-FOUND IN DIAG_CODES" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 MOVE SQLCODE TO  PATIENT-ID IN INPATIENT-TREATMENT-REC
                 MOVE DIAG-CODE IN DCLDIAG-CODES
                 TO PRIMARY-DIAGNOSTIC-CODE
                 MOVE SQLCODE TO  EXPECTED-VAL
                 MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                 TO ACTUAL-VAL
                 WRITE SYSOUT-REC FROM ABEND-REC
                 GO TO 600-EXIT
              ELSE
                 IF SQLCODE < 0
                    MOVE "***  FATAL DB2 ERROR" TO
                    ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                    MOVE "Y" TO ERROR-FOUND-SW
                  MOVE SQLCODE TO  PATIENT-ID IN INPATIENT-TREATMENT-REC
                    MOVE DIAG-CODE IN DCLDIAG-CODES
                    TO PRIMARY-DIAGNOSTIC-CODE
                    MOVE SQLCODE TO  EXPECTED-VAL
                    MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                    TO ACTUAL-VAL
                    WRITE SYSOUT-REC FROM ABEND-REC
                    GO TO 1000-DB2-ERROR-RTN.

      ****** CHECK FOR VALID BED IDENTITY
           MOVE BED-IDENTITY TO BED-ID.
           EXEC SQL
                SELECT BED_ID INTO :BED-ID
                FROM DDS0001.HOSP_BED
                WHERE BED_ID = :BED-ID
                END-EXEC.

           IF SQLCODE = -811 OR 0
              NEXT SENTENCE
           ELSE
              IF SQLCODE = +100
                 MOVE "*** BED IDENT NOT-FOUND IN HOSP_BED" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 MOVE SQLCODE TO  EXPECTED-VAL
                 MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                 TO ACTUAL-VAL
                 WRITE SYSOUT-REC FROM ABEND-REC
                 GO TO 600-EXIT
              ELSE
                 IF SQLCODE < 0
                    MOVE "***  FATAL DB2 ERROR" TO
                    ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                    MOVE "Y" TO ERROR-FOUND-SW
                    MOVE SQLCODE TO  EXPECTED-VAL
                    MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                    TO ACTUAL-VAL
                    WRITE SYSOUT-REC FROM ABEND-REC
                    GO TO 1000-DB2-ERROR-RTN.

      ****** CHECK FOR VALID PHYSICIAN-ID
           MOVE ATTENDING-PHYS-ID TO PRIMARY-PHYSICIAN-ID.
           EXEC SQL
                SELECT PRIMARY_PHYSICIAN_ID INTO :PRIMARY-PHYSICIAN-ID
                FROM DDS0001.WARD_DATA
                WHERE PRIMARY_PHYSICIAN_ID = :PRIMARY-PHYSICIAN-ID
                END-EXEC.

           IF SQLCODE = -811 OR 0
              NEXT SENTENCE
           ELSE
              IF SQLCODE = +100
                 MOVE "*** ATTENDING PHYSICIAN NOT FOUND IN TABLE" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 MOVE SQLCODE TO  EXPECTED-VAL
                 MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                 TO ACTUAL-VAL
                 WRITE SYSOUT-REC FROM ABEND-REC
                 GO TO 600-EXIT
              ELSE
                 MOVE "***  FATAL DB2 ERROR" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 MOVE SQLCODE TO  EXPECTED-VAL
                 MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                 TO ACTUAL-VAL
                 WRITE SYSOUT-REC FROM ABEND-REC
                 GO TO 1000-DB2-ERROR-RTN.

      ****** CHECK FOR VALID MEDICATION-ID
           MOVE MEDICATION-ID IN INPATIENT-TREATMENT-REC TO
           MEDICATION-ID IN DCLMEDICATION.

           EXEC SQL
                SELECT MEDICATION_ID
                INTO :DCLMEDICATION.MEDICATION-ID
                FROM DDS0001.MEDICATION
                WHERE MEDICATION_ID = :DCLMEDICATION.MEDICATION-ID
                END-EXEC.

           IF SQLCODE = -811 OR 0
              NEXT SENTENCE
           ELSE
              IF SQLCODE = +100
                 MOVE "*** MEDICATION-ID NOT FOUND IN TABLE" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 MOVE SQLCODE TO  EXPECTED-VAL
                 MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                 TO ACTUAL-VAL
                 WRITE SYSOUT-REC FROM ABEND-REC
                 GO TO 600-EXIT
              ELSE
                 IF SQLCODE < 0
                    MOVE "***  FATAL DB2 ERROR" TO
                    ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                    MOVE "Y" TO ERROR-FOUND-SW
                    MOVE SQLCODE TO  EXPECTED-VAL
                    MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                    TO ACTUAL-VAL
                    WRITE SYSOUT-REC FROM ABEND-REC
                    GO TO 1000-DB2-ERROR-RTN.

      ****** CHECK FOR VALID SUPERVISOR NURSE-ID
           MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.
           EXEC SQL
                SELECT SUPERVISE_NURSE_ID
                INTO :SUPERVISE-NURSE-ID
                FROM DDS0001.WARD_DATA
                WHERE SUPERVISE_NURSE_ID = :SUPERVISE-NURSE-ID
                END-EXEC.

           IF SQLCODE = -811 OR 0
              NEXT SENTENCE
           ELSE
              IF SQLCODE = +100
                 MOVE "*** SUPERVISOR NURSE NOT FOUND" TO
                 ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                 MOVE "Y" TO ERROR-FOUND-SW
                 MOVE SQLCODE TO  EXPECTED-VAL
                 MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                 TO ACTUAL-VAL
                 WRITE SYSOUT-REC FROM ABEND-REC
                 GO TO 600-EXIT
              ELSE
                 IF SQLCODE < 0
                    MOVE "*** FATAL DB2 ERROR" TO
                    ERR-MSG IN INPATIENT-TREATMENT-REC-ERR
                    MOVE "Y" TO ERROR-FOUND-SW
                    MOVE SQLCODE TO  EXPECTED-VAL
                    MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC
                    TO ACTUAL-VAL
                    WRITE SYSOUT-REC FROM ABEND-REC
                    GO TO 1000-DB2-ERROR-RTN.
       600-EXIT.
           EXIT.

       700-WRITE-TRMTEDIT.
           MOVE "700-WRITE-TRMTEDIT" TO PARA-NAME.

           WRITE INPATIENT-TREATMENT-REC-EDIT
           FROM INPATIENT-TREATMENT-REC.
           ADD MEDICATION-COST  TO WS-MEDICATION-CHARGES.
           ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.
           ADD PHARMACY-COST IN INPATIENT-TREATMENT-REC
           TO WS-PHARMACY-CHARGES.
           ADD +1 TO RECORDS-WRITTEN.
       700-EXIT.
           EXIT.

       710-WRITE-TRMTERR.
           MOVE INPATIENT-TREATMENT-REC TO REST-OF-REC.
           WRITE INPATIENT-TREATMENT-REC-ERR.
           ADD +1 TO RECORDS-IN-ERROR.
       710-EXIT.
           EXIT.

       800-OPEN-FILES.
           MOVE "800-OPEN-FILES" TO PARA-NAME.
           OPEN INPUT TRMTDATA.
           OPEN OUTPUT TRMTEDIT, SYSOUT, TRMTERR.
           OPEN I-O PATMSTR.
       800-EXIT.
           EXIT.

       850-CLOSE-FILES.
           MOVE "850-CLOSE-FILES" TO PARA-NAME.
           CLOSE TRMTDATA,
           TRMTEDIT, SYSOUT, TRMTERR,
           PATMSTR.
       850-EXIT.
           EXIT.

       900-READ-TRMTDATA.
      *  Code your statements here to read the input file
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ TRMTDATA  INTO INPATIENT-TREATMENT-REC
           AT END MOVE "N" TO MORE-DATA-SW
              GO TO 900-EXIT
           END-READ
           MOVE "N" TO ERROR-FOUND-SW.
           ADD +1 TO RECORDS-READ.
       900-EXIT.
           EXIT.

       999-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF NOT TRAILER-REC
              MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
              GO TO 1000-ABEND-RTN.

           MOVE INPATIENT-TREATMENT-REC-DATA TO WS-TRAILER-REC.

           IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
              MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
              TO ABEND-REASON
              GO TO 1000-ABEND-RTN.


           IF WS-ANCILLARY-CHARGES NOT EQUAL TO IN-ANCILLARY-CHARGES
              MOVE "** ANCILLARY CHARGES OUT OF BALANCE"
              TO ABEND-REASON
              MOVE WS-ANCILLARY-CHARGES TO EXPECTED-VAL
              MOVE IN-ANCILLARY-CHARGES TO ACTUAL-VAL
              DISPLAY "** ANCILLARY CHARGES IN **"
              DISPLAY WS-ANCILLARY-CHARGES
              DISPLAY "** ANCILLARY CHARGES EXPECTED **"
              DISPLAY  IN-ANCILLARY-CHARGES.

           IF WS-MEDICATION-CHARGES  NOT EQUAL TO IN-MEDICATION-CHARGES
              MOVE "** MEDICATION CHARGES OUT OF BALANCE"
              TO ABEND-REASON
              DISPLAY "** MEDICATION CHARGES IN **"
              DISPLAY WS-MEDICATION-CHARGES
              DISPLAY "** MEDICATION CHARGES EXPECTED **"
              DISPLAY  IN-MEDICATION-CHARGES.

           IF WS-PHARMACY-CHARGES  NOT EQUAL TO IN-PHARMACY-CHARGES
              MOVE "** PHARMACY CHARGES OUT OF BALANCE"
              TO ABEND-REASON
              DISPLAY "** PHARMACY CHARGES IN **"
              DISPLAY WS-PHARMACY-CHARGES
              DISPLAY "** PHARMACY CHARGES EXPECTED **"
              DISPLAY  IN-PHARMACY-CHARGES.

           MOVE "T" TO RECORD-TYPE.
           ADD +1 TO RECORDS-WRITTEN.
           MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.
           MOVE WS-ANCILLARY-CHARGES TO IN-ANCILLARY-CHARGES.
           MOVE WS-MEDICATION-CHARGES TO IN-MEDICATION-CHARGES.
           MOVE WS-PHARMACY-CHARGES TO IN-PHARMACY-CHARGES.
           WRITE INPATIENT-TREATMENT-REC-EDIT FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.

           DISPLAY "** RECORDS READ **".
           DISPLAY RECORDS-READ.
           DISPLAY "** RECORD-IN EXPECTED **".
           DISPLAY  IN-RECORD-COUNT.
           DISPLAY "** RECORDS WRITTEN **".
           DISPLAY  RECORDS-WRITTEN.
           DISPLAY "** ERROR RECORDS FOUND **".
           DISPLAY  RECORDS-IN-ERROR.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "******** NORMAL END OF JOB TRTMNT ********".
       999-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB - TRTMNT ***" UPON CONSOLE.
           DIVIDE ZERO-VAL INTO ONE-VAL.

       1000-DB2-ERROR-RTN.
      ************************************************************
      *       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *
      ************************************************************

           DISPLAY '**** WE HAVE A SERIOUS PROBLEM HERE *****'.
           DISPLAY '999-ERROR-TRAP-RTN '.
           MULTIPLY SQLCODE BY -1 GIVING SQLCODE.
           DISPLAY 'SQLCODE ==> ' SQLCODE.
           DISPLAY SQLCA.
           DISPLAY SQLERRM.
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
           EXEC SQL ROLLBACK WORK END-EXEC.
           GO TO 1000-ABEND-RTN.