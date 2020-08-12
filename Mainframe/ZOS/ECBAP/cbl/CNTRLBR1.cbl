       IDENTIFICATION DIVISION.
       PROGRAM-ID. "CNTRLBR1".
       AUTHOR. IBM.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILLING-FILE ASSIGN TO INHOSP
           ORGANIZATION IS SEQUENTIAL.

            SELECT REPORT-FILE ASSIGN TO PRTLINE
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       FD  BILLING-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS INFILE-RECORD.
       01  INFILE-RECORD.
           05 INFILE-HOSP                   PIC X(16).
           05 INFILE-WARD                   PIC X(19).
           05 INFILE-AMOUNT                 PIC S9(6)V9(2).
           05 FILLER                        PIC X(35).

       FD REPORT-FILE
           DATA RECORD IS REPORT-RECORD.
       01  REPORT-RECORD                    PIC X(92).

       WORKING-STORAGE SECTION.
       01  HOLD-RECORD.
           05 HOLD-HOSP                     PIC X(16).
           05 HOLD-WARD                     PIC X(19).
           05 HOLD-AMOUNT                   PIC S9(6)V9(2).
           05 HOLD-PATIENT                  PIC X(35).

       01  LEVEL-CONTROL                   PIC 9999.
           88 HOSPITAL-CONTROL-BREAK  VALUE 20 THRU 99.
           88 PATIENT-DETAIL-PROCESS  VALUE 0.
           88 END-OF-FILE             VALUE 100.
           88 INITIAL-RECORD          VALUE 99.

       01  WORKING-NUMERICS.
           05  AMOUNT-BY-TOTALS            PIC S9(8)V9(2).
           05  AMOUNT-BY-HOSP              PIC S9(8)V9(2).
           05  PATIENT-COUNT-BY-TOTALS     PIC 9(5).
           05  PATIENT-COUNT-BY-HOSP       PIC 9(5).
           05  HOSP-COUNT-BY-TOTALS        PIC 9(5).

       01  HOSP-HEADER.
           05  FILLER                     PIC X(10) VALUE " HOSPITAL:".
           05  RPT-HDR-HOSP                 PIC X(20).

       01  PATIENT-DETAIL.
           05  FILLER                     PIC X(6) VALUE SPACES.
           05  FILLER                     PIC X(11) VALUE SPACES.
           05  FILLER                     PIC X(22) VALUE
                                         'PATIENT COSTS ACCRUED:'.
           05  DETAIL-AMOUNT              PIC $,$$$,$$9.99.
           05  FILLER                     PIC X(40) VALUE SPACES.

       01  HOSP-TRAILER.
           05  FILLER                     PIC X(17)
                                VALUE " HOSPITAL TOTALS".
           05  HOSP-SUMM                  PIC X(16).
           05  FILLER                     PIC X(6) VALUE SPACES.
           05  HOSP-SUMMARY-AMOUNT        PIC $,$$$,$$9.99.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(12) VALUE " PATIENTS ".

       01  TOTALS-TRAILER.
           05  FILLER                     PIC X(12) VALUE "GRAND TOTAL".
           05  RPT-TRLR-AMT               PIC $,$$$,$$$,$$9.99.
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  RPT-TRLR-PATIENT-COUNT        PIC ZZZ,ZZ9.
           05  FILLER                     PIC X(12) VALUE " PATIENTS ".
           05  RPT-TRLR-HOSP-COUNT           PIC ZZZ,ZZ9.
           05  FILLER                     PIC X(11) VALUE " HOSPITALS ".

       01  BLANK-LINE                     PIC X(88) VALUE SPACES.
       01  HOSP-UNDER-LINE                PIC X(50) VALUE ALL '='.

       PROCEDURE DIVISION.

       PROCEDURE-MAIN.

           PERFORM 000-HOUSEKEEPING.
           PERFORM PROCESS-DATA  UNTIL END-OF-FILE.
           PERFORM 900-WRAP-UP.
           GOBACK.

       000-HOUSEKEEPING.
           MOVE ZERO                      TO AMOUNT-BY-TOTALS.
           MOVE ZERO                      TO HOSP-COUNT-BY-TOTALS.
           MOVE ZERO                      TO PATIENT-COUNT-BY-TOTALS.
           OPEN INPUT BILLING-FILE.
           OPEN OUTPUT REPORT-FILE.
           READ BILLING-FILE AT END MOVE 100 TO LEVEL-CONTROL
                         NOT AT END MOVE 99  TO LEVEL-CONTROL
           END-READ.

       900-WRAP-UP.
           MOVE AMOUNT-BY-TOTALS            TO RPT-TRLR-AMT.
           MOVE PATIENT-COUNT-BY-TOTALS     TO RPT-TRLR-PATIENT-COUNT.
           MOVE HOSP-COUNT-BY-TOTALS        TO RPT-TRLR-HOSP-COUNT.
           WRITE REPORT-RECORD              FROM BLANK-LINE.
           WRITE REPORT-RECORD              FROM TOTALS-TRAILER.
           CLOSE BILLING-FILE REPORT-FILE.

      *****************************************************************
      * Control Break Driver Logic
      * Upon reading each new record, there are 3 possible conditions
      *    1. Outer Control Break
      *       - Do Hospital Control Break
      *          - Do Ward Control Break
      *             - Do Detail Procesing
      *    2. Inner Control Break
      *          - Do Ward Control Break
      *             - Do Detail Procesing
      *    3. No Control Break - Process the detail record data
      *          - Do Detail Procesing
      *****************************************************************
       PROCESS-DATA.
      ** Start logic with new record - move input-control
           MOVE INFILE-RECORD TO HOLD-RECORD.

      ** Is the hold-outer-control break field = input-control-break-field?
           IF HOSPITAL-CONTROL-BREAK
                PERFORM HOSP-INITIAL.

      ** Both hold-control-break fields are equal
           PERFORM PATIENT-NORMAL-PROCESS.
           PERFORM GET-INFILE-RECORD.
           IF HOSPITAL-CONTROL-BREAK    PERFORM HOSP-SUMMARY.

       HOSP-INITIAL.
           MOVE ZERO                      TO AMOUNT-BY-HOSP.

           MOVE ZERO                      TO PATIENT-COUNT-BY-HOSP.
           MOVE HOLD-HOSP                 TO RPT-HDR-HOSP.
           WRITE REPORT-RECORD            FROM HOSP-HEADER.
           WRITE REPORT-RECORD            FROM HOSP-UNDER-LINE.
           WRITE REPORT-RECORD            FROM BLANK-LINE.

       PATIENT-NORMAL-PROCESS.
           MOVE HOLD-AMOUNT            TO DETAIL-AMOUNT.
           ADD  HOLD-AMOUNT            TO AMOUNT-BY-HOSP.
           WRITE REPORT-RECORD         FROM PATIENT-DETAIL.

       GET-INFILE-RECORD.
           READ BILLING-FILE
               AT END
                     MOVE 100 TO LEVEL-CONTROL
               NOT AT END
                     PERFORM SET-LEVEL-CONTROL
           END-READ.

      *****************************************************************
      * Infile-Key <=> Hold-Key comparison.
      * Upon reading each new record, there are 3 possible conditions
      *    1. Outer Control Break - Move 20 to LEVEL-CONTROL
      *    2. Inner Control Break - Move 10 to LEVEL-CONTROL
      *    3. No Control Break    - Move  0 to LEVEL-CONTROL
      *****************************************************************
       SET-LEVEL-CONTROL.
           IF INFILE-HOSP NOT = HOLD-HOSP

      **   20 == Outer Control Break
                THEN MOVE 20              TO LEVEL-CONTROL

      **   0 == Detail line process
                  ELSE MOVE 0               TO LEVEL-CONTROL.

       HOSP-SUMMARY.
           MOVE HOLD-HOSP                 TO HOSP-SUMM.
           MOVE AMOUNT-BY-HOSP            TO HOSP-SUMMARY-AMOUNT.
           WRITE REPORT-RECORD       FROM BLANK-LINE.
      *     MOVE PATIENT-COUNT-BY-HOSP     TO HOSP-SUMMARY-PATIENT-COUNT.
           WRITE REPORT-RECORD       FROM HOSP-TRAILER.
           WRITE REPORT-RECORD       FROM BLANK-LINE.
           WRITE REPORT-RECORD       FROM BLANK-LINE.
           ADD AMOUNT-BY-HOSP             TO AMOUNT-BY-TOTALS.
           ADD PATIENT-COUNT-BY-HOSP      TO PATIENT-COUNT-BY-TOTALS.
           ADD 1                          TO HOSP-COUNT-BY-TOTALS.