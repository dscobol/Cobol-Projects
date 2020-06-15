       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EMPPROJ.
       AUTHOR.          IBM.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROJFILE
             ASSIGN TO UT-S-PROJ
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PROJFILE-ST.
           SELECT PRINTFILE
             ASSIGN TO PROJRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PRINTFILE-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  PROJFILE
           RECORD CONTAINS 80 CHARACTERS.
       01  PROJ-RECORD                  PIC X(80).

       FD  PRINTFILE
           RECORD CONTAINS 132 CHARACTERS.
       01 PRINT-LINE                    PIC X(132).

       WORKING-STORAGE SECTION.
       77 WS-STORAGE-IND                PIC X(60)
                                                       VALUE
               'WORKING STORAGE BEGINS HERE'.

       77 ALLOWED-AMT                   PIC S9(7)V99   VALUE 9999999.99.
       77 CONTRACT-PERC               PIC V999        VALUE .002.

       01 PROJ-RECORD-WS.
           05 PROJECT-DETAILS.
               10 PROJECT-BUDGET-NO      PIC 9(07).
               10 PROJECT-LAST-NAME      PIC X(15).
               10 PROJECT-FIRST-NAME     PIC X(10).
           05 BUDGET-DETAILS.
               10 BUDGET-TYPE            PIC 9.
                   88 PRIVATE           VALUE 1.
                   88 MEDICARE          VALUE 2.
                   88 AFFORDABLE-CARE   VALUE 3.
               10 BUDGET-ACCOUNT-DATE-NUM PIC 9(08).
               10 BUDGET-ACCOUNT-DATE-X REDEFINES
                         BUDGET-ACCOUNT-DATE-NUM PIC X(08).
               10 BUDGET-ACCOUNT-PERIOD REDEFINES
                         BUDGET-ACCOUNT-DATE-NUM.
                   15 BUDGET-YEAR        PIC 9(04).
                   15 BUDGET-MONTH       PIC 9(02).
                   15 BUDGET-DAY         PIC 9(02).
               10 BUDGET-COST          PIC S9(7)V99.
               10 BUDGET-CONTRACT-PAID PIC S9(4).
               10 BUDGET-COINSURANCE     PIC V99.
           05 PROJ-DETAILS.
               10 PROJ-COST           PIC S9(7)V99.
               10 PROJ-COST-PAID      PIC S9(7)V99.
           05 FILLER                     PIC X(6).

       01 PROGRAM-SWITCHES.
           05 PROJECT-SUB               PIC 999        VALUE 1.
           05 PROJFILE-EOF             PIC X(1)       VALUE 'N'.
               88 NO-MORE-PROJS                       VALUE 'Y'.
           05 PROJFILE-ST              PIC X(2).
               88 PROJFILE-OK                         VALUE '00'.
           05 PRINTFILE-ST              PIC X(2).
               88 PRINTFILE-OK                         VALUE '00'.
           05 ACCOUNT-PERIOD            PIC X(1).
               88 ACCOUNT-PERIOD-OK                    VALUE 'Y'.
           05 BUDGET-CONTRACT-MET-WS  PIC X(1).
               88 CONTRACT-MET                       VALUE 'Y'.
           05 PAY-THE-PROJ-WS          PIC X(1).
               88 PAY-THE-PROJ                        VALUE 'Y'.

       01 COUNTERS-AND-ACCUMULATORS-WS.
           05 CONTRACT-WS             PIC S9(5)V99.
           05 PROJ-PAID-WS             PIC S9(7)V99.

       01 DATE-FIELDS-WS.
           05 CURR-DATE-OUT             PIC X(10).
           05 CURR-DATE-WS              PIC S9(8).
           05 CURR-DATE-WS-X REDEFINES CURR-DATE-WS.
               10 WS-YEAR               PIC X(4).
               10 WS-MONTH              PIC X(2).
               10 WS-DAY                PIC X(2).

       01 REPORT-FIELDS.
           05 LINE-COUNT                PIC S9(2)       VALUE +6.
           05 PAGE-COUNT                PIC S9(2)       VALUE ZEROS.
           05 LINES-PER-PAGE            PIC S9(2)       VALUE +5.

       01 TOT-BILL-INFORMATION.
           05 TOT-BUDGET-COST         PIC S9(9)V99.
           05 TOT-CONTRACT-PAID       PIC S9(9)V99.
           05 TOT-PROJ-COST-PAID     PIC S9(9)V99.
           05 TOT-PROJ-COST          PIC S9(9)V99.

       01 HEADING-LINE-ONE.
           05 HDG-DATE                  PIC XXXX/XX/XX.
           05 FILLER                    PIC X(46)      VALUE SPACES.
           05 FILLER                    PIC X(25)
                  VALUE 'Group PROJs Daily Totals'.
           05 FILLER                    PIC X(10)      VALUE SPACES.
           05 HDG-DAY                   PIC X(9).
           05 FILLER                    PIC X(3)       VALUE ' '.
           05 FILLER                    PIC X(31)      VALUE SPACES.
           05 FILLER                    PIC X(5)       VALUE 'Page '.
           05 HDG-PAGE-NUMBER           PIC Z9.
           05 FILLER                    PIC X(3)       VALUE SPACES.

       01 HEADING-LINE-TWO.
           05 FILLER                    PIC X(24)      VALUE 'BUDGET'.
           05 FILLER                    PIC X(11)      VALUE 'BUDGET'.
           05 FILLER                    PIC X(15)      VALUE 'FIRST'.
           05 FILLER                    PIC X(13)      VALUE 'LAST'.
           05 FILLER                    PIC X(14)      VALUE 'RENEW'.
           05 FILLER                    PIC X(8)       VALUE 'COPAY'.
           05 FILLER                    PIC X(9)       VALUE 'COPAY'.
           05 FILLER                    PIC X(14)      VALUE 'DEDUC'.
           05 FILLER                    PIC X(14)      VALUE 'PROJ'.
           05 FILLER                    PIC X(14)      VALUE 'PROJ'.

       01 HEADING-LINE-THREE.
           05 FILLER                    PIC X(24)      VALUE 'TYPE'.
           05 FILLER                    PIC X(11)      VALUE 'NUMBER'.
           05 FILLER                    PIC X(15)      VALUE 'NAME'.
           05 FILLER                    PIC X(13)      VALUE 'NAME'.
           05 FILLER                    PIC X(15)      VALUE 'DATE'.
           05 FILLER                    PIC X(6)       VALUE 'MET'.
           05 FILLER                    PIC X(10)      VALUE 'PERCENT'.
           05 FILLER                    PIC X(14)      VALUE 'COST'.
           05 FILLER                    PIC X(14)      VALUE 'COST'.
           05 FILLER                    PIC X(14)      VALUE 'PAID'.

       01 HEADING-LINE-FOUR.
           05 FILLER                    PIC X(23)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(10)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(14)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(12)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(13)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(5)       VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(7)       VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(09)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(15)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(13)      VALUE ALL '-'.

       01 DETAIL-LINE.
           05 DET-BUDGET-TYPE           PIC X(20)      VALUE SPACES.
           05 FILLER                    PIC X(4)       VALUE SPACES.
           05 DET-BUDGET-NO             PIC 9B999B99.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 DET-NAME.
               10 DET-FIRST-NAME        PIC X(15).
               10 DET-LAST-NAME         PIC X(10).
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 DET-RENEW-DATE            PIC XXXX/XX/XX.
           05 FILLER                    PIC X(6)       VALUE SPACES.
           05 DET-CONTRACT-MET        PIC X.
           05 FILLER                    PIC X(5)       VALUE SPACES.
           05 DET-CONTRACT-PERC       PIC .999.
           05 FILLER                    PIC X(5)       VALUE SPACES.
           05 DET-COINSURANCE           PIC $$$9.
           05 FILLER                    PIC X(6)       VALUE SPACES.
           05 DET-PROJ-COST          PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 DET-PROJ-PAID            PIC $$,$$9.99.
           05 FILLER                    PIC X(5)       VALUE SPACES.

       01 TOTAL-DASH-LINE.
           05 FILLER                    PIC X(91)      VALUE SPACE.
           05 FILLER                    PIC X(09)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(15)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(13)      VALUE ALL '-'.
       01 TOTAL-LINE-OUT.
           05 FILLER                    PIC X(92)      VALUE SPACES.
           05 TOT-CONTRACT-OUT        PIC $$$,$$9.99.
           05 FILLER                    PIC X          VALUE SPACES.
           05 TOT-PROJ-COST-OUT      PIC $$$,$$$,$$9.99.
           05 FILLER                    PIC XX          VALUE SPACES.
           05 TOT-PROJ-COST-PAID-OUT PIC $$$,$$$,$$9.99.
           05 FILLER                    PIC X(5)       VALUE SPACES.

       01 FILLER                        PIC X(12)
                         VALUE 'WS ENDS HERE'.
      *
       PROCEDURE DIVISION.
           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-PROCESS-PROJ UNTIL NO-MORE-PROJS.
           PERFORM 700-WRITE-PROJ-TOTALS.
           PERFORM 900-WRAP-UP.
           GOBACK.

       100-HOUSEKEEPING.
      * INITIALIZATION ROUTINE
           INITIALIZE TOT-BILL-INFORMATION,
                      COUNTERS-AND-ACCUMULATORS-WS,
                      DATE-FIELDS-WS.
           MOVE FUNCTION CURRENT-DATE TO HDG-DATE.
           PERFORM 300-OPEN-FILES.
           PERFORM 400-READ-PROJS.

       200-PROCESS-PROJ.
           IF PROJ-COST < ALLOWED-AMT
               PERFORM 300-COMPUTE-PROJ
               IF PAY-THE-PROJ
                   PERFORM 340-DETAIL-LINE
                   PERFORM 360-COMPUTE-INSURANCE-TOTAL
                   IF LINE-COUNT > LINES-PER-PAGE
                       PERFORM 400-WRITE-HEADING-LINES
                   END-IF
                   PERFORM 500-WRITE-DETAIL-LINE
                   PERFORM 600-INCREMENT-TOTALS
               END-IF
           END-IF
           PERFORM 400-READ-PROJS.

       300-OPEN-FILES.
           OPEN INPUT PROJFILE
           IF NOT PROJFILE-OK
              DISPLAY 'PROJ FILE PROBLEM'
              GO TO 999-ERROR-RTN.

           OPEN OUTPUT PRINTFILE
           IF NOT PRINTFILE-OK
              DISPLAY 'PRINT REPORT PROBLEM'
              GO TO 999-ERROR-RTN.

       300-COMPUTE-CONTRACT.
           COMPUTE CONTRACT-WS ROUNDED =
              BUDGET-COST * CONTRACT-PERC
      *
           IF BUDGET-CONTRACT-PAID >= CONTRACT-WS
              MOVE "Y" TO BUDGET-CONTRACT-MET-WS
           ELSE
              MOVE "N" TO BUDGET-CONTRACT-MET-WS
           END-IF.
      *
       300-COMPUTE-PROJ.
           PERFORM 300-COMPUTE-CONTRACT
           IF CONTRACT-MET
              COMPUTE PROJ-PAID-WS ROUNDED = PROJ-COST
                - (BUDGET-COINSURANCE) *(PROJ-COST)

           ELSE
              COMPUTE PROJ-PAID-WS ROUNDED = PROJ-COST
                - CONTRACT-WS - (BUDGET-COINSURANCE) *(PROJ-COST)
           END-IF

           SUBTRACT PROJ-PAID-WS FROM BUDGET-COST
           END-SUBTRACT

           IF BUDGET-COST > ZERO
              MOVE 'Y' TO PAY-THE-PROJ-WS
           ELSE
              MOVE 'N' TO PAY-THE-PROJ-WS
           END-IF.
      *
       340-DETAIL-LINE.
      *
       360-COMPUTE-INSURANCE-TOTAL.

       400-READ-PROJS.
           READ PROJFILE INTO PROJ-RECORD-WS
           AT END
              MOVE "Y" TO PROJFILE-EOF
           END-READ.
           IF PROJFILE-OK OR NO-MORE-PROJS
           NEXT SENTENCE
           ELSE
              DISPLAY 'PROJ FILE PROBLEM'
              GO TO 999-ERROR-RTN.

       400-WRITE-HEADING-LINES.
           MOVE +1          TO LINE-COUNT.
           ADD  +1          TO PAGE-COUNT.
           MOVE PAGE-COUNT  TO HDG-PAGE-NUMBER.
           WRITE PRINT-LINE FROM HEADING-LINE-ONE.
           MOVE SPACES      TO PRINT-LINE.
           WRITE PRINT-LINE.
           WRITE PRINT-LINE FROM HEADING-LINE-TWO.
           WRITE PRINT-LINE FROM HEADING-LINE-THREE.
           WRITE PRINT-LINE FROM HEADING-LINE-FOUR.
      *
       500-WRITE-DETAIL-LINE.
           MOVE PROJECT-BUDGET-NO TO DET-BUDGET-NO.

           EVALUATE BUDGET-TYPE
           WHEN 1
                MOVE 'EMPLOYER-PRIVATE'
                   TO DET-BUDGET-TYPE
           WHEN 2
                MOVE 'STANDARD MEDICARE'
                   TO DET-BUDGET-TYPE
           WHEN 3
                MOVE 'AFFORDABLE CARE ACT'
                   TO DET-BUDGET-TYPE
           WHEN OTHER
                MOVE 'UNKNOWN' TO DET-BUDGET-TYPE.

           INSPECT DET-BUDGET-NO REPLACING ALL ' ' BY '-'.
           MOVE 1                        TO PROJECT-SUB.
           MOVE SPACES                   TO DET-NAME.
           MOVE PROJECT-LAST-NAME        TO DET-LAST-NAME.
           MOVE PROJECT-FIRST-NAME       TO DET-FIRST-NAME.
           MOVE BUDGET-ACCOUNT-DATE-X    TO DET-RENEW-DATE.
           MOVE BUDGET-CONTRACT-MET-WS TO DET-CONTRACT-MET.
           MOVE CONTRACT-PERC          TO DET-CONTRACT-PERC.
           MOVE CONTRACT-WS            TO DET-COINSURANCE.
           MOVE PROJ-COST-PAID        TO DET-PROJ-PAID.
           MOVE PROJ-COST             TO DET-PROJ-COST.

           WRITE PRINT-LINE FROM DETAIL-LINE
              AFTER ADVANCING 2 LINES
           ADD 1 TO LINE-COUNT.
      *
       600-INCREMENT-TOTALS.
           ADD BUDGET-COST TO TOT-BUDGET-COST
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL DAYS PROJECT'
           END-ADD.
           ADD BUDGET-CONTRACT-PAID TO TOT-CONTRACT-PAID
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL PROJ'
           END-ADD.
           ADD PROJ-COST TO TOT-PROJ-COST
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL PROJECT COST'
           END-ADD.
           ADD PROJ-COST-PAID TO TOT-PROJ-COST-PAID
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL SUBROGT'
           END-ADD.

      *
       700-WRITE-PROJ-TOTALS.
           WRITE PRINT-LINE FROM TOTAL-DASH-LINE
              AFTER ADVANCING 2 LINES.
           MOVE TOT-PROJ-COST      TO TOT-PROJ-COST-OUT
           MOVE TOT-CONTRACT-PAID   TO TOT-CONTRACT-OUT
           MOVE TOT-PROJ-COST-PAID TO TOT-PROJ-COST-PAID-OUT
           WRITE PRINT-LINE FROM TOTAL-LINE-OUT.
       900-WRAP-UP.
           CLOSE PROJFILE, PRINTFILE.
       999-ERROR-RTN.
           GOBACK.