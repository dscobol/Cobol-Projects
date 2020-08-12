       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROL02.
      * This
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL
           ASSIGN TO UT-S-PAYROLL
             ORGANIZATION IS SEQUENTIAL.

           SELECT PAYCHECK
           ASSIGN TO UT-S-PAYCHECK
             ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYROLL-REC.
       01  PAYROLL-REC  PIC X(80).
       FD  PAYCHECK
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYCHECK-REC.
       01  PAYCHECK-REC  PIC X(80).

       WORKING-STORAGE SECTION.
      * End of File switch
       01 PAYROLL-EOF               PIC X(01) VALUE SPACE.
       01 PAYROLL-IN.
           05 NAME-IN.
              10 FIRST-IN              PIC X(10).
              10 LAST-IN               PIC X(10).
           05  DATE-IN                 PIC X(10).
           05  HOURLY-RATE-IN          PIC 99V99.
           05  HOURS-WORKED-IN         PIC 9(2).
           05  CATEGORY-IN             PIC X(1).
           05  GROSS-PAY-IN            PIC 999V99.

       01 PAYROLL-OUT.
           05 NAME-WS-OUT.
              10 FIRST-OUT             PIC X(10).
              10 FILLER                PIC XX VALUE ' '.
              10 LAST-OUT              PIC X(10).
           05  FILLER                  PIC X(02).
           05  DATE-OUT                PIC X(10).
           05  FILLER                  PIC X(02).
           05  HOURLY-RATE-OUT         PIC $$.99.
           05  FILLER                  PIC X(02).
           05  HOURS-WORKED-OUT        PIC ZZ.
           05  FILLER                  PIC X(02).
           05  CATEGORY-OUT            PIC X(1).
           05  FILLER                  PIC X(02).
           05  GROSS-PAY-OUT           PIC $$$.99.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main UNTIL PAYROLL-EOF = 'Y'.
           PERFORM 600-CLOSE-FILES.
           GOBACK.
       000-Housekeeping.
      * Initialization Routine
           INITIALIZE PAYROLL-IN, PAYROLL-OUT.
      * Priming Read
           PERFORM 300-Open-Files.    *> Comment out to get ABEND 4038
           PERFORM 400-Read-Payroll.  *> Comment out with empty input file
       100-Main.
           DISPLAY '100-main'.        *> For shops not using the Debugger
           DISPLAY "PAYROLL REC: " PAYROLL-IN.
           PERFORM 200-PROCESS-DATA.
           PERFORM 500-Write-Paycheck.
           PERFORM 400-Read-Payroll.
       200-PROCESS-DATA.
           MOVE FIRST-IN          TO  FIRST-OUT.
           MOVE LAST-IN           TO  LAST-OUT.
           MOVE DATE-IN           TO  DATE-OUT.
           MOVE HOURLY-RATE-IN    TO  HOURLY-RATE-OUT.
           MOVE HOURS-WORKED-IN   TO  HOURS-WORKED-OUT.
           MOVE CATEGORY-IN       TO  CATEGORY-OUT.
           MOVE GROSS-PAY-IN      TO  GROSS-PAY-OUT.
           COMPUTE GROSS-PAY-OUT = HOURLY-RATE-IN * HOURS-WORKED-IN.
       300-Open-Files.
           OPEN INPUT PAYROLL.
           OPEN OUTPUT PAYCHECK.
       400-Read-Payroll.
           DISPLAY 'READ Payroll'.
           READ PAYROLL INTO PAYROLL-IN
      * Set AT END Switch
               AT END MOVE "Y" TO PAYROLL-EOF
           END-READ.
       500-Write-Paycheck.
           DISPLAY 'WRITE Payroll'.
           WRITE PAYCHECK-REC FROM PAYROLL-OUT.
       600-CLOSE-FILES.
           CLOSE PAYROLL, PAYCHECK.