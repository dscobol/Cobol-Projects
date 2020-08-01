       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROL01.
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
       01 PAYROLL-WS.
           05 NAME-WS.
              10 FIRST-WS           PIC X(10).
              10 LAST-WS            PIC X(10).
           05  DATE-WS              PIC X(10).
           05  HOURLY-RATE          PIC 9(3).
           05  HOURS-WORKED         PIC 9(3).
           05  CATEGORY             PIC X(1).
           05  GROSS-PAY            PIC 9(5).

       PROCEDURE DIVISION.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main.
           PERFORM 600-CLOSE-FILES.
           GOBACK.
       000-Housekeeping.
      * Initialization Routine
           MOVE SPACES TO PAYROLL-WS.
           PERFORM 300-OPEN-FILES.
       100-Main.
      * To avoid Read past EOF -- there's some better logic structure
           PERFORM 400-Read-Payroll.
           COMPUTE GROSS-PAY = HOURLY-RATE * HOURS-WORKED
           PERFORM 500-Write-Paycheck.
           PERFORM 400-Read-Payroll.
           COMPUTE GROSS-PAY = HOURLY-RATE * HOURS-WORKED
           PERFORM 500-Write-Paycheck.
           PERFORM 400-Read-Payroll.
           COMPUTE GROSS-PAY = HOURLY-RATE * HOURS-WORKED
           PERFORM 500-Write-Paycheck.
       300-Open-Files.
           OPEN INPUT PAYROLL.
           OPEN OUTPUT PAYCHECK.
       400-Read-Payroll.
           READ PAYROLL  INTO PAYROLL-WS
      * Set AT END Switch
               AT END MOVE "Y" TO PAYROLL-EOF
           END-READ.
       500-Write-Paycheck.
           WRITE PAYCHECK-REC FROM PAYROLL-WS.
       600-CLOSE-FILES.
           CLOSE PAYROLL, PAYCHECK.