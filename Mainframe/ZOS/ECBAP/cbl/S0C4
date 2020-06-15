      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    S0C4.
       AUTHOR.        ABEND-S0C4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL
           ASSIGN TO UT-S-PAYCHECK
             ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYROLL-REC.
       01  PAYROLL-REC  PIC X(80).
       WORKING-STORAGE SECTION.
       01  CURRENT-MONTH            PIC X.
       01   MONTH-IN                PIC S9(02)   COMP.
           88 VALID-MONTH VALUES ARE 1 THRU 12.
       01   WS-USER-ABEND-CODE      PIC S9(04)   COMP.
       01  SUB                      PIC S9(04) COMP VALUE 10.

       01  A-TABLE.
           05 A-TAB PIC X(1) OCCURS 9 TIMES.
       PROCEDURE DIVISION.
      *--- Reference past end-of-table ABEND
           MOVE A-TAB(SUB) TO CURRENT-MONTH. *> Comment out for READ ABEND
      *--- READ file before file is open.
           READ PAYROLL.
           OPEN INPUT PAYROLL.