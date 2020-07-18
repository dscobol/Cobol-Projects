       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROL01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL
           ASSIGN TO "../../../common/data/ECBAP/payrol01.dat.txt"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYCHECK
              ASSIGN TO DISPLAY.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYROLL-REC.
       01  PAYROLL-REC.
           05 NAME-IN.
              10 FIRST-IN              PIC X(10).
              10 LAST-IN               PIC X(10).
           05  DATE-IN                 PIC X(10).
           05  HOURLY-RATE-IN          PIC 9(3).
           05  HOURS-WORKED-IN         PIC 9(3).
           05  CATEGORY-IN             PIC X(1).
           05  GROSS-PAY-IN            PIC 9(5).
       FD  PAYCHECK
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYCHECK-REC.
       01  PAYCHECK-REC.
           05 NAME-OUT.
              10 FIRST-OUT              PIC X(10).
              10 LAST-OUT               PIC X(10).
           05  DATE-OUT                 PIC X(10).
           05  HOURLY-RATE-OUT          PIC 9(3).
           05  HOURS-WORKED-OUT         PIC 9(3).
           05  CATEGORY-OUT             PIC X(1).
           05  GROSS-PAY-OUT            PIC 9(5).
       PROCEDURE DIVISION.
           OPEN INPUT PAYROLL.
           OPEN OUTPUT PAYCHECK.

           READ PAYROLL.
           COMPUTE GROSS-PAY-IN =
                      HOURLY-RATE-IN * HOURS-WORKED-IN.
           MOVE PAYROLL-REC TO PAYCHECK-REC.
           WRITE PAYCHECK-REC FROM PAYROLL-REC.

           READ PAYROLL.
           COMPUTE GROSS-PAY-IN =
                      HOURLY-RATE-IN * HOURS-WORKED-IN.
           MOVE PAYROLL-REC TO PAYCHECK-REC.
           WRITE PAYCHECK-REC FROM PAYROLL-REC.

           READ PAYROLL.
           COMPUTE GROSS-PAY-IN =
                      HOURLY-RATE-IN * HOURS-WORKED-IN.
           MOVE PAYROLL-REC TO PAYCHECK-REC.
           WRITE PAYCHECK-REC FROM PAYROLL-REC.

           CLOSE  PAYROLL, PAYCHECK.

           GOBACK.
