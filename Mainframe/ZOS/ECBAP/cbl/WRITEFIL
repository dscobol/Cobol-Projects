       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITEFIL.
      ****** Monitor PAYROLL-WS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYCHECK
           ASSIGN TO UT-S-PAYCHECK
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYCHECK
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYCHECK-REC.
       01  PAYCHECK-REC  PIC X(80).
       WORKING-STORAGE SECTION.
      * End of File switch
       01  PAYROLL-EOF                 PIC X(01) VALUE SPACE.
       77  WAGE-FACTOR                 PIC V99   VALUE ZERO.
       01  PAYROLL-WS.
           05 NAME.
              10 FIRST-OUT              PIC X(10).
              10 LAST-OUT               PIC X(10).
           05  FILLER                   PIC X(01) VALUE SPACE.
           05  DATE-OUT                 PIC XXXX/XX/XX.
           05  HOURLY-RATE-OUT          PIC S99V99 COMP-3.
           05  HOURS-WORKED-OUT         PIC 9(02)V99 COMP.
           05  CHECK-NBR-OUT            PIC 9(03).
           05  SALARY-OUT               PIC S9(05)V99 COMP-3.
           05  TAX-OUT                  PIC S9(3)V99 COMP.
           05  MANAGEMENT-BONUS-OUT     PIC V99.
           05  HIGH-VALUES-OUT          PIC X(4) VALUE HIGH-VALUES.
           05  FILLER                   PIC X(01) VALUE SPACE.
           05  LOW-VALUES-OUT           PIC X(4) VALUE LOW-VALUES.
       01  REDEFINES-EXAMPLE.
           05  NAME.
                10  FNAME               PIC X(20).
                10  LNAME               PIC X(20).
           05  CORPORATION REDEFINES NAME.
                10  CORP-NAME           PIC X(35).
                10  SUFFIX              PIC X(5).
                10  LLC-SUFFIX REDEFINES SUFFIX.
                    15 LLC              PIC X(3).
                    15 FILLER           PIC X(2).
                10  DOT-COM-SUFFEX REDEFINES SUFFIX.
                    15 DOTCOM           PIC X(4).
                    15 FILLER           PIC X(1).
       01  REFMOD-EXAMPLE.
           05   TRAN-KEY                PIC X(12)
                VALUE '1234567890AB'.

       PROCEDURE DIVISION.
           DISPLAY "TRAN-KEY BYTES 3 THRU 9 INCLUSIVE: " TRAN-KEY(3:9).
           OPEN OUTPUT PAYCHECK.
           INITIALIZE PAYROLL-WS.
           PERFORM 100-Main UNTIL PAYROLL-EOF = 'Y'.
           CLOSE PAYCHECK.
           GOBACK.
       100-Main.
           MOVE FUNCTION CURRENT-DATE TO DATE-OUT.
           ADD +1 TO CHECK-NBR-OUT.
           MOVE ALL 'X' TO FIRST-OUT.
           MOVE ALL 'Y' TO LAST-OUT.
           MOVE 23 TO HOURLY-RATE-OUT, HOURS-WORKED-OUT.
      ****** Initialize to ones - not zeros. Why?
           MOVE 123.11 TO TAX-OUT, MANAGEMENT-BONUS-OUT.
           COMPUTE SALARY-OUT =
             (  ( (HOURLY-RATE-OUT * HOURS-WORKED-OUT)
                  * 1 + MANAGEMENT-BONUS-OUT )
                  - TAX-OUT )
      ****** Enter values here
           WRITE PAYCHECK-REC FROM PAYROLL-WS.