      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    S0CB.
       AUTHOR.        ABEND-S0CB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  S0CB-TEST-VARIABLES.
           05  CURRENT-HOURS-WORKED     PIC S9(02)  COMP-3.
           05  TOTAL-HOURS-WORKED       PIC S9(03)  COMP-3.
           05  FIELD-A                  PIC X(05).
           05  FIELD-A-RDF  REDEFINES FIELD-A.
                10  FIRST-4                  PIC 9(4).
                10  LAST-DIGIT               PIC 9(1).
           05  SUB                      PIC 9(4) COMP.
       01  A-TABLE.
           05 A-TAB PIC 9(1) COMP OCCURS 9 TIMES.
       PROCEDURE DIVISION.
           MOVE LOW-VALUES TO A-TABLE.
           MOVE 12340 TO CURRENT-HOURS-WORKED, TOTAL-HOURS-WORKED,
                          FIELD-A.
           DIVIDE A-TAB(2) INTO CURRENT-HOURS-WORKED
              ON SIZE ERROR DISPLAY 'DIVIDE BY ZERO!!'.
           COMPUTE SUB = TOTAL-HOURS-WORKED / LAST-DIGIT
      *        ON SIZE ERROR DISPLAY 'DIVIDE BY ZERO!!'
      *    Uncomment the ON SIZE ERROR statement to see the difference.
           .

           GOBACK.