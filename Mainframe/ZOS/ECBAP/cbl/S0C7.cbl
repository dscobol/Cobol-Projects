      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    S0C7.
       AUTHOR.        ABEND-S0C7.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  S0C7-TEST-VARIABLES.
           05  CURRENT-MONTH            PIC X.
           05  MONTH-IN                 PIC S9(02)  COMP.
           05  CURRENT-HOURS-WORKED     PIC S9(02)  COMP-3.
           05  TOTAL-HOURS-WORKED       PIC S9(03)  COMP-3.
             88 VALID-MONTH VALUES ARE 1 THRU 12.
           05   WS-USER-ABEND-CODE      PIC S9(04)   COMP.
           05  SUB                      PIC S9(01)   COMP-3.
           05  SUB-COMP                 PIC S9(04)   COMP.
           05  SUB-COMP-3               PIC S9(04)   COMP-3.
           05  SUB-DISPLAY              PIC S9(04).
           05  SUB-VALID-10             PIC S9(04)   VALUE 10.

       01  A-TABLE.
           05 A-TAB PIC X(1) OCCURS 9 TIMES.
       PROCEDURE DIVISION.
      *    MOVE HIGH-VALUES TO A-TABLE.
      *    MOVE SPACES TO A-TABLE.
      * Unitialized fields
           IF SUB NUMERIC MOVE SUB TO SUB-COMP.
           GOBACK.
           MOVE SUB TO SUB-DISPLAY.
           MOVE SUB TO SUB-COMP-3.
           MOVE A-TAB(SUB) TO SUB-COMP. *> ABEND

      *    MOVE SUB-VALID-10 TO SUB-COMP. *> No ABEND
      *    MOVE SUB-VALID-10 TO SUB-COMP-3. *> No ABEND
      *    MOVE SUB-VALID-10 TO SUB-DISPLAY. *> No ABEND
      *    MOVE SUB TO SUB-COMP.
      *    MOVE SUB TO SUB-VALID-10.
      *    COMPUTE SUB = SUB + SUB-COMP. *> ABEND

      *    MOVE A-TAB(SUB) TO SUB-COMP-3. *> ABEND
      *    COMPUTE SUB = SUB + SUB-COMP-3. *> ABEND
      *    MOVE SUB TO SUB-COMP-3.

      *    MOVE A-TAB(SUB) TO SUB-DISPLAY. *> ABEND
      *    COMPUTE SUB = SUB + SUB-DISPLAY. *> ADD bad data
      *    MOVE SUB TO SUB-DISPLAY.
           GOBACK.