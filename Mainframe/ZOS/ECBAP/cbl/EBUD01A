       ID DIVISION.
       PROGRAM-ID. EBUD01.
      *    THIS IS THE FIRST OF SEVERAL SAMPLE PROGRAMS FOR EBU 2004
      *
      *    THIS PROGRAM WILL RECEIVE A DATE AND COVERT THE DATE TO
      *    AN INTEGER IN A CALLED PROGRAM TO DETERMINE DAYS FROM
      *    CURRENT DATE.
      *
      *    (C) 2003 IBM - KEVIN J. CUMMINGS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
          FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01 W-CALL-PROGRAM       PIC X(8).
      *
       01 W-RETIREMENT-WA          PIC 9(4).

       01 W-DIAGNOSTIC             PIC 9(4).

       01 W-EBUD02-LINKAGE-AREA.
           05 W-INPUT-DATE.
             10 W-CCYY  PIC 9(4).
             10 W-MM    PIC 9(2).
             10 W-DD    PIC 9(2).
           05 W-DAY-DIFFERENCE       PIC 9(8).
           05 W-EBUD02-PROGRAM-RETCODE PIC 9(4).
             88 W-EBUD02-REQUEST-SUCCESS   VALUE 0.

       01 W-EBUD03-LINKAGE-AREA.
           05 W-RETIREMENT-DATE-IN.
             10 W-RET-YYYY  PIC X(4).
             10 FILLLER     PIC X(1) VALUE '/'.
             10 W-RET-MM    PIC X(2).
             10 FILLLER     PIC X(1) VALUE '/'.
             10 W-RET-DD    PIC X(2).
           05 W-RETIREMENT-DATE        PIC X(80).
           05 W-EBUD03-PROGRAM-RETCODE PIC 9(4).
             88 W-EBUD03-REQUEST-SUCCESS   VALUE 0.
      *
       LINKAGE SECTION.
      *
       01 INTERFACE-AREA.
           05 L-INPUT-LENGTH    PIC 9(4) COMP.
           05 L-INPUT-DATE.
             10 L-CCYY  PIC X(4).
             10 L-MM    PIC X(2).
             10 L-DD    PIC X(2).

       PROCEDURE DIVISION USING INTERFACE-AREA.
      *
       A000-MAINLINE SECTION.
           PERFORM A100-VERIFY-INPUT-DATE
           PERFORM A200-CALL-DAY-DIFFERENCE-PROG
           PERFORM A300-CALCULATE-RETIREMENT
           GOBACK
           .
       END-OF-SECTION.
           EXIT.
      *
      *
       A100-VERIFY-INPUT-DATE SECTION.
           IF L-INPUT-DATE NUMERIC
              MOVE L-INPUT-DATE TO W-INPUT-DATE W-DIAGNOSTIC
              DISPLAY 'WORKING DATE:          - ' W-INPUT-DATE
              MOVE W-CCYY TO RETURN-CODE
           ELSE
              DISPLAY 'INPUT DATE NOT NUMERIC - ' L-INPUT-DATE
           END-IF
           .
      *
       END-OF-SECTION.
           EXIT.
      *
      *
       A200-CALL-DAY-DIFFERENCE-PROG SECTION.
           MOVE 'EBUD02' TO W-CALL-PROGRAM
           MOVE 0        TO W-DAY-DIFFERENCE
           MOVE 0        TO W-EBUD02-PROGRAM-RETCODE

           CALL W-CALL-PROGRAM USING W-EBUD02-LINKAGE-AREA

           IF W-EBUD02-REQUEST-SUCCESS
              DISPLAY 'DAYS DIFFERENCE = ' W-DAY-DIFFERENCE
           ELSE
              DISPLAY 'PROBLEMS IN CALL OF ' W-CALL-PROGRAM
              DISPLAY 'PROGRAM RETURN CODE ' W-EBUD02-PROGRAM-RETCODE
           END-IF
           .
      *
       END-OF-SECTION.
           EXIT.
      *
      *
       A300-CALCULATE-RETIREMENT     SECTION.
           COMPUTE W-RETIREMENT-WA = W-CCYY + 65
           MOVE W-RETIREMENT-WA      TO W-RET-YYYY
           MOVE W-MM                 TO W-DIAGNOSTIC
           MOVE W-DIAGNOSTIC         TO W-RETIREMENT-DATE-IN
           MOVE W-DD                 TO W-RET-DD
           MOVE SPACES   TO W-RETIREMENT-DATE
           MOVE 0        TO W-EBUD03-PROGRAM-RETCODE
           MOVE 'EBUD03' TO W-CALL-PROGRAM

           CALL W-CALL-PROGRAM USING W-EBUD03-LINKAGE-AREA

           IF W-EBUD03-REQUEST-SUCCESS
              DISPLAY 'RETIREMENT-DATE = ' W-RETIREMENT-DATE
           ELSE
              DISPLAY 'PROBLEMS IN CALL OF ' W-CALL-PROGRAM
              DISPLAY 'PROGRAM RETURN CODE ' W-EBUD03-PROGRAM-RETCODE
           END-IF
           .
      *
       END-OF-SECTION.
           EXIT.
      *