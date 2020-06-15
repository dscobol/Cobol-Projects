       ID DIVISION.
       PROGRAM-ID. EBUD03.
      *    THIS IS A SAMPLE PROGRAM FOR EBU 2004
      *
      *    THIS PROGRAM WILL BE CALLED BY ANOTHER, RECEIVE A
      *    DATE(YY/MM/DD) AND DETERMINE A PROPER FORMATTED
      *    RETIREMENT DATE.
      *
      *    (C) 2004 IBM - KEVIN J. CUMMINGS RESERVED.
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
       01  LILIAN                  PIC S9(9) BINARY.
       01  CHRDATE                 PIC X(80).

       01  IN-DATE.
           02  VSTRING-LENGTH      PIC S9(4) BINARY.
           02  VSTRING-TEXT.
               03  VSTRING-CHAR    PIC X
                           OCCURS 0 TO 256 TIMES
                           DEPENDING ON VSTRING-LENGTH
                               OF IN-DATE.

       01  PICSTR.
           02  VSTRING-LENGTH      PIC S9(4) BINARY.
           02  VSTRING-TEXT.
               03  VSTRING-CHAR    PIC X
                           OCCURS 0 TO 256 TIMES
                           DEPENDING ON VSTRING-LENGTH
                              OF PICSTR.
       01  FC.
           02  CONDITION-TOKEN-VALUE.
           COPY  CEEIGZCT.
               03  CASE-1-CONDITION-ID.
                   04  SEVERITY    PIC S9(4) BINARY.
                   04  MSG-NO      PIC S9(4) BINARY.
               03  CASE-2-CONDITION-ID
                         REDEFINES CASE-1-CONDITION-ID.
                   04  CLASS-CODE  PIC S9(4) BINARY.
                   04  CAUSE-CODE  PIC S9(4) BINARY.
               03  CASE-SEV-CTL    PIC X.
               03  FACILITY-ID     PIC XXX.
           02  I-S-INFO            PIC S9(9) BINARY.

      *
       LINKAGE SECTION.
      *
       01 INTERFACE-AREA.
          05 L-RETIREMENT-YEAR     PIC X(10).
          05 L-RETIREMENT-DATE     PIC X(80).
          05 L-PROGRAM-RETCODE PIC 9(4).

       PROCEDURE DIVISION USING INTERFACE-AREA.
      *
       A000-MAINLINE SECTION.
           PERFORM A100-DETERMINE-RETIREMENT
           IF L-PROGRAM-RETCODE = 0
              PERFORM A200-FORMAT-DATE
           GOBACK
           .
       END-OF-SECTION.
           EXIT.
      *
       A100-DETERMINE-RETIREMENT SECTION.
      *************************************************
      ** CALL CEEDAYS TO CONVERT THE RETIREMENT DATE **
      ** TO  LILIAN REPRESENTATION                   **
      *************************************************
           MOVE 10 TO VSTRING-LENGTH OF IN-DATE.
           MOVE L-RETIREMENT-YEAR   TO
              VSTRING-TEXT OF IN-DATE(1:10).
           MOVE 10 TO VSTRING-LENGTH OF PICSTR.
           MOVE "YYYY/MM/DD" TO VSTRING-TEXT OF PICSTR(1:10).
           CALL "CEEDAYS" USING IN-DATE, PICSTR,
                                LILIAN, FC.


      *************************************************
      ** IF CEEDAYS RUNS SUCCESSFULLY, DISPLAY RESULT**
      *************************************************
           IF  CEE000 OF FC  THEN
               DISPLAY VSTRING-TEXT OF IN-DATE
                   " IS LILIAN DAY: " LILIAN
           ELSE
               DISPLAY "CEEDAYS FAILED WITH MSG "
                   MSG-NO OF FC UPON CONSOLE
               MOVE 9999 TO L-PROGRAM-RETCODE
           END-IF
           .
      *
       END-OF-SECTION.
           EXIT.

       A200-FORMAT-DATE         SECTION.
      *************************************************
      ** SPECIFY PICTURE STRING THAT DESCRIBES THE   **
      **  DESIRED FORMAT OF THE OUTPUT FROM CEEDATE, **
      **  AND THE PICTURE STRING'S LENGTH.           **
      *************************************************
           MOVE 37 TO VSTRING-LENGTH OF PICSTR.
           MOVE "Wwwwwwwwwwz, ZD Mmmmmmmmmmmmmmz YYYY" TO
                        VSTRING-TEXT OF PICSTR(1:37).

      *************************************************
      ** CALL CEEDATE TO CONVERT THE LILIAN DATE     **
      **     TO  A PICTURE STRING.                   **
      *************************************************
           CALL "CEEDATE" USING LILIAN, PICSTR,
                                CHRDATE, FC.


      *************************************************
      ** IF CEEDATE RUNS SUCCESSFULLY, DISPLAY RESULT**
      *************************************************
           IF CEE000 OF FC  THEN
               MOVE CHRDATE TO L-RETIREMENT-DATE
           ELSE
               DISPLAY "CEEDATE FAILED WITH MSG "
                   MSG-NO OF FC UPON CONSOLE
               MOVE 9999 TO L-PROGRAM-RETCODE
           END-IF
           .
      *
       END-OF-SECTION.
           EXIT.