       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDSPTAB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
        FILE SECTION.
        WORKING-STORAGE SECTION.
       01 WS-OneD.
           12 WS-CHAR                  PIC AA OCCURS 5 TIMES.

       01 WS-TwoD.
           12 WS-First OCCURS 3 TIMES.
              15 WS-A                  PIC A.
              15 WS-Second OCCURS 2 TIMES.
                 18 WS-B               PIC X.
       PROCEDURE DIVISION.
           MOVE 'AABBCCDDEE' TO WS-OneD.
           DISPLAY '  One Dimensional Table '.
           DISPLAY '  First Entry in WS-OneD: ' WS-CHAR(1).
           DISPLAY ' Second Entry in WS-OneD: ' WS-CHAR(2).
           DISPLAY '  Third Entry in WS-OneD: ' WS-CHAR(3).
           DISPLAY '  Forth Entry in WS-OneD: ' WS-CHAR(4).
           DISPLAY '  Fifth Entry in WS-OneD: ' WS-CHAR(5).                                 

           MOVE 'A12B34C56' TO WS-TwoD.
           DISPLAY '  Two Dimensional Table '.
           DISPLAY '  First Entry in WS-TwoD : ' WS-First(1).
           DISPLAY '  (1,1)                  : ' WS-Second(1,1).
           DISPLAY '  (1,2)                  : ' WS-Second(1,2).
           DISPLAY ' Second Entry in WS-TwoD : ' WS-First(2).
           DISPLAY '  (2,1)                  : ' WS-Second(2,1).
           DISPLAY '  (2,2)                  : ' WS-Second(2,2).
           DISPLAY '  Third Entry in WS-TwoD : ' WS-First(3).
           DISPLAY '  (3,1)                  : ' WS-Second(3,1).
           DISPLAY '  (3,2)                  : ' WS-Second(3,2).
       
           GOBACK.
