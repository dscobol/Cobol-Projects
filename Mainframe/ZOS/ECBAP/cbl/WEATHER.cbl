       IDENTIFICATION DIVISION.
         PROGRAM-ID. WEATHER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WEATHER-FILE
           ASSIGN TO UT-S-WEATHER
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS W-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  WEATHER-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 20 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS WEATHER-REC.
       01  WEATHER-REC  PIC X(20).

       WORKING-STORAGE SECTION.
       01  WEATHER-REC-WS.
           05 DAY-WS                    PIC X(01).
           05 TIME-WS                   PIC X(07).
           05 TEMP-WS                   PIC X(02).
           05 WIND-DIRECTION-WS         PIC X(01).
           05 WIND-SPEED-WS             PIC X(02).
           05 TEMP-WS                   PIC X(02).
       01  TBL-SUBSCRIPTS.
           05  ROW-SUB                  PIC 9(1).
           05  COL-SUB                  PIC 9(1).
       01  FLAGS-AND-FLDS.
           05  W-CODE                   PIC X(02).
           05  END-OF-WEATHER-FILE      PIC X(01).
           88   END-OF-FILE    VALUE 'Y'.

       01  TWO-DIM-TBL.
           05  ROWS OCCURS 5 TIMES.
               10 COLUMNS OCCURS 5 TIMES.
                  20 CELL PIC X(4).
       PROCEDURE DIVISION.
           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-LOAD-TABLE UNTIL END-OF-FILE.
           PERFORM 300-WRAP-UP.
           GOBACK.
       100-HOUSEKEEPING.
           OPEN INPUT WEATHER-FILE.
           INITIALIZE TWO-DIM-TBL, TBL-SUBSCRIPTS.
           PERFORM 600-READ-WEATHER-FILE.
       200-LOAD-TABLE.
      *    PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL ROW-SUB > 5
      *       PERFORM VARYING COL-SUB FROM 1 BY 1 UNTIL COL-SUB > 5
      *          MOVE ROW-SUB TO CELL-TXT(1:1)
      *          MOVE COL-SUB TO CELL-TXT(3:1)
      *          MOVE CELL-TXT TO CELL OF TWO-DIM-TBL (ROW-SUB COL-SUB)
      *       END-PERFORM
      *    END-PERFORM.
       300-WRAP-UP.
           CLOSE WEATHER-FILE.
       600-READ-WEATHER-FILE.
           READ WEATHER-FILE INTO WEATHER-REC-WS.

