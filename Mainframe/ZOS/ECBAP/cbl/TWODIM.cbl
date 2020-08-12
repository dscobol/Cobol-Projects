       IDENTIFICATION DIVISION.
         PROGRAM-ID. TWODIM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TBL-SUBSCRIPTS.
           05  ROW-SUB                 PIC 9(1).
           05  COL-SUB                 PIC 9(1).
       77  CELL-TXT                  PIC X(4) VALUE ' :  '.
       01  TWO-DIM-TBL.
           05  ROWS OCCURS 5 TIMES.
               10 COLUMNS OCCURS 5 TIMES.
                  20 CELL PIC X(4).
       PROCEDURE DIVISION.
           INITIALIZE TWO-DIM-TBL, TBL-SUBSCRIPTS.
           PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL ROW-SUB > 5
              PERFORM VARYING COL-SUB FROM 1 BY 1 UNTIL COL-SUB > 5
                 MOVE ROW-SUB TO CELL-TXT(1:1)
                 MOVE COL-SUB TO CELL-TXT(3:1)
                 MOVE CELL-TXT TO CELL OF TWO-DIM-TBL (ROW-SUB COL-SUB)
              END-PERFORM
           END-PERFORM
           DISPLAY TWO-DIM-TBL.
           GOBACK
           .

