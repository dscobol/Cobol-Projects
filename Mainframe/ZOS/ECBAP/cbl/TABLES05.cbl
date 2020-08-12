       IDENTIFICATION DIVISION.
         PROGRAM-ID. TABLES05.
      ** EXAMPLE OF TWO-DIM OCCURS -
      *  OCCURS DEPENDING ON, AND PLAIN OCCURS WIHIN AN OCCURS
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SUB-1                 PIC S9(4) BINARY.
       77  SUB-2                 PIC S9(4) BINARY.
       01  DIMENSIONS.
           05  M             PIC S9(4) BINARY VALUE 6.
           05  N             PIC S9(4) BINARY VALUE 7.
       01  TABLE-REC-1.
           05  ROWS OCCURS 1 TO 10 TIMES DEPENDING ON M.
               10 COLUMNS OCCURS 1 TO 10 TIMES DEPENDING ON N.
                  20 CELL PIC X(1).
       01  TABLE-REC-2.
           05  ROWS OCCURS 10 TIMES.
               10 COLUMNS OCCURS 10 TIMES.
                  20 CELL PIC X(1).
       PROCEDURE DIVISION.
           INITIALIZE TABLE-REC-2.
           PERFORM VARYING SUB-1 FROM 1 BY 1 UNTIL SUB-1 > M
              PERFORM VARYING SUB-2 FROM 1 BY 1 UNTIL SUB-2 > N
                 MOVE 'X' TO CELL OF TABLE-REC-1 (SUB-1 SUB-2)
                 MOVE 'X' TO CELL OF TABLE-REC-2 (SUB-1 SUB-2)
              END-PERFORM
           END-PERFORM
           DISPLAY TABLE-REC-1
           DISPLAY TABLE-REC-2
           GOBACK
           .
      *> can not initialize variable-lth table