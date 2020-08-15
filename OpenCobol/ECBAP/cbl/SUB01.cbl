       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALLING-FLDS.
           05  CF-D     PIC X(05).
           05  CF-E     PIC X(05).
           05  CF-F     PIC X(05).
           05 G-PROG PIC X(05).
       LINKAGE SECTION.
       01  LF-A     PIC X(05).  *> Linkage Parms
       01  LF-B     PIC X(05).  *> Data values
       01  LF-C     PIC X(05).  *> passed from MAIN
       01  LF-D     PIC X(05).
       01  LF-E     PIC X(05).
       01  LF-F     PIC X(05).
       01  LF-G     PIC X(05).
      * Enter SUB01 - establish "address-ability" to
      *    the variables passed from MAINPGM
       PROCEDURE DIVISION USING
           LF-A, LF-B, LF-C, LF-D, LF-E, LF-F, LF-G.

           MOVE 'SUB1' TO LF-A, LF-B, LF-C, LF-D, LF-E, LF-F, LF-G.

           DISPLAY "In SUB01:Before static call: "
              LF-A, LF-B, LF-C, LF-D, LF-E, LF-F, LF-G.

           CALL 'SUB03' USING CF-D, CF-E, CF-F.

           DISPLAY "In SUB01:Before static call - Link fields: "
              LF-A, LF-B, LF-C, LF-D, LF-E, LF-F, LF-G.

           DISPLAY "In SUB01: After static call WS fields: "
              CF-D, " ", CF-E, " ", CF-F.

           MOVE CF-D TO LF-D
           MOVE CF-E TO LF-E
           MOVE CF-F TO LF-F

           DISPLAY "In SUB01:After static call - After MOVE: "
              LF-A, LF-B, LF-C, LF-D, LF-E, LF-F, LF-G.

           MOVE "SUB04" to G-PROG.

           CALL G-PROG USING LF-G.
           DISPLAY "In SUB01:After dynamic call: "
              LF-A, LF-B, LF-C, LF-D, LF-E, LF-F, LF-G.

           GOBACK. *> Control returned to MAIN