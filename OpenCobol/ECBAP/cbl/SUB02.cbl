       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB02.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALLING-FLDS.
           05 CF-K     PIC X(05).
           05 CF-L     PIC X(05).
           05 CF-M     PIC X(05).
           05 G-PROG PIC X(05).
       LINKAGE SECTION.
       01  LF-H     PIC X(05).  *> Linkage Parms
       01  LF-I     PIC X(05).  *> Data values
       01  LF-J     PIC X(05).  *> passed from MAIN

       PROCEDURE DIVISION USING LF-H, LF-I, LF-J.

           MOVE 'SUB2' TO CF-K, CF-L, CF-M.

           DISPLAY "In SUB02:Before Dynamic call - WS fields: ", 
              CF-K, CF-L, CF-M.

           DISPLAY "In SUB02:Before Dynamic call - Link fields: ", 
              LF-H, LF-I, LF-J.

           MOVE 'SUB2' TO LF-H, LF-I, LF-J.

           DISPLAY "In SUB02:After MOVE - Link fields: ", 
              LF-H, LF-I, LF-J.

           MOVE 'SUB05' TO G-PROG.
           CALL G-PROG USING CF-K, CF-L, CF-M.

           DISPLAY "In SUB02:After Dynamic call - WS fields: ", 
              CF-K, CF-L, CF-M.

           DISPLAY "In SUB02:After Dynamic call - Link fields: ", 
              LF-H, LF-I, LF-J.


           GOBACK.
