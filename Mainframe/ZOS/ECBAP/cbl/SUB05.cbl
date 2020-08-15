       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB05.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.

       01 LF-K     PIC X(05).
       01 LF-L     PIC X(05).
       01 LF-M     PIC X(05).

       PROCEDURE DIVISION USING LF-K, LF-L, LF-M.
           MOVE 'SUB5' TO LF-K, LF-L, LF-M.

           DISPLAY "In SUB05:After MOVE - Link fields: ", 
              LF-K, LF-L, LF-M.

           GOBACK.
