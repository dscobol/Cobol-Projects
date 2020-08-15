       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB04.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  LF-G     PIC X(05).  *> passed from SUB01
       PROCEDURE DIVISION USING LF-G.
      ** Addressability to data from SUB01
           MOVE 'SUB4' TO LF-G.
           GOBACK. *> Control returned to SUB01
