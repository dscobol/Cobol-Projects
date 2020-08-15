       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB03.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  LF-D     PIC X(05).  *> passed from SUB01
       01  LF-E     PIC X(05).  *> passed from SUB01
       01  LF-F     PIC X(05).  *> passed from SUB01
       PROCEDURE DIVISION USING LF-D, LF-E, LF-F.
      ** Addressability to data from SUB01
           MOVE 'SUB3' TO LF-D, LF-E, LF-F.
           GOBACK. *> Control returned to SUB01
