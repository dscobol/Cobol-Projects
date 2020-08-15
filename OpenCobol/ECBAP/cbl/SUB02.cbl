       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALLING-FLDS.
           05 CF-E     PIC X(05).
           05 CF-F     PIC X(05).
           05 CF-G     PIC X(05).
           05 G-PROG PIC X(05).
       LINKAGE SECTION.
       01  LF-A     PIC X(05).  *> Linkage Parms
       01  LF-B     PIC X(05).  *> Data values
       01  LF-C     PIC X(05).  *> passed from MAIN
       01  LF-D     PIC X(05).  *> Program to call next -pass from MAINPGM
      * Enter SUB01 - establish "address-ability" to
      *    the variables passed from MAINPGM
       PROCEDURE DIVISION USING LF-A, LF-B, LF-C, LF-D.
      ** Addressability to data from MAIN
           MOVE 'SUB01' TO LF-A, LF-B, LF-C, LF-D, CF-E, CF-F, CF-G.
      ** Static call to SUB02 passing E, F, G WS-variables
      *     CALL 'SUB02' USING E, F, G. *> Program to call from LINKAGE
      ** Dynamic call to SUB03
      *     MOVE 'SUB03' TO G-PROG. *> Move called program to variable
      *     CALL G-PROG USING  F.  *> Dynamic Call using variable
           GOBACK. *> Control returned to MAIN
