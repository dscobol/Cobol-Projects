       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALLING-FLDS.
           05 F     PIC X(05).
           05 E     PIC X(05).
           05 G     PIC X(05).
           05 G-PROG PIC X(05).
       LINKAGE SECTION.
       01  A     PIC X(05).  *> Linkage Parms
       01  B     PIC X(05).  *> Data values
       01  C     PIC X(05).  *> passed from MAIN
       01  D     PIC X(05).  *> Program to call next - passed from MAINPGM
      * Enter SUB01 - establish "address-ability" to
      *    the variables passed from MAINPGM
       PROCEDURE DIVISION USING A, B, C, D.
      ** Addressability to data from MAIN
           MOVE 'SUB01' TO A, B, C, D, E, F, G.
      ** Static call to SUB02 passing E, F, G WS-variables
      *     CALL 'SUB02' USING E, F, G. *> Program name to call from LINKAGE
      ** Dynamic call to SUB03
      *     MOVE 'SUB03' TO G-PROG. *> Move called program name to variable
      *     CALL G-PROG USING  F.  *> Dynamic Call using variable
           GOBACK. *> Control returned to MAIN