       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOW.
      * Comment: This program Displays a number of text strings
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  VAR0   PIC X(30) VALUE 'HEY... IM OVER HERE'.
       77  var1   pic S9(8)V99.
       77  VAR1-RDF REDEFINES VAR1 PIC X(10).
       77  VAR2   PIC 99.

       PROCEDURE DIVISION.
           MOVE 11111.1 TO VAR1.
           MOVE -11111.1 TO VAR1.
           IF VAR1 NUMERIC
                COMPUTE VAR2 =  FUNCTION LENGTH (VAR1-RDF).
      *    IF VAR2 > CCCC
           GOBACK.

