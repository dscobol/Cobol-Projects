       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SANDBOX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77   PRICE                       PIC     9(5)v99.
       77   EDITED-PRICE        PIC  $zz,zz9.99.
       PROCEDURE DIVISION.
           MOVE 12345.6 TO PRICE.
           MOVE PRICE TO EDITED-PRICE.
           DISPLAY EDITED-PRICE.
           GOBACK.