       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOVSC.
      * REMARKS. ThiS program Displays a number of literals
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  VAR1Q             PIC 9(1) VALUE 2.
       77   PRICE                       PIC     9(5)v99.
       77   EDITED-PRICE        PIC  $zz,zz9.99.
       77  COMPANY-NAME-L   PIC X(60). *> Left Justify is the COBOL default
       77  COMPANY-NAME-R   PIC X(60) JUSTIFIED RIGHT.
       77  VAR1    PIC 99 VALUE 0.
       77  VAR2    PIC 99999 VALUE 34556.
       77  VAR3    PIC 99 VALUE 4.
       77  var4    pic $zzz,zzz,zzz.99CR.

       PROCEDURE DIVISION.
           move -12345678.99 to var4.
           GOBACK.