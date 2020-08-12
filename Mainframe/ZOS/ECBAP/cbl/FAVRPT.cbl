       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPTA.
      ***** This is an unbelievably simple COBOL program
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAVIN  ASSIGN TO FAVIN.
           SELECT FAVRPT ASSIGN TO FAVRPT.

       DATA DIVISION.
       FILE SECTION.
       FD  FAVIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVIN-REC.
       01  FAVIN-REC.
           05  ARTIST-NAME            PIC X(30).
           05  NUMBER-OF-MUSICIANS    PIC 9(2).
           05  MUSICAL-GENRE          PIC X(12).
           05  COST.
                10  CD-COST             PIC 9(3)V99.
                10  SHIPPING-COST       PIC 9(2)V99.
                10  TAX                 PIC 9(2).
           05  BAND-IS-STILL-TOGETHER   PIC X(1).
       FD  FAVRPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVRPT-REC.
       01  FAVRPT-REC.
           05  ARTIST-NAME-O            PIC X(30).
           05  NUMBER-OF-MUSICIANS-O    PIC 9(2).
           05  MUSICAL-GENRE-O          PIC X(12).
           05  FINAL-COST               PIC 9(4)V99.
           05  COST.
                10  CD-COST-O             PIC 9(3)V99.
                10  SHIPPING-COST-O       PIC 9(2)V99.
                10  TAX-O                 PIC 9(2)V99.
           05  BAND-IS-STILL-TOGETHER-O   PIC X(1).
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           OPEN INPUT FAVIN.
           OPEN OUTPUT FAVRPT.

           READ FAVIN.
           COMPUTE FINAL-COST =
                      (CD-COST * (TAX/100) ) + CD-COST
                           + SHIPPING-COST.
           MOVE ARTIST-NAME TO ARTIST-NAME-O.
           WRITE FAVRPT-REC.

           READ FAVIN.
           COMPUTE CD-COST-O =
                      CD-COST * TAX.
           MOVE ARTIST-NAME TO ARTIST-NAME-O.
           WRITE FAVRPT-REC.

           READ FAVIN.
           COMPUTE CD-COST-O =
                      CD-COST * TAX.
           MOVE ARTIST-NAME TO ARTIST-NAME-O.
           WRITE FAVRPT-REC.


           CLOSE  FAVIN, FAVRPT.

           GOBACK.