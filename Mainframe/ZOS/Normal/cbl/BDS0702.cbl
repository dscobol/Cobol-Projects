       IDENTIFICATION DIVISION.
       PROGRAM-ID. BDS0702.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPFILE ASSIGN TO DA-S-EMPFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 43 CHARACTERS.
       01  EMPDETAILS.
           02 EMPSSN              PIC 9(9).
           02 EMPNAME.
              03 EMPSURNAME       PIC X(15).
              03 EMPFORENAME      PIC X(10).
           02 EMPDATEOFBIRTH.
              03 EMPYOB           PIC 9(4).
              03 EMPMOB           PIC 99.
              03 EMPDOB           PIC 99.
           02 EMPGENDER           PIC X.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS.
           15 WS-EOF              PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       0000-MAINLINE.
           PERFORM 1000-BOJ.
           PERFORM 2000-PROCESS UNTIL WS-EOF = 'Y'.
           PERFORM 3000-EOJ.

       1000-BOJ.
           OPEN INPUT EMPFILE.
           READ EMPFILE
               AT END MOVE 'Y' TO WS-EOF.


       2000-PROCESS.
           DISPLAY EMPFORENAME SPACE EMPSURNAME SPACE '- '
      -    EMPMOB '/' EMPDOB '/' EMPYOB.
           READ EMPFILE
               AT END MOVE 'Y' TO WS-EOF.


       3000-EOJ.
           CLOSE EMPFILE.
           STOP RUN.

