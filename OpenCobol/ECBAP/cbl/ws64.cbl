       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILECALC.
      * This program reads a file of input values into INVALS-WS
      * The operation read into the W-S structure drives the arithmetic
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVALS
           ASSIGN TO "../../../common/data/ECBAP/invals.dat.txt"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INVALS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INVALS-REC.
       01  INVALS-REC  PIC X(80).
       WORKING-STORAGE SECTION.
      * End of File switch
       01  INVALS-EOF               PIC X(01) VALUE SPACE.
       01  INVALS-WS.
           05  OPERATION            PIC X(01).
              88 DO-ADD                      VALUE "A".
              88 DO-SUB                      VALUE "S".
              88 DO-MUL                      VALUE "M".
              88 DO-DIV                      VALUE "D".
              88 DO-SQR                      VALUE "R".
           05  INVALS-1             PIC S99V99.
           05  INVALS-2             PIC S99.
           05  INVALS-RESULT        PIC S99999V99.
           05  INVALS-RESULT-DIS    PIC 99999.99+.

       PROCEDURE DIVISION.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main UNTIL INVALS-EOF = 'Y'.
           PERFORM 900-CLOSE-FILES.
           GOBACK.
       000-Housekeeping.
           INITIALIZE INVALS-WS.
           PERFORM 300-OPEN-FILES.
      * Priming Read
           PERFORM 400-Read-INVALS.
       100-Main.
           IF DO-ADD PERFORM 501-ADD
           ELSE IF DO-SUB PERFORM 502-SUBTRACT
           ELSE IF DO-MUL PERFORM 503-MULTIPLY
           ELSE IF DO-DIV PERFORM 504-DIVIDE
           ELSE IF DO-SQR PERFORM 505-SQRT.

           DISPLAY INVALS-RESULT.
           DISPLAY INVALS-RESULT-DIS.
           PERFORM 400-Read-INVALS.
       300-Open-Files.
           OPEN INPUT INVALS.
       400-Read-INVALS.
           READ INVALS INTO INVALS-WS
      * Set AT END Switch
               AT END MOVE "Y" TO INVALS-EOF
           END-READ.
       501-ADD.
           DISPLAY "Add".
           ADD INVALS-1, INVALS-2 GIVING
              INVALS-RESULT INVALS-RESULT-DIS.
       502-SUBTRACT.
           DISPLAY "Subtract".
           SUBTRACT INVALS-2 FROM INVALS-1 GIVING
              INVALS-RESULT INVALS-RESULT-DIS.
       503-MULTIPLY.
           DISPLAY "Mulitply".
           MULTIPLY INVALS-1 BY INVALS-2 GIVING
              INVALS-RESULT INVALS-RESULT-DIS.
       504-DIVIDE.
           DISPLAY "Divide".
           DIVIDE INVALS-2 BY INVALS-1 GIVING
              INVALS-RESULT INVALS-RESULT-DIS.
       505-SQRT.
           DISPLAY "Square Root".
           COMPUTE INVALS-RESULT INVALS-RESULT-DIS =
              FUNCTION SQRT(INVALS-1).
       900-CLOSE-FILES.
           CLOSE INVALS.
