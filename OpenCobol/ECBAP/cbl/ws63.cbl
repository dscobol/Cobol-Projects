       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
      * REMARKS:
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAVIN
           ASSIGN TO "../../../common/data/ECBAP/favin.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-Favin-Status.
      *    SELECT FAVIN
      *    ASSIGN TO DA-S-FAVIN
      *       ORGANIZATION IS SEQUENTIAL
      *       FILE STATUS IS WS-Favin-Status.


       DATA DIVISION.
       FILE SECTION.
       FD  FAVIN.
      *     LABEL RECORDS ARE STANDARD
      *     RECORDING MODE IS F
      *     BLOCK CONTAINS 0 RECORDS
      *     RECORD CONTAINS 58 CHARACTERS.
       01  FAV-RECORD.
           12 FI-Group-Name               PIC X(30).
           12 FI-Number-Of-Musicians      PIC 9(02).
           12 FI-Musical-Genre            PIC X(12).
           12 FI-Costs.
              15 FI-CD-Cost               PIC 9(3)V99.
              15 FI-Shipping-Cost         PIC 9(2)V99.
              15 FI-Tax                   PIC 9(2)V99.
           12 FI-Group-Is-Still-Together  PIC X.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           12  WS-Favin-Status         PIC X(2) VALUE SPACES.
               88 WS-Favin-EOF                  VALUE '10'.
               88 WS-Favin-Okay                 VALUE '00'.

       01  FAVOUT-RECORD.
           12 FO-Group-Name               PIC X(30).
           12 FO-Number-Of-Musicians      PIC 9(02).
           12 FO-Musical-Genre            PIC X(12).
           12 FO-Costs.
              15 FO-CD-Cost               PIC 9(3)V99.
              15 FO-Shipping-Cost         PIC 9(2)V99.
              15 FO-Tax                   PIC 9(2)V99.
           12 FO-Group-Is-Still-Together  PIC X.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           OPEN INPUT FAVIN.
           PERFORM 5000-Read-FAVIN.

       2000-Process.
           PERFORM 2010-Move-Favin-Values UNTIL WS-Favin-EOF.

       2010-Move-Favin-Values.
           MOVE FI-Group-Name TO FO-Group-Name.
           MOVE FI-Number-Of-Musicians TO FO-Number-Of-Musicians.
           MOVE FI-Musical-Genre TO FO-Musical-Genre.
           COMPUTE FO-CD-Cost =
               FI-CD-Cost + FI-Shipping-Cost + FI-Tax.
           MOVE FI-Shipping-Cost TO FO-Shipping-Cost.
           MOVE FI-Tax TO FO-Tax.
           MOVE FI-Group-Is-Still-Together TO
                    FO-Group-Is-Still-Together.

           DISPLAY FAVOUT-RECORD.

           PERFORM 5000-Read-FAVIN.

       3000-End-Job.
           CLOSE FAVIN.

       5000-Read-FAVIN.
           READ FAVIN
              AT END SET WS-Favin-EOF TO TRUE
           END-READ.
