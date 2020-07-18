       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
      * REMARKS:
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAVIN
           ASSIGN TO "../../../common/data/favin.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-Favin-Status.


       DATA DIVISION.
       FILE SECTION.
       FD  FAVIN.
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
       01  FAVOUT-RECORD.
           12 FO-Group-Name               PIC X(30).
           12 FO-Number-Of-Musicians      PIC 9(02).
           12 FO-Musical-Genre            PIC X(12).
           12 FO-Costs.
              15 FO-CD-Cost               PIC 9(3)V99.
              15 FO-Shipping-Cost         PIC 9(2)V99.
              15 FO-Tax                   PIC 9(2)V99.
           12 FO-Group-Is-Still-Together  PIC X.

       01  WS-FILE-STATUS.
           12  WS-Favin-Status         PIC X(2) VALUE SPACES.
               88 WS-Favin-EOF                  VALUE '10'.
               88 WS-Favin-Okay                 VALUE '00'.
           12  WS-Favout-Status        PIC X(2) VALUE SPACES.
               88 WS-Favout-EOF                 VALUE '10'.
               88 WS-Favout-Okay                VALUE '00'.

       PROCEDURE DIVISION.

           OPEN INPUT FAVIN.

           READ FAVIN.
           DISPLAY "Read Favin."
           DISPLAY FAV-RECORD.
           INITIALIZE FAVOUT-RECORD. 

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

           READ FAVIN.
           DISPLAY FAV-RECORD.

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

           READ FAVIN.
           DISPLAY FAV-RECORD.

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

           CLOSE FAVIN.

           GOBACK.
