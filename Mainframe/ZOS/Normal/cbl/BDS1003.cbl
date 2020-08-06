       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BDS1003.
      * Applies the transactions ordered on ascending GadgetId-TF
      * to the MasterStockFile ordered on ascending GadgetId-MF.
      * Assumption: Insert not followed by updates to inserted record
      *             Multiple updates per master record permitted

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MasterStockFile
      *     ASSIGN TO "../../../common/data/c10-3master.dat.txt"
      *     ORGANIZATION IS LINE SEQUENTIAL.
           ASSIGN TO DA-S-MSTIN3
           ORGANIZATION IS SEQUENTIAL.

           SELECT NewStockFile
      *     ASSIGN TO "../../../common/data/c10-bdsnewmast.dat.txt"
      *     ORGANIZATION IS LINE SEQUENTIAL.
           ASSIGN TO DA-S-MSTOUT3
           ORGANIZATION IS SEQUENTIAL.


           SELECT TransactionFile
      *     ASSIGN TO "../../../common/data/c10-3trans.dat.txt"
      *     ORGANIZATION IS LINE SEQUENTIAL.
           ASSIGN TO DA-S-TRANS3
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MasterStockFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01 MasterStockRec.
           88 EndOfMasterFile      VALUE HIGH-VALUES.
           02 GadgetId-MF          PIC 9(6).
           02 GadgetName-MF        PIC X(30).
           02 QtyInStock-MF        PIC 9(4).
           02 Price-MF             PIC 9(4)V99.

       FD NewStockFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01 NewStockRec.
           02 GadgetId-NSF        PIC 9(6).
           02 GadgetName-NSF      PIC X(30).
           02 QtyInStock-NSF      PIC 9(4).
           02 Price-NSF           PIC 9(4)V99.

       FD TransactionFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS V
           BLOCK CONTAINS 0 RECORDS.
       01 InsertionRec.
           88 EndOfTransFile      VALUE HIGH-VALUES.
           02 TypeCode-TF         PIC 9.
              88 Insertion       VALUE 1.
              88 Deletion        VALUE 2.
              88 UpdatePrice     VALUE 3.
           02 GadgetId-TF         PIC 9(6).
           02 GadgetName-IR       PIC X(30).
           02 QtyInStock-IR       PIC 9(4).
           02 Price-IR            PIC 9(4)V99.

       01 DeletionRec.
           02 FILLER              PIC 9(7).

       01 PriceChangeRec.
           02 FILLER              PIC 9(7).
           02 Price-PCR           PIC 9(4)V99.


       WORKING-STORAGE SECTION.
       01  ErrorMessage.
           02 PrnGadgetId        PIC 9(6).
           02 FILLER             PIC XXX VALUE " - ".
           02 FILLER             PIC X(45).
              88 InsertError
              VALUE "Insert Error - Record already exists".
              88 DeleteError
              VALUE "Delete Error - No such record in Master".
              88 PriceUpdateError
              VALUE "Price Update Error - No such record in Master".

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-BOJ.
           PERFORM 2000-Process.
           PERFORM 3000-EOJ.
           GOBACK.

       1000-BOJ.
           OPEN INPUT  MasterStockFile
                       TransactionFile.
           OPEN OUTPUT NewStockFile.
           PERFORM 5100-ReadMasterFile.
           PERFORM 5000-ReadTransFile.

       2000-Process.
           PERFORM UNTIL EndOfMasterFile AND EndOfTransFile
             EVALUATE TRUE
               WHEN GadgetId-TF > GadgetId-MF
                  PERFORM 2100-CopyToNewMaster
               WHEN GadgetId-TF = GadgetId-MF
                  PERFORM 2200-TryToApplyToMaster
               WHEN GadgetId-TF < GadgetId-MF
                  PERFORM 2300-TryToInsert
             END-EVALUATE
           END-PERFORM.


       2100-CopyToNewMaster.
           MOVE MasterStockRec TO NewStockRec.
           PERFORM 6000-Write-NewStockRec
           PERFORM 5100-ReadMasterFile.

       2200-TryToApplyToMaster.
           EVALUATE TRUE
             WHEN UpdatePrice
                MOVE Price-PCR TO Price-MF
             WHEN Deletion
                PERFORM 5100-ReadMasterFile
             WHEN Insertion
                SET InsertError TO TRUE
                DISPLAY ErrorMessage
           END-EVALUATE
           PERFORM 5000-ReadTransFile.

       2300-TryToInsert.
           IF Insertion
              MOVE GadgetId-TF   TO GadgetId-NSF
              MOVE GadgetName-IR TO GadgetName-NSF
              MOVE QtyInStock-IR TO QtyInStock-NSF
              MOVE Price-Ir      TO Price-NSF
              PERFORM 6000-Write-NewStockRec
             ELSE
               IF UpdatePrice
                  SET PriceUpdateError TO TRUE
               END-IF
               IF Deletion
                  SET DeleteError TO TRUE
               END-IF
               DISPLAY ErrorMessage
           END-IF
           PERFORM 5000-ReadTransFile.

       3000-EOJ.
           CLOSE MasterStockFile,
                 TransactionFile,
                 NewStockFile.

       5000-ReadTransFile.
           READ TransactionFile
               AT END SET EndOfTransFile TO TRUE
           END-READ
           MOVE GadgetId-TF TO PrnGadgetId.

       5100-ReadMasterFile.
           READ MasterStockFile
               AT END SET EndOfMasterFile TO TRUE
           END-READ.

       6000-Write-NewStockRec.
           WRITE NewStockRec.
