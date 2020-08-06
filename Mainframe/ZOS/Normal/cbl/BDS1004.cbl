       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BDS1004.
      * File Update program based on the algorithm described by Barry
      * Dwyer in "One more time - How to update a Master File"
      * Applies the transactions ordered on ascending GadgetId-TF
      * to the MasterStockFile ordered on ascending GadgetId-MF.
      * Within each key value records are ordered on the sequence
      * in which events occurred in the outside world.
      * All valid, real world, transaction sequences are accommodated

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MasterStockFile
      *     ASSIGN TO "../../../common/data/c10-4master.dat.txt"
      *     ORGANIZATION IS LINE SEQUENTIAL.
           ASSIGN TO DA-S-MSTIN4
           ORGANIZATION IS SEQUENTIAL.

           SELECT NewStockFile
      *     ASSIGN TO "../../../common/data/c10-4newmast.dat.txt"
      *     ORGANIZATION IS LINE SEQUENTIAL.
           ASSIGN TO DA-S-MSTOUT4
           ORGANIZATION IS SEQUENTIAL.


           SELECT TransactionFile
      *     ASSIGN TO "../../../common/data/c10-4trans.dat.txt"
      *     ORGANIZATION IS LINE SEQUENTIAL.
           ASSIGN TO DA-S-TRANS4
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MasterStockFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01 MasterStockRec.
           88 EndOfMasterFile     VALUE HIGH-VALUES.
           02 GadgetID-MF         PIC 9(6).
           02 GadgetName-MF       PIC X(30).
           02 QtyInStock-MF       PIC 9(4).
           02 Price-MF            PIC 9(4)V99.

       FD NewStockFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01 NewStockRec.
           02 GadgetID-NSF        PIC 9(6).
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
           02 RecordBody-IR.
             03 GadgetID-TF      PIC 9(6).
             03 GadgetName-IR    PIC X(30).
             03 QtyInStock-IR    PIC 9(4).
             03 Price-IR         PIC 9(4)V99.

       01 DeletionRec.
           02 FILLER              PIC 9(7).

       01 PriceChangeRec.
           02 FILLER              PIC 9(7).
           02 Price-PCR           PIC 9(4)V99.


       WORKING-STORAGE SECTION.
       01 ErrorMessage.
           02 PrnGadgetId         PIC 9(6).
           02 FILLER              PIC XXX VALUE " - ".
           02 FILLER              PIC X(45).
             88 InsertError
             VALUE "Insert Error - Record already exists".
             88 DeleteError
             VALUE "Delete Error - No such record in Master".
             88 PriceUpdateError
             VALUE "Price Update Error - No such record in Master".


       01 FILLER                 PIC X VALUE "n".
           88 RecordInMaster      VALUE "y".
           88 RecordNotInMaster   VALUE "n".

       01 CurrentKey             PIC 9(6).

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
           PERFORM 2010-ChooseNextKey.

       2000-Process.
           PERFORM UNTIL EndOfMasterFile AND EndOfTransFile
             PERFORM 2020-SetInitialStatus
             PERFORM 2100-ProcessOneTransaction
                     UNTIL GadgetID-TF NOT = CurrentKey
      *     CheckFinalStatus
             IF RecordInMaster
                PERFORM 6000-Write-NewStockRec
             END-IF
             PERFORM 2010-ChooseNextKey
           END-PERFORM.

       2010-ChooseNextKey.
           IF GadgetID-TF < GadgetID-MF
             MOVE GadgetID-TF TO CurrentKey
           ELSE
             MOVE GadgetID-MF TO CurrentKey
           END-IF.

       2020-SetInitialStatus.
           IF GadgetID-MF =  CurrentKey
             MOVE MasterStockRec TO NewStockRec
             SET RecordInMaster TO TRUE
             PERFORM 5100-ReadMasterFile
           ELSE SET RecordNotInMaster TO TRUE
           END-IF.

       2100-ProcessOneTransaction.
      *  ApplyTransToMaster
           EVALUATE TRUE
              WHEN Insertion
                 PERFORM 2110-ApplyInsertion
              WHEN UpdatePrice
                 PERFORM 2120-ApplyPriceChange
              WHEN Deletion
                 PERFORM 2130-ApplyDeletion
           END-EVALUATE.
           PERFORM 5000-ReadTransFile.

       2110-ApplyInsertion.
           IF RecordInMaster
             SET InsertError TO TRUE
             DISPLAY ErrorMessage
           ELSE
             SET RecordInMaster TO TRUE
             MOVE RecordBody-IR TO NewStockRec
           END-IF.

       2120-ApplyPriceChange.
           IF RecordNotInMaster
             SET PriceUpdateError TO TRUE
             DISPLAY ErrorMessage
           ELSE
             MOVE Price-PCR TO Price-NSF
           END-IF.

       2130-ApplyDeletion.
           IF RecordNotInMaster
             SET DeleteError TO TRUE
             DISPLAY ErrorMessage
           ELSE
             SET RecordNotInMaster TO TRUE
           END-IF.

       3000-EOJ.
           CLOSE MasterStockFile,
                 TransactionFile,
                 NewStockFile.

       5000-ReadTransFile.
           READ TransactionFile
                AT END SET EndOfTransFile TO TRUE
           END-READ
           MOVE GadgetID-TF TO PrnGadgetId.

       5100-ReadMasterFile.
           READ MasterStockFile
                AT END SET EndOfMasterFile TO TRUE
           END-READ.

       6000-Write-NewStockRec.
           WRITE NewStockRec.
