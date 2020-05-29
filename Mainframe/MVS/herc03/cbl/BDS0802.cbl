       IDENTIFICATION DIVISION.
       PROGRAM-ID. BDS0802.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SHOP-FILE ASSIGN TO DA-S-SHOPFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  SHOP-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS V
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 36 CHARACTERS.

       01  SHOP-DETAILS.
           02 SF-REC-TYPE            PIC X.
              88 SF-SHOP-HEADER      VALUE 'H'.
              88 SF-SHOP-SALE        VALUE 'S'.
              88 SF-SHOP-FOOTER      VALUE 'F'.
           02 SF-SHOP-ID             PIC X(5).
           02 SF-SHOP-LOCATION       PIC X(30).

       01  SALES-RECEIPT.
           02 SF-REC-TYPE            PIC X.
           02 SR-ITEM-ID             PIC X(8).
           02 SR-QTY-SOLD            PIC 9(3).
           02 SR-ITEM-COST           PIC 999V99.

       01  SHOP-SALES-COUNT.
           02 SF-REC-TYPE            PIC X.
           02 SC-REC-COUNT           PIC 9(5).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           12 WS-EOF           PIC X(1) VALUE 'N'.

      01 RPT-SHOP-SALES-TOTAL.
           02 FILLER            PIC X(21) VALUE 'TOTAL SALES FOR SHOP '.
           02 RPT-SHOP-ID       PIC X(5).
           02 RPT-SHOP-TOTAL    PIC $$$$,$$9.99.

       01 RPT-ERROR-MESSAGE.
           02 FILLER                PIC X(15) VALUE 'ERROR ON SHOP: '.
           02 RPT-ERR-SHOP-ID       PIC X(5).
           02 FILLER                PIC X(10) VALUE ' RCOUNT = '.
           02 RPT-ERR-REC-COUNT     PIC 9(5).
           02 FILLER                PIC X(10) VALUE ' ACOUNT = '.
           02 RPT-ERR-ACTUAL-COUNT  PIC 9(5).

       01  WS-SHOP-TOTAL             PIC 9(5)V99.
       01  WS-ACTUAL-COUNT           PIC 9(5).

       PROCEDURE DIVISION.
       0000-MAINLINE.
           PERFORM 1000-BOJ.
           PERFORM 2000-PROCESS.
           PERFORM 3000-EOJ.
           STOP RUN.

       1000-BOJ.
           OPEN INPUT SHOP-FILE.
           PERFORM 5000-READ-SHOP-FILE.

       2000-PROCESS.
           PERFORM 2010-SUMMARIZE-COUNTRY-SALES
               UNTIL WS-EOF = 'Y'.

       2010-SUMMARIZE-COUNTRY-SALES.
           MOVE SF-SHOP-ID  TO RPT-SHOP-ID, RPT-ERR-SHOP-ID.
           MOVE ZEROS TO WS-SHOP-TOTAL.

           PERFORM 5000-READ-SHOP-FILE.

           PERFORM 2020-SUMMARIZE-SHOP-SALES
              VARYING WS-ACTUAL-COUNT FROM 0 BY 1 UNTIL SF-SHOP-FOOTER.

           IF SC-REC-COUNT = WS-ACTUAL-COUNT
              MOVE WS-SHOP-TOTAL TO RPT-SHOP-TOTAL
              DISPLAY RPT-SHOP-SALES-TOTAL
           ELSE
              MOVE SC-REC-COUNT TO RPT-ERR-REC-COUNT
              MOVE WS-ACTUAL-COUNT TO RPT-ERR-ACTUAL-COUNT
              DISPLAY RPT-ERROR-MESSAGE.

           PERFORM 5000-READ-SHOP-FILE.

       2020-SUMMARIZE-SHOP-SALES.
           COMPUTE WS-SHOP-TOTAL =
              WS-SHOP-TOTAL + (SR-QTY-SOLD * SR-ITEM-COST).

           PERFORM 5000-READ-SHOP-FILE.

       3000-EOJ.
           CLOSE SHOP-FILE.

       5000-READ-SHOP-FILE.
           READ SHOP-FILE
              AT END MOVE 'Y' TO WS-EOF.
