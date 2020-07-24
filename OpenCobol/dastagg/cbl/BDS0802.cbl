       IDENTIFICATION DIVISION.
       PROGRAM-ID. BDS0802.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           Select Shop-File
           ASSIGN TO "../../../common/data/c08-shopsales2.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-Shop-File-Status.


       DATA DIVISION.
       FILE SECTION.
       FD  Shop-File.
       01  Shop-Details.
           02 SF-Rec-Type            PIC X.
              88 SF-Shop-Header      VALUE "H".
              88 SF-Shop-Sale        VALUE "S".
              88 SF-Shop-Footer      VALUE "F".
           02 SF-Shop-Id             PIC X(5).
           02 SF-Shop-Location       PIC X(30).

       01  Sales-Receipt.
           02 SF-Rec-Type            PIC X.
           02 SR-Item-Id             PIC X(8).
           02 SR-Qty-Sold            PIC 9(3).
           02 SR-Item-Cost           PIC 999V99.

       01  Shop-Sales-Count.
           02 SF-Rec-Type            PIC X.
           02 SC-Rec-Count           PIC 9(5).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==Shop-File==.

       01  RPT-Shop-Sales-Total.
           02 FILLER            PIC X(21) VALUE "Total sales for shop ".
           02 RPT-Shop-Id       PIC X(5).
           02 RPT-Shop-Total    PIC $$$$,$$9.99.

       01  RPT-Error-Message.
           02 FILLER                PIC X(15) VALUE "Error on Shop: ".
           02 RPT-ERR-Shop-Id       PIC X(5).
           02 FILLER                PIC X(10) VALUE " RCount = ".
           02 RPT-ERR-Rec-Count     PIC 9(5).
           02 FILLER                PIC X(10) VALUE " ACount = ".
           02 RPT-ERR-Actual-Count  PIC 9(5).

       01  WS-Shop-Total             PIC 9(5)V99.
       01  WS-Actual-Count           PIC 9(5).

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-BOJ.
           PERFORM 2000-Process.
           PERFORM 3000-EOJ.
           STOP RUN.

       1000-BOJ.
           OPEN INPUT Shop-File.
           PERFORM 5000-Read-Shop-File.

       2000-Process.
           PERFORM 2010-Summarize-Country-Sales
              UNTIL WS-Shop-File-EOF.

       2010-Summarize-Country-Sales.
           MOVE SF-Shop-Id  TO RPT-Shop-Id, RPT-ERR-Shop-Id
           MOVE ZEROS TO WS-Shop-Total

           PERFORM 5000-Read-Shop-File

           PERFORM 2020-Summarize-Shop-Sales
              VARYING WS-Actual-Count FROM 0 BY 1 UNTIL SF-Shop-Footer

           IF SC-Rec-Count = WS-Actual-Count
              MOVE WS-Shop-Total TO RPT-Shop-Total
              DISPLAY RPT-Shop-Sales-Total
           ELSE
              MOVE SC-Rec-Count TO RPT-ERR-Rec-Count
              MOVE WS-Actual-Count TO RPT-ERR-Actual-Count
              DISPLAY RPT-Error-Message
           END-IF

           PERFORM 5000-Read-Shop-File.

       2020-Summarize-Shop-Sales.
           COMPUTE WS-Shop-Total =
              WS-Shop-Total + (SR-Qty-Sold * SR-Item-Cost)

           PERFORM 5000-Read-Shop-File.

       3000-EOJ.
           CLOSE Shop-File.

       5000-Read-Shop-File.
           READ Shop-File
              AT END SET WS-Shop-File-EOF TO TRUE
           END-READ.
