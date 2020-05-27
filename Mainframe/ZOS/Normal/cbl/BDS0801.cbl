       IDENTIFICATION DIVISION.
       PROGRAM-ID. BDS0801.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           Select Shop-File  
           ASSIGN TO SHOPFILE
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-Shop-File-Status.
       
       DATA DIVISION.
       FILE SECTION.
       FD Shop-File. 
       01 SF-Details.
           02 SF-Rec-Type-Code     PIC X.
               88 SF-Header        VALUE "H".
               88 SF-Sale          VALUE "S".
           02 SH-Shop-Id           PIC X(5).
           02 SH-Shop-Location     PIC X(30).
       
       01 SF-Receipt.
           02 SF-Rec-Type-Code     PIC X.
           02 SR-Item-Id           PIC X(8).
           02 SR-Qty-Sold          PIC 9(3).
           02 SR-Item-Cost         PIC 999V99.
       
       WORKING-STORAGE SECTION.

       01 WS-FILE-STATUS.
       COPY wsfst REPLACING ==:tag:== BY ==Shop-File==.

       01 RPT-Shop-Sales-Total-Line.
           02 FILLER           PIC X(21) VALUE "Total sales for shop ".
           02 RPT-Shop-Id      PIC X(5).
           02 RPT-Shop-Total   PIC $$$$,$$9.99. 
       
       01 WS-Shop-Total           PIC 9(5)V99 VALUE ZERO.
       
       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-BOJ.
           PERFORM 2000-Process.
           PERFORM 3000-EOJ.
           STOP RUN.

       1000-BOJ.
           OPEN INPUT Shop-File
           READ Shop-File
               AT END SET WS-Shop-File-EOF  TO TRUE
           END-READ.
           IF NOT SF-Header
               DISPLAY "*** ERROR ***" " First Record not HEADER."
               PERFORM 3000-EOJ
               STOP RUN
           END-IF.

       2000-Process.
           PERFORM 5010-Summarize-Country-Sales 
               UNTIL WS-Shop-File-EOF.

       3000-EOJ.
           CLOSE Shop-File.
       
      
       5010-Summarize-Country-Sales.
           MOVE SH-Shop-Id  TO RPT-Shop-Id
           MOVE ZEROS TO WS-Shop-Total 
           READ Shop-File
               AT END SET WS-Shop-File-EOF TO TRUE
           END-READ
           PERFORM 5020-Summarize-Shop-Sales
                   UNTIL SF-Header OR WS-Shop-File-EOF
           MOVE WS-Shop-Total TO RPT-Shop-Total
           DISPLAY RPT-Shop-Sales-Total-Line.
           
       5020-Summarize-Shop-Sales.
           COMPUTE  WS-Shop-Total = 
               WS-Shop-Total + (SR-Qty-Sold * SR-Item-Cost)
           READ Shop-File
               AT END SET WS-Shop-File-EOF TO TRUE
           END-READ.
