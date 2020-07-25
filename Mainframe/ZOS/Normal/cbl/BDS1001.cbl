       IDENTIFICATION DIVISION.
       PROGRAM-ID. BDS1001.
      * A three level Control Break program to process Electronics2Go
      * Sales file and produce a report that shows value of sales for
      * each Salesperson, each branch, each state, and for the country.
      * The SalesFile is sorted ascending SalespersonId within BranchId
      * within Statename.
      * The report must be printed in the same order

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SalesFile
      *     ASSIGN TO "../../../common/data/c10-1testdata.dat.txt"
      *     ORGANIZATION IS LINE SEQUENTIAL
           ASSIGN TO DA-S-SALEFILE
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-SaleFile-Status.

           SELECT SalesReport
      *     ASSIGN TO "../spool/bds1001-1.rpt"
      *     ORGANIZATION IS LINE SEQUENTIAL
           ASSIGN TO DA-S-SALERPT
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-SaleRpt-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  SalesFile.
       01  SalesRecord.
           02 StateName         PIC X(14).
           02 BranchId          PIC X(5).
           02 SalesPersonId     PIC X(6).
           02 ValueOfSale       PIC 9(4)V99.

       FD SalesReport.
       01 PrintLine             PIC X(55).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==SaleFile==.
           COPY WSFST REPLACING ==:tag:== BY ==SaleRpt==.

       01  ReportHeading.
           02 FILLER               PIC X(35)
              VALUE "        Electronics2Go Sales Report".

       01  SubjectHeading.
           02 FILLER               PIC X(43)
              VALUE "State Name      Branch  SalesId  SalesTotal".

       01  DetailLine.
           02 PrnStateName         PIC X(14).
              88 SuppressStateName VALUE SPACES.
           02 PrnBranchId          PIC BBX(5).
              88 SuppressBranchId  VALUE SPACES.
           02 PrnSalespersonId     PIC BBBBX(6).
           02 PrnSalespersonTotal  PIC BB$$,$$9.99.

       01  BranchTotalLine.
           02 FILLER               PIC X(43)
              VALUE "                         Branch Total:    ".
           02 PrnBranchTotal       PIC $$$,$$9.99.

       01  StateTotalLine.
           02 FILLER               PIC X(40)
              VALUE "                         State Total :  ".
           02 PrnStateTotal        PIC $$,$$$,$$9.99.

       01  FinalTotalLine.
           02 FILLER               PIC X(39)
              VALUE "                         Final Total :".
           02 PrnFinalTotal        PIC $$$,$$$,$$9.99.

       01  SalespersonTotal        PIC 9(4)V99.
       01  BranchTotal             PIC 9(6)V99.
       01  StateTotal              PIC 9(7)V99.
       01  FinalTotal              PIC 9(9)V99.

       01  PrevStateName           PIC X(14).
       01  PrevBranchId            PIC X(5).
       01  PrevSalespersonId       PIC X(6).

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-BOJ.
           PERFORM 2000-Process.
           PERFORM 3000-EOJ.
           GOBACK.

       1000-BOJ.
           OPEN INPUT SalesFile
           OPEN OUTPUT SalesReport
           WRITE PrintLine FROM ReportHeading  AFTER ADVANCING 1 LINE
           WRITE PrintLine FROM SubjectHeading AFTER ADVANCING 2 LINE
           PERFORM 5000-Read-Sales-File
           .

       2000-Process.
           PERFORM UNTIL WS-SaleFile-EOF
              MOVE StateName TO PrevStateName, PrnStateName
              MOVE ZEROS TO StateTotal
              PERFORM 2100-SumSalesForState
                 UNTIL StateName NOT = PrevStateName
                    OR WS-SaleFile-EOF
              MOVE StateTotal TO PrnStateTotal
              WRITE PrintLine FROM StateTotalLine AFTER ADVANCING 1 LINE
           END-PERFORM
           .

       2100-SumSalesForState.
           MOVE SPACES TO Printline
           WRITE PrintLine AFTER ADVANCING 1 LINE
           MOVE BranchId TO PrevBranchId, PrnBranchId
           MOVE ZEROS TO BranchTotal
           PERFORM 2200-SumSalesForBranch
              UNTIL BranchId NOT = PrevBranchId
                 OR StateName NOT = PrevStateName
                 OR WS-SaleFile-EOF
           MOVE BranchTotal TO PrnBranchTotal
           WRITE PrintLine FROM BranchTotalLine
              AFTER ADVANCING 1 LINE
           .

       2200-SumSalesForBranch.
           MOVE SalespersonId TO PrevSalespersonId, PrnSalespersonId
           MOVE ZEROS TO SalespersonTotal
           PERFORM 2300-SumSalespersonSales
              UNTIL SalespersonId NOT = PrevSalespersonId
                 OR BranchId   NOT = PrevBranchId
                 OR StateName  NOT = PrevStateName
                 OR WS-SaleFile-EOF
           MOVE SalespersonTotal TO PrnSalespersonTotal
           WRITE PrintLine FROM DetailLine AFTER ADVANCING 1 LINE
           SET SuppressBranchId TO TRUE
           SET SuppressStateName TO TRUE
           .

       2300-SumSalespersonSales.
           ADD ValueOfSale TO
              SalespersonTotal, BranchTotal, StateTotal, FinalTotal
           PERFORM 5000-Read-Sales-File
           .

       3000-EOJ.
           MOVE FinalTotal TO PrnFinalTotal
           WRITE PrintLine FROM FinalTotalLine AFTER ADVANCING 1 LINE
           CLOSE SalesFile, SalesReport
           .

       5000-Read-Sales-File.
           READ SalesFile
              AT END SET WS-SaleFile-EOF TO TRUE
           END-READ
           .
