       IDENTIFICATION DIVISION.
       PROGRAM-ID.  rpt1.
       AUTHOR.  IBM.
      * A simple Report Program with straight-line reporting logic
      * Read-and-Print-Read-and-Print, but check for Page Breaks

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS NEXT-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMERDETAILSREPORT
           ASSIGN TO UT-S-CUSTOUT
           FILE STATUS IS OFCODE
             ORGANIZATION IS SEQUENTIAL.

           SELECT CustomerFile
           ASSIGN TO UT-S-CUSTFILE
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS IFCODE.

       DATA DIVISION.
       FILE SECTION.
       FD  CustomerFile.
       01  CustomerRec.
           05  customerId      PIC 9(7).
           05  customerName    PIC X(30).
           05  LastActivityDate.
              10 yyyy          PIC 9(4).
              10 MM            PIC 9(2).
              10 dddd          PIC 9(2).
           05  TerritoryCode   PIC X(4).
           05  ActiveInd       PIC X.
           05  FILLER          PIC X(30).

       FD  CustomerDetailsReport.
       01  PrintLine          PIC X(80).

       WORKING-STORAGE SECTION.
      **** Report Header and Footer Records
       01  Page-Heading.
           05 FILLER                       PIC X(12)  VALUE SPACES.
           05 FILLER                       PIC X(35)
                             VALUE "IBM Customer Territory Report".
       01  Rpt-Detail-Hdg-1.
           05 FILLER                       PIC X(41)
                             VALUE "CustomerId CustomerName".
           05 FILLER                       PIC X(33)
                             VALUE " Territory Active/Inactive".
       01  Rpt-Detail-Hdg-2.
           05 FILLER                       PIC X(41)
                      VALUE "========== ==============================".
           05 FILLER                       PIC X(33)
                             VALUE " ========= ===============".
       01  PageFooter.
           05 FILLER                       PIC X(19) VALUE SPACES.
           05 FILLER                       PIC X(7) VALUE "Page> ".
           05 PrintCustPageNbr             PIC Z9.
       01  ReportFooter.
           05 FILLER                       PIC X(12) VALUE SPACES.
           05 End-Literal                  PIC X(58)
                  VALUE "*** End of Customer Territory Report ***".
      **** Report Detail line record
       01  CustomerDetailLine.
           05 Filler                       PIC X VALUE SPACES.
           05 PrintCustCustId              PIC X(11).
           05 PrintCustCustName            PIC X(31).
           05 PrintCustTerritory           PIC X(15).
           05 PrintCustInd                 PIC X.
      **** Standard report processing line, page counters, etc.
       01  Misc-Ctrs-Flags.
      **** Change Page Break to 3
           05 LineCount                    PIC 99 VALUE ZEROS.
             88 PageBrkReq VALUE 6.
           05 PageNbr                      PIC 99 VALUE ZEROS.
           05 EOF-Flag                     PIC X.
                88  EndOfCustomerFile  VALUE Spaces.
           05  IFCODE                      PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  OFCODE                      PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       PROCEDURE DIVISION.
      **** Main business logic (driver logic) area
           OPEN INPUT customerFile
           OPEN OUTPUT customerDetailsReport

      **** Do initiail Heading Routine and priming file read
           PERFORM 200-PrtHdrs
           READ CustomerFile
               AT END SET EndOfCustomerFile TO TRUE
           END-READ

      **** Do report logic - until no more input records
           PERFORM 100-PrintReportBody UNTIL EndOfcustomerFile

      **** Do end-of-job tasks
           WRITE PrintLine FROM ReportFooter
                AFTER ADVANCING 3 LINES
           CLOSE customerFile, customerDetailsReport
           GOBACK.

       100-PrintReportBody.
      **** Is LineCount > 50?  If yes, do Page Break Logic
           IF PageBrkReq
             ADD 1 TO PageNbr
             MOVE PageNbr TO PrintCustPageNbr
             WRITE PrintLine FROM PageFooter
                   AFTER ADVANCING 3 LINES
             PERFORM 200-PrtHdrs
           END-IF

      **** Report body logic
           MOVE customerId TO PrintCustCustId
           MOVE customerName TO PrintCustCustName
           MOVE TerritoryCode  TO PrintCustTerritory
           IF activeInd = "A"
               MOVE "Active" TO PrintCustInd IN customerDetailLine
           else
               MOVE "InActive" TO PrintCustInd IN customerDetailLine
           END-IF
           WRITE PrintLine FROM customerDetailLine
               AFTER ADVANCING 2 LINES *> DOUBLE SPACE REPORT
           ADD 1 TO LineCount
      **** Read next record/prepare for next report-line
           READ customerFile
               AT END SET EndOfcustomerFile TO TRUE
           END-READ.

       200-PrtHdrs.
      **** Print next page - and reset counters
           WRITE PrintLine FROM Page-Heading
                AFTER ADVANCING PAGE
           WRITE PrintLine FROM Rpt-Detail-Hdg-1
                AFTER ADVANCING 2 LINES
           WRITE PrintLine FROM Rpt-Detail-Hdg-2
           MOVE 3 TO LineCount.
