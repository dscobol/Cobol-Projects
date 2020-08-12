       IDENTIFICATION DIVISION.
       PROGRAM-ID.  rpt2.
       AUTHOR.  IBM.
      * A simple Report Program with straight-line reporting logic
      * Read-and-Print-Read-and-Print, but check for Page Breaks

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT customerDetailsReport
             ASSIGN TO "c:\customerOut.dat"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS IFCODE.

           SELECT customerFile
             ASSIGN TO "c:\customerIn.dat"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT errorFile
             ASSIGN TO "c:\customerErr.dat"
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS EFCODE.

       DATA DIVISION.
       FILE SECTION.
       FD  CustomerFile.
       01  CustomerRec.
           05  customerId      PIC 9(7).
           05  customerID-RDF REDEFINES customerId  PIC X(7).
           05  customerName    PIC X(30).
           05  LastActivityDate.
              10 yyyy          PIC 9(4).
              10 MM            PIC 9(2).
              10 dddd          PIC 9(2).
           05  TerritoryCode   PIC X(4).
           05  ActiveInd       PIC X.
           05  Q1-Tax          PIC 9(5)V99.
           05  Q2-Tax          PIC 9(5)V99.
           05  Q3-Tax          PIC 9(5)V99.
           05  Q4-Tax          PIC 9(5)V99.

       FD  CustomerDetailsReport.
       01  PrintLine           PIC X(133).

       FD  errorFile.
       01  ErrorRec            PIC X(80).

       WORKING-STORAGE SECTION.
      **** Report Header and Footer Records
       01  Page-Heading.
           05 FILLER                       PIC X(12)  VALUE SPACES.
           05 FILLER                       PIC X(35)
                             VALUE "IBM Customer Territory Report".
       01  Rpt-Detail-Hdg-1.
           05 FILLER                       PIC X(41)
                             VALUE "CustomerId CustomerName".
           05 FILLER                       PIC X(27)
                             VALUE " Territory Active/Inactive".
           05 FILLER                       PIC X(33)
              VALUE  "Q1 Taxes   Q2 Taxes   Q3 Taxes   ".
           05 FILLER                       PIC X(26)
              VALUE  "Q4 Taxes    Total Taxes".
       01  Rpt-Detail-Hdg-2.
           05 FILLER                       PIC X(41)
                      VALUE "========== ==============================".
           05 FILLER                       PIC X(27)
                             VALUE " ========= ===============".
           05 FILLER                       PIC X(34)
              VALUE  "=========  =========  =========   ".
           05 FILLER                       PIC X(26)
              VALUE  "=========    ============".
       01  PageFooter.
           05 FILLER                       PIC X(19) VALUE SPACES.
           05 FILLER                       PIC X(7) VALUE "Page: ".
           05 PrintCustPageNbr             PIC Z9.
       01  ReportFooter.
           05 FILLER                       PIC X(1) VALUE SPACES.
           05 End-Literal                  PIC X(58)
                  VALUE "*** End of Customer Territory Report ***".

       01  ReportFinalStats-1.
           05 FILLER                       PIC X(33)
                       VALUE " Number of Records in:".
           05 InRecCount-Final             PIC Z(4).

       01  ReportFinalStats-2.
           05 Filler                       PIC X(33)
                       VALUE " Number of Error Records:".
           05 BadRecCount-Final            PIC Z(4).

       01  ReportFinalStats-3.
           05 Filler                       PIC X(33)
                       VALUE " Number of Good Output Records:".
           05 OutGoodRecCount-Final        PIC Z(4).

       01  ReportFinalStats-4.
           05 Filler                       PIC X(24)
                       VALUE " Average Taxes Paid:".
           05 AvgTaxesPaidOut              PIC $$,$$$,$$$.99.


      **** Report Detail line record
       01  CustomerDetailLine.
           05 Filler                       PIC X VALUE SPACES.
           05 PrintCustCustId              PIC X(11).
           05 PrintCustCustName            PIC X(31).
           05 PrintCustTerritory           PIC X(10).
           05 PrintCustInd                 PIC X(15).
           05  Q1-Tax-Out                  PIC $$,$$$.99.
           05 Filler                       PIC XX VALUE SPACES.
           05  Q2-Tax-Out                  PIC $$,$$$.99.
           05 Filler                       PIC XX VALUE SPACES.
           05  Q3-Tax-Out                  PIC $$,$$$.99.
           05 Filler                       PIC XX VALUE SPACES.
           05  Q4-Tax-Out                  PIC $$,$$$.99.
           05 Filler                       PIC XX VALUE SPACES.
           05  Total-Tax-Out               PIC $$,$$$,$$$.99.
      **** Standard report processing line, page counters, etc.
       01  Misc-Ctrs-Flags.
           05 LineCount                    PIC 99 VALUE ZEROS.
             88 PageBrkReq VALUE 10 THRU 99.
           05 PageNbr                      PIC 99 VALUE ZEROS.
           05 BadRecCount                  PIC 9(4) comp value zeros.
           05 OutGoodRecCount              PIC 9(4) comp value zeros.
           05 InRecCount                   PIC 9(4) comp value zeros.
           05 BadRecSw                     PIC X(1) value spaces.
           88 BadRecord    value "Y".
           05 Total-Tax                    PIC 9(7)V99 comp-3.
           05 RPT-Total-Tax                PIC 9(9)V99 comp-3 value 0.

           05 EOF-Flag                     PIC X.
                88  EndOfCustomerFile  VALUE Spaces.
           05  IFCODE                      PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  OFCODE                      PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  EFCODE                      PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       01  WS-DATE.
           05 CURR-YEAR        PIC 9(4).
           05 CURR-MONTH       PIC 9(2).
           05 CURR-DAY         PIC 9(2).

       PROCEDURE DIVISION.
      **** Main business logic (driver logic) area
           ACCEPT  WS-DATE FROM DATE YYYYMMDD.
           OPEN INPUT customerFile
           OPEN OUTPUT customerDetailsReport, errorFile

      **** Do initiail Heading Routine and priming file read
           PERFORM 200-PrtHdrs
           READ CustomerFile
               AT END SET EndOfCustomerFile TO TRUE
           END-READ

      **** Do report logic - until no more input records
           PERFORM 100-PrintReportBody thru 100-EXIT
               UNTIL EndOfcustomerFile

      **** Do end-of-job tasks
           move InRecCount         to InRecCount-Final.
           move BadRecCount        to BadRecCount-Final.
           move OutGoodRecCount    to OutGoodRecCount-Final.
           compute AvgTaxesPaidOut =
               RPT-Total-Tax / OutGoodRecCount * 1.0.
           write PrintLine FROM ReportFinalStats-1
                           AFTER ADVANCING 2 LINES.
           write PrintLine FROM ReportFinalStats-2.
           write PrintLine FROM ReportFinalStats-3.
           write PrintLine FROM ReportFinalStats-4.

           WRITE PrintLine FROM ReportFooter
                AFTER ADVANCING 2 LINES
           CLOSE customerFile, customerDetailsReport, errorFile.
           GOBACK.

       100-PrintReportBody.
           add +1 to InRecCount.
           Perform 300-EditInputRec thru 300-EXIT
           if BadRecord
               Write ErrorRec from CustomerRec
               READ customerFile
                   AT END SET EndOfcustomerFile TO TRUE
               END-READ
               go to 100-EXIT
           end-if.

      **** Is LineCount > 10?  If yes, do Page Break Logic
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
           Move Q1-Tax to Q1-Tax-Out.
           Move Q2-Tax to Q2-Tax-Out.
           Move Q3-Tax to Q3-Tax-Out.
           Move Q4-Tax to Q4-Tax-Out.
           compute Total-Tax  =
                   Q1-TAX + Q2-TAX + Q3-TAX + Q4-TAX.
           move Total-Tax to Total-Tax-Out.
           WRITE PrintLine FROM customerDetailLine
               AFTER ADVANCING 1 LINE
           ADD 1 TO LineCount
           add 1 to OutGoodRecCount.
           add Total-Tax to RPT-Total-Tax.
      **** Read next record/prepare for next report-line
           READ customerFile
               AT END SET EndOfcustomerFile TO TRUE
           END-READ.

       100-EXIT.
           EXIT.

       200-PrtHdrs.
      **** Print next page - and reset counters
           WRITE PrintLine FROM Page-Heading
                AFTER ADVANCING PAGE
           WRITE PrintLine FROM Rpt-Detail-Hdg-1
                AFTER ADVANCING 2 LINES
           WRITE PrintLine FROM Rpt-Detail-Hdg-2
           MOVE 3 TO LineCount.

       300-EditInputRec.
           Move "N" to BadRecSw.
           If customerID-RDF is Not NUMERIC
              add +1 to BadRecCount
              move "Y" to BadRecSw
              go to 300-EXIT.

           If yyyy < 1990 or > CURR-YEAR
              add +1 to BadRecCount
              move "Y" to BadRecSw
              go to 300-EXIT.

       300-EXIT.
           EXIT.