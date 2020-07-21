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

           SELECT FAV-Report
           ASSIGN TO "../spool/fav-report.rpt"
           ORGANIZATION IS LINE SEQUENTIAL.


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

       FD  FAV-Report.
       01  Print-Line        PIC X(132).


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           12 WS-Favin-Status         PIC X(2) VALUE SPACES.
              88 WS-Favin-EOF                  VALUE '10'.
              88 WS-Favin-Okay                 VALUE '00'.

       01  CURRENT-DATE-AND-TIME.
           12 CDT-Year                PIC 9(4).
           12 CDT-Month               PIC 9(2). *> 01-12
           12 CDT-Day                 PIC 9(2). *> 01-31
           12 CDT-Hour                PIC 9(2). *> 00-23
           12 CDT-Minutes             PIC 9(2). *> 00-59
           12 CDT-Seconds             PIC 9(2). *> 00-59
           12 CDT-Hundredths-Of-Secs  PIC 9(2). *> 00-99
           12 CDT-GMT-Diff-Hours      PIC S9(2)
                                      SIGN LEADING SEPARATE.
           12 CDT-GMT-Diff-Minutes    PIC 9(2). *> 00 or 30
       
       01  WS-Counters.
           12 WS-FI-Record-Cnt           PIC 9(4) COMP. 

       01  FAVOUT-RECORD.
           12 FO-Group-Name               PIC X(30).
           12 FO-Number-Of-Musicians      PIC 9(02).
           12 FO-Musical-Genre            PIC X(12).
           12 FO-Costs.
              15 FO-CD-Cost               PIC 9(3)V99.
              15 FO-Shipping-Cost         PIC 9(2)V99.
              15 FO-Tax                   PIC 9(2)V99.
           12 FO-Group-Is-Still-Together  PIC X.

       01  R1-Counters.
           12 R1-Max-Lines         PIC 9(4) COMP VALUE 60.
           12 R1-Line-Count        PIC 9(4) COMP VALUE ZEROES.
           12 R1-Line-Advance      PIC 9(4) COMP VALUE ZEROES.
           12 R1-Page-Count        PIC 9(4) COMP VALUE ZEROES.

       01  R1-Page-Header.
           12 FILLER                   PIC X(006) VALUE "Date: ".
           12 R1-HDR-DATE.
              15 R1-HDR-YY             PIC 9(4).
              15 FILLER                PIC X(1) VALUE "-".
              15 R1-HDR-MM             PIC 9(2).
              15 FILLER                PIC X(1) VALUE "-".
              15 R1-HDR-DD             PIC 9(2).
           12 FILLER                   PIC X(020) VALUE SPACE.
           12 FILLER                   PIC X(044)
                 VALUE "Favorite Artists Report".
 
       01  R1-Column-Header1.
           12 FILLER   PIC X(030) VALUE "Artist".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "Number of".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "Musical".
           12 FILLER   PIC X(005) VALUE SPACES.
           12 FILLER   PIC X(007) VALUE "CD".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(008) VALUE "SHIP".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(008) VALUE "Tax".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "Total".
           12 FILLER   PIC X(003) VALUE SPACES.

       01  R1-Column-Header2.   
           12 FILLER   PIC X(030) VALUE "Name".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "Musicians".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "Genre".
           12 FILLER   PIC X(004) VALUE SPACES.
           12 FILLER   PIC X(008) VALUE "Cost".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(008) VALUE "Cost".
           12 FILLER   PIC X(014) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "Cost".
           12 FILLER   PIC X(003) VALUE SPACES.


       01  R1-Column-Header3.
           12 FILLER    PIC X(030) VALUE ALL "=".
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 FILLER    PIC X(009) VALUE ALL "=".
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 FILLER    PIC X(009) VALUE ALL "=".
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 FILLER    PIC X(008) VALUE ALL "=".
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 FILLER    PIC X(008) VALUE ALL "=".
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 FILLER    PIC X(009) VALUE ALL "=".
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "=".

       01  R1-Detail-Line.
           12 R1-Group-Name      PIC X(030).
           12 FILLER             PIC X(006) VALUE SPACES.
           12 R1-No-Of-Musicians PIC Z(002).
           12 FILLER             PIC X(007) VALUE SPACES.
           12 R1-Musical-Genre   PIC X(010).
           12 FILLER             PIC X(002) VALUE SPACES.
           12 R1-CD-Cost         PIC $,$$9.99.
           12 FILLER             PIC X(002) VALUE SPACES.
           12 R1-Shipping-Cost   PIC $,$$9.99.
           12 FILLER             PIC X(003) VALUE SPACES.
           12 R1-Tax             PIC $,$$9.99.
           12 FILLER             PIC X(003) VALUE SPACES.
           12 R1-Total-Cost      PIC $$,$$9.99.

       01  R1-Footer.
           12 FILLER             PIC X(034)   
              VALUE "Number of Artists in this Report: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Recs-Read PIC ZZ9.




       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           OPEN INPUT FAVIN.
           OPEN OUTPUT FAV-Report.
           PERFORM 6101-Setup-R1
           PERFORM 6110-Write-R1-Page-Header.
           PERFORM 5100-Read-FAVIN.

       2000-Process.
           PERFORM 2010-Move-Favin-Values UNTIL WS-Favin-EOF.

       2010-Move-Favin-Values.
           MOVE FI-Group-Name TO 
              FO-Group-Name R1-Group-Name. 
           MOVE FI-Number-Of-Musicians TO 
              FO-Number-Of-Musicians R1-No-Of-Musicians. 
           MOVE FI-Musical-Genre TO 
              FO-Musical-Genre R1-Musical-Genre.
           MOVE FI-CD-Cost TO R1-CD-Cost.
           MOVE FI-Shipping-Cost TO 
              FO-Shipping-Cost R1-Shipping-Cost.
           MOVE FI-Tax TO 
              FO-Tax R1-Tax.
           MOVE FI-Group-Is-Still-Together TO
                    FO-Group-Is-Still-Together.

           COMPUTE R1-Total-Cost =
               FI-CD-Cost + FI-Shipping-Cost + FI-Tax.

           MOVE 1 TO R1-Line-Advance.
           PERFORM 6100-Write-R1.

           PERFORM 5100-Read-FAVIN.

       3000-End-Job.
           MOVE WS-FI-Record-Cnt to R1-Total-Recs-Read.
           MOVE 2 TO R1-Line-Advance.
           MOVE R1-Footer TO Print-Line. 
           PERFORM 6100-Write-R1.

           CLOSE FAVIN
                 FAV-Report.

       5100-Read-FAVIN.
           READ FAVIN
              AT END SET WS-Favin-EOF TO TRUE
           END-READ.
           IF WS-Favin-Okay
               ADD 1 TO WS-FI-Record-Cnt
           END-IF.

       6100-Write-R1.

           IF R1-Line-Count + R1-Line-Advance > R1-Max-Lines
                 PERFORM 6110-Write-R1-Page-Header
           END-IF.

           IF WS-Favin-Okay
              PERFORM 6120-Write-R1-Detail
           ELSE
              PERFORM 6130-Write-R1-Footer
           END-IF.

       6101-Setup-R1.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CDT-Year     TO R1-HDR-YY.
           MOVE CDT-Month    TO R1-HDR-MM.
           MOVE CDT-Day      TO R1-HDR-DD.

       6110-Write-R1-Page-Header.
           ADD +1 TO R1-Page-Count.
           WRITE Print-Line FROM R1-Page-Header 
              AFTER ADVANCING PAGE.
           WRITE Print-Line FROM R1-Column-Header1 
              AFTER ADVANCING 2.
           WRITE Print-Line FROM R1-Column-Header2 
              AFTER ADVANCING 1.
           WRITE Print-Line FROM R1-Column-Header3 
              AFTER ADVANCING 1.
           MOVE 5 TO R1-Line-Count.

       6120-Write-R1-Detail.
           MOVE R1-Detail-Line TO Print-Line. 
           WRITE Print-Line
              AFTER ADVANCING R1-Line-Advance LINES.
           ADD R1-Line-Advance TO R1-Line-Count.

       6130-Write-R1-Footer.
           MOVE R1-Footer TO Print-Line. 
           WRITE Print-Line
              AFTER ADVANCING R1-Line-Advance LINES.
           ADD R1-Line-Advance TO R1-Line-Count.
