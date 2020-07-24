       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLMRPT.
      * REMARKS:
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSCLAIM
           ASSIGN TO "../../../common/data/ECBAP/insclaim.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
      *     ASSIGN TO DA-S-INSCLAIM
      *     ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-INSClaim-Status.

           SELECT INSRPT
           ASSIGN TO "../spool/insclaim-report.rpt"
           ORGANIZATION IS LINE SEQUENTIAL
      *     ASSIGN TO DA-S-INSRPT
      *     ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-INSRpt-Status.


       DATA DIVISION.
       FILE SECTION.
       FD  INSCLAIM
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CLAIMREC.

       FD  INSRPT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  Print-Line        PIC X(131).


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==INSClaim==.
           COPY WSFST REPLACING ==:tag:== BY ==INSRpt==.

       01  CURRENT-DATE-AND-TIME.
           COPY WSDT REPLACING ==:tag:== BY ==CDT==.

       01  WS-Counters.
           12 WS-INSClaim-Record-Cnt  PIC 9(4) COMP.

           12 WS-Days-Diff            PIC S9(7).

       01  R1-Counters.
           12 R1-Max-Lines         PIC S9(4) COMP VALUE 60.
           12 R1-Line-Count        PIC S9(4) COMP VALUE ZEROES.
           12 R1-Line-Advance      PIC S9(4) COMP VALUE ZEROES.
           12 R1-Page-Count        PIC S9(4) COMP VALUE ZEROES.
           12 R1-Lines-Written     PIC S9(4) COMP VALUE ZEROES.

       01  R1-Page-Header.
           12 FILLER                   PIC X(006) VALUE "Date: ".
           12 R1-HDR-DATE.
              15 R1-HDR-YY             PIC 9(4).
              15 FILLER                PIC X(1) VALUE "-".
              15 R1-HDR-MM             PIC 9(2).
              15 FILLER                PIC X(1) VALUE "-".
              15 R1-HDR-DD             PIC 9(2).
           12 FILLER                   PIC X(021) VALUE SPACE.
           12 FILLER                   PIC X(034)
                 VALUE "    Group Claims Daily Totals     ".
           12 FILLER                   PIC X(049) VALUE SPACE.
           12 FILLER                   PIC X(005) VALUE "Page:".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 R1-HDR-Page-Count        PIC ZZ9.

       01 R1-Page-Header2.
           12 FILLER PIC X(040) VALUE
              "All policies STD deductible: .002%".

       01  R1-Column-Header1.
           12 FILLER   PIC X(026) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Policy".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "First".
           12 FILLER   PIC X(006) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Last".
           12 FILLER   PIC X(014) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Renew".
           12 FILLER   PIC X(004) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Deduc".
           12 FILLER   PIC X(005) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Co-Pay".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Deduc".
           12 FILLER   PIC X(012) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Claim".
           12 FILLER   PIC X(011) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Claim".

       01  R1-Column-Header2.
           12 FILLER   PIC X(011) VALUE "Policy Type".
           12 FILLER   PIC X(015) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Number".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Name".
           12 FILLER   PIC X(007) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Name".
           12 FILLER   PIC X(015) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Date".
           12 FILLER   PIC X(005) VALUE SPACES.
           12 FILLER   PIC X(003) VALUE "Met".
           12 FILLER   PIC X(006) VALUE SPACES.
           12 FILLER   PIC X(007) VALUE "Amount".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Amount".
           12 FILLER   PIC X(010) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Amount".
           12 FILLER   PIC X(012) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Paid".


       01  R1-Column-Header3.
           12 FILLER    PIC X(023) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(015) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(005) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(011) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(006) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(015) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(015) VALUE ALL "-".

       01  R1-Detail-Line.
           12 R1-Policy-Type                PIC X(23).
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 R1-Insured-Policy-No          PIC XXXXXXXXX.
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 R1-Insured-First-Name         PIC X(10).
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 R1-Insured-Last-Name          PIC X(15).
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 R1-Policy-Benefit-Date-Num    PIC XXXX/XX/XX.
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 R1-Policy-Deductible-Met     PIC X.
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 R1-Policy-Copay-Amount         PIC $$$$,$$9.99.
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 R1-Deductible-Amount           PIC $$$$.
           12 FILLER    PIC X(004) VALUE ALL SPACES.
           12 R1-Claim-Amount               PIC $$,$$$,$$9.99.
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 R1-Claim-Amount-Paid          PIC $$,$$$,$$9.99.

       01  R1-Footer1.
           12 FILLER             PIC X(036)
              VALUE " Number of Input Records Read: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Recs-Read PIC ZZ9.
       01  R1-Footer2.
           12 FILLER             PIC X(036)
              VALUE "        Number of Claims Paid: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Valid-Records PIC ZZ9.
       01  R1-Footer3.
           12 FILLER             PIC X(036)
              VALUE "    Number of Claims Not Paid: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-InValid-Records PIC ZZ9.

       01  EOJ-Display-Messages.
           12 EOJ-End-Message PIC X(040) VALUE
              "*** Program CLMRPT - End of Run ***".
           12 EOJ-Print-Message PIC X(40) VALUE SPACES.
           12 EOJ-Print-Number  PIC ZZ,ZZ9 VALUE ZEROES.
           12 EOJ-Print-Money   PIC $$,$$9.99 VALUE ZEROES.

       01  WS-Claim-Flags.
           12 WS-Valid-Claim-Flag         PIC X.
              88 WS-Valid-Claim              VALUE "V".
              88 WS-InValid-Claim            VALUE "I".

       01  WS-Claim-Counters.
           12 WS-Claim-Deductible-Amt     PIC S9(7)V99.
           12 WS-Claim-CoPay-Amt          PIC S9(7)V99.
           12 WS-Claim-Paid-Amt           PIC S9(7)V99.
           12 WS-Claim-Valid-Claims       PIC S9(5) VALUE ZEROES.
           12 WS-Claim-InValid-Claims     PIC S9(5) VALUE ZEROES.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           OPEN INPUT INSCLAIM.
           OPEN OUTPUT INSRPT.
           PERFORM 6101-Setup-R1
           PERFORM 6110-Write-R1-Page-Header.
           PERFORM 5100-Read-INSClaim.

       2000-Process.
           PERFORM 2100-Process-Claims UNTIL WS-INSClaim-EOF.

       2100-Process-Claims.
           Set WS-Valid-Claim to TRUE.
           PERFORM 2110-Determine-Claim-Payment
           IF WS-Valid-Claim
              Add +1 TO WS-Claim-Valid-Claims
              PERFORM 2120-Move-Fixed-Fields

              MOVE 1 TO R1-Line-Advance
              PERFORM 6100-Write-R1
           ELSE
              Add +1 TO WS-Claim-InValid-Claims
           END-IF.

           PERFORM 5100-Read-INSClaim.

       2110-Determine-Claim-Payment.
           PERFORM 2111-Check-Benefit-Date.
           IF WS-Valid-Claim
              PERFORM 2112-Check-Deductible
           END-IF.

       2111-Check-Benefit-Date.

           COMPUTE WS-Days-Diff =
              FUNCTION INTEGER-OF-DATE (Policy-Benefit-Date-Num) -
              FUNCTION INTEGER-OF-DATE (CDT-Full-Date)

           IF WS-Days-Diff < ZERO
              SET WS-InValid-Claim TO TRUE
              DISPLAY "** Invalid Claim **"
              DISPLAY "Policy Not in Effect."
              DISPLAY "Policy Number: " Insured-Policy-No
              DISPLAY SPACES
           ELSE
              MOVE Policy-Benefit-Date-Num TO
                 R1-Policy-Benefit-Date-Num
           END-IF.

       2112-Check-Deductible.
           COMPUTE WS-Claim-Deductible-Amt ROUNDED =
              Policy-Amount * .002
           END-COMPUTE.

           IF Policy-Deductible-Paid > WS-Claim-Deductible-Amt
              MOVE ZEROES TO WS-Claim-Deductible-Amt
           ELSE
              IF Policy-Deductible-Paid > ZEROES
                 SUBTRACT Policy-Deductible-Paid  FROM
                    WS-Claim-Deductible-Amt       GIVING
                    WS-Claim-Deductible-Amt
                 END-SUBTRACT
              END-IF
           END-IF.

           COMPUTE WS-Claim-CoPay-Amt ROUNDED =
              Claim-Amount * Policy-Coinsurance
           END-COMPUTE.

           COMPUTE WS-Claim-Paid-Amt =
              Claim-Amount - WS-Claim-Deductible-Amt -
                 WS-Claim-CoPay-Amt
           END-COMPUTE.

           IF WS-Claim-Paid-Amt > Policy-Amount
              SET WS-InValid-Claim TO TRUE
              DISPLAY "** Invalid Claim **"
              DISPLAY "Claim exceeds Policy Amount."
              DISPLAY "Policy Number: " Insured-Policy-No
              DISPLAY SPACES
           ELSE
             IF WS-Claim-Deductible-Amt > ZEROES
                Move "N" TO R1-Policy-Deductible-Met
             ELSE
                Move "Y" TO R1-Policy-Deductible-Met
             END-IF
             MOVE WS-Claim-Deductible-Amt TO R1-Deductible-Amount
             MOVE WS-Claim-CoPay-Amt TO R1-Policy-Copay-Amount
             MOVE WS-Claim-Paid-Amt TO R1-Claim-Amount-Paid
           END-IF.

       2120-Move-Fixed-Fields.
           STRING Insured-Policy-No(1:1), "-",
                  Insured-Policy-No(2:3), "-",
                  Insured-Policy-No(5:3) DELIMITED BY SIZE
              INTO R1-Insured-Policy-No.
           MOVE Insured-Last-Name TO R1-Insured-Last-Name.
           MOVE Insured-First-Name TO R1-Insured-First-Name.
           EVALUATE TRUE
              WHEN PRIVATE
                 MOVE "Employer-Private" TO R1-Policy-Type
              WHEN MEDICARE
                 MOVE "Standard Medicare" TO R1-Policy-Type
              WHEN AFFORDABLE-CARE
                 MOVE "Affordable Care Act" TO R1-Policy-Type
           END-EVALUATE.
           MOVE Claim-Amount TO R1-Claim-Amount.

       3000-End-Job.
           MOVE WS-INSClaim-Record-Cnt  TO R1-Total-Recs-Read.
           MOVE WS-Claim-Valid-Claims   TO R1-Total-Valid-Records.
           MOVE WS-Claim-InValid-Claims TO R1-Total-InValid-Records.
           PERFORM 6130-Write-R1-Footer.

           CLOSE INSCLAIM
                 INSRPT.

       5100-Read-INSClaim.
           READ INSCLAIM
              AT END SET WS-INSClaim-EOF TO TRUE
           END-READ.
           IF WS-INSClaim-Good
              ADD 1 TO WS-INSClaim-Record-Cnt
           ELSE
              IF WS-INSClaim-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 5100-Read-INSClaim"
                 DISPLAY "Read INSCLAIM Failed."
                 DISPLAY "File Status: " WS-INSClaim-Status
                 GOBACK
              END-IF
           END-IF.

       6100-Write-R1.
           IF R1-Line-Count + R1-Line-Advance > R1-Max-Lines
              PERFORM 6110-Write-R1-Page-Header
              PERFORM 6120-Write-R1-Detail
           ELSE
              PERFORM 6120-Write-R1-Detail
           END-IF.

       6101-Setup-R1.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CDT-Year     TO R1-HDR-YY.
           MOVE CDT-Month    TO R1-HDR-MM.
           MOVE CDT-Day      TO R1-HDR-DD.

       6110-Write-R1-Page-Header.
           ADD +1 TO R1-Page-Count.
           MOVE R1-Page-Count TO R1-HDR-Page-Count.
           WRITE Print-Line FROM R1-Page-Header
              AFTER ADVANCING PAGE.
           WRITE Print-Line FROM R1-Page-Header2
              AFTER ADVANCING 1.
           WRITE Print-Line FROM R1-Column-Header1
              AFTER ADVANCING 2.
           WRITE Print-Line FROM R1-Column-Header2
              AFTER ADVANCING 1.
           WRITE Print-Line FROM R1-Column-Header3
              AFTER ADVANCING 1.
           MOVE 6 TO R1-Line-Count.

       6120-Write-R1-Detail.
           MOVE R1-Detail-Line TO Print-Line.
           WRITE Print-Line
              AFTER ADVANCING R1-Line-Advance LINES.
           ADD R1-Line-Advance TO R1-Line-Count.
           ADD +1 TO R1-Lines-Written.

       6130-Write-R1-Footer.
           IF R1-Line-Count + 4 > R1-Max-Lines
              PERFORM 6110-Write-R1-Page-Header
           END-IF.
           MOVE R1-Footer1 TO Print-Line.
           WRITE Print-Line
              AFTER ADVANCING 2 LINES.
           MOVE R1-Footer2 TO Print-Line.
           WRITE Print-Line
              AFTER ADVANCING 1 LINES.
           MOVE R1-Footer3 TO Print-Line.
           WRITE Print-Line
              AFTER ADVANCING 1 LINES.
           PERFORM 6140-Display-EOJ-Messages.

       6140-Display-EOJ-Messages.
           DISPLAY EOJ-End-Message.
