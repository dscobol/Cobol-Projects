       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
      * REMARKS.
      *
      * The specifications present a circular calculation.
      * COST-PER-INSTRUMENT = Price + Quality + Tax + Shipping.
      *
      * Tax = 8% of COST-PER-INSTRUMENT
      * Shipping = % of COST-PER-INSTRUMENT
      * If both are included in the COST, how to calculate?
      *
      * This program will calculate the amounts as follows:
      * P1 = Price + Quality
      * P2 = P1 * Shipping
      * P3 = P1 * Tax.
      * COST-PER-INSTRUMENT = P1 + P2 + P3.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RFPIN
           ASSIGN TO "../../../common/data/ECBAP/favrfp.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
      *     ASSIGN TO DA-S-RFPIN
      *     ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-RFPIN-Status.

           SELECT PROPOSAL
           ASSIGN TO "../../../common/data/ECBAP/proposal.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
      *     ASSIGN TO DA-S-PROPOSAL
      *     ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-Proposal-Status.

           SELECT BADRFP
           ASSIGN TO "../../../common/data/ECBAP/badrfp.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
      *     ASSIGN TO DA-S-BADRFP
      *     ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-BadRFP-Status.

           SELECT PROPRPT
           ASSIGN TO "../spool/prop-report.rpt"
           ORGANIZATION IS LINE SEQUENTIAL
      *     ASSIGN TO DA-S-PROPRPT
      *     ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-Proprpt-Status.


       DATA DIVISION.
       FILE SECTION.
       FD  RFPIN
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  RFP-Record.
           12 RFP-Artist-Acct-No             PIC X(08).
           12 RFP-Artist-Musical-Genre       PIC X(06).
              88 RFP-Rock      VALUE 'ROCK'.
              88 RFP-Jazz      VALUE 'JAZZ'.
              88 RFP-Fusion    VALUE 'FUSION'.
           12 RFP-Musician.
              15 RFP-Musician-Lname          PIC X(15).
              15 RFP-Musician-Fname          PIC X(15).
           12 RFP-Musician-Instrument-Type   PIC X(06).
           12 RFP-Instrument-Quality         PIC X(01).
              88 RFP-Used-Flag       VALUE 'U'.
              88 RFP-New-Flag        VALUE 'N'.
              88 RFP-Premium-Flag    VALUE 'P'.
           12 RFP-Max-Musician-Budget-Amount PIC 9(5)V99.
           12 RFP-Ship-To                    PIC X(03).
              88 RFP-In-Country      VALUE 'IN'.
              88 RFP-Out-of-Country  VALUE 'OUT'.
           12 FILLER                         PIC X(19).

       FD  PROPOSAL
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 91 CHARACTERS.
       01  PRP-Record.
           12 PRP-Artist-Acct-No             PIC X(08).
           12 PRP-Artist-Musical-Genre       PIC X(06).
              88 PRP-Rock      VALUE 'ROCK'.
              88 PRP-Jazz      VALUE 'JAZZ'.
              88 PRP-Fusion    VALUE 'FUSION'.
           12 PRP-Musician.
              15 PRP-Musician-Lname          PIC X(15).
              15 PRP-Musician-Fname          PIC X(15).
           12 PRP-Musician-Instrument-Type   PIC X(12).
              88 PRP-Keyboards      VALUE 'KEYS'.
              88 PRP-Vocals         VALUE 'VOCALS'.
              88 PRP-Guitar         VALUE 'GUITAR'.
              88 PRP-Bass           VALUE 'BASS'.
              88 PRP-Drums          VALUE 'DRUMS'.
              88 PRP-Percussion     VALUE 'PERC'.
           12 PRP-Instrument-Quality         PIC X(12).
           12 PRP-Ship-To                    PIC X(03).
              88 PRP-In-Country     VALUE 'In'.
              88 PRP-Out-of-Country VALUE 'Out'.
           12 PRP-Cost-Per-Instrument        PIC 9(7)V99.
           12 PRP-Additional-Costs.
              15 PRP-Shipping-Cost           PIC 9(4)V99.
              15 PRP-Tax                     PIC 9(3)V99.
       FD  BADRFP
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  BRFP-Record.
           12 BRFP-Artist-Acct-No             PIC X(08).
           12 BRFP-Artist-Musical-Genre       PIC X(06).
              88 BRFP-Rock      VALUE 'ROCK'.
              88 BRFP-Jazz      VALUE 'JAZZ'.
              88 BRFP-Fusion    VALUE 'FUSION'.
           12 BRFP-Musician.
              15 BRFP-Musician-Lname          PIC X(15).
              15 BRFP-Musician-Fname          PIC X(15).
           12 BRFP-Musician-Instrument-Type   PIC X(06).
           12 BRFP-Instrument-Quality         PIC X(01).
              88 BRFP-Used-Flag       VALUE 'U'.
              88 BRFP-New-Flag        VALUE 'N'.
              88 BRFP-Premium-Flag    VALUE 'P'.
           12 BRFP-Max-Musician-Budget-Amt    PIC 9(5)V99.
           12 BRFP-Ship-To                    PIC X(03).
              88 BRFP-In-Country      VALUE 'IN'.
              88 BRFP-Out-of-Country  VALUE 'OUT'.
           12 FILLER                         PIC X(19).

       FD  PROPRPT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS.
       01  Print-Line        PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           12 WS-RFPIN-Status         PIC X(2) VALUE SPACES.
              88 WS-RFPIN-EOF                  VALUE '10'.
              88 WS-RFPIN-Okay                 VALUE '00'.
           12 WS-Proposal-Status      PIC X(2) VALUE SPACES.
              88 WS-Proposal-EOF               VALUE '10'.
              88 WS-Proposal-Okay              VALUE '00'.
           12 WS-Proprpt-Status       PIC X(2) VALUE SPACES.
              88 WS-Proprpt-EOF                VALUE '10'.
              88 WS-Proprpt-Okay               VALUE '00'.
           12 WS-BadRFP-Status        PIC X(2) VALUE SPACES.
              88 WS-BadRFP-EOF                 VALUE '10'.
              88 WS-BadRFP-Okay                VALUE '00'.

       01  WS-Counters.
           12 WS-RFPIN-Record-Cnt     PIC 9(4) COMP VALUE ZEROES.
           12 WS-Proposal-Record-Cnt  PIC 9(4) COMP VALUE ZEROES.
           12 WS-BadRFP-Record-Cnt    PIC 9(4) COMP VALUE ZEROES.


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
                 VALUE "Artist Request for Proposal Report".
           12 FILLER                   PIC X(049) VALUE SPACE.
           12 FILLER                   PIC X(005) VALUE "Page:".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 R1-HDR-Page-Count        PIC ZZ9.

       01  R1-Column-Header1.
           12 FILLER   PIC X(007) VALUE "Account".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Music".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "Musician's".
           12 FILLER   PIC X(022) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Music".
           12 FILLER   PIC X(019) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "In/Out".
           12 FILLER   PIC X(003) VALUE SPACES.

       01  R1-Column-Header2.
           12 FILLER   PIC X(006) VALUE "Number".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Genre".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(009) VALUE "Last Name".
           12 FILLER   PIC X(007) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "First Name".
           12 FILLER   PIC X(006) VALUE SPACES.
           12 FILLER   PIC X(010) VALUE "Instrument".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(007) VALUE "Quality".
           12 FILLER   PIC X(004) VALUE SPACES.
           12 FILLER   PIC X(007) VALUE "Country".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(011) VALUE "Total Price".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(008) VALUE "Shipping".
           12 FILLER   PIC X(005) VALUE SPACES.
           12 FILLER   PIC X(003) VALUE "Tax".


       01  R1-Column-Header3.
           12 FILLER    PIC X(008) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(006) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(015) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(015) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(012) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(007) VALUE ALL "=".
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 FILLER    PIC X(012) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(009) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(007) VALUE ALL "=".

       01  R1-Detail-Line.
           12 R1-Artist-Acct-No           PIC X(008).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Artist-Musical-Genre     PIC X(006).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Musician-Lname           PIC X(015).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Musician-Fname           PIC X(015).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Musician-Instrument-Type PIC X(012).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Instrument-Quality       PIC X(012).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Ship-To                  PIC X(003).
           12 FILLER                      PIC X(003) VALUE SPACES.
           12 R1-Cost-Per-Instrument      PIC $$,$$$,$$9.99.
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Shipping-Cost            PIC $$,$$9.99.
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Tax                      PIC $$$9.99.

       01  R1-Footer1.
           12 FILLER             PIC X(036)
              VALUE " Number of Input Records Read: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Recs-Read PIC ZZ9.
       01  R1-Footer2.
           12 FILLER             PIC X(036)
              VALUE "      Number of Valid Records: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Valid-Records PIC ZZ9.
       01  R1-Footer3.
           12 FILLER             PIC X(036)
              VALUE "    Number of Invalid Records: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Invalid-Records PIC ZZ9.
       01  R1-Footer4.
           12 FILLER             PIC X(036)
              VALUE " Grand Total of all Proposals: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Value-Proposals PIC $$,$$$,$$9.99.

       01 EOJ-Display-Messages.
           12 EOJ-End-Message PIC X(034) VALUE
              "*** Program WS81E - End of Run ***".
           12 EOJ-Print-Message PIC X(40) VALUE SPACES.
           12 EOJ-Print-Number  PIC ZZ,ZZ9 VALUE ZEROES.
           12 EOJ-Print-Money   PIC $$,$$9.99 VALUE ZEROES.

       01  WS-RPF-Price-List.
           12 WS-Price-Keyboards PIC 9(5)V99.
           12 WS-Price-Vocals    PIC 9(5)V99.
           12 WS-Price-Guitar    PIC 9(5)V99.
           12 WS-Price-Bass      PIC 9(5)V99.
           12 WS-Price-Drums     PIC 9(5)V99.
           12 WS-Price-Perc      PIC 9(5)V99.
           12 WS-STD-Tax-Rate    PIC V99.
           12 WS-Ship-Rate-In    PIC V99.
           12 WS-Ship-Rate-Out   PIC V99.
           12 WS-Used-Adjustment PIC V99.
           12 WS-Prem-Adjustment PIC V99.

       01 WS-RPF-Storage.
           12 WS-RFP-Total-Prop  PIC 9(7)V99 VALUE ZEROES.
           12 WS-RFP-P1          PIC 9(5)V99 VALUE ZEROES.
           12 WS-RFP-P2          PIC 9(5)V99 VALUE ZEROES.
           12 WS-RFP-P3          PIC 9(5)V99 VALUE ZEROES.
           12 WS-RFP-Valid-Flag  PIC X.
              88 WS-RFP-Valid       VALUE 'Y'.
              88 WS-RFP-Invalid     VALUE 'N'.

           12 WS-RFP-Error-Counters.
              15 WS-RFP-Err-Account-Number      PIC S9(5) COMP-3.
              15 WS-RFP-Err-Genre               PIC S9(5) COMP-3.
              15 WS-RFP-Err-Names               PIC S9(5) COMP-3.
              15 WS-RFP-Err-Instrument-Type     PIC S9(5) COMP-3.
              15 WS-RFP-Err-Quality             PIC S9(5) COMP-3.
              15 WS-RFP-Err-Budget              PIC S9(5) COMP-3.
              15 WS-RFP-Err-Ship-To             PIC S9(5) COMP-3.


       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           OPEN INPUT RFPIN.
           OPEN OUTPUT PROPOSAL
                       BADRFP
                       PROPRPT.
           PERFORM 1010-Populate-RFP-Price-List.
           PERFORM 6101-Setup-R1.
           PERFORM 6110-Write-R1-Page-Header.
           PERFORM 5100-Read-RFPIN.

       1010-Populate-RFP-Price-List.
           INITIALIZE WS-RPF-Price-List WS-RPF-Storage.
           MOVE 3017.89 TO WS-Price-Keyboards.
           MOVE  599.05 TO WS-Price-Vocals.
           MOVE 2648.99 TO WS-Price-Guitar.
           MOVE 1876.10 TO WS-Price-Bass.
           MOVE 3087.22 TO WS-Price-Drums.
           MOVE  799.99 TO WS-Price-Perc.

           MOVE .08 TO WS-STD-Tax-Rate.
           MOVE .10 TO WS-Ship-Rate-In.
           MOVE .20 TO WS-Ship-Rate-Out.
           MOVE .20 TO WS-Used-Adjustment.
           MOVE .20 TO WS-Prem-Adjustment.

       2000-Process.
           PERFORM 2100-Create-RFP UNTIL WS-RFPIN-EOF.

       2100-Create-RFP.
           SET WS-RFP-Valid TO TRUE.
           PERFORM 2180-Validate-RFP-Record.
           IF WS-RFP-Valid
             PERFORM 2110-Move-Fixed-Fields
             PERFORM 2120-Calculate-Inst-Price
             PERFORM 2130-Calculate-Shipping
             PERFORM 2140-Calculate-Tax

             COMPUTE PRP-Cost-Per-Instrument =
                WS-RFP-P1 + WS-RFP-P2 + WS-RFP-P3
             MOVE PRP-Cost-Per-Instrument TO R1-Cost-Per-Instrument

             MOVE WS-RFP-P2 TO PRP-Shipping-Cost R1-Shipping-Cost
             MOVE WS-RFP-P3 TO PRP-Tax R1-Tax

             COMPUTE WS-RFP-Total-Prop =
                WS-RFP-Total-Prop + PRP-Cost-Per-Instrument

             MOVE 1 TO R1-Line-Advance
             PERFORM 6100-Write-R1
             PERFORM 6000-Write-Proposal
           ELSE
              DISPLAY "*** Warning *** Invalid RFP-Record."
              DISPLAY "* This record was not processed. *"
              DISPLAY RFP-Record
              DISPLAY SPACES
              MOVE RFP-Record TO BRFP-Record
              PERFORM 6200-Write-BadRFP
           END-IF.

           PERFORM 5100-Read-RFPIN.

       2110-Move-Fixed-Fields.
           MOVE RFP-Artist-Acct-No TO
              PRP-Artist-Acct-No R1-Artist-Acct-No.
           MOVE RFP-Artist-Musical-Genre TO
              PRP-Artist-Musical-Genre R1-Artist-Musical-Genre.
           MOVE RFP-Musician-Lname TO
              PRP-Musician-Lname R1-Musician-Lname.
           MOVE RFP-Musician-Fname TO
              PRP-Musician-Fname R1-Musician-Fname.
           MOVE RFP-Musician-Instrument-Type TO
              PRP-Musician-Instrument-Type R1-Musician-Instrument-Type.
           EVALUATE TRUE
              WHEN RFP-Used-Flag
                 MOVE "USED" TO
                 PRP-Instrument-Quality R1-Instrument-Quality
              WHEN RFP-New-Flag
                 MOVE "NEW" TO
                 PRP-Instrument-Quality R1-Instrument-Quality
              WHEN RFP-Premium-Flag
                 MOVE "PREMIUM" TO
                 PRP-Instrument-Quality R1-Instrument-Quality
           END-EVALUATE.
           MOVE RFP-Ship-To TO
              PRP-Ship-To R1-Ship-To.

       2120-Calculate-Inst-Price.
           EVALUATE RFP-Musician-Instrument-Type
              WHEN 'KEYS'
                 EVALUATE TRUE
                    WHEN RFP-Used-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Keyboards -
                          (WS-Price-Keyboards * WS-Used-Adjustment)
                    WHEN RFP-Premium-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Keyboards +
                          (WS-Price-Keyboards * WS-Prem-Adjustment)
                    WHEN RFP-New-Flag
                       COMPUTE WS-RFP-P1 = WS-Price-Keyboards
                    END-EVALUATE
              WHEN 'VOCALS'
                 EVALUATE TRUE
                    WHEN RFP-Used-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Vocals -
                          (WS-Price-Vocals * WS-Used-Adjustment)
                    WHEN RFP-Premium-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Vocals +
                          (WS-Price-Vocals * WS-Prem-Adjustment)
                    WHEN RFP-New-Flag
                       COMPUTE WS-RFP-P1 = WS-Price-Vocals
                    END-EVALUATE
              WHEN 'GUITAR'
                 EVALUATE TRUE
                    WHEN RFP-Used-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Guitar -
                          (WS-Price-Guitar * WS-Used-Adjustment)
                    WHEN RFP-Premium-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Guitar +
                          (WS-Price-Guitar * WS-Prem-Adjustment)
                    WHEN RFP-New-Flag
                       COMPUTE WS-RFP-P1 = WS-Price-Guitar
                    END-EVALUATE
              WHEN 'BASS'
                 EVALUATE TRUE
                    WHEN RFP-Used-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Bass -
                          (WS-Price-Bass * WS-Used-Adjustment)
                    WHEN RFP-Premium-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Bass +
                          (WS-Price-Bass * WS-Prem-Adjustment)
                    WHEN RFP-New-Flag
                       COMPUTE WS-RFP-P1 = WS-Price-Bass
                    END-EVALUATE
              WHEN 'DRUMS'
                 EVALUATE TRUE
                    WHEN RFP-Used-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Drums -
                          (WS-Price-Drums * WS-Used-Adjustment)
                    WHEN RFP-Premium-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Drums +
                          (WS-Price-Drums * WS-Prem-Adjustment)
                    WHEN RFP-New-Flag
                       COMPUTE WS-RFP-P1 = WS-Price-Drums
                    END-EVALUATE
              WHEN 'PERC'
                 EVALUATE TRUE
                    WHEN RFP-Used-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Perc -
                          (WS-Price-Perc * WS-Used-Adjustment)
                    WHEN RFP-Premium-Flag
                       COMPUTE WS-RFP-P1 ROUNDED = WS-Price-Perc +
                          (WS-Price-Perc * WS-Prem-Adjustment)
                    WHEN RFP-New-Flag
                       COMPUTE WS-RFP-P1 = WS-Price-Perc
                    END-EVALUATE
              WHEN OTHER
                 DISPLAY "** ERROR **: 2120-Calculate-Inst-Price"
                 DISPLAY "RFP-Musician-Instrument-Type is INVALID: "
                    RFP-Musician-Instrument-Type
           END-EVALUATE.

       2130-Calculate-Shipping.
           EVALUATE TRUE
              WHEN RFP-In-Country
                 COMPUTE WS-RFP-P2 ROUNDED =
                    (WS-RFP-P1 * WS-Ship-Rate-In)
              WHEN RFP-Out-of-Country
                 COMPUTE WS-RFP-P2 ROUNDED =
                    (WS-RFP-P1 * WS-Ship-Rate-Out)
              WHEN OTHER
                 DISPLAY "** ERROR **: 2130-Calculate-Shipping"
                 DISPLAY "RFP-Ship-To is INVALID: " RFP-Ship-To
           END-EVALUATE.

       2140-Calculate-Tax.
           COMPUTE WS-RFP-P3 ROUNDED =
              (WS-RFP-P1 * WS-STD-Tax-Rate )
           END-COMPUTE.

       2180-Validate-RFP-Record.
           PERFORM 2181-Validate-Account-Number.
           PERFORM 2182-Validate-Genre.
           PERFORM 2183-Validate-Names.
           PERFORM 2184-Validate-Instrument-Type.
           PERFORM 2185-Validate-Quality.
           PERFORM 2186-Validate-Budget.
           PERFORM 2187-Validate-Ship-To.

       2181-Validate-Account-Number.
           IF RFP-Artist-Acct-No IS NUMERIC
              NEXT SENTENCE
           ELSE
              SET WS-RFP-Invalid TO TRUE
              ADD +1 TO WS-RFP-Err-Account-Number
              DISPLAY "Bad Acct. Num.: " RFP-Artist-Acct-No
           END-IF.

       2182-Validate-Genre.
           IF RFP-Artist-Musical-Genre =
              "ROCK" OR "JAZZ" OR "FUSION" OR
              "FOLK" OR "CLASS" OR "CNTRY"
              NEXT SENTENCE
           ELSE
              SET WS-RFP-Invalid TO TRUE
              ADD +1 TO WS-RFP-Err-Genre
              DISPLAY "Bad Genre: " RFP-Artist-Musical-Genre
           END-IF.

       2183-Validate-Names.
           IF RFP-Musician-Lname > SPACES AND
              RFP-Musician-Fname > SPACES
              NEXT SENTENCE
           ELSE
              SET WS-RFP-Invalid TO TRUE
              ADD +1 TO WS-RFP-Err-Names
              DISPLAY "Bad Names: " RFP-Musician
           END-IF.

       2184-Validate-Instrument-Type.
           IF RFP-Musician-Instrument-Type =
              "DRUMS" OR "GUITAR" OR "BASS" OR
              "VOCALS" OR "KEYS" OR "PERC"
              NEXT SENTENCE
           ELSE
              SET WS-RFP-Invalid TO TRUE
              ADD +1 TO WS-RFP-Err-Instrument-Type
              DISPLAY "Bad Instrument: " RFP-Musician-Instrument-Type
           END-IF.

       2185-Validate-Quality.
           IF RFP-Instrument-Quality =
              "U" OR "N" OR "P"
              NEXT SENTENCE
           ELSE
              SET WS-RFP-Invalid TO TRUE
              ADD +1 TO WS-RFP-Err-Quality
              DISPLAY "Bad Quality: " RFP-Instrument-Quality
           END-IF.

       2186-Validate-Budget.
           IF RFP-Max-Musician-Budget-Amount >= 1000.00 AND
              RFP-Max-Musician-Budget-Amount <= 9999.99
              NEXT SENTENCE
           ELSE
              SET WS-RFP-Invalid TO TRUE
              ADD +1 TO WS-RFP-Err-Budget
              DISPLAY "Bad Budget Amount: "
                 RFP-Max-Musician-Budget-Amount
           END-IF.

       2187-Validate-Ship-To.
           IF RFP-Ship-To = "IN" OR "OUT"
              NEXT SENTENCE
           ELSE
              SET WS-RFP-Invalid TO TRUE
              ADD +1 TO WS-RFP-Err-Ship-To
              DISPLAY "Bad Ship To: " RFP-Ship-To
           END-IF.

       3000-End-Job.
           MOVE WS-RFPIN-Record-Cnt    TO R1-Total-Recs-Read.
           MOVE WS-Proposal-Record-Cnt TO R1-Total-Valid-Records.
           MOVE WS-BadRFP-Record-Cnt   TO R1-Total-Invalid-Records.
           MOVE WS-RFP-Total-Prop      TO R1-Total-Value-Proposals.
           PERFORM 6130-Write-R1-Footer.

           CLOSE RFPIN
                 PROPOSAL
                 BADRFP
                 PROPRPT.

       5100-Read-RFPIN.
           READ RFPIN
              AT END SET WS-RFPIN-EOF TO TRUE
           END-READ.
           IF WS-RFPIN-Okay
               ADD 1 TO WS-RFPIN-Record-Cnt
           ELSE
              IF WS-RFPIN-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 5100-Read-RFPIN"
                 DISPLAY "Read PFPIN Failed."
                 DISPLAY "File Status: " WS-RFPIN-Status
                 GOBACK
              END-IF
           END-IF.

       6000-Write-Proposal.
           WRITE PRP-Record.
           ADD +1 TO WS-Proposal-Record-Cnt.
           IF WS-Proposal-Okay
              NEXT SENTENCE
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE PROPOSAL Failed."
              DISPLAY "File Status: " WS-Proposal-Status
              GOBACK
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
           ADD +1 TO R1-Lines-Written.

       6130-Write-R1-Footer.
           IF R1-Line-Count + 6 > R1-Max-Lines
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
           MOVE R1-Footer4 TO Print-Line.
           WRITE Print-Line
              AFTER ADVANCING 1 LINES.
           PERFORM 6140-Display-EOJ-Messages.

       6140-Display-EOJ-Messages.
           DISPLAY EOJ-End-Message.

       6200-Write-BadRFP.
           WRITE BRFP-Record.
           ADD +1 TO WS-BadRFP-Record-Cnt
           IF WS-BadRFP-Okay
              NEXT SENTENCE
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE Bad Proposal Failed."
              DISPLAY "File Status: " WS-BadRFP-Status
              GOBACK
           END-IF.
