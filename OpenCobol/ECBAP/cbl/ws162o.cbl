       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOSPEDIT.
      ***********************************************************
      * Program name:    HOSPEDIT
      * Original author: dastagg
      *
      * Description: Program to print Hospital/Insurance Report.
      *
      * WARNINGS:
      * RETURN-CODE = 0009
      *           This program loads an external dataset into a WS
      *           table. If the table is not large enough, the pgm
      *           will end with RETURN-CODE = 0009.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  --------------------------------
      * 2020-08-01 dastagg       Created for ECBAP class
      *
      **********************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE
      *     ASSIGN TO HOSPIN
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/hospin.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-INFile-Status.

           SELECT RPTFILE
      *     ASSIGN TO RPTFILE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/hrptfile.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-HRpt-Status.

           SELECT OUTFILE
      *     ASSIGN TO HOSPOUT
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/houtfile.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-OutFile-Status.

           SELECT ERRFILE
      *     ASSIGN TO ERRFILE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/herrfile.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-ErrFile-Status.

           SELECT INSTYPE
      *     ASSIGN TO INSTYPE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/hinstype.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-InsType-Status.


       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PATIENT REPLACING ==:tag:== BY ==INFile==.

       FD  OUTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PATIENT REPLACING ==:tag:== BY ==OutFile==.


       FD  ERRFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PATIENT REPLACING ==:tag:== BY ==ErrFile==.

       FD  RPTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  R1-Print-Line        PIC X(132).

       FD  INSTYPE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  InsType-Record    PIC X(003).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==INFile==.
           COPY WSFST REPLACING ==:tag:== BY ==HRpt==.
           COPY WSFST REPLACING ==:tag:== BY ==OutFile==.
           COPY WSFST REPLACING ==:tag:== BY ==ErrFile==.
           COPY WSFST REPLACING ==:tag:== BY ==InsType==.

       01  WS-Counters.
           12 FD-INFile-Record-Cnt     PIC 9(4) COMP VALUE ZEROES.
           12 FD-HRpt-Record-Cnt       PIC 9(4) COMP VALUE ZEROES.
           12 FD-OutFile-Record-Cnt    PIC 9(4) COMP VALUE ZEROES.
           12 FD-ErrFile-Record-Cnt    PIC 9(4) COMP VALUE ZEROES.
           12 FD-InsType-Record-Cnt    PIC 9(4) COMP VALUE ZEROES.
           12 WS-INFile-Record-Status  PIC X(1).
              88 WS-INFile-Valid          VALUE "V".
              88 WS-INFile-InValid        VALUE "I".

       01  WS-Type-Table-Storage.
           12 WS-Type-Max-Element-Counter  PIC S9(4) COMP VALUE +10.
           12 WS-Type-Occurs-Dep-Counter   PIC S9(4) COMP VALUE ZERO.
           12 WS-Type-Table OCCURS 0 TO 10 TIMES
              DEPENDING ON WS-Type-Occurs-Dep-Counter
              INDEXED BY WS-Type-IDX.
              15 WS-Type     PIC X(3).
                88  HMO         VALUE 'HMO'.
                88  I-PRIVATE   VALUE 'PRI'.
                88  PPO         VALUE 'PPO'.
                88  AFFORDABLE  VALUE 'AFF'.
                88  MEDICARE    VALUE 'MED'.

       01  CURRENT-DATE-AND-TIME.
           COPY WSDT REPLACING ==:tag:== BY ==CDT==.

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
           12 EOJ-End-Message PIC X(040) VALUE
              "*** Program HOSPEDIT - End of Run ***".
           12 EOJ-Print-Message PIC X(40) VALUE SPACES.
           12 EOJ-Print-Number  PIC ZZ,ZZ9 VALUE ZEROES.
           12 EOJ-Print-Money   PIC $$,$$9.99 VALUE ZEROES.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           PERFORM 1010-Load-Type-Table.

           OPEN INPUT  INFILE.
           OPEN OUTPUT OUTFILE
                       ERRFILE
                       RPTFILE.
           PERFORM 6101-Setup-R1.
           PERFORM 6110-Write-R1-Page-Header.
           PERFORM 5100-Read-INFILE.

       1010-Load-Type-Table.
           OPEN INPUT INSTYPE.
           SET WS-Type-IDX TO +1.
           PERFORM 1015-Load-Type Until WS-InsType-EOF.
           CLOSE INSTYPE.
           PERFORM 1099-Verify-Type-Table.

       1015-Load-Type.
           READ INSTYPE
              AT END SET WS-InsType-EOF TO TRUE
           END-READ.
           IF WS-InsType-Good
              ADD +1 TO
                 FD-InsType-Record-Cnt
                 WS-Type-Occurs-Dep-Counter
             MOVE InsType-Record TO WS-Type(WS-Type-IDX)
             SET WS-Type-IDX UP BY +1 
           ELSE
              IF WS-InsType-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 1015-Load-Type"
                 DISPLAY "Read INSTYPE Failed."
                 DISPLAY "File Status: " WS-InsType-Status
                 MOVE +8 TO RETURN-CODE
                 GOBACK
              END-IF
           END-IF.

       1099-Verify-Type-Table.
      D     DISPLAY "WS-Type-Table: "
      D     PERFORM VARYING WS-Type-IDX FROM 1 BY 1 
      D        UNTIL WS-Type-IDX > WS-Type-Occurs-Dep-Counter
      D        DISPLAY WS-Type(WS-Type-IDX)
      D     END-PERFORM.
           IF WS-Type-Occurs-Dep-Counter >
              WS-Type-Max-Element-Counter
                 DISPLAY "** ERROR **: 1099-Verify-Type-Table"
                 DISPLAY "WS table size is too small for file."
                 DISPLAY "Increase WS-Type-Table-Storage variables."
                 MOVE +9 TO RETURN-CODE
                 GOBACK
           END-IF.

       2000-Process.
           PERFORM 2100-Process-INFile-Records UNTIL WS-INFile-EOF.

       2100-Process-INFile-Records.
           SET WS-INFile-Valid TO TRUE.
           PERFORM 2180-Validate-RFP-Record.
           IF WS-INFile-Valid
             PERFORM 2110-Move-Fixed-Fields
             PERFORM 2120-Calculate-Inst-Price
             PERFORM 2130-Calculate-Shipping
             PERFORM 2140-Calculate-Tax


             MOVE 1 TO R1-Line-Advance
             PERFORM 6100-Write-R1
             PERFORM 6000-Write-OutFile-Patient
           ELSE
              DISPLAY "*** Warning *** Invalid Patient Record."
              DISPLAY "* This record was not processed. *"
              DISPLAY FD-ErrFile-Patient-Record
              DISPLAY SPACES
              MOVE FD-INFile-Patient-Record TO 
                 FD-ErrFile-Patient-Record
              PERFORM 6200-Write-ErrFile
           END-IF.

           PERFORM 5100-Read-INFILE.

       2110-Move-Fixed-Fields.

       2120-Calculate-Inst-Price.

       2130-Calculate-Shipping.

       2140-Calculate-Tax.

       2180-Validate-RFP-Record.
           PERFORM 2181-Validate-Account-Number.
           PERFORM 2182-Validate-Genre.
           PERFORM 2183-Validate-Names.
           PERFORM 2184-Validate-Instrument-Type.
           PERFORM 2185-Validate-Quality.
           PERFORM 2186-Validate-Budget.
           PERFORM 2187-Validate-Ship-To.

       2181-Validate-Account-Number.

       2182-Validate-Genre.

       2183-Validate-Names.

       2184-Validate-Instrument-Type.

       2185-Validate-Quality.

       2186-Validate-Budget.

       2187-Validate-Ship-To.

       3000-End-Job.
           MOVE FD-INFile-Record-Cnt    TO R1-Total-Recs-Read.
           MOVE FD-OutFile-Record-Cnt   TO R1-Total-Valid-Records.
           MOVE FD-ErrFile-Record-Cnt   TO R1-Total-Invalid-Records.
           PERFORM 6130-Write-R1-Footer.

           CLOSE INFILE
                 OUTFILE
                 ERRFILE
                 RPTFILE.

       5100-Read-INFILE.
           READ INFILE
              AT END SET WS-INFile-EOF TO TRUE
           END-READ.
           IF WS-INFile-Good
               ADD +1 TO FD-INFile-Record-Cnt
           ELSE
              IF WS-INFile-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 5100-Read-INFILE"
                 DISPLAY "Read PFPIN Failed."
                 DISPLAY "File Status: " WS-INFile-Status
                 GOBACK
              END-IF
           END-IF.

       6000-Write-OutFile-Patient.
           MOVE FD-INFile-Patient-Record TO 
              FD-OutFile-Patient-Record.
           WRITE FD-OutFile-Patient-Record.
           ADD +1 TO FD-OutFile-Record-Cnt.
           IF WS-OutFile-Good
              NEXT SENTENCE
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE Patient OutFile Failed."
              DISPLAY "File Status: " WS-OutFile-Status
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
           WRITE R1-Print-Line FROM R1-Page-Header
              AFTER ADVANCING PAGE.
           WRITE R1-Print-Line FROM R1-Column-Header1
              AFTER ADVANCING 2.
           WRITE R1-Print-Line FROM R1-Column-Header2
              AFTER ADVANCING 1.
           WRITE R1-Print-Line FROM R1-Column-Header3
              AFTER ADVANCING 1.
           MOVE 5 TO R1-Line-Count.

       6120-Write-R1-Detail.
           MOVE R1-Detail-Line TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING R1-Line-Advance LINES.
           ADD R1-Line-Advance TO R1-Line-Count.
           ADD +1 TO R1-Lines-Written.

       6130-Write-R1-Footer.
           IF R1-Line-Count + 6 > R1-Max-Lines
              PERFORM 6110-Write-R1-Page-Header
           END-IF.
           MOVE R1-Footer1 TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING 2 LINES.
           MOVE R1-Footer2 TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING 1 LINES.
           MOVE R1-Footer3 TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING 1 LINES.
           MOVE R1-Footer4 TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING 1 LINES.
           PERFORM 6140-Display-EOJ-Messages.

       6140-Display-EOJ-Messages.
           DISPLAY EOJ-End-Message.

       6200-Write-ErrFile.
           WRITE FD-ErrFile-Patient-Record.
           ADD +1 TO FD-ErrFile-Record-Cnt
           IF WS-ErrFile-Okay
              NEXT SENTENCE
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE ErrFile Failed."
              DISPLAY "File Status: " WS-ErrFile-Status
              GOBACK
           END-IF.
