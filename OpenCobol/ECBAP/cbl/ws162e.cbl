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
      * ---------- ------------  --------------------------------
      * 2020-08-01 dastagg       Created for ECBAP class
      *
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.

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
      *     ASSIGN TO HOSPERR
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
       01  FD-OutFile-Record  PIC X(085).

       FD  ERRFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PATIENT REPLACING ==:tag:== BY ==ErrFile==.

       FD  RPTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  R1-Print-Line  PIC X(132).

       FD  INSTYPE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  InsType-Record  PIC X(003).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==INFile==.
           COPY WSFST REPLACING ==:tag:== BY ==HRpt==.
           COPY WSFST REPLACING ==:tag:== BY ==OutFile==.
           COPY WSFST REPLACING ==:tag:== BY ==ErrFile==.
           COPY WSFST REPLACING ==:tag:== BY ==InsType==.
           12 WS-INFile-Valid-Status  PIC X(1).
              88 WS-INFile-Valid          VALUE "V".
              88 WS-INFile-InValid        VALUE "I".

       01  WS-File-Counters.
           12 FD-INFile-Record-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 FD-HRpt-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-OutFile-Record-Cnt    PIC S9(4) COMP VALUE ZERO.
           12 FD-ErrFile-Record-Cnt    PIC S9(4) COMP VALUE ZERO.
           12 FD-InsType-Record-Cnt    PIC S9(4) COMP VALUE ZERO.


       01  WS-Program-Storage.
           12 WS-Ins-Coverage-Perc      PIC 9(3) VALUE 10.
           12 WS-In-Patient-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 WS-Out-Patient-Cnt    PIC S9(4) COMP VALUE ZERO.
           12 WS-Daily-Amt          PIC S9(5)V99 COMP-3 VALUE ZERO.
           12 WS-Daily-Co-Pay       PIC S9(5)V99 COMP-3 VALUE ZERO.
           12 WS-Gross-Total-Cnt    PIC S9(8)V99 COMP-3 VALUE ZERO.

       01  WS-Type-Table-Storage.
           12 WS-Type-Max-Element-Counter   PIC S9(4) COMP VALUE +10.
           12 WS-Type-Occurs-Dep-Counter    PIC S9(4) COMP VALUE ZERO.
           12 WS-Type-Table OCCURS 0 TO 10 TIMES
              DEPENDING ON WS-Type-Occurs-Dep-Counter
              INDEXED BY WS-Type-IDX.
              15 WS-Type     PIC X(3).
                88  INS-HMO         VALUE 'HMO'.
                88  INS-PRIVATE     VALUE 'PRI'.
                88  INS-PPO         VALUE 'PPO'.
                88  INS-AFFORDABLE  VALUE 'AFF'.
                88  INS-MEDICARE    VALUE 'MED'.
              15 WS-Type-Counter  PIC S9(4) COMP.

       01  WS-OutFile-Record.
           12 WS-OutFile-Patient-Num           PIC 9(5).
           12 FILLER                  PIC X(2) VALUE SPACES.
           12 WS-OutFile-Patient-NAME          PIC X(20).
           12 WS-OutFile-Patient-Phone         PIC X(10).
           12 FILLER                  PIC X(2) VALUE SPACES.
           12 WS-OutFile-Patient-Type          PIC X(2).
           12 WS-OutFile-Patient-Bed-Id        PIC ZZZ9.
           12 FILLER                  PIC X(2) VALUE SPACES.
           12 WS-OutFile-Curr-Date             PIC X(8).
           12 FILLER                  PIC X(2) VALUE SPACES.
           12 WS-OutFile-Patient-Amt-PDay      PIC $$$,$$9.99.
           12 FILLER                  PIC X(2) VALUE SPACES.
           12 WS-OutFile-Ins-Cov-Percent       PIC 999.
           12 FILLER                  PIC X(2) VALUE SPACES.
           12 WS-OutFile-Patient-Ins-Type      PIC X(4).
           12 WS-OutFile-Patient-Stay-LTH      PIC 999.
           12 FILLER                  PIC X(4) VALUE SPACES.


       01  CURRENT-DATE-AND-TIME.
           COPY WSDT REPLACING ==:tag:== BY ==CDT==.

       01  R1-Counters.
           12 R1-Max-Lines         PIC S9(4) COMP VALUE 60.
           12 R1-Line-Count        PIC S9(4) COMP VALUE ZERO.
           12 R1-Line-Advance      PIC S9(4) COMP VALUE ZERO.
           12 R1-Page-Count        PIC S9(4) COMP VALUE ZERO.
           12 R1-Lines-Written     PIC S9(4) COMP VALUE ZERO.

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

       01  R1-Column-Header1.
           12 FILLER   PIC X(007) VALUE "Account".
           12 FILLER   PIC X(037) VALUE SPACES.
           12 FILLER   PIC X(003) VALUE "I/O".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(003) VALUE "Bed".
           12 FILLER   PIC X(004) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Admit".
           12 FILLER   PIC X(010) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Daily".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Diag".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(003) VALUE "Ins".

       01  R1-Column-Header2.
           12 FILLER   PIC X(006) VALUE "Number".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(012) VALUE "Patient Name".
           12 FILLER   PIC X(008) VALUE SPACES.
           12 FILLER   PIC X(012) VALUE "Phone Number".
           12 FILLER   PIC X(004) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Pat.".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(003) VALUE "No.".
           12 FILLER   PIC X(004) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Date".
           12 FILLER   PIC X(009) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Amount".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Code".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(003) VALUE "Typ".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Stay".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(007) VALUE "Network".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Co-Pay".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Deduct".


       01  R1-Column-Header3.
           12 FILLER    PIC X(005) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(021) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(014) VALUE ALL "=".
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 FILLER    PIC X(003) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(004) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "=".
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "=".
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 FILLER    PIC X(003) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(003) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(004) VALUE ALL "=".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(007) VALUE ALL "=".
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 FILLER    PIC X(004) VALUE ALL "=".
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 FILLER    PIC X(006) VALUE ALL "=".

       01  R1-Detail-Line.
           12 R1-Patient-Num              PIC X(005).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Patient-FName            PIC X(010).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Patient-LName            PIC X(010).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Patient-Phone            PIC X(015).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Patient-Type             PIC X(003).
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Patient-Bed-Id           PIC ZZZ9.
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Patient-Date-Admit       PIC X(010).
           12 FILLER                      PIC X(003) VALUE SPACES.
           12 R1-Patient-Amt-PDay         PIC $$,$$9.99.
           12 FILLER                      PIC X(003) VALUE SPACES.
           12 R1-Patient-Diag-Code        PIC ZZ9.
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Patient-Ins-Type         PIC X(003).
           12 FILLER                      PIC X(002) VALUE SPACES.
           12 R1-Patient-Stay-LTH         PIC ZZ9.
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Patient-In-Out-Net       PIC XXX.
           12 FILLER                      PIC X(006) VALUE SPACES.
           12 R1-Patient-CoPay            PIC $$$$9.
           12 FILLER                      PIC X(002) VALUE SPACES.
           12 R1-Patient-Deduct           PIC $$,$$$.

       01  R1-Footer1.
           12 FILLER             PIC X(031)
              VALUE " Number of Input Records Read: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Recs-Read PIC ZZ9.
       01  R1-Footer2.
           12 FILLER             PIC X(031)
              VALUE "      Number of Valid Records: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Valid-Records PIC ZZ9.
       01  R1-Footer3.
           12 FILLER             PIC X(031)
              VALUE "    Number of Invalid Records: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Invalid-Records PIC ZZ9.
       01  R1-Footer4.
           12 FILLER             PIC X(031)
              VALUE "          In Network Patients: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-In-Patient-Cnt PIC ZZ9.
       01  R1-Footer5.
           12 FILLER             PIC X(031)
              VALUE "      Out of Network Patients: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Out-Patient-Cnt PIC ZZ9.
       01  R1-Footer6.
           12 FILLER             PIC X(031)
              VALUE "   Grand Total: Daily Amounts: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Gross-Total PIC $$,$$$,$$9.99.

       01  R1-Footer-Type-Print-Line.
           12 R1-Footer-Print-Message.
              15 FILLER          PIC X(19) VALUE SPACES.
              15 R1-Footer-Type  PIC X(10).
              15 R1-Footer-End   PIC X(02).
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Footer-Print-Number  PIC ZZ9 VALUE ZERO.

       01 EOJ-Display-Messages.
           12 EOJ-End-Message PIC X(040) VALUE
              "*** Program HOSPEDIT - End of Run ***".
           12 EOJ-Print-Message PIC X(40) VALUE SPACES.
           12 EOJ-Print-Number  PIC ZZ,ZZ9 VALUE ZERO.
           12 EOJ-Print-Money   PIC $$,$$9.99 VALUE ZERO.

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
           PERFORM 5000-Read-INFILE.

       1010-Load-Type-Table.
           OPEN INPUT INSTYPE.
           SET WS-Type-IDX TO +1.
           PERFORM 1015-Load-Type Until WS-InsType-EOF.
           CLOSE INSTYPE.
           PERFORM 1019-Verify-Type-Table.

       1015-Load-Type.
           READ INSTYPE
              AT END SET WS-InsType-EOF TO TRUE
           END-READ.
           IF WS-InsType-Good
              ADD +1 TO
                 FD-InsType-Record-Cnt
                 WS-Type-Occurs-Dep-Counter
             MOVE InsType-Record TO WS-Type(WS-Type-IDX)
             MOVE ZERO TO WS-Type-Counter(WS-Type-IDX)
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

       1019-Verify-Type-Table.
      D    DISPLAY "WS-Type-Table: "
      D    PERFORM VARYING WS-Type-IDX FROM 1 BY 1
      D       UNTIL WS-Type-IDX > WS-Type-Occurs-Dep-Counter
      D       DISPLAY WS-Type(WS-Type-IDX)
      D    END-PERFORM.
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
           PERFORM 2180-Validate-INFile-Record.
           IF WS-INFile-Valid
             PERFORM 2110-Move-Static-Fields
             PERFORM 2111-Move-Dynamic-Fields
             PERFORM 2120-Update-R1-Counters
             PERFORM 2130-Calculate-Totals

             MOVE 1 TO R1-Line-Advance
             PERFORM 6100-Write-R1
             MOVE WS-OutFile-Record TO FD-OutFile-Record
             PERFORM 6000-Write-OutFile
           ELSE
              DISPLAY "*** Warning *** Invalid Patient Record."
              DISPLAY "* This record was not processed. *"
              DISPLAY FD-ErrFile-Patient-Record
              DISPLAY SPACES
              MOVE FD-INFile-Patient-Record TO
                 FD-ErrFile-Patient-Record
              PERFORM 6200-Write-ErrFile
           END-IF.

           PERFORM 5000-Read-INFILE.

       2110-Move-Static-Fields.
           MOVE FD-INFile-Patient-Num TO
              R1-Patient-Num, WS-OutFile-Patient-Num.

           MOVE FD-INFile-Patient-NAME TO
              WS-OutFile-Patient-NAME.
           MOVE FD-INFile-Patient-FName TO R1-Patient-FName.
           MOVE FD-INFile-Patient-LName TO R1-Patient-LName.

           MOVE FD-INFile-Patient-Phone TO WS-OutFile-Patient-Phone.
           STRING "(", FD-INFile-Patient-Phone(1:3), ") ",
                  FD-INFile-Patient-Phone(4:3), "-",
                  FD-INFile-Patient-Phone(7:4)
              DELIMITED BY SIZE
              INTO R1-Patient-Phone
           END-STRING.

           MOVE FD-INFile-Patient-Type TO WS-OutFile-Patient-Type.
           IF FD-INFile-Patient-Type = "I"
              MOVE "In" TO R1-Patient-Type
           ELSE
              MOVE "Out" TO R1-Patient-Type
           END-IF.

           MOVE FD-INFile-Patient-Bed-Id TO
              R1-Patient-Bed-Id, WS-OutFile-Patient-Bed-Id.

           MOVE FD-INFile-Patient-Date-Admit TO R1-Patient-Date-Admit.
           MOVE FD-INFile-Patient-Amt-PDay TO R1-Patient-Amt-PDay.
           MOVE FD-INFile-Patient-Diag-Code TO R1-Patient-Diag-Code.

           MOVE FD-INFile-Patient-Ins-Type TO
              R1-Patient-Ins-Type, WS-OutFile-Patient-Ins-Type.

           MOVE FD-INFile-Patient-Stay-LTH TO R1-Patient-Stay-LTH.

           IF FD-INFile-Patient-In-Out-Net = "N"
              MOVE "Net" TO R1-Patient-In-Out-Net
           ELSE
              MOVE "Out" TO R1-Patient-In-Out-Net
           END-IF.
           MOVE FD-INFile-Patient-CoPay TO R1-Patient-CoPay.
           MOVE FD-INFile-Patient-Deduct TO R1-Patient-Deduct.

       2111-Move-Dynamic-Fields.

           MOVE CDT-Full-Date TO WS-OutFile-Curr-Date.
           ADD +1 TO FD-INFile-Patient-Stay-LTH GIVING
              WS-OutFile-Patient-Stay-LTH.

       2120-Update-R1-Counters.
           ADD +1 to WS-Type-Counter(WS-Type-IDX).

           IF FD-INFile-Patient-In-Net
              ADD +1 TO WS-In-Patient-Cnt
           ELSE
              ADD +1 TO WS-Out-Patient-Cnt
           END-IF.

       2130-Calculate-Totals.
           COMPUTE WS-Daily-Amt =
              (FD-INFile-Patient-Amt-PDay *
                 (100 - WS-Ins-Coverage-Perc) / 100)
           END-COMPUTE.

           COMPUTE WS-Daily-Co-Pay =
              FD-INFile-Patient-Amt-PDay - WS-Daily-Amt
           END-COMPUTE.

           MOVE WS-Daily-Amt TO R1-Patient-Amt-PDay.
           MOVE WS-Daily-Co-Pay TO R1-Patient-CoPay.

           COMPUTE WS-Gross-Total-Cnt =
              WS-Gross-Total-Cnt + WS-Daily-Amt
           END-COMPUTE.

           COMPUTE WS-OutFile-Patient-Amt-PDay =
              FD-INFile-Patient-Tot-Amt +
                 FD-INFile-Patient-Amt-PDay
           END-COMPUTE.

       2180-Validate-INFile-Record.
           PERFORM 2181-Validate-INS-Type.
           PERFORM 2182-Validate-Patient-Type.
           PERFORM 2183-Validate-Network.

       2181-Validate-INS-Type.
           SET WS-Type-IDX TO 1.
           SEARCH WS-Type-Table VARYING WS-Type-IDX
              AT END SET WS-INFile-InValid TO TRUE
              DISPLAY "Insurance Type not found. "
                 FD-INFile-Patient-Ins-Type
              WHEN WS-Type(WS-Type-IDX) =
                 FD-INFile-Patient-Ins-Type
                 NEXT SENTENCE
           END-SEARCH.

       2182-Validate-Patient-Type.
           IF FD-INFile-Patient-V-Type
              NEXT SENTENCE
           ELSE
              SET WS-INFile-InValid TO TRUE
              DISPLAY "Patient Type was not valid: "
                 FD-INFile-Patient-Type
           END-IF.

       2183-Validate-Network.
           IF FD-INFile-Patient-In-Net OR
              FD-INFile-Patient-Out-Net
              NEXT SENTENCE
           ELSE
              SET WS-INFile-InValid TO TRUE
              DISPLAY "Patient Network was not valid: "
                 FD-INFile-Patient-In-Out-Net
           END-IF.

       3000-End-Job.
           PERFORM 6130-Write-R1-Footer.

           CLOSE INFILE
                 OUTFILE
                 ERRFILE
                 RPTFILE.

       5000-Read-INFILE.
           READ INFILE
              AT END SET WS-INFile-EOF TO TRUE
           END-READ.
           IF WS-INFile-Good
              ADD +1 TO FD-INFile-Record-Cnt
           ELSE
              IF WS-INFile-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 5000-Read-INFILE"
                 DISPLAY "Read INFILE Failed."
                 DISPLAY "File Status: " WS-INFile-Status
                 GOBACK
              END-IF
           END-IF.

       6000-Write-OutFile.
           WRITE FD-OutFile-Record.

           IF WS-OutFile-Good
              ADD +1 TO FD-OutFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE OutFile Failed."
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
      *    Remember to double-check this number.
           MOVE 5 TO R1-Line-Count.

       6120-Write-R1-Detail.
           MOVE R1-Detail-Line TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING R1-Line-Advance LINES.
           ADD R1-Line-Advance TO R1-Line-Count.
           ADD +1 TO R1-Lines-Written.

       6130-Write-R1-Footer.
           MOVE FD-INFile-Record-Cnt    TO R1-Total-Recs-Read.
           MOVE FD-OutFile-Record-Cnt   TO R1-Total-Valid-Records.
           MOVE FD-ErrFile-Record-Cnt   TO R1-Total-Invalid-Records.
           MOVE WS-In-Patient-Cnt       TO R1-In-Patient-Cnt.
           MOVE WS-Out-Patient-Cnt      TO R1-Out-Patient-Cnt.
           MOVE WS-Gross-Total-Cnt      TO R1-Total-Gross-Total.
      *    Remember to double-check this number.
      *    With table load, might still be off.
           IF R1-Line-Count + 15 > R1-Max-Lines
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

           MOVE "Network In/Out Patients: " TO
              R1-Footer-Print-Message
           MOVE R1-Footer-Print-Message TO R1-Print-Line
           WRITE R1-Print-Line
              AFTER ADVANCING 1 LINES.

           MOVE R1-Footer4 TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING 1 LINES.

           MOVE R1-Footer5 TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING 1 LINES.

           MOVE "Number of Insurance Types: " TO
              R1-Footer-Print-Message
           MOVE R1-Footer-Print-Message TO R1-Print-Line
           WRITE R1-Print-Line
              AFTER ADVANCING 1 LINES.

           MOVE SPACES TO R1-Footer-Type-Print-Line.
           PERFORM 6135-Display-Ins-Type-Messages.

           MOVE R1-Footer6 TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING 2 LINES.

           PERFORM 6140-Display-EOJ-Messages.

       6135-Display-Ins-Type-Messages.
           PERFORM VARYING WS-Type-IDX FROM +1 BY +1
              UNTIL WS-Type-IDX > WS-Type-Occurs-Dep-Counter
              EVALUATE TRUE
                 WHEN INS-HMO(WS-Type-IDX)
                    MOVE "       HMO" TO R1-Footer-Type
                 WHEN INS-PRIVATE(WS-Type-IDX)
                    MOVE "   Private" TO R1-Footer-Type
                 WHEN INS-PPO(WS-Type-IDX)
                    MOVE "       PPO" TO R1-Footer-Type
                 WHEN INS-AFFORDABLE(WS-Type-IDX)
                    MOVE "Affordable" TO R1-Footer-Type
                 WHEN INS-MEDICARE(WS-Type-IDX)
                    MOVE "  Medicare" TO R1-Footer-Type
              END-EVALUATE
              MOVE ": " TO R1-Footer-End
              MOVE WS-Type-Counter(WS-Type-IDX) TO
                 R1-Footer-Print-Number
              MOVE R1-Footer-Type-Print-Line TO R1-Print-Line
              WRITE R1-Print-Line
                 AFTER ADVANCING 1 LINES
           END-PERFORM.

       6140-Display-EOJ-Messages.
           DISPLAY EOJ-End-Message.

       6200-Write-ErrFile.
           WRITE FD-ErrFile-Patient-Record.

           IF WS-ErrFile-Good
              ADD +1 TO FD-ErrFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE ErrFile Failed."
              DISPLAY "File Status: " WS-ErrFile-Status
              GOBACK
           END-IF.
