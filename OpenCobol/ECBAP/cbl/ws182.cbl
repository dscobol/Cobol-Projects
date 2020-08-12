       IDENTIFICATION DIVISION.
       PROGRAM-ID. WS182.
      ***********************************************************
      * Program name:    WS182
      * Original author: dastagg
      *
      * Description: One Level Control Break reporting program.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-11 dastagg       Created for ECBAP class
      *
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER. IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCTREC
      *     ASSIGN TO ACCTREC
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/acctrec.sort.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-ACCTREC-Status.

           SELECT RPTFILE
      *     ASSIGN TO RPTFILE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../spool/pres-cb1-report.rpt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-Report-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCTREC
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-Account-Record.
           12 FD-Year-Term-Start              PIC 9(04).
           12 FD-Year-Term-End                PIC 9(04).
           12 FD-Salary                       PIC 9(7)V99.
           12 FD-Net-Worth                    PIC 9(7)V99.
           12 FD-Last-Name                    PIC X(20).
           12 FD-First-Name                   PIC X(15).
           12 FD-Address                      PIC X(25).
           12 FD-City                         PIC X(20).
           12 FD-State                        PIC X(20).
           12 FD-Text                         PIC X(50).

       FD  RPTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-Report-Record                   PIC X(132).


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==ACCTREC==.
           COPY WSFST REPLACING ==:tag:== BY ==Report==.

       01  WS-File-Counters.
           12 FD-ACCTREC-Record-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 FD-Report-Record-Cnt      PIC S9(4) COMP VALUE ZERO.


       01  WS-Program-Storage.
           12 WS-Total-Salary         PIC S9(9)V99.
           12 WS-Total-Net-Worth      PIC S9(9)V99.
           12 WS-High-Salary          PIC S9(9)V99 VALUE ZEROES.
           12 WS-High-Salary-Name     PIC X(17).
           12 WS-Low-Salary           PIC S9(9)V99 VALUE 999999999.99.
           12 WS-Low-Salary-Name      PIC X(17).

       01  WS-CB-Hold-Storage.
           12 WS-CB-State                PIC X(20).
           12 WS-CB-Total-Salary         PIC S9(9)V99.
           12 WS-CB-Total-Net-Worth      PIC S9(9)V99.

       01  CURRENT-DATE-AND-TIME.
           COPY WSDT REPLACING ==:tag:== BY ==CDT==.

       01  R1-Storage.
           12 R1-Max-Lines         PIC S9(4) COMP VALUE 60.
           12 R1-Line-Count        PIC S9(4) COMP VALUE ZERO.
           12 R1-Line-Advance      PIC S9(4) COMP VALUE ZERO.
           12 R1-Page-Count        PIC S9(4) COMP VALUE ZERO.
           12 R1-Lines-Written     PIC S9(4) COMP VALUE ZERO.
           12 R1-Print-Line        PIC X(132).

       01  R1-Page-Header.
           12 FILLER                   PIC X(007) VALUE "Report:".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 R1-Report-Name           PIC X(004).
           12 FILLER                   PIC X(033) VALUE SPACE.
           12 FILLER                   PIC X(039)
                 VALUE "Presidents Broken Out by State of Birth".
           12 FILLER                   PIC X(014) VALUE SPACE.
           12 R1-HDR-DATE.
              15 R1-HDR-YY             PIC 9(4).
              15 FILLER                PIC X(1) VALUE "/".
              15 R1-HDR-MM             PIC 9(2).
              15 FILLER                PIC X(1) VALUE "/".
              15 R1-HDR-DD             PIC 9(2).
           12 FILLER                   PIC X(012) VALUE SPACE.
           12 FILLER                   PIC X(004) VALUE "Page".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 R1-HDR-Page-Count        PIC ZZZ9.

       01  R1-Column-Header1.
           12 FILLER                   PIC X(005) VALUE "State".
           12 FILLER                   PIC X(013) VALUE SPACE.
           12 FILLER                   PIC X(009) VALUE "President".
           12 FILLER                   PIC X(024) VALUE SPACE.
           12 FILLER                   PIC X(007) VALUE "Elected".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 FILLER                   PIC X(004) VALUE "Thru".
           12 FILLER                   PIC X(010) VALUE SPACE.
           12 FILLER                   PIC X(006) VALUE "Salary".
           12 FILLER                   PIC X(006) VALUE SPACE.
           12 FILLER                   PIC X(009) VALUE "Net Worth".
           12 FILLER                   PIC X(003) VALUE SPACE.
           12 FILLER                   PIC X(012) VALUE "Salary-Accum".

       01  R1-Column-Header2.
           12 FILLER                   PIC X(017) VALUE ALL "=".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 FILLER                   PIC X(032) VALUE ALL "=".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 FILLER                   PIC X(007) VALUE ALL "=".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 FILLER                   PIC X(005) VALUE ALL "=".
           12 FILLER                   PIC X(003) VALUE SPACE.
           12 FILLER                   PIC X(012) VALUE ALL "=".
           12 FILLER                   PIC X(002) VALUE SPACE.
           12 FILLER                   PIC X(013) VALUE ALL "=".
           12 FILLER                   PIC X(002) VALUE SPACE.
           12 FILLER                   PIC X(013) VALUE ALL "=".

       01  R1-Detail-Line1.
           12 R1-State                 PIC X(017).
           12 FILLER                   PIC X(001) VALUE SPACES.
           12 R1-Name-Combined         PIC X(035).
           12 FILLER                   PIC X(001) VALUE SPACES.
           12 R1-Year-Term-Start       PIC X(004).
           12 FILLER                   PIC X(002) VALUE SPACES.
           12 R1-Year-Term-End         PIC X(004).
           12 FILLER                   PIC X(002) VALUE SPACES.
           12 R1-Salary                PIC $$,$$$,$$9.99.
           12 FILLER                   PIC X(002) VALUE SPACES.
           12 R1-Net-Worth             PIC $$,$$$,$$9.99.
           12 FILLER                   PIC X(002) VALUE SPACES.
           12 R1-Salary-Accum          PIC $,$$$,$$$,999.
           12 FILLER                   PIC X(001) VALUE SPACES.

       01  R1-CB-Line1.
           12 FILLER                   PIC X(012) VALUE "Sub Totals: ".
           12 FILLER                   PIC X(002) VALUE SPACE.
           12 R1-CB-State              PIC X(017).
           12 FILLER                   PIC X(005) VALUE SPACES.
           12 FILLER                   PIC X(019) 
               VALUE "Salary | Net Worth:".
           12 FILLER                   PIC X(010) VALUE SPACES.
           12 R1-CB-Total-Salary       PIC $$$,$$$,$$9.99.
           12 FILLER                   PIC X(001) VALUE SPACES.
           12 R1-CB-Total-Net-Worth    PIC $$$,$$$,$$9.99.

       01  R1-Footer1.
           12 FILLER                   PIC X(040)
              VALUE "      Total of All Presidents Salaries: ".
           12 FILLER                   PIC X VALUE SPACE.
           12 R1-FT-Total-Salary       PIC $$$,$$$,$$9.99.

       01  R1-Footer2.
           12 FILLER                   PIC X(040)
              VALUE "     President with the Highest Salary: ".
           12 FILLER                   PIC X VALUE SPACE.
           12 R1-FT-Highest-Name       PIC X(17).

       01  R1-Footer3.
           12 FILLER                   PIC X(040)
              VALUE "      President with the Lowest Salary: ".
           12 FILLER                   PIC X VALUE SPACE.
           12 R1-FT-Lowest-Name        PIC X(17).

       01  R1-Footer4.
           12 FILLER                   PIC X(040)
              VALUE "     Average Salary for all Presidents: ".
           12 FILLER                   PIC X VALUE SPACE.
           12 R1-FT-Avg-Salary         PIC $$$,$$$,$$9.99.

       01  R1-Footer-Type-Print-Line.
           12 R1-Footer-Print-Message.
              15 FILLER                PIC X(19) VALUE SPACES.
              15 R1-Footer-Type        PIC X(10).
              15 R1-Footer-End         PIC X(02).
           12 FILLER                   PIC X VALUE SPACE.
           12 R1-Footer-Print-Number   PIC ZZ9 VALUE ZERO.

       01 EOJ-Display-Messages.
           12 EOJ-End-Message          PIC X(040) VALUE
              "*** Program CBRPT1 - End of Run ***".
           12 EOJ-Print-Message        PIC X(40) VALUE SPACES.
           12 EOJ-Print-Number         PIC ZZ,ZZ9 VALUE ZERO.
           12 EOJ-Print-Money          PIC $$,$$9.99 VALUE ZERO.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           OPEN INPUT  ACCTREC.
           OPEN OUTPUT RPTFILE.
           PERFORM 5000-Read-ACCTREC.
           PERFORM 6101-Setup-R1.
           PERFORM 6110-Write-R1-Page-Header.
           PERFORM 2120-Setup-CB-Fields.
       
       
       2000-Process.

           PERFORM UNTIL WS-ACCTREC-EOF
              IF WS-ACCTREC-Good
                 IF FD-State NOT = WS-CB-State
                    PERFORM 2100-Main-Break
                    PERFORM 2900-Detail-Line
                 ELSE
                    PERFORM 2900-Detail-Line 
                 END-IF
                 PERFORM 5000-Read-ACCTREC
              END-IF
           END-PERFORM.

           PERFORM 2110-Print-Main-Footer.
                 
           
       2100-Main-Break.
           PERFORM 2110-Print-Main-Footer.
           PERFORM 2120-Setup-CB-Fields.
      *     PERFORM 2130-Print-Main-Header.

       2110-Print-Main-Footer.
           MOVE WS-CB-State TO R1-CB-State.
           MOVE WS-CB-Total-Salary TO R1-CB-Total-Salary.
           MOVE WS-CB-Total-Net-Worth TO R1-CB-Total-Net-Worth.
           MOVE +2 TO R1-Line-Advance.
           MOVE R1-CB-Line1 TO R1-Print-Line.
           PERFORM 6100-Write-R1.

       2120-Setup-CB-Fields.
           MOVE FD-State TO WS-CB-State.
           INITIALIZE WS-CB-Total-Salary
                      WS-CB-Total-Net-Worth.

       2900-Detail-Line. 
           
           PERFORM 2910-Print-Detail-Line.

       2910-Print-Detail-Line.
           MOVE FD-State TO R1-State.
           MOVE SPACES TO R1-Name-Combined.
           STRING FD-First-Name, " ",
                  FD-Last-Name
              DELIMITED BY "  " 
              INTO R1-Name-Combined.
           MOVE FD-Year-Term-Start TO R1-Year-Term-Start.
           MOVE FD-Year-Term-End TO R1-Year-Term-End.

           MOVE FD-Salary TO R1-Salary.
           ADD FD-Salary TO 
              WS-CB-Total-Salary, WS-Total-Salary
           END-ADD.

           MOVE FD-Net-Worth TO R1-Net-Worth.
           ADD FD-Net-Worth TO 
              WS-CB-Total-Net-Worth , WS-Total-Net-Worth
           END-ADD.

           COMPUTE R1-Salary-Accum ROUNDED = 
              FD-Salary * (FD-Year-Term-End - FD-Year-Term-Start)
           
           IF FD-Salary < WS-Low-Salary
              MOVE FD-Salary TO WS-Low-Salary
              MOVE R1-Name-Combined TO WS-Low-Salary-Name 
           END-IF.
           IF FD-Salary > WS-High-Salary
              MOVE FD-Salary TO WS-High-Salary
              MOVE R1-Name-Combined TO WS-High-Salary-Name 
           END-IF.

           MOVE +1 TO R1-Line-Advance.
           MOVE R1-Detail-Line1 TO R1-Print-Line.
           PERFORM 6100-Write-R1. 

       3000-End-Job.
           MOVE WS-Total-Salary TO R1-FT-Total-Salary.
           MOVE WS-High-Salary-Name TO R1-FT-Highest-Name.
           MOVE WS-Low-Salary-Name TO R1-FT-Lowest-Name.

           COMPUTE R1-FT-Avg-Salary =
              WS-Total-Salary / FD-ACCTREC-Record-Cnt
           END-COMPUTE.

           PERFORM 6130-Write-R1-Footer.

           CLOSE ACCTREC
                 RPTFILE.                 

       5000-Read-ACCTREC.
           READ ACCTREC
              AT END SET WS-ACCTREC-EOF TO TRUE
           END-READ.
           IF WS-ACCTREC-Good
              ADD +1 TO FD-ACCTREC-Record-Cnt
           ELSE
              IF WS-ACCTREC-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 5000-Read-ACCTREC"
                 DISPLAY "Read ACCTREC Failed."
                 DISPLAY "File Status: " WS-ACCTREC-Status
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
           MOVE 'A124'       TO R1-Report-Name.

       6110-Write-R1-Page-Header.
           ADD +1 TO R1-Page-Count.
           MOVE R1-Page-Count TO R1-HDR-Page-Count.
           MOVE R1-Page-Header TO FD-Report-Record.
           WRITE FD-Report-Record AFTER ADVANCING PAGE. 
           MOVE R1-Column-Header1 TO FD-Report-Record.
           WRITE FD-Report-Record AFTER ADVANCING 2 LINES. 
           MOVE R1-Column-Header2 TO FD-Report-Record.
           WRITE FD-Report-Record AFTER ADVANCING 1 LINES.         
      *     MOVE SPACES TO R1-Print-Line.
      *    Remember to double-check this number.
           MOVE 4 TO R1-Line-Count.

       6120-Write-R1-Detail.
           MOVE R1-Print-Line TO FD-Report-Record
           WRITE FD-Report-Record 
              AFTER ADVANCING R1-Line-Advance LINES.
           ADD R1-Line-Advance TO R1-Line-Count.
           ADD +1 TO R1-Lines-Written.

       6130-Write-R1-Footer.
      *    Remember to double-check this number.
           IF R1-Line-Count + 5 > R1-Max-Lines
              PERFORM 6110-Write-R1-Page-Header
           END-IF.

           MOVE R1-Footer1 TO FD-Report-Record.
           WRITE FD-Report-Record AFTER ADVANCING 2 LINES.
              
           MOVE R1-Footer2 TO FD-Report-Record.
           WRITE FD-Report-Record AFTER ADVANCING 1 LINES.
              
           MOVE R1-Footer3 TO FD-Report-Record.
           WRITE FD-Report-Record AFTER ADVANCING 1 LINES.
              
           MOVE R1-Footer4 TO FD-Report-Record.
           WRITE FD-Report-Record AFTER ADVANCING 1 LINES.

           PERFORM 6140-Display-EOJ-Messages.

       6140-Display-EOJ-Messages.
           DISPLAY EOJ-End-Message.
