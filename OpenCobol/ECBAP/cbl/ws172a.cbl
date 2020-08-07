       IDENTIFICATION DIVISION.
       PROGRAM-ID. WS172A.
      ***********************************************************
      * Program name:    WS172A
      * Original author: dastagg
      *
      * Description: Program to print Student Course Records.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-07 dastagg       Created for ECBAP class
      *
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER. IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STCOURS
      *     ASSIGN TO STCOURS
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/student-cours.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-STCOURS-Status.

           SELECT RPTFILE
      *     ASSIGN TO RPTFILE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../spool/student-grade-report.rpt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-Report-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  STCOURS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-Student-Record.
           12 FD-Student-Name             PIC X(20).
           12 FD-Student-Courses.
              15 FD-Student-Course-Table OCCURS 5 TIMES.
                 18  FD-Course-Nbr        PIC X(7).
                 18  FD-Course-Grade      PIC X(1).
           12  FILLER                     PIC X(20).

       FD  RPTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  R1-Print-Line  PIC X(080).


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==STCOURS==.
           COPY WSFST REPLACING ==:tag:== BY ==Report==.

       01  WS-File-Counters.
           12 FD-STCOURS-Record-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 FD-Report-Record-Cnt       PIC S9(4) COMP VALUE ZERO.


       01  WS-Program-Storage.
           12 WS-Student-Counter                  PIC 99 VALUE 0 COMP.

       01  WS-Stdt-Course-Table-Storage.
           12 WS-Student-Sub                  PIC 99 VALUE 0 COMP.
           12 WS-Courses-Sub                  PIC 99 VALUE 0 COMP.
           12 WS-Stdt-Course-Table OCCURS 5 TIMES.
              15 WS-Student-Name           PIC X(20).
              15 WS-Student-Courses.
                 18 WS-Student-Course-Table OCCURS 5 TIMES.
                    21  WS-Course-Nbr      PIC X(7).
                    21  WS-Course-Grade    PIC X(1).

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
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 FILLER                   PIC X(034)
                 VALUE "    Student Courses Breakout     ".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 FILLER                   PIC X(005) VALUE "Page:".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 R1-HDR-Page-Count        PIC ZZ9.

       01  R1-Column-Header1.
           12 FILLER   PIC X(007) VALUE "Account".
           12 FILLER   PIC X(037) VALUE SPACES.

       01  R1-Column-Header2.
           12 FILLER   PIC X(006) VALUE "Number".
           12 FILLER   PIC X(002) VALUE SPACES.

       01  R1-Column-Header3.
           12 FILLER   PIC X(006) VALUE "Number".
           12 FILLER   PIC X(002) VALUE SPACES.

       01  R1-Detail-Line.
      *     12 R1-Patient-Num              PIC X(005).
           12 FILLER                      PIC X(080) VALUE SPACES.

       01  R1-Footer1.
           12 FILLER             PIC X(031)
              VALUE " Number of Input Records Read: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Recs-Read PIC ZZ9.

       01  R1-Footer-Type-Print-Line.
           12 R1-Footer-Print-Message.
              15 FILLER          PIC X(19) VALUE SPACES.
              15 R1-Footer-Type  PIC X(10).
              15 R1-Footer-End   PIC X(02).
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Footer-Print-Number  PIC ZZ9 VALUE ZERO.

       01 EOJ-Display-Messages.
           12 EOJ-End-Message PIC X(040) VALUE
              "*** Program WS172A - End of Run ***".
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
           OPEN INPUT  STCOURS.
           OPEN OUTPUT RPTFILE.
                       
           PERFORM 6101-Setup-R1.
           PERFORM 6110-Write-R1-Page-Header.
           PERFORM 5000-Read-STCOURS.

       2000-Process.
           PERFORM 2100-Process-STCOURS-Records 
              VARYING WS-Student-Sub FROM 1 BY 1
                 UNTIL WS-STCOURS-EOF OR WS-Student-Sub  > 5.

       2100-Process-STCOURS-Records.
           MOVE FD-Student-Record TO 
              WS-Stdt-Course-Table(WS-Student-Sub).
           PERFORM VARYING WS-Courses-Sub FROM 1 BY 1
                UNTIL WS-Courses-Sub > 5
              EVALUATE WS-Course-Grade(WS-Student-Sub, WS-Courses-Sub)
                  WHEN 'A' MOVE '4' TO
                    WS-Course-Grade(WS-Student-Sub, WS-Courses-Sub)
                  WHEN 'B' MOVE '3' TO
                    WS-Course-Grade(WS-Student-Sub, WS-Courses-Sub)
                  WHEN 'C' MOVE '2' TO
                    WS-Course-Grade(WS-Student-Sub, WS-Courses-Sub)
                  WHEN 'D' MOVE '1' TO
                    WS-Course-Grade(WS-Student-Sub, WS-Courses-Sub)
                  WHEN 'F' MOVE '0' TO
                    WS-Course-Grade(WS-Student-Sub, WS-Courses-Sub)
              END-EVALUATE
           END-PERFORM.

           MOVE WS-Stdt-Course-Table(WS-Student-Sub) TO R1-Detail-Line.
           PERFORM 6100-Write-R1.
           PERFORM 5000-Read-STCOURS.


       3000-End-Job.
           PERFORM 6130-Write-R1-Footer.

           CLOSE STCOURS
                 RPTFILE.

       5000-Read-STCOURS.
           READ STCOURS
              AT END SET WS-STCOURS-EOF TO TRUE
           END-READ.
           IF WS-STCOURS-Good
              ADD +1 TO FD-STCOURS-Record-Cnt
           ELSE
              IF WS-STCOURS-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 5000-Read-STCOURS"
                 DISPLAY "Read STCOURS Failed."
                 DISPLAY "File Status: " WS-STCOURS-Status
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
           MOVE FD-STCOURS-Record-Cnt    TO R1-Total-Recs-Read.
      *    Remember to double-check this number.
           IF R1-Line-Count + 2 > R1-Max-Lines
              PERFORM 6110-Write-R1-Page-Header
           END-IF.
           MOVE R1-Footer1 TO R1-Print-Line.
           WRITE R1-Print-Line
              AFTER ADVANCING 2 LINES.


           PERFORM 6140-Display-EOJ-Messages.

       6140-Display-EOJ-Messages.
           DISPLAY EOJ-End-Message.

