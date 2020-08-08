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

           SELECT ERRFILE
      *     ASSIGN TO ERRFILE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO
              "../../../common/data/ECBAP/student-ERRFILE.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-ERRFILE-Status.

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
              15 FD-Student-Course-Table OCCURS 6 TIMES.
                 18  FD-Course-Nbr        PIC X(7).
                 18  FD-Course-Grade      PIC X(1).
           12  FILLER                     PIC X(12).

       FD  ERRFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-ERRFILE-Record PIC X(080).

       FD  RPTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  R1-Print-Line  PIC X(080).


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==STCOURS==.
           COPY WSFST REPLACING ==:tag:== BY ==ERRFILE==.
           COPY WSFST REPLACING ==:tag:== BY ==Report==.

       01  WS-File-Counters.
           12 FD-STCOURS-Record-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 FD-ERRFILE-Record-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 FD-Report-Record-Cnt      PIC S9(4) COMP VALUE ZERO.


       01  WS-Program-Storage.
           12 WS-Total-Courses-Cnt  PIC S9(4) COMP VALUE 0.
           12 WS-Total-Grades-Cnt   PIC S9(4) COMP VALUE 0.
           12 WS-Hold-QPA           PIC S9V99 COMP-3 VALUE 0.
           12 WS-Highest-QPA        PIC S9V99 COMP-3 VALUE LOW-VALUES.
           12 WS-High-Name          PIC X(20) VALUE SPACES.
           12 WS-Lowest-QPA         PIC S9V99 COMP-3 VALUE HIGH-VALUES.
           12 WS-Low-Name           PIC X(20) VALUE SPACES.

       01  WS-Stdt-Course-Table-Storage.
           12 WS-Student-Sub                  PIC 99 COMP VALUE 0.
           12 WS-Courses-Sub                  PIC 99 COMP VALUE 0.
           12 WS-Stdt-Course-Counters OCCURS 5 TIMES.
              15 WS-Stdt-Course-Cnt           PIC S9(4) COMP VALUE 0.
              15 WS-Stdt-Grade-Tot            PIC S9(4) COMP VALUE 0.
           12 WS-Stdt-Course-Table    OCCURS 5 TIMES.
              15 WS-Student-Name           PIC X(20).
              15 WS-Student-Courses.
                 18 WS-Student-Course-Table OCCURS 6 TIMES.
                    21  WS-Course-Nbr      PIC X(7).
                    21  WS-Course-Grade    PIC X(1).
                       88 WS-Valid-Grade
                          VALUES "A", "B", "C", "D", "F".

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
           12 FILLER                   PIC X(014) VALUE SPACE.
           12 FILLER                   PIC X(024)
                 VALUE "Student Courses Breakout".
           12 FILLER                   PIC X(016) VALUE SPACE.
           12 FILLER                   PIC X(005) VALUE "Page:".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 R1-HDR-Page-Count        PIC ZZ9.

       01  R1-Column-Header1.
           12 FILLER                      PIC X(080) VALUE SPACES.

       01  R1-Detail-Line1.
           12 FILLER                      PIC X(013)
                 VALUE 'Student Name:'.
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Student-Name             PIC X(020).
           12 FILLER                      PIC X(046) VALUE SPACES.

       01  R1-Detail-Line2.
           12 FILLER                      PIC X(006) VALUE SPACES.
           12 FILLER                      PIC X(007)
                 VALUE 'Course:'.
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Course-Name              PIC X(007).
           12 FILLER                      PIC X(005) VALUE SPACES.
           12 FILLER                      PIC X(006)
                 VALUE 'Grade:'.
           12 FILLER                      PIC X(001) VALUE SPACES.
           12 R1-Grade                    PIC X.
           12 R1-Grade-Valid              PIC X.

       01  R1-Detail-Line3.
           12 FILLER             PIC X(031)
              VALUE "                Student's QPA: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Stdt-Avg-QPA         PIC 9.99.

       01  R1-Footer1.
           12 FILLER             PIC X(031)
              VALUE " Number of Input Records Read: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Recs-Read PIC ZZ9.

       01  R1-Footer2.
           12 FILLER             PIC X(031)
              VALUE "     Student with highest QPA: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Stdt-High-QPA   PIC X(20).

       01  R1-Footer3.
           12 FILLER             PIC X(031)
              VALUE "      Student with lowest QPA: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Stdt-Low-QPA    PIC X(20).

       01  R1-Footer4.
           12 FILLER             PIC X(031)
              VALUE " Average QPA for all students: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Avg-QPA         PIC 9.99.

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
           OPEN OUTPUT ERRFILE
                       RPTFILE.

           PERFORM 6101-Setup-R1.
           PERFORM 6110-Write-R1-Page-Header.
           PERFORM 5000-Read-STCOURS.

       2000-Process.
           PERFORM 2100-Process-STCOURS-Records
              VARYING WS-Student-Sub FROM 1 BY 1
                 UNTIL WS-STCOURS-EOF OR WS-Student-Sub > 5.

       2100-Process-STCOURS-Records.
           INITIALIZE WS-Stdt-Course-Table(WS-Student-Sub).
           MOVE FD-Student-Record TO
              WS-Stdt-Course-Table(WS-Student-Sub).

           PERFORM 2110-Print-Stdt-Detail-Report.
           PERFORM 2110-Print-Stdt-Total-Report.


       2110-Print-Stdt-Detail-Report.
           MOVE WS-Student-Name(WS-Student-Sub) TO
              R1-Student-Name.
           MOVE R1-Detail-Line1 TO R1-Print-Line.
           PERFORM 6100-Write-R1.

           PERFORM VARYING WS-Courses-Sub FROM 1 BY 1
                UNTIL WS-Courses-Sub > 6
              IF WS-Course-Nbr
                 (WS-Student-Sub, WS-Courses-Sub) > SPACES
                 MOVE WS-Course-Nbr
                    (WS-Student-Sub, WS-Courses-Sub) TO
                    R1-Course-Name
                 MOVE WS-Course-Grade
                    (WS-Student-Sub, WS-Courses-Sub) TO
                    R1-Grade
                 IF WS-Valid-Grade
                    (WS-Student-Sub, WS-Courses-Sub)
                    MOVE SPACES TO R1-Grade-Valid
                    PERFORM 2120-Calculate-Grades
                 ELSE
                    MOVE "*" TO R1-Grade-Valid
                    DISPLAY "There was an incorrect grade assigned."
                    DISPLAY "Please check the report for the *."
                    DISPLAY "And the ERRFILE for the actual record."
                    MOVE "INVALID GRADE" TO FD-ERRFILE-Record
                    PERFORM 6200-Write-ERRFILE
                    MOVE FD-Student-Record TO FD-ERRFILE-Record
                    PERFORM 6200-Write-ERRFILE
                 END-IF
                 MOVE R1-Detail-Line2 TO R1-Print-Line
                 PERFORM 6100-Write-R1
               ELSE
                 MOVE 7 TO WS-Courses-Sub
               END-IF
           END-PERFORM.

           PERFORM 5000-Read-STCOURS.

       2120-Calculate-Grades.
           EVALUATE WS-Course-Grade(WS-Student-Sub, WS-Courses-Sub)
              WHEN 'A'
                 ADD +4 TO WS-Total-Grades-Cnt
                           WS-Stdt-Grade-Tot(WS-Student-Sub)
                 ADD +1 TO WS-Total-Courses-Cnt
                           WS-Stdt-Course-Cnt(WS-Student-Sub)
              WHEN 'B'
                 ADD +3 TO WS-Total-Grades-Cnt
                           WS-Stdt-Grade-Tot(WS-Student-Sub)
                 ADD +1 TO WS-Total-Courses-Cnt
                           WS-Stdt-Course-Cnt(WS-Student-Sub)
              WHEN 'C'
                 ADD +2 TO WS-Total-Grades-Cnt
                           WS-Stdt-Grade-Tot(WS-Student-Sub)
                 ADD +1 TO WS-Total-Courses-Cnt
                           WS-Stdt-Course-Cnt(WS-Student-Sub)
              WHEN 'D'
                 ADD +1 TO WS-Total-Grades-Cnt
                           WS-Stdt-Grade-Tot(WS-Student-Sub)
                 ADD +1 TO WS-Total-Courses-Cnt
                           WS-Stdt-Course-Cnt(WS-Student-Sub)
              WHEN 'F'
                 ADD +1 TO WS-Total-Courses-Cnt
                           WS-Stdt-Course-Cnt(WS-Student-Sub)
              WHEN OTHER
                 CONTINUE
           END-EVALUATE.

       2110-Print-Stdt-Total-Report.

           COMPUTE WS-Hold-QPA ROUNDED =
              WS-Stdt-Grade-Tot(WS-Student-Sub) /
                 WS-Stdt-Course-Cnt(WS-Student-Sub)
           END-COMPUTE.
           MOVE WS-Hold-QPA TO R1-Stdt-Avg-QPA.
           MOVE R1-Detail-Line3 TO R1-Print-Line.
           PERFORM 6100-Write-R1.
           MOVE SPACES TO R1-Print-Line.
           PERFORM 6100-Write-R1.

           IF WS-Hold-QPA > WS-Highest-QPA
              MOVE WS-Hold-QPA TO WS-Highest-QPA
              MOVE WS-Student-Name(WS-Student-Sub) TO WS-High-Name
           END-IF.

           IF WS-Hold-QPA < WS-Lowest-QPA
              MOVE WS-Hold-QPA TO WS-Lowest-QPA
              MOVE WS-Student-Name(WS-Student-Sub) TO WS-Low-Name
           END-IF.

       3000-End-Job.

           MOVE WS-High-Name TO R1-Stdt-High-QPA.
           MOVE WS-Low-Name TO R1-Stdt-Low-QPA.
           COMPUTE R1-Avg-QPA ROUNDED =
              WS-Total-Grades-Cnt / WS-Total-Courses-Cnt.
           PERFORM 6130-Write-R1-Footer.

           CLOSE STCOURS
                 ERRFILE
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
              AFTER ADVANCING 1.
      *    Remember to double-check this number.
           MOVE 2 TO R1-Line-Count.

       6120-Write-R1-Detail.
           WRITE R1-Print-Line
              AFTER ADVANCING R1-Line-Advance LINES.
           ADD R1-Line-Advance TO R1-Line-Count.
           ADD +1 TO R1-Lines-Written.

       6130-Write-R1-Footer.
           MOVE FD-STCOURS-Record-Cnt    TO R1-Total-Recs-Read.
      *    Remember to double-check this number.
           IF R1-Line-Count + 5 > R1-Max-Lines
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

       6200-Write-ERRFILE.
           WRITE FD-ERRFILE-Record.

           IF WS-ERRFILE-Good
              ADD +1 TO FD-ERRFILE-Record-Cnt
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE ERRFILE Failed."
              DISPLAY "File Status: " WS-ERRFILE-Status
              GOBACK
           END-IF.
