       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Membership-Report
           ASSIGN TO "../spool/Members.rpt"
           ORGANIZATION IS SEQUENTIAL.

           SELECT Member-File
           ASSIGN TO "../../../common/data/c08-members.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  Membership-Report.
       01  Print-Line        PIC X(44).

       FD  Member-File.
       01  MemberRec.
           12 MemberId      PIC X(5).
           12 MemberName    PIC X(20).
           12 MemberType    PIC 9.
           12 Gender        PIC X.

       WORKING-STORAGE SECTION.

       01 WS-FILE-STATUS.
       COPY wsfst REPLACING ==:tag:== BY ==Memb-File==.

       01  Page-Heading.
           12 FILLER        PIC X(44)
              VALUE "Rolling Greens Golf Club - Membership Report".

       01  Page-Footing.
           12 FILLER        PIC X(15) VALUE SPACES.
           12 FILLER        PIC X(7)  VALUE "Page : ".
           12 PrnPageNum    PIC Z9.
       01  Column-Headings   PIC X(41)
               VALUE "MemberID  Member Name         Type Gender".
       01  Member-Detail-Line.
           12 FILLER        PIC X  VALUE SPACES.
           12 PrnMemberId   PIC X(5).
           12 FILLER        PIC X(4) VALUE SPACES.
           12 PrnMemberName PIC X(20).
           12 FILLER        PIC XX VALUE SPACES.
           12 PrnMemberType PIC X.
           12 FILLER        PIC X(4) VALUE SPACES.
           12 PrnGender     PIC X.
       01  Report-Footing    PIC X(38)
              VALUE "**** End of Membership Report ****".
       01  Line-Count        PIC 99 VALUE ZEROS.
           88 NewPageRequired  VALUE 40 THRU 99.
       01  Page-Count        PIC 99 VALUE ZEROS.

       PROCEDURE DIVISION.

       0000-Mainline.
           PERFORM 1000-BOJ.
           PERFORM 2000-Process.
           PERFORM 3000-EOJ.
           GOBACK.

       1000-BOJ.
           OPEN INPUT Member-File.
           OPEN OUTPUT Membership-Report.
           PERFORM 6010-PrintPageHeadings.
           PERFORM 5000-Read-Member-File.

       2000-Process.
           PERFORM 6020-Print-Report-Body UNTIL WS-Memb-File-EOF.

       3000-EOJ.
           WRITE Print-Line FROM Report-Footing AFTER ADVANCING 5 LINES.
           CLOSE Member-File, Membership-Report.

       5000-Read-Member-File.
           READ Member-File
                AT END SET WS-Memb-File-EOF TO TRUE
           END-READ.

       6010-PrintPageHeadings.
           WRITE Print-Line FROM Page-Heading AFTER ADVANCING PAGE
           WRITE Print-Line FROM Column-Headings AFTER ADVANCING 2 LINES
           MOVE 3 TO Line-Count
           ADD 1 TO Page-Count.

       6020-Print-Report-Body.
           IF NewPageRequired
              MOVE Page-Count TO PrnPageNum
              WRITE Print-Line
                 FROM Page-Footing AFTER ADVANCING 5 LINES
              PERFORM 6010-PrintPageHeadings
           END-IF.
           MOVE MemberId   TO PrnMemberId
           MOVE MemberName TO PrnMemberName
           MOVE MemberType TO PrnMemberType
           MOVE Gender     TO PrnGender
           WRITE Print-Line
              FROM Member-Detail-Line AFTER ADVANCING 1 LINE
           ADD 1 TO Line-Count
           PERFORM 5000-Read-Member-File.
