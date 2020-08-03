       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRTRPT1.
      * REMARKS:
      * This is an extension of Michael Coughlan's
      * report program from the Apress book
      * "Beginning COBOL for Programers"
      * Chapter 8.
      * I have added a basic structure and error checking.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MembershipReport
           ASSIGN TO  "../spool/roster.rpt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-Report-Status.

           SELECT MemberFile  
           ASSIGN TO "../data/members.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-Member-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  MembershipReport.
       01  PrintLine        PIC X(44).

       FD  MemberFile.
       01  MemberRec.
           02 MemberId      PIC X(5).
           02 MemberName    PIC X(20).
           02 MemberType    PIC 9.
           02 MemberGender  PIC X.


       WORKING-STORAGE SECTION.

        01 WS-File-Status.
       COPY wsfst REPLACING ==:tag:== BY ==Member==. 
       COPY wsfst REPLACING ==:tag:== BY ==Report==.

       01  PageHeading.
           02 FILLER        PIC X(44)
           VALUE "Rolling Greens Golf Club - Membership Report".

       01  PageFooting.
           02 FILLER        PIC X(15) VALUE SPACES.
           02 FILLER        PIC X(7)  VALUE "Page : ".
           02 PrnPageNum    PIC Z9.

       01  ColumnHeadings   PIC X(41)
           VALUE "MemberID  Member Name         Type Gender".

       01  MemberDetailLine.
           02 FILLER        PIC X  VALUE SPACES.
           02 PrnMemberId   PIC X(5).
           02 FILLER        PIC X(4) VALUE SPACES.
           02 PrnMemberName PIC X(20).
           02 FILLER        PIC XX VALUE SPACES.
           02 PrnMemberType PIC X.
           02 FILLER        PIC X(4) VALUE SPACES.
           02 PrnMemberGender     PIC X.

       01  ReportFooting    PIC X(44)
           VALUE "**** End - Rolling Greens Member Report ****".

       01  Abnormal-Line.
           12 FILLER        PIC X(9) VALUE "**ERROR: ".
           12 Abn-Paragraph PIC X(4).
           12 FILLER        PIC X(21)
               VALUE "File Status Code is: ".
           12 Abn-Code      PIC X(02).

       01  LineCount        PIC 99 VALUE ZEROS.
           88 NewPageRequired  VALUE 40 THRU 99.

       01  PageCount        PIC 99 VALUE ZEROS.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           STOP RUN.

       1000-Begin-Job.
           PERFORM 1010-Open-Report.
           PERFORM 1020-Open-Member-File.
           PERFORM 6100-Print-Page-Headings.
           PERFORM 5000-Read-Member-File.

       1010-Open-Report.
           OPEN OUTPUT MembershipReport.
      * If the report FD doesn't open correctly,
      * this won't print anything. But all the files
      * will be closed and the program will just stop.
      * This could be changed to be more robust.
      * It also acts as a placeholder if an output file was
      * being opened instead of a report.
           EVALUATE TRUE
               WHEN WS-Report-Good
                   CONTINUE
               WHEN OTHER
                   MOVE "1010" TO Abn-Paragraph
                   MOVE WS-Report-Status TO Abn-Code
                   PERFORM 9000-Abnormal-End
           END-EVALUATE.

       1020-Open-Member-File.
           OPEN INPUT MemberFile.
           EVALUATE TRUE
               WHEN WS-Member-Good
                   CONTINUE
               WHEN OTHER
                   MOVE "1020" TO Abn-Paragraph
                   MOVE WS-Member-Status TO Abn-Code
                   PERFORM 9000-Abnormal-End
           END-EVALUATE.

       2000-Process.
           PERFORM 6200-Print-Report-Body UNTIL WS-Member-EOF.

       3000-End-Job.
           WRITE PrintLine FROM ReportFooting AFTER ADVANCING 5 LINES.
           CLOSE MemberFile
                 MembershipReport.

       5000-Read-Member-File.
           READ MemberFile
               AT END SET WS-Member-EOF TO TRUE
           END-READ.
           EVALUATE TRUE
               WHEN WS-Member-Okay
                   CONTINUE
               WHEN OTHER
                   MOVE "5000" TO Abn-Paragraph
                   MOVE WS-Member-Status TO Abn-Code
                   PERFORM 9000-Abnormal-End
           END-EVALUATE.

       6100-Print-Page-Headings.
           WRITE PrintLine FROM PageHeading AFTER ADVANCING PAGE
           WRITE PrintLine FROM ColumnHeadings AFTER ADVANCING 2 LINES
           MOVE 3 TO LineCount
           ADD 1 TO PageCount.

       6200-Print-Report-Body.
           IF NewPageRequired
               MOVE PageCount TO PrnPageNum
               WRITE PrintLine FROM PageFooting AFTER ADVANCING 5 LINES
               PERFORM 6100-Print-Page-Headings
           END-IF.
           MOVE MemberId   TO PrnMemberId.
           MOVE MemberName TO PrnMemberName.
           MOVE MemberType TO PrnMemberType.
           MOVE MemberGender     TO PrnMemberGender.
           WRITE PrintLine FROM MemberDetailLine AFTER ADVANCING 1 LINE.
           ADD 1 TO LineCount.
           PERFORM 5000-Read-Member-File.

       9000-Abnormal-End.
           WRITE PrintLine FROM Abnormal-Line AFTER ADVANCING PAGE.
           CLOSE MemberFile
                 MembershipReport.
           STOP RUN.
