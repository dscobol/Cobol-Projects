       IDENTIFICATION DIVISION.
       PROGRAM-ID. BDS1002.
      * Control Break program to process the Census file and produce
      * a report that shows, for each county, the most popular surname
      * and the number of times it occurs.
      * The Records in the sequential Census file are ordered on
      * ascending Surname within ascending CountyName.
      * The report must be printed in ascending CountyName order

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CensusFile
           ASSIGN TO "../../../common/data/c10-2testdata.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
      *     ASSIGN TO DA-S-SALEFILE
      *     ORGANIZATION IS SEQUENTIAL

           SELECT SurnameReport
           ASSIGN TO "../spool/bds1002.rpt"
           ORGANIZATION IS LINE SEQUENTIAL.
      *     ASSIGN TO DA-S-SALEFILE
      *     ORGANIZATION IS SEQUENTIAL


       DATA DIVISION.
       FILE SECTION.
       FD  CensusFile.
       01  CensusRec.
           88 EndOfCensusFile VALUE HIGH-VALUES.
           02 CensusNum            PIC 9(8).
           02 Surname              PIC X(20).
           02 CountyName           PIC X(9).

       FD SurnameReport.
       01 PrintLine                PIC X(45).


       WORKING-STORAGE SECTION.
       01  ReportHeading.
           02 FILLER               PIC X(13) VALUE SPACES.
           02 FILLER               PIC X(22)
              VALUE "Popular Surname Report".

       01  SubjectHeading.
           02 FILLER               PIC X(42)
              VALUE "CountyName  Surname                  Count".

       01  CountySurnameLine.
           02 PrnCountyName        PIC X(9).
           02 FILLER               PIC X(3) VALUE SPACES.
           02 PrnSurname           PIC X(20).
           02 PrnCount             PIC BBBZZZ,ZZ9.

       01  ReportFooter            PIC X(43)
           VALUE "************* end of report ***************".

       01  PrevCountyName          PIC X(9).
       01  PrevSurname             PIC X(20).
       01  MostPopularSurname      PIC X(20).
       01  MostPopularCount        PIC 9(6).
       01  SurnameCount            PIC 9(6).

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-BOJ.
           PERFORM 2000-Process.
           PERFORM 3000-EOJ.
           GOBACK.

       1000-BOJ.
           OPEN INPUT CensusFile.
           OPEN OUTPUT SurnameReport.
           WRITE PrintLine FROM ReportHeading  AFTER ADVANCING 1 LINE.
           WRITE PrintLine FROM SubjectHeading AFTER ADVANCING 1 LINE.

           PERFORM 5000-Read-Census-File.



       2000-Process.
           PERFORM UNTIL EndOfCensusFile
             MOVE CountyName TO PrevCountyName, PrnCountyName
             MOVE ZEROS  TO MostPopularCount
             MOVE SPACES TO MostPopularSurname
             PERFORM 2100-FindMostPopularSurname
                     UNTIL CountyName NOT EQUAL TO PrevCountyName
                           OR EndOfCensusFile
             MOVE MostPopularCount   TO PrnCount
             MOVE MostPopularSurname TO PrnSurname
             WRITE PrintLine FROM CountySurnameLine
                AFTER ADVANCING 1 LINE
          END-PERFORM.


       2100-FindMostPopularSurname.
           MOVE Surname TO PrevSurname.
           PERFORM 5000-Read-Census-File
              VARYING SurnameCount FROM 0 BY 1
                   UNTIL Surname NOT EQUAL TO PrevSurname
                         OR CountyName NOT EQUAL TO PrevCountyName
                         OR EndOfCensusFile

           IF SurnameCount > MostPopularCount
              MOVE SurnameCount TO MostPopularCount
              MOVE PrevSurname  TO MostPopularSurname
           END-IF.


       3000-EOJ.
           WRITE PrintLine FROM ReportFooter AFTER ADVANCING 2 LINES.
           CLOSE CensusFile, SurnameReport.

       5000-Read-Census-File.
           READ CensusFile
              AT END SET EndOfCensusFile TO TRUE
           END-READ.
