       IDENTIFICATION DIVISION.
       PROGRAM-ID.   FAV11W2.
      * Optional Workshop 11.2 - Multiple Input Record Types
      **********************************************************
      * REMARKS
      * Fixing program student.cbl
      ***********************************************************
      *
      * ***************************************************
      * *** student.cbl
      * ***
      * ***     The program produces a report that shows
      * ***     the number of courses and credits that
      * ***     a student has taken.  The input file has
      * ***     two types of records, a student record showing
      * ***     information about a student, followed by
      * ***     a number of course records for that student.
      * ***     Each course record shows the information
      * ***     for a course taken by the student.
      * ***
      * ***************************************************
      * **************************************************
       INSTALLATION.  IBM.
       DATE-WRITTEN.  01-01-2009.
       DATE-COMPILED. 01-01-2009.
       SECURITY.   NONE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   IBM.
       OBJECT-COMPUTER.   IBM.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE   ASSIGN TO UT-S-STDNTFL
                  ORGANIZATION IS SEQUENTIAL.
           SELECT CREDITS-REPORT ASSIGN TO UT-S-REPORTFL
                  ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01  STUDENT-RECORD.
           05  SR-NAME                 PIC X(19).
           05  FILLER                  PIC X(5).
           05  SR-ADDRESS              PIC X(20).
           05  FILLER                  PIC XXXXX.
           05  SR-PHONE                PIC X(7).
           05  FILLER                  PIC XXX.
           05  SR-BIRTH-DATE           PIC X(6).
           05  FILLER                  PIC XXXX.
           05  SR-RECORD-TYPE          PIC X.
           05  FILLER                  PIC X(10).
       01  COURSE-RECORD.
           05  CR-NAME                 PIC X(19).
           05  FILLER                  PIC X(5).
           05  CR-COURSE-NUMBER        PIC X(5).
           05  FILLER                  PIC X(5).
           05  CR-CREDITS              PIC 9.
           05  FILLER                  PIC X(34).
           05  FILLER                  PIC X(11).
       FD  CREDITS-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01  REPORT-LINE-OUT             PIC X(60).
       WORKING-STORAGE SECTION.
       01  SWITCHES-IN-PROGRAM.
           05  SW-END-OF-DATA          PIC X VALUE 'N'.
               88  END-OF-DATA               VALUE 'Y'.
       01  ACCUMS-AND-COUNTERS.
           05  ACCUM-CREDITS           PIC 999 VALUE 0.
           05  CTR-COURSES             PIC 999 VALUE 0.
           05  CTR-STUDENTS            PIC 9(5) VALUE 0.
           05  CTR-LINES               PIC 99 VALUE 0.
       01  SAVE-AREAS.
           05  SAVE-NAME               PIC X(19).
       01  GRAND-TOTAL-LINE.
           05  FILLER                  PIC X(30)
                    VALUE ' TOTAL STUDENTS PROCESSED IS: '.
           05  GTL-STUDENT-COUNT       PIC ZZZZZ.
       01  DETAIL-LINE.
           05  FILLER                  PIC X(5) VALUE SPACE.
           05  DL-NAME                 PIC X(19).
           05  FILLER                  PIC X(8) VALUE SPACE.
           05  DL-COURSES              PIC ZZZ.
           05  FILLER                  PIC X(10) VALUE SPACE.
           05  DL-CREDITS              PIC ZZZZ.
       01  HEADING-1.
           05  FILLER                  PIC X(10) VALUE SPACE.
           05  FILLER                  PIC X(80) VALUE
               'S T U D E N T   C R E D I T S   R E P O R T'.
       01  HEADING-2.
           05  FILLER                  PIC X(5)  VALUE SPACE.
           05  FILLER                  PIC X(25) VALUE 'STUDENT NAME'.
           05  FILLER                  PIC X(15) VALUE 'COURSES'.
           05  FILLER                  PIC X(7)  VALUE 'CREDITS'.
       01  HEADING-3.
           05  FILLER                  PIC X(5)  VALUE SPACE.
           05  FILLER                  PIC X(20) VALUE ALL '-'.
           05  FILLER                  PIC X(05) VALUE SPACE.
           05  FILLER                  PIC X(07) VALUE ALL '-'.
           05  FILLER                  PIC X(08) VALUE SPACE.
           05  FILLER                  PIC X(7)  VALUE ALL '-'.
      *
       PROCEDURE DIVISION.
       000-TOP-LEVEL.
           PERFORM 100-INITIALIZATION
           PERFORM 200-PROCESS-RECORDS UNTIL END-OF-DATA
           PERFORM 300-WRAP-UP
           GOBACK
           .
       100-INITIALIZATION.
           OPEN INPUT  STUDENT-FILE
           OPEN OUTPUT CREDITS-REPORT
           PERFORM 211-PAGE-CHANGE-RTN
           PERFORM 230-READ-A-RECORD
           MOVE SR-NAME       TO SAVE-NAME
      *    ADD 1              TO CTR-STUDENTS *> Add ONLY for Rec type 1
           .
       200-PROCESS-RECORDS.
           IF SR-RECORD-TYPE IS EQUAL TO '1'
               DISPLAY STUDENT-RECORD
               IF CTR-STUDENTS > 1
                 PERFORM 210-PROCESS-1-RECORDS
               END-IF
               MOVE SR-NAME   TO SAVE-NAME
      *        ADD 1          TO CTR-COURSES
               MOVE ZEROS     TO CTR-COURSES ACCUM-CREDITS
               ADD 1          TO CTR-STUDENTS
           ELSE
               PERFORM 220-PROCESS-2-RECORDS
           END-IF
           PERFORM 230-READ-A-RECORD
           .
       210-PROCESS-1-RECORDS.
           IF CTR-LINES IS GREATER THAN 30
               PERFORM 211-PAGE-CHANGE-RTN
           END-IF
           PERFORM 212-BUILD-DETAIL-LINE
           WRITE REPORT-LINE-OUT FROM DETAIL-LINE
               AFTER ADVANCING 1.
           MOVE ZERO          TO CTR-LINES
           .
       211-PAGE-CHANGE-RTN.
           WRITE REPORT-LINE-OUT FROM HEADING-1
               AFTER ADVANCING PAGE
           WRITE REPORT-LINE-OUT FROM HEADING-2
               AFTER ADVANCING 2
           WRITE REPORT-LINE-OUT FROM HEADING-3
           MOVE ZERO TO CTR-LINES
           .
       212-BUILD-DETAIL-LINE.
           MOVE SAVE-NAME     TO DL-NAME
           MOVE CTR-COURSES   TO DL-COURSES
           MOVE ACCUM-CREDITS TO DL-CREDITS
           .
       220-PROCESS-2-RECORDS.
           ADD CR-CREDITS     TO ACCUM-CREDITS
           ADD 1              TO CTR-COURSES
           .
       230-READ-A-RECORD.
           READ STUDENT-FILE
              AT END MOVE 'Y' TO SW-END-OF-DATA
           END-READ
           .
       300-WRAP-UP.
           PERFORM 210-PROCESS-1-RECORDS    *> Prints last Student data
           MOVE CTR-STUDENTS TO GTL-STUDENT-COUNT
           WRITE REPORT-LINE-OUT FROM HEADING-3 *> Adding a separator
           WRITE REPORT-LINE-OUT FROM GRAND-TOTAL-LINE
               AFTER ADVANCING 2
           CLOSE CREDITS-REPORT  STUDENT-FILE
           .
