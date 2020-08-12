       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MyProg.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SPECIAL-NAMES. C01 IS TOP-OF-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO UT-S-FILENM1.
           SELECT CREDITS-REPORT ASSIGN TO UT-S-FILENM2.
       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE
           DATA RECORDS ARE STUDENT-RECORD, COURSE-RECORD
           RECORD CONTAINS 72 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
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
           05  FILLER                  PIC XX.
       01  COURSE-RECORD.
           05  CR-NAME                 PIC X(19).
           05  FILLER                  PIC X(5).
           05  CR-COURSE-NUMBER        PIC X(5).
           05  FILLER                  PIC X(5).
           05  CR-CREDITS              PIC 9.
           05  FILLER                  PIC X(29).
       FD  CREDITS-REPORT
           LABEL RECORDS ARE STANDARD.
       01  REPORT-LINE-OUT             PIC X(80).
       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  IFCODE           PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  OFCODE           PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       01 COUNTERS-AND-ACCUMULATORS.
           05  REC-KTR        PIC S9(4)     COMP.
           05  TOTAL-AMOUNT   PIC S9(3)V99  COMP-3.

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 100-HOUSEKEEPING THRU 100-EXIT.
           PERFORM 200-MAINLINE THRU 200-EXIT.
           PERFORM 300-CLEANUP THRU 300-EXIT.
           GOBACK.

      ******************************************************************
      *  This routine should perform file open and initial(priming) reads
      ******************************************************************
       100-HOUSEKEEPING.
       100-EXIT.
            EXIT.

      ******************************************************************
      *  This routine contains the business logic for the program
      ******************************************************************
       200-MAINLINE.
       200-EXIT.
            EXIT.

      ******************************************************************
      *  This routine should perform file close operations
      ******************************************************************
       300-CLEANUP.
       300-EXIT.
            EXIT.


