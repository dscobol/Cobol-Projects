       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID.  DTEVAL.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.
      **** New comment
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  MISC-FIELDS.
           05 L   PIC S9(4) COMP.

       01  DFH014-WORK-AREA.
           05 DFH014-DATE.
              10 DFH014-YEAR             PIC 9(04).
                 88 DFH014-VALID-YEAR    VALUES 1990 THRU 2050.
              10 DFH014-YEAR-X-TYP
                 REDEFINES
                 DFH014-YEAR.
                 15  FILLER              PIC X(02).
                 15  DFH014-YEAR-JJ      PIC 9(02).
                     88  DFH014-YEAR-NOT-A-LEAP VALUES 01 03 05 07 09.
                     88  DFH014-YEAR-MUST-BE-LEAP VALUES 00 04 08.
              10 DFH014-MONTH            PIC 9(02).
                 88 MONTH-OK             VALUE 1  THRU 12.
                 88 MONTH-31             VALUE 1 3 5 7 8 10 12.
                 88 MONTH-30             VALUE 4 6 9 11.
                 88 MONTH-28-29          VALUE 2.
              10 DFH014-DAY              PIC 9(02).
                 88 DAY-31               VALUE 1  THRU 31.
                 88 DAY-30               VALUE 1  THRU 30.
                 88 DAY-29               VALUE 1  THRU 29.
                 88 DAY-28               VALUE 1  THRU 28.
      *
      *
           05 DFH014-DIVIDE-WORK.
              10 QUOTIENT                PIC S9(04).
              10 REST                    PIC S9(02).
                 88  IT-IS-A-LEAP-YEAR   VALUE ZERO.
                 88  NOT-A-LEAP-YEAR     VALUE 1 2 3.

       LINKAGE SECTION.
       01  LINK-VALS.
           05  DATE-IN     PIC  X(08).
           05  RETURN-CD   PIC S9(04).

       PROCEDURE DIVISION USING  LINK-VALS.
           MOVE +0 TO RETURN-CD.
           IF DATE-IN NOT NUMERIC
              MOVE -4 TO RETURN-CD
              GOBACK.
           MOVE DATE-IN
             TO DFH014-DATE
                                       IN DFH014-WORK-AREA
      *
           IF  DFH014-VALID-YEAR
                                       IN DFH014-YEAR
                                       IN DFH014-DATE
                                       IN DFH014-WORK-AREA
           THEN
               CONTINUE
           ELSE
               MOVE -3 TO RETURN-CD
               GOBACK
           END-IF
      ******************************
           IF  MONTH-OK
                                       IN DFH014-MONTH
                                       IN DFH014-DATE
                                       IN DFH014-WORK-AREA
           THEN
               CONTINUE
           ELSE
               MOVE -2 TO RETURN-CD
               GOBACK
           END-IF
      *
      *
           IF  MONTH-28-29
           THEN
      *
               EVALUATE  TRUE
      *
                   WHEN  DFH014-YEAR-NOT-A-LEAP
                                       IN DFH014-YEAR-JJ
                                       IN DFH014-YEAR-X-TYP
                                       IN DFH014-DATE
                                       IN DFH014-WORK-AREA
                         SET  NOT-A-LEAP-YEAR
                                       IN REST
                                       IN DFH014-DIVIDE-WORK
                           TO TRUE
      *
                   WHEN  DFH014-YEAR-MUST-BE-LEAP
                                       IN DFH014-YEAR-JJ
                                       IN DFH014-YEAR-X-TYP
                                       IN DFH014-DATE
                                       IN DFH014-WORK-AREA
                         SET  IT-IS-A-LEAP-YEAR
                                       IN REST
                                       IN DFH014-DIVIDE-WORK
                           TO TRUE
      * FIXME ... my new revision *
                   WHEN  OTHER
                         DIVIDE      DFH014-YEAR
                                       IN DFH014-DATE
                                       IN DFH014-WORK-AREA
                          BY         4
                          GIVING     QUOTIENT
                          REMAINDER  REST
                         END-DIVIDE
      *
               END-EVALUATE
      *
           END-IF
      *
           EVALUATE  TRUE              ALSO     TRUE
      *
               WHEN  MONTH-31          ALSO     DAY-31
               WHEN  MONTH-30          ALSO     DAY-30
               WHEN  MONTH-28-29       ALSO     DAY-28
               WHEN  MONTH-28-29       ALSO     DAY-29
                 AND IT-IS-A-LEAP-YEAR
                     CONTINUE
      *
               WHEN  OTHER
               MOVE -1 TO RETURN-CD
           END-EVALUATE.
      *