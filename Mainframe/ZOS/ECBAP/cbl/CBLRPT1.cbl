      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBLRPT1.
       AUTHOR.        SIMPLE READ WRITE OF QSAM FILE.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC              PIC X(80).

      *
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
           05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
           05  LAST-NAME          PIC X(20).
           05  FIRST-NAME         PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR    PIC X(25).
               10  CITY-COUNTY    PIC X(20).
               10  USA-STATE      PIC X(15).
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
      *
       WORKING-STORAGE SECTION.
       01 FLAGS.
           05 LASTREC           PIC X VALUE SPACE.
           05 PAGE-NBR             PIC 99 VALUE 0.

       01  PRINT-REC-O.
           05  FIRST-NAME-O         PIC X(15).
           05  LAST-NAME-O          PIC X(20).
           05  ACCT-NO-O.
                10  INAUG           PIC X(4).
                10  FILLER          PIC X(3) VALUE ' - '.
                10  LEFT-OFFICE     PIC X(4).
           05   FILLER              PIC X(2) VALUE SPACE.
           05  ACCT-LIMIT-O         PIC $$,$$$,$$9.99.
           05   FILLER              PIC X(2) VALUE SPACE.
           05  ACCT-BALANCE-O       PIC $$,$$$,$$9.99.

       01  HDR01.
           05  FILLER               PIC X(20) VALUE SPACES.
           05  FILLER               PIC X(40)
                     VALUE 'FICTITIOUS FINANCES FOR U.S. PRESIDENTS'.
           05  FILLER               PIC X(08) VALUE SPACES.
           05  FILLER               PIC X(07) VALUE 'PAGE #'.
           05  PAGE-NBR-O           PIC ZZ9.

       01  HDR02.
           05  FILLER               PIC X(15) VALUE 'FIRST NAME'.
           05  FILLER               PIC X(20) VALUE 'LAST NAME'.
           05  FILLER               PIC X(11) VALUE 'FROM - TO'.
           05   FILLER              PIC X(2) VALUE SPACE.
           05  FILLER               PIC X(13) VALUE 'FINANCE LIMIT'.
           05   FILLER              PIC X(2) VALUE SPACE.
           05  FILLER               PIC X(15) VALUE 'FINANCE BALANCE'.

       01  HDR03.
           05  FILLER               PIC X(14) VALUE ALL '='.
           05  FILLER               PIC X(01) VALUE SPACE.
           05  FILLER               PIC X(19) VALUE ALL '='.
           05  FILLER               PIC X(01) VALUE SPACE.
           05  FILLER               PIC X(11) VALUE ALL '='.
           05   FILLER              PIC X(2) VALUE SPACE.
           05  FILLER               PIC X(13) VALUE ALL '='.
           05   FILLER              PIC X(2) VALUE SPACE.
           05  FILLER               PIC X(15) VALUE ALL '='.

       01  BLANK-LINE               PIC X(80) VALUE SPACES.

      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
           ADD +1 TO PAGE-NBR.
           MOVE PAGE-NBR TO PAGE-NBR-O.
           WRITE PRINT-REC FROM HDR01.
           WRITE PRINT-REC FROM BLANK-LINE.
           WRITE PRINT-REC FROM HDR02.
           WRITE PRINT-REC FROM HDR03.
      *
       READ-NEXT-RECORD.
            PERFORM READ-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM WRITE-RECORD
            PERFORM READ-RECORD
            END-PERFORM
           .
      *
       CLOSE-STOP.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           GOBACK.
      *
       READ-RECORD.
           READ ACCT-REC
               AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       WRITE-RECORD.
           MOVE ACCT-NO(1:4) TO  INAUG.
           MOVE ACCT-NO(5:4) TO  LEFT-OFFICE.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           MOVE FIRST-NAME   TO  FIRST-NAME-O.
           WRITE PRINT-REC   FROM PRINT-REC-O.
      *