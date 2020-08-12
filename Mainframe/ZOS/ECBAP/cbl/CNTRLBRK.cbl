      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CNTRLBRK.
       AUTHOR.        SAYLES.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTSORT.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05 FILLER                    PIC X(03)      VALUE SPACE.
           05 USA-STATE-O               PIC X(18).
           05 FIRST-NAME-O              PIC X(15).
           05 LAST-NAME-O               PIC X(20).
           05 ELECTED-O                 PIC X(6).
           05 LAST-YEAR-O               PIC X(6).
           05 ACCT-LIMIT-O              PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(03)      VALUE SPACES.
           05 ACCT-BALANCE-O            PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(30)  VALUE SPACES.
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
               10  USA-STATE      PIC X(15).  *> Input Sort Key
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
      *
       WORKING-STORAGE SECTION.
       01 PROGRAM-INDICATOR-SWITCHES.
           05 WS-EOF-INPUT-SW           PIC X(1)       VALUE 'N'.
               88 EOF-INPUT                            VALUE 'Y'.

009800 01 WS-BREAK-CONTROLS.
009900     05 WS-CONTROL-KEY            PIC X(15). *> Hold/Control Key

      *************************************************************
      ****** Report headings begin here ******
      *************************************************************
       01 WS-BLANK-LINE                 PIC X(133)     VALUE SPACES.

017000 01 WS-HEADER-1.
017100     05 FILLER                    PIC X(1)       VALUE SPACES.
017200     05 FILLER                    PIC X(12)      VALUE
                                                         'Report: A124'.
           05 DATE-O                    PIC X(10)      VALUE SPACE.
017300     05 FILLER                    PIC X(13)      VALUE SPACES.
017400     05 FILLER                    PIC X(47)
017500                                                 VALUE
                              'Presidents Broken Out By State of Birth'.
017600     05 RPT-DATE                  PIC XXXX/XX/XX.
017700     05 FILLER                    PIC X(10)      VALUE SPACES.
017800     05 FILLER                    PIC X(5)       VALUE 'PAGE '.
017900     05 RPT-PAGE-NO               PIC ZZ.
018000     05 FILLER                    PIC X(12)      VALUE SPACES.
018100
018200 01 WS-HEADER-2.
018300     05 FILLER                    PIC X(3)       VALUE SPACES.
018400     05 FILLER                    PIC X(18)      VALUE 'STATE'.
018500     05 FILLER                    PIC X(9)       VALUE 'PRESIDENT'
                                                                      .
018600     05 FILLER                    PIC X(24)      VALUE SPACES.
019100     05 FILLER                    PIC X(7)       VALUE 'ELECTED'.
019200     05 FILLER                    PIC X(1)       VALUE SPACES.
019300     05 FILLER                    PIC X(8)       VALUE 'THRU'.
019500     05 FILLER                    PIC X(14)     VALUE 'SALARY'.
019700     05 FILLER                    PIC X(25)   VALUE '   NET WORTH'
                                                                      .
018200 01  WS-HEADER-3.
018300     05 FILLER                    PIC X(3)       VALUE SPACES.
018400     05 FILLER                    PIC X(17)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
018600     05 FILLER                    PIC X(32)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
019100     05 FILLER                    PIC X(7)       VALUE '======='.
019200     05 FILLER                    PIC X(1)       VALUE SPACES.
019300     05 FILLER                    PIC X(7)        VALUE '====='.
019400     05 FILLER                    PIC X(01)      VALUE SPACES.
019500     05 FILLER                    PIC X(12)       VALUE ALL '='.
019600     05 FILLER                    PIC X(2)       VALUE SPACES.
019700     05 FILLER                    PIC X(25)      VALUE
                                                        '============='.
      *************************************************************
      ****** Control Break Subtotal Line ******
      *************************************************************
018200 01  WS-TRLR-LINE-1.
018300     05 FILLER                    PIC X(03)       VALUE SPACES.
           05 FILLER                    PIC X(12) VALUE 'Sub Totals:'.
           05 STATE-TRLR-LINE           PIC X(15).
           05 FILLER                    PIC X(16) VALUE SPACE.
           05 FILLER                    PIC X(21)
                            VALUE 'Salary | Net Worth: ' JUST RIGHT.
           05 SALARY-SUB-TOT-OUT        PIC $$$,$$$,$$$.99.
           05 FILLER                    PIC X(02)       VALUE SPACES.
           05 NET-WORTH-SUB-TOT-OUT     PIC $$$,$$$,$$$.99.
           05 FILLER                    PIC X(17)      VALUE SPACE.

       01 WS-COUNTERS-AND-ACCUMULATORS.
           05 WS-CONTROL-BREAK-TOTAL    PIC S9(7)V99 COMP-3.
           05 WS-STATE-CTR              PIC  9(2) COMP.

       01 WS-FLAGS.
           05 WS-LASTREC                PIC X          VALUE SPACE.
           05 WS-LINE-KTR               PIC 9(4) COMP  VALUE 0.
           05 WS-SALARY-SUB-TOT          PIC 9(09)V99 VALUE 0.
           05 WS-NET-WORTH-SUB-TOT      PIC 9(09)V99 VALUE 0.
      *------------------
       PROCEDURE DIVISION.
      *------------------
           PERFORM 100-INIT-RTN *> Housekeeping, Initial Report Headings
           PERFORM 300-PROCESS-RECORDS UNTIL EOF-INPUT
           PERFORM 500-CONTROL-BREAK *> Final Control Break paragraphs
           PERFORM 900-WRAP-UP
           GOBACK
           .
       100-INIT-RTN.
           MOVE FUNCTION CURRENT-DATE TO RPT-DATE.
           PERFORM 150-INIT-WS-FIELDS
           PERFORM 200-OPEN-FILES
           MOVE SPACES TO PRINT-REC
           PERFORM 700-READ-RECORD
           PERFORM 500-CONTROL-BREAK *> Initial Control creates Rpt Headings
           .
       150-INIT-WS-FIELDS.
           INITIALIZE WS-COUNTERS-AND-ACCUMULATORS
           .
       200-OPEN-FILES.
           OPEN INPUT ACCT-REC
           OPEN OUTPUT PRINT-LINE
           .
       300-PROCESS-RECORDS.
           IF NOT EOF-INPUT   *> No duplicating last record
               IF WS-CONTROL-KEY = USA-STATE *> Control Break Conditional
                   PERFORM 400-MOVE-DATA
                   PERFORM 600-WRITE-DATA
                   PERFORM 700-READ-RECORD
               ELSE
                   PERFORM 500-CONTROL-BREAK
               END-IF
           END-IF
           .
       400-MOVE-DATA.
           MOVE SPACES TO PRINT-REC
           ADD +1 TO WS-STATE-CTR
           IF WS-STATE-CTR > 1 *> Logic to create outline view in State column
                MOVE SPACES TO USA-STATE-O
           ELSE
                MOVE USA-STATE TO USA-STATE-O,  *> MOVE IN-STATE -> HOLD-KEY
                                  STATE-TRLR-LINE
           END-IF
           ADD ACCT-LIMIT TO WS-SALARY-SUB-TOT.
           ADD ACCT-BALANCE TO WS-NET-WORTH-SUB-TOT
      *** The ACCT file is actually a repurposed file for the presidents
      *** The first four bytes is their inaugural yr => last year in office
           MOVE ACCT-NO(1:4) TO ELECTED-O
           MOVE ACCT-NO(5:4) TO LAST-YEAR-O
           MOVE ACCT-LIMIT TO ACCT-LIMIT-O
           MOVE ACCT-BALANCE TO ACCT-BALANCE-O
           MOVE LAST-NAME TO LAST-NAME-O
           MOVE FIRST-NAME TO FIRST-NAME-O
           .
       500-CONTROL-BREAK.
           IF WS-LINE-KTR > 0  *> Check for first time (beginning of program)
                MOVE WS-SALARY-SUB-TOT TO SALARY-SUB-TOT-OUT
                MOVE WS-NET-WORTH-SUB-TOT TO NET-WORTH-SUB-TOT-OUT
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-TRLR-LINE-1
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-BLANK-LINE
           END-IF
           IF NOT EOF-INPUT
                ADD +1 TO WS-LINE-KTR
                MOVE ZERO TO WS-SALARY-SUB-TOT, WS-NET-WORTH-SUB-TOT
                MOVE WS-LINE-KTR TO RPT-PAGE-NO
                MOVE USA-STATE TO WS-CONTROL-KEY *> SET NEW CONTROL KEY
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-HEADER-1
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-HEADER-2
                WRITE PRINT-REC FROM WS-HEADER-3
                PERFORM 150-INIT-WS-FIELDS
           END-IF
           .
       600-WRITE-DATA.
           WRITE PRINT-REC
           .
       700-READ-RECORD.
           READ ACCT-REC
           AT END
              MOVE 'Y' TO WS-EOF-INPUT-SW
           END-READ
           .
       900-WRAP-UP.
           CLOSE ACCT-REC
           CLOSE PRINT-LINE
           .