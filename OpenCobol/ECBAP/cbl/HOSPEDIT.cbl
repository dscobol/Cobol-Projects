       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPEDIT.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE
      *     ASSIGN TO HOSPIN
           ASSIGN TO "../../../common/data/ECBAP/hospin.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS IFCODE.

           SELECT RPTFILE
      *     ASSIGN TO RPTFILE
           ASSIGN TO "../../../common/data/ECBAP/hrptfile.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS RFCODE.

           SELECT OUTFILE
      *     ASSIGN TO HOSPOUT
           ASSIGN TO "../../../common/data/ECBAP/houtfile.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT ERRFILE
      *     ASSIGN TO ERRFILE
           ASSIGN TO "../../../common/data/ECBAP/herrfile.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS EFCODE.

           SELECT IT-INS-TYPE
      *     ASSIGN TO INSTYPE
           ASSIGN TO "../../../common/data/ECBAP/hinstype.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS IT-CODE-FLAG.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS In-Rec.
       01  IN-REC  PIC X(100).

       FD  OUTFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS OUT-Rec.
       01  OUT-REC  PIC X(133).

       FD  ERRFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS ERR-Rec.
       01  ERR-REC  PIC X(133).

       FD  RPTFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS RPT-Rec.
       01  RPT-REC PIC X(133).

       FD  IT-INS-TYPE
           RECORD CONTAINS 3 CHARACTERS
           DATA RECORD IS IT-Rec.
       01  IT-REC PIC X(005).

       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  IFCODE                  PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  IT-CODE-FLAG            PIC X(2).
               88 IT-CODE-READ     VALUE SPACES.
               88 IT-NO-MORE-DATA  VALUE "10".
           05  OFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  EFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  RFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       77  INS-COVERAGE-PERC           PIC 9(3) VALUE 10.

       01  WS-OUTPUT-REC.
           05  PATIENT-NBR-O           PIC 9(5).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-NAME-O          PIC X(20).
           05  PATIENT-PHONE-O         PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-TYPE-O          PIC X(2).
           05  BED-IDENTITY-O          PIC ZZZ9.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  CURR-DATE-O             PIC X(6).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-AMT-PER-DAY-O   PIC $$,$$$,$$9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-COVERAGE-PERC-O     PIC 999.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-TYPE-O              PIC X(4).
           05  HOSPITAL-STAY-LTH-O     PIC 999.
           05  FILLER                  PIC X(4) VALUE SPACES.


       01  EOJ-Display-Messages.
           12 EOJ-End-Message PIC X(040) VALUE
              "*** Program HOSPEDIT - End of Run ***".
       01  EOJ-Print-Line.
           12 EOJ-Print-Message PIC X(40) VALUE SPACES.
           12 EOJ-Print-Number  PIC ZZ,ZZZ,ZZ9.
           12 EOJ-Print-Money   REDEFINES 
              EOJ-Print-Number   PIC $$$,$$9.99.

       77  WS-DATE                     PIC 9(6).
       77  MORE-RECORDS-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-RECORDS  VALUE 'N'.

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ             PIC S9(4) COMP.
           05 RECORDS-WRITTEN          PIC S9(4) COMP.
           05 ERROR-RECS               PIC S9(4) COMP.
           05 NBR-INPATIENTS           PIC S9(4) COMP.
           05 NBR-OUTPATIENTS          PIC S9(4) COMP.
           05 NBR-HMO                  PIC S9(4) COMP.
           05 NBR-STATE-FED            PIC S9(4) COMP.
           05 NBR-NO-COVERAGE          PIC S9(4) COMP.
           05 NBR-PPO                  PIC S9(4) COMP.
           05 NBR-PRIVATE              PIC S9(4) COMP.
           05 NBR-AFFORDABLE           PIC S9(4) COMP.
           05 PAT-TOTAL-AMT-NET        PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-GROSS          PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-NET            PIC S9(7)V99 COMP-3.

       01  WS-INPUT-REC.
           05  PATIENT-NBR             PIC 9(5).
           05  PATIENT-NAME.
               10 LAST-NAME            PIC X(10).
               10 FIRST-NAME           PIC X(10).
           05  PATIENT-PHONE           PIC X(10).
           05  PATIENT-TYPE            PIC X(1).
               88 INPATIENT   VALUE "I".
               88 OUTPATIENT  VALUE "O".
               88 VALID-TYPE  VALUES ARE "I", "O".
           05  BED-IDENTITY            PIC 9(4).
           05  DATE-ADMIT              PIC X(10).
           05  AMT-PER-DAY             PIC 9(5)V99.
           05  DIAGNOSTIC-CODE         PIC 999.
           05  INS-TYPE                PIC X(3).
           05  HOSPITAL-STAY-LTH       PIC 999.
           05  PATIENT-TOT-AMT         PIC 9(7)V99.
           05  PCP-ID                  PIC X(6).
           05  IN-OUT-NETWORK          PIC X(1).
               88 IN-NETWORK       VALUE "N".
               88 OUT-OF-NETWORK   VALUE "O".
           05  COPAY                   PIC S9(3).
           05  DEDUCTIBLE              PIC S9(4).

       01  WS-INS-TYPE-FLAG.
           05 WS-INS-TYPE                PIC X.
              88 VALID-INS-TYPE          VALUE "V".
              88 INVALID-INS-TYPE        VALUE "I".

       01  WS-INS-TYPE-TABLE.
           05  INS-TYPE-ITEM OCCURS 5 TIMES INDEXED BY T-IDX PIC X(3).
                88  HMO VALUE 'HMO'.
                88  I-PRIVATE VALUE 'PRI'.
                88  PPO VALUE 'PPO'.
                88  AFFORDABLE VALUE 'AFF'.
                88  MEDICARE VALUE 'MED'.

       01  R1-Counters.
           12 R1-Max-Lines         PIC S9(4) COMP VALUE 60.
           12 R1-Line-Count        PIC S9(4) COMP VALUE ZEROES.
           12 R1-Line-Advance      PIC S9(4) COMP VALUE ZEROES.
           12 R1-Page-Count        PIC S9(4) COMP VALUE ZEROES.
           12 R1-Lines-Written     PIC S9(4) COMP VALUE ZEROES.

       01  R1-Page-Header.
           12 FILLER                   PIC X(006) VALUE "Date: ".
           12 R1-HDR-DATE.
              15 R1-HDR-YY             PIC 9(4).
              15 FILLER                PIC X(1) VALUE "-".
              15 R1-HDR-MM             PIC 9(2).
              15 FILLER                PIC X(1) VALUE "-".
              15 R1-HDR-DD             PIC 9(2).
           12 FILLER                   PIC X(021) VALUE SPACE.
           12 FILLER                   PIC X(034)
                 VALUE "    Group Claims Daily Totals     ".
           12 FILLER                   PIC X(049) VALUE SPACE.
           12 FILLER                   PIC X(005) VALUE "Page:".
           12 FILLER                   PIC X(001) VALUE SPACE.
           12 R1-HDR-Page-Count        PIC ZZ9.

       01 R1-Page-Header2.
           12 FILLER PIC X(040) VALUE
              "All policies STD deductible: .002%".

       01  R1-Column-Header1.
           12 FILLER   PIC X(026) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Policy".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "First".
           12 FILLER   PIC X(006) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Last".
           12 FILLER   PIC X(014) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Renew".
           12 FILLER   PIC X(004) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Deduc".
           12 FILLER   PIC X(005) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Co-Pay".
           12 FILLER   PIC X(002) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Deduc".
           12 FILLER   PIC X(012) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Claim".
           12 FILLER   PIC X(011) VALUE SPACES.
           12 FILLER   PIC X(005) VALUE "Claim".

       01  R1-Column-Header2.
           12 FILLER   PIC X(011) VALUE "Policy Type".
           12 FILLER   PIC X(015) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Number".
           12 FILLER   PIC X(003) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Name".
           12 FILLER   PIC X(007) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Name".
           12 FILLER   PIC X(015) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Date".
           12 FILLER   PIC X(005) VALUE SPACES.
           12 FILLER   PIC X(003) VALUE "Met".
           12 FILLER   PIC X(006) VALUE SPACES.
           12 FILLER   PIC X(007) VALUE "Amount".
           12 FILLER   PIC X(001) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Amount".
           12 FILLER   PIC X(010) VALUE SPACES.
           12 FILLER   PIC X(006) VALUE "Amount".
           12 FILLER   PIC X(012) VALUE SPACES.
           12 FILLER   PIC X(004) VALUE "Paid".


       01  R1-Column-Header3.
           12 FILLER    PIC X(023) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(015) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(010) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(005) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(011) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(006) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(015) VALUE ALL "-".
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 FILLER    PIC X(015) VALUE ALL "-".

       01  R1-Detail-Line.
           12 R1-Policy-Type                PIC X(23).
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 R1-Insured-Policy-No          PIC XXXXXXXXX.
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 R1-Insured-First-Name         PIC X(10).
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 R1-Insured-Last-Name          PIC X(15).
           12 FILLER    PIC X(001) VALUE ALL SPACES.
           12 R1-Policy-Benefit-Date-Num    PIC XXXX/XX/XX.
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 R1-Policy-Deductible-Met     PIC X.
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 R1-Policy-Copay-Amount         PIC $$$$,$$9.99.
           12 FILLER    PIC X(002) VALUE ALL SPACES.
           12 R1-Deductible-Amount           PIC $$$$.
           12 FILLER    PIC X(004) VALUE ALL SPACES.
           12 R1-Claim-Amount               PIC $$,$$$,$$9.99.
           12 FILLER    PIC X(003) VALUE ALL SPACES.
           12 R1-Claim-Amount-Paid          PIC $$,$$$,$$9.99.

       01  R1-Footer1.
           12 FILLER             PIC X(036)
              VALUE " Number of Input Records Read: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Recs-Read PIC ZZ9.
       01  R1-Footer2.
           12 FILLER             PIC X(036)
              VALUE "        Number of Claims Paid: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-Valid-Records PIC ZZ9.
       01  R1-Footer3.
           12 FILLER             PIC X(036)
              VALUE "    Number of Claims Not Paid: ".
           12 FILLER             PIC X VALUE SPACE.
           12 R1-Total-InValid-Records PIC ZZ9.


       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-RECORDS.
           PERFORM 200-CLEANUP THRU 200-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
      *     DISPLAY "HOUSEKEEPING".
           ACCEPT  WS-DATE FROM DATE.
           OPEN INPUT INFILE.
           OPEN OUTPUT OUTFILE.
           OPEN OUTPUT RPTFILE.
           OPEN OUTPUT ERRFILE.
           INITIALIZE  COUNTERS-AND-ACCUMULATORS,
                       WS-OUTPUT-REC.
           READ INFILE INTO WS-INPUT-REC
               AT END
               MOVE 'N' TO MORE-RECORDS-SW
               GO TO 000-EXIT
           END-READ.
           ADD +1 TO RECORDS-READ.
      *     DISPLAY "INREC " IN-REC.
      *     DISPLAY "WS REC" WS-INPUT-REC.

           OPEN INPUT IT-INS-TYPE.
           SET T-IDX TO 1
           PERFORM 5 TIMES
              READ IT-INS-TYPE INTO INS-TYPE-ITEM(T-IDX)
              SET T-IDX UP BY 1
           END-PERFORM.
           CLOSE IT-INS-TYPE.

       000-EXIT.
           EXIT.

       100-MAINLINE.
      *     DISPLAY "MAINLINE".
      *     DISPLAY WS-INPUT-REC.

           SET T-IDX TO 1.
           SEARCH INS-TYPE-ITEM VARYING T-IDX
              AT END SET INVALID-INS-TYPE TO TRUE
              WHEN INS-TYPE-ITEM(T-IDX) = INS-TYPE
                 SET VALID-INS-TYPE TO TRUE 
           END-SEARCH

           IF VALID-TYPE AND VALID-INS-TYPE
               MOVE WS-INPUT-REC TO OUT-REC
               WRITE OUT-REC
           ELSE
               MOVE WS-INPUT-REC TO ERR-REC
               WRITE ERR-REC
               ADD +1 TO ERROR-RECS
               READ INFILE INTO WS-INPUT-REC
                   AT END MOVE "N" TO MORE-RECORDS-SW
                   GO TO 100-EXIT
               END-READ
               ADD +1 TO RECORDS-READ
               GO TO 100-EXIT
           END-IF

           SET T-IDX TO 1.
           SEARCH INS-TYPE-ITEM VARYING T-IDX
              AT END ADD +1 TO NBR-NO-COVERAGE
              WHEN INS-TYPE-ITEM(T-IDX) = INS-TYPE
                 EVALUATE TRUE 
                    WHEN HMO(T-IDX)
                       ADD +1 TO NBR-HMO
                    WHEN MEDICARE(T-IDX)
                       ADD +1 TO NBR-STATE-FED
                    WHEN AFFORDABLE(T-IDX)
                       ADD +1 TO NBR-AFFORDABLE
                    WHEN PPO(T-IDX)
                       ADD +1 TO NBR-PPO
                    WHEN I-PRIVATE(T-IDX)
                       ADD +1 TO NBR-PRIVATE
                 END-EVALUATE
           END-SEARCH
      
           IF INPATIENT
               ADD +1 TO NBR-INPATIENTS
           ELSE
               ADD +1 TO NBR-OUTPATIENTS
           END-IF

           COMPUTE PAT-TOTAL-AMT-NET =
               (PATIENT-TOT-AMT  +
                   AMT-PER-DAY * ((100 - INS-COVERAGE-PERC) / 100))
           END-COMPUTE

           ADD PAT-TOTAL-AMT-NET   TO TOTAL-AMT-NET.
           ADD PATIENT-TOT-AMT     TO TOTAL-AMT-GROSS.

           MOVE PATIENT-NBR        TO 
              PATIENT-NBR-O.
           MOVE PATIENT-NAME       TO 
              PATIENT-NAME-O.
           MOVE PATIENT-PHONE      TO 
              PATIENT-PHONE-O.
           MOVE PATIENT-TYPE       TO 
              PATIENT-TYPE-O.
           MOVE WS-DATE            TO 
              CURR-DATE-O.
           MOVE BED-IDENTITY       TO 
              BED-IDENTITY-O.
           ADD PAT-TOTAL-AMT-NET  TO PATIENT-TOT-AMT
               GIVING PATIENT-AMT-PER-DAY-O.
           MOVE INS-COVERAGE-PERC  TO 
              INS-COVERAGE-PERC-O.
           MOVE INS-TYPE           TO 
              INS-TYPE-O.
           ADD  +1                 TO HOSPITAL-STAY-LTH                       
              GIVING HOSPITAL-STAY-LTH-O.

           WRITE RPT-REC FROM WS-OUTPUT-REC.
           ADD +1 TO RECORDS-WRITTEN.

           READ INFILE INTO WS-INPUT-REC
               AT END MOVE "N" TO MORE-RECORDS-SW
               GO TO 100-EXIT
           END-READ
           ADD +1 TO RECORDS-READ.
       100-EXIT.
           EXIT.

       200-CLEANUP.
      *  Move the final computational fields
      *     DISPLAY "CLEAN-UP".
           DISPLAY EOJ-End-Message.

           MOVE "Rec in:" TO EOJ-Print-Message.
           MOVE RECORDS-READ TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.

           MOVE "Rec written:" TO EOJ-Print-Message.
           MOVE RECORDS-WRITTEN TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "Rec Errors:" TO EOJ-Print-Message.
           MOVE ERROR-RECS TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "Inpat:" TO EOJ-Print-Message.
           MOVE NBR-INPATIENTS TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "Outpat:" TO EOJ-Print-Message.
           MOVE NBR-OUTPATIENTS TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "HMO:" TO EOJ-Print-Message.
           MOVE NBR-HMO TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "PPO:" TO EOJ-Print-Message.
           MOVE NBR-PPO TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "MED:" TO EOJ-Print-Message.
           MOVE NBR-STATE-FED TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "PRI:" TO EOJ-Print-Message.
           MOVE NBR-PRIVATE TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "AFF:" TO EOJ-Print-Message.
           MOVE NBR-AFFORDABLE TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.
           MOVE "No Cov:" TO EOJ-Print-Message.
           MOVE NBR-NO-COVERAGE TO EOJ-Print-Number.
           MOVE EOJ-Print-Line TO RPT-REC.
           WRITE RPT-REC.

      *     WRITE RPT-REC FROM WS-TOTALS-REC.
           CLOSE OUTFILE, RPTFILE, ERRFILE, INFILE.

       200-EXIT.
           EXIT.

