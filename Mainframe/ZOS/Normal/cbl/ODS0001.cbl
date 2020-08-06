       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ODS0001.
      *****************************************************************
      * Program name:    ODS0001
      * Original author: dastagg
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  ---------------------------------------
      * 2020-08-06 dastagg        Created for VSCode/COBOL class
      *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-REC
      *     ASSIGN TO "../../../common/data/acctrec.dat.txt"
      *     ORGANIZATION IS LINE SEQUENTIAL
           ASSIGN TO DA-S-ACCTREC
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-AcctRec-Status.

           SELECT PRINT-LINE
      *     ASSIGN TO "../spool/ODS0001-AccRpt.rpt"
      *     ORGANIZATION IS LINE SEQUENTIAL
           ASSIGN TO DA-S-PRTLINE
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-AcctRpt-Status.

       DATA DIVISION.
       FILE SECTION.

       FD  ACCT-REC
           RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
      *     05  ACCT-LIMIT         PIC 9(7)V99.
      *     05  ACCT-BALANCE       PIC 9(7)V99.
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

       FD  PRINT-LINE
           RECORDING MODE F.
       01  FD-PRINT-REC.
           05  ACCT-NO-O      PIC X(8).
           05  ACCT-LIMIT-O   PIC $$,$$$,$$9.99.
           05  ACCT-BALANCE-O PIC $$,$$$,$$9.99.
           05  LAST-NAME-O    PIC X(20).
           05  FIRST-NAME-O   PIC X(15).
           05  COMMENTS-O     PIC X(50).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==AcctRec==.
           COPY WSFST REPLACING ==:tag:== BY ==AcctRpt==.

       PROCEDURE DIVISION.

       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
           PERFORM READ-ACCT-REC.

       2000-Process.
           PERFORM UNTIL WS-AcctRec-EOF
              PERFORM 2100-Move-Fields
              PERFORM WRITE-RECORD
              PERFORM READ-ACCT-REC
           END-PERFORM.

       2100-Move-Fields.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           MOVE FIRST-NAME   TO  FIRST-NAME-O.
           MOVE COMMENTS     TO  COMMENTS-O.

       3000-End-Job.
           CLOSE ACCT-REC
                 PRINT-LINE.

       READ-ACCT-REC.
           READ ACCT-REC
              AT END SET WS-AcctRec-EOF TO TRUE
           END-READ.
           IF WS-AcctRec-Good
              NEXT SENTENCE
           ELSE
              IF WS-AcctRec-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 5000-Read-INFILE"
                 DISPLAY "Read INFILE Failed."
                 DISPLAY "File Status: " WS-AcctRec-Status
                 GOBACK
              END-IF
           END-IF.

       WRITE-RECORD.
           WRITE FD-PRINT-REC.

           IF WS-AcctRpt-Good
              NEXT SENTENCE
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE Accout Report Failed."
              DISPLAY "File Status: " WS-AcctRpt-Status
              GOBACK
           END-IF.
