      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBLCONV.
       AUTHOR.        Convert.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO OUTFILE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  OUTFILE RECORDING MODE F.
       01  OUTFILE-FLDS.
           05  ACCT-NO-O            PIC X(8).
           05  ACCT-LIMIT-O         PIC S9(7)V99.
           05  ACCT-BALANCE-O       PIC S9(7)V99.
           05  LAST-NAME-O          PIC X(20).
           05  FIRST-NAME-O         PIC X(15).
           05  CLIENT-ADDR-O.
               10  STREET-ADDR      PIC X(25).
               10  CITY-COUNTY      PIC X(20).
               10  USA-STATE        PIC X(15).
           05  RESERVED-O           PIC X(7).
           05  COMMENTS-O           PIC X(50).
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
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT OUTFILE.
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
           CLOSE OUTFILE.
           STOP RUN.
      *
       READ-RECORD.
           READ ACCT-REC
           AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       WRITE-RECORD.
           MOVE ACCT-NO      TO  ACCT-NO-O.
           MOVE ACCT-LIMIT   TO  ACCT-LIMIT-O.
           MOVE ACCT-BALANCE TO  ACCT-BALANCE-O.
           MOVE LAST-NAME    TO  LAST-NAME-O.
           MOVE FIRST-NAME   TO  FIRST-NAME-O.
           MOVE CLIENT-ADDR  TO  CLIENT-ADDR-O.
           MOVE RESERVED     TO  RESERVED-O.
           MOVE COMMENTS     TO  COMMENTS-O.
           WRITE OUTFILE-FLDS.
      *