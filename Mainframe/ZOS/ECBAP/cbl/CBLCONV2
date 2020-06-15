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
           SELECT INFILE ASSIGN TO INFILE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC1.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  INFILE RECORDING MODE F.
       01  INFILE-FLDS.
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
           OPEN INPUT INFILE .
           OPEN OUTPUT ACCT-REC.
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
           CLOSE INFILE.
           STOP RUN.
      *
       READ-RECORD.
           READ INFILE
           AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       WRITE-RECORD.
           MOVE ACCT-NO-O      TO  ACCT-NO
           MOVE ACCT-LIMIT-O   TO  ACCT-LIMIT.
           MOVE ACCT-BALANCE-O TO  ACCT-BALANCE.
           MOVE LAST-NAME-O    TO  LAST-NAME.
           MOVE FIRST-NAME-O   TO  FIRST-NAME.
           MOVE CLIENT-ADDR-O  TO  CLIENT-ADDR.
           MOVE RESERVED-O     TO  RESERVED.
           MOVE COMMENTS-O     TO  COMMENTS.
           WRITE ACCT-FIELDS.
      *