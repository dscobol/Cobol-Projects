       IDENTIFICATION DIVISION.
       PROGRAM-ID.  INSCLIO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMFILE
             ASSIGN TO UT-S-CLAIM
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS CLAIMFILE-ST-WS.
       DATA DIVISION.
       FILE SECTION.
       FD  CLAIMFILE
           RECORD CONTAINS 90 CHARACTERS.
       01  CLAIM-RECORD                          PIC X(90).

       WORKING-STORAGE SECTION.
       01  MISC-FIELDS.
           05 CLAIMFILE-ST-WS                    PIC X(02).
             88 OPEN-FILE       VALUE 'OP'.
             88 READ-FILE       VALUE 'RE'.
             88 CLOSE-FILE      VALUE 'CL'.
             88 CLAIMFILE-OK    VALUE '00'.
           05 CLAIMFILE-EOF                      PIC X(01).
             88 NO-MORE-CLAIMS  VALUE 'Y'.

       LINKAGE SECTION.
       01  CLAIM-RECORD-WS                       PIC X(90).
       01  IO-AREA REDEFINES CLAIM-RECORD-WS     PIC X(90).
       01  CLAIMFILE-ST-LS                       PIC X(02).

       PROCEDURE DIVISION USING IO-AREA, CLAIMFILE-ST-LS.
           MOVE CLAIMFILE-ST-LS TO CLAIMFILE-ST-WS.
           PERFORM 100-PROCESSING.
           GOBACK.

       100-PROCESSING.
           EVALUATE TRUE
                WHEN OPEN-FILE
                   PERFORM 300-OPEN-FILE
                WHEN READ-FILE
                   PERFORM 400-READ-FILE
                WHEN CLOSE-FILE
                    PERFORM 500-CLOSE-FILE
           END-EVALUATE.
      *
       300-OPEN-FILE.
           OPEN INPUT CLAIMFILE
           IF NOT CLAIMFILE-OK
              DISPLAY 'CLAIM FILE PROBLEM'.
      *
       400-READ-FILE.
           READ CLAIMFILE INTO CLAIM-RECORD-WS
           AT END
              MOVE "Y" TO CLAIMFILE-EOF
           END-READ.
           IF CLAIMFILE-OK OR NO-MORE-CLAIMS
                MOVE '00' TO CLAIMFILE-ST-WS
           ELSE
              DISPLAY 'CLAIM FILE PROBLEM'.
       500-CLOSE-FILE.
