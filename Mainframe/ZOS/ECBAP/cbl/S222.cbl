      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    S222.
       AUTHOR.        ABEND-222.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FLAG-STOP        PIC X(1) VALUE 'F'.
       PROCEDURE DIVISION.
           PERFORM UNTIL FLAG-STOP = 'Y'
                MOVE 'F' TO FLAG-STOP END-PERFORM.
           GOBACK.