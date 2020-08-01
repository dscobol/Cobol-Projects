       ID DIVISION.
       PROGRAM-ID. GROUPVAR.
      *    THIS IS A SAMPLE PROGRAM THAT DEMONSTRATES GROUP VARIABLES
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-SPACE             PIC X(1) VALUE SPACE.
       01  NAME-AND-AGE.
           05 FILLER            PIC X(12) VALUE 'MY NAME IS: '.
           05 FULLNAME.
                10 WS-FNAME     PIC X(4).
                10 WS-LNAME     PIC X(6).
           05 VITALS.
                10 FILLER       PIC X(16) VALUE 'AND MY AGE IS: '.
                10 WS-AGE       PIC 9(05).
       01  NAME-AND-AGE-OUT.
           05 FILLER            PIC X(12) VALUE 'MY NAME IS: '.
           05 FULLNAME-OUT.
                10 WS-FNAME-OUT     PIC X(4).
                10 WS-LNAME-OUT     PIC X(6).
           05 VITALS-OUT.
                10 FILLER       PIC X(16) VALUE 'AND MY AGE IS: '.
                10 WS-AGE-OUT   PIC 9(05).
       PROCEDURE DIVISION.
           MOVE 'JON' TO WS-FNAME.
           MOVE 'SAYLES' TO WS-LNAME.
           MOVE 68910 TO WS-AGE.
           MOVE NAME-AND-AGE TO NAME-AND-AGE-OUT.
           DISPLAY "Use Case 1 (FULLNAME GROUP FIELD): " FULLNAME.
           DISPLAY SPACES.
           DISPLAY "Use Case 2 (VITALS GROUP FIELD): " VITALS.
           DISPLAY SPACES.
           DISPLAY "Use Case 3 (BOTH GROUP FIELDS): " NAME-AND-AGE.
           DISPLAY "Use Case 3 (BOTH: " NAME-AND-AGE-OUT.
           DISPLAY "???" WS-AGE-OUT.
           MOVE WS-SPACE TO NAME-AND-AGE-OUT
           DISPLAY "Use Case 3 (BOTH GROUP FIELDS): " NAME-AND-AGE.
           DISPLAY "Use Case 3 (BOTH: " NAME-AND-AGE-OUT.
           DISPLAY "???" WS-AGE-OUT.
           GOBACK.