      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    S806.
       AUTHOR.        ABEND-S0C7.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BUDDY        PIC X(8) VALUE 'BUDDY'.
       01  BUDDY1         PIC X(8) VALUE SPACES.
       01  A-TABLE.
           05 A-TAB PIC X(1) OCCURS 9 TIMES.
       PROCEDURE DIVISION.
      *     call 'BUDDY'.
           call BUDDY.
           MOVE 'BUDDY' TO BUDDY1.
           CALL BUDDY1.
           GOBACK.