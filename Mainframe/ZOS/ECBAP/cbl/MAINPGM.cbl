       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CALLING-FLDS.
           05 CF-A         PIC X(05).
           05 CF-B         PIC X(05).
           05 CF-C         PIC X(05).
           05 CF-D         PIC X(05).
           05 CF-E         PIC X(05).
           05 CF-F         PIC X(05).
           05 CF-G         PIC X(05).
           05 CF-H         PIC X(05).
           05 CF-I         PIC X(05).
           05 CF-J         PIC X(05).
           05 CF-K         PIC X(05).
           05 CF-L         PIC X(05).
           05 CF-M         PIC X(05).
       77  WS-SUB02     PIC X(5).
       LINKAGE SECTION.
       PROCEDURE DIVISION.
      * Value the W-S variables prior to the call.
           MOVE 'MAIN' TO 
              CF-A, CF-B, CF-C, CF-D, CF-E, CF-F, CF-G,
              CF-H, CF-I, CF-J, CF-K, CF-L, CF-M.

           DISPLAY "Before calling all programs: ". 
           DISPLAY "SUB01 Track: ", 
              CF-A, CF-B, CF-C, CF-D, CF-E, CF-F, CF-G.
           DISPLAY "SUB02 Track: ", 
              CF-H, CF-I, CF-J, CF-K, CF-L, CF-M.

      *    For the SUB01 track, all the variables will be returned to
      *       MAIN.

      *    For the SUB02 track, only H, I and J will be returned to 
      *       MAIN. SUB05 will send back its values to SUB02 but thats
      *       as far as they will go. 

           DISPLAY "Before calling SUB01: ", 
              CF-A, CF-B, CF-C, CF-D, CF-E, CF-F, CF-G.

           CALL "SUB01" USING 
              CF-A, CF-B, CF-C, CF-D, CF-E, CF-F, CF-G.

           DISPLAY "After calling SUB01: ", 
              CF-A, CF-B, CF-C, CF-D, CF-E, CF-F, CF-G.


           DISPLAY "Before calling SUB02: ", 
              CF-H, CF-I, CF-J, CF-K, CF-L, CF-M.

           MOVE 'SUB02' TO WS-SUB02.
           CALL WS-SUB02 USING CF-H, CF-I, CF-J.

           DISPLAY "After calling SUB02: ", 
              CF-H, CF-I, CF-J, CF-K, CF-L, CF-M.


           DISPLAY "After calling all programs: ". 
           DISPLAY "SUB01 Track: ", 
              CF-A, CF-B, CF-C, CF-D, CF-E, CF-F, CF-G.
           DISPLAY "SUB02 Track: ", 
              CF-H, CF-I, CF-J, CF-K, CF-L, CF-M.

           GOBACK.
