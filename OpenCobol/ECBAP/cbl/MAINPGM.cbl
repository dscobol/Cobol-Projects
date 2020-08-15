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
              CF-A, CF-B, CF-C, CF-D, CF-E, CF-G, 
              CF-H, CF-I, CF-J, CF-K, CF-L, CF-M.
           CALL "SUB01" USING CF-A, CF-B, CF-C, CF-D.
      * Verify that the LINKAGE values manipulated by SUB01 affected.
      *    the sending W-S variables.
      * Call SUB02 using a dynamic call (program name in a variable)
      *     MOVE 'SUB02' TO WS-SUB02.
      *     CALL WS-SUB02   USING D, G, H
      *
      * Verify that the LINKAGE values manipulated by SUB01 affected.
      *    the sending W-S variables.
      *
           GOBACK. *> Control returned to Z/OS
