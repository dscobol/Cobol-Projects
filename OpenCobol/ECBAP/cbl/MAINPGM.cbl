       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CALLING-FLDS.
           05 A         PIC X(05).
           05 B         PIC X(05).
           05 C         PIC X(05).
           05 D         PIC X(05).
           05 E         PIC X(05).
           05 G         PIC X(05).
           05 H         PIC X(05).
           05 I         PIC X(05).
           05 J         PIC X(05).
           05 K         PIC X(05).
           05 L         PIC X(05).
           05 M         PIC X(05).
       77  WS-SUB02     PIC X(5).
       LINKAGE SECTION.
       PROCEDURE DIVISION.
      * Value the W-S variables prior to the call.
           MOVE 'MAIN' TO A, B, C, E, D, G, H, I, J, K, L, M.
      * Call SUB01 as a static call (apostrophes surround the name)
           CALL 'SUB01' USING A, B, C, D
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