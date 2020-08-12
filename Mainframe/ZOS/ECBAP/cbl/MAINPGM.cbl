       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CALLING-FLDS.
           05 A     PIC X(01).
           05 B     PIC X(01).
           05 C     PIC X(01).
           05 D     PIC X(01).
           05 G     PIC X(01).
           05 H     PIC X(01).
       77 SUB02   PIC X(5) VALUE 'SUB02'.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
            *> Addressability to data from MAIN
      *  ... Processing ...
            MOVE '0' TO A, B, C, D, G, H.
            CALL 'SUB01' USING A, B, C.
            CALL SUB02   USING D, G, H.
      *  ... Processing ...
            GOBACK. *> Control returned to Z/OS