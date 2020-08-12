       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALLING-FLDS.
           05 Z     PIC X(04).
           05 K     PIC X(04).
           05 U     PIC X(04).
       LINKAGE SECTION.
       01  A     PIC X(01).  *> Linkage Parms
       01  B     PIC X(01).  *> Data values
       01  C     PIC X(01).  *> passed from MAIN
       PROCEDURE DIVISION USING A, B, C.
            *> Addressability to data from MAIN
           MOVE 'SUB01' TO A, B, C, Z, K, U.
           CALL 'SUB0O' USING Z, K, U.
           GOBACK. *> Control returned to MAIN