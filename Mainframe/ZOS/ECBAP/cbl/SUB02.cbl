       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB02.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CALLING-FLDS.
           05 D     PIC X(01).
           05 X     PIC X(01).
           05 Z     PIC X(01).
       LINKAGE SECTION.
       01  D    PIC X(01).   *> Linkage Parms
       01  G    PIC X(01).   *> Values passed
       01  H    PIC X(01).   *> from SUB01
       PROCEDURE DIVISION USING D, G, H.
      *> Addressability to data from SUB01
           MOVE '2' TO Z, K, U.
           GOBACK. *> Control returned to SUB01