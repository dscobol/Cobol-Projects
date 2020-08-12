       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB03.
       DATA DIVISION.
       LINKAGE SECTION.
       01  Z    PIC X(01).   *> Linkage Parms
       01  K    PIC X(01).   *> Values passed
       01  U    PIC X(01).   *> from SUB01
       PROCEDURE DIVISION USING Z, K, U.
      *> Addressability to data from SUB01
           MOVE '2' TO Z, K, U.
           GOBACK. *> Control returned to SUB01