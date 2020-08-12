       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DEDUCT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  DEDUCTIBLE-LS            PIC S9(5)V99.
       01  POLICY-AMOUNT            PIC S9(7)V99.
       01  DEDUCTIBLE-PERC          PIC V999.
       01  POLICY-DEDUCTIBLE-MET-LS PIC X(1).
       01  POLICY-DEDUCTIBLE-PAID   PIC S9(4).
       PROCEDURE DIVISION USING
                               DEDUCTIBLE-LS
                               POLICY-AMOUNT,
                               DEDUCTIBLE-PERC,
                               POLICY-DEDUCTIBLE-MET-LS
                               POLICY-DEDUCTIBLE-PAID.
      *
           MOVE .002 TO DEDUCTIBLE-PERC.
           COMPUTE DEDUCTIBLE-LS ROUNDED =
              POLICY-AMOUNT * DEDUCTIBLE-PERC

           IF POLICY-DEDUCTIBLE-PAID >= DEDUCTIBLE-LS
              MOVE "Y" TO POLICY-DEDUCTIBLE-MET-LS
           ELSE
              MOVE "N" TO POLICY-DEDUCTIBLE-MET-LS
           END-IF.