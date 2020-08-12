       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CMPCLAIM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  DEDUCTIBLE-LS                PIC S9(5)V99.
       01  POLICY-AMOUNT                PIC S9(7)V99.
       01  DEDUCTIBLE-PERC              PIC V999.
       01  POLICY-DEDUCTIBLE-MET-LS     PIC X(1).
           88 DEDUCTIBLE-MET VALUE 'Y'.
       01  POLICY-DEDUCTIBLE-PAID       PIC S9(4).
       01  CLAIM-PAID-LS                PIC S9(7)V99.
       01  POLICY-COINSURANCE           PIC V99.
       01  CLAIM-AMOUNT                 PIC S9(7)V99.
       01  PAY-THE-CLAIM-LS             PIC X(1).
       PROCEDURE DIVISION USING
                               DEDUCTIBLE-LS
                               POLICY-AMOUNT,
                               DEDUCTIBLE-PERC,
                               POLICY-DEDUCTIBLE-MET-LS
                               POLICY-DEDUCTIBLE-PAID
                               CLAIM-PAID-LS
                               POLICY-COINSURANCE
                               CLAIM-AMOUNT
                               PAY-THE-CLAIM-LS.
           CALL 'DEDUCT' USING
                               DEDUCTIBLE-LS
                               POLICY-AMOUNT,
                               DEDUCTIBLE-PERC,
                               POLICY-DEDUCTIBLE-MET-LS
                               POLICY-DEDUCTIBLE-PAID.

           IF DEDUCTIBLE-MET
              COMPUTE CLAIM-PAID-LS ROUNDED = CLAIM-AMOUNT
                - (POLICY-COINSURANCE) *(CLAIM-AMOUNT)

           ELSE
              COMPUTE CLAIM-PAID-LS ROUNDED = CLAIM-AMOUNT
                - DEDUCTIBLE-LS - (POLICY-COINSURANCE) *(CLAIM-AMOUNT)
           END-IF

           SUBTRACT CLAIM-PAID-LS FROM POLICY-AMOUNT
           END-SUBTRACT

           IF POLICY-AMOUNT > ZERO
              MOVE 'Y' TO PAY-THE-CLAIM-LS
           ELSE
              MOVE 'N' TO PAY-THE-CLAIM-LS
           END-IF.
           GOBACK.