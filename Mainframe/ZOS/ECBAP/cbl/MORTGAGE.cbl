       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORTGAGE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FACTORS-WS.
           05  PRINCIPAL                 PIC 9(07)V99 VALUE 100000.00.
           05  INT-RATE                  PIC 9(9)V9(9).
           05  NBR-OF-PAYMENTS           PIC 999      VALUE 360.
           05  MONTHLY-PAYMENT           PIC $$,$$$.99.
      *
       PROCEDURE DIVISION.
           COMPUTE INT-RATE =
                 (03 / 100) / 12.
           COMPUTE MONTHLY-PAYMENT
                    = PRINCIPAL *
                      (INT-RATE *
                  (1 + INT-RATE) ** NBR-OF-PAYMENTS) /
                     (((1 + INT-RATE ) ** NBR-OF-PAYMENTS) - 1).
      *
           MOVE .03 TO INT-RATE.
           COMPUTE MONTHLY-PAYMENT =
           PRINCIPAL * FUNCTION ANNUITY((INT-RATE/12) NBR-OF-PAYMENTS).
           GOBACK.
