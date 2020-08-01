      *****************************************************************
      * Program name:    INTRFINA
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Created for ECBAP class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  INTRFINA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  Present-Value-Vars.
           12 Series-Amt1             PIC 9(9)V99    VALUE 100.
           12 Series-Amt2             PIC 9(9)V99    VALUE 200.
           12 Series-Amt3             PIC 9(9)V99    VALUE 300.
           12 Discount-Rate           PIC S9(2)V9(6) VALUE .10.
           12 Todays-Value            PIC 9(9)V99.

       01  Annuity-Vars.
           12 Loan                    PIC 9(9)V99.
           12 Payment                 PIC 9(9)V99.
           12 Interest                PIC 9(9)V99.
           12 Number-Periods          PIC 99.

       PROCEDURE DIVISION.
           PERFORM 100-Present-Value.
           PERFORM 200-Annuity.
           GOBACK.

       100-Present-Value.
           COMPUTE Todays-Value =
              FUNCTION PRESENT-VALUE
                 (Discount-Rate Series-Amt1 Series-Amt2 Series-Amt3).
           DISPLAY "Today's Value = " Todays-Value.

       200-Annuity.
           COMPUTE Loan = 15000.
           COMPUTE Interest = .12.
           COMPUTE Number-Periods = 36.
           COMPUTE Payment =
              Loan * FUNCTION ANNUITY ((Interest / 12) Number-Periods ).

           DISPLAY "The Annuity Payment will be: " Payment.
