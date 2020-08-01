      *****************************************************************
      * Program name:    INTRSTAT
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Created for ECBAP class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  INTRSTAT.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-Compute-Variables.
           12 Tax-S                PIC 99V999 VALUE .045.
           12 Tax-T                PIC 99V999 VALUE .02.
           12 Tax-W                PIC 99V999 VALUE .035.
           12 Tax-B                PIC 99V999 VALUE .03.

       01  WS-Display-Variables.
           12 Avg-Tax              PIC 99.999.
           12 Median-Tax           PIC 99.999.
           12 Tax-Range            PIC 99.999.

       PROCEDURE DIVISION.

           COMPUTE Avg-Tax = 
              FUNCTION MEAN (Tax-S Tax-T Tax-W Tax-B).
           COMPUTE Median-Tax  = 
              FUNCTION MEDIAN  (Tax-S Tax-T Tax-W Tax-B).
           COMPUTE Tax-Range  = 
              FUNCTION RANGE  (Tax-S Tax-T Tax-W Tax-B).

           DISPLAY "Avg Tax: " Avg-Tax.
           DISPLAY "Medaian Tax: " Median-Tax.
           DISPLAY "Range Tax: " Tax-Range.
           
           GOBACK.
