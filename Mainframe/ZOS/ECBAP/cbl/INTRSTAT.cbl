000005 IDENTIFICATION DIVISION.
000006 PROGRAM-ID. INTRSTAT.
000015 DATA DIVISION.
000016 WORKING-STORAGE  SECTION.
       01  Tax-S            Pic 99v999 value .045.
       01  Tax-T            Pic 99v999 value .02.
       01  Tax-W            Pic 99v999 value .035.
       01  Tax-B            Pic 99v999 value .03.
       01  DISPLAY-VARIABLES.
           05  Ave-Tax          Pic 99v999.
           05  Median-Tax       Pic 99v999.
           05  Tax-Range        Pic 99v999.
       PROCEDURE DIVISION.
           Compute Ave-Tax  = Function Mean (Tax-S Tax-T Tax-W Tax-B)
           Compute Median-Tax = Function Median(Tax-S Tax-T Tax-W Tax-B)
           Compute Tax-Range  = Function Range (Tax-S Tax-T Tax-W Tax-B)
           DISPLAY "AVE TAX: " Ave-Tax.
           DISPLAY "MEDIAN TAX: " MEDIAN-Tax.
           DISPLAY "RANGE TAX: " Tax-Range.
000055     GOBACK.
