       IDENTIFICATION DIVISION.
      * ******* EXAMPLES OF TRUNCTION, ROUNDING ERRORS AND OVERFLOW
      * ******* ALSO NEGATIVE NUMBERS
       PROGRAM-ID. MATHEX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NEG-1            PIC S9 VALUE -1.
       01  ZERO-VAL         PIC S9 VALUE +0.
       01  WS-FAHR          PIC 999 VALUE 60.
       01  WS-CEL           PIC 999 VALUE 0.
       01  EXP1             PIC 999 VALUE 5.
       01  EXP2             PIC 999 VALUE 2.
       01  HYPOTENUSE       PIC 9(05) VALUE 0.
       01  SIDE1            PIC 999 VALUE 10.
       01  SIDE2            PIC 999 VALUE 10.
       01  NUMERIC-VARIABLES.
           05 FLD1         PIC 99.
           05 FLD1-BIG     PIC S9(5).
           05 FLD1-SIGNED  PIC S99.
           05 FLD2         PIC 99.
           05 FLD3         PIC 99.
           05 FLD4         PIC 99.
           05 FLD4-DEC     PIC 9(5)V999.
           05 FLD5         PIC 99.
           05 FLD5-BIG     PIC 9(5).
           05 FLD6         PIC 99.
           05 FLD7         PIC 99.
           05 FLD8         PIC 99.
           05 FLD9         PIC 999V9.
           05 FLD9-BIG     PIC 9(5).
           05 FLDA         PIC 99.
           05 FLDA-DEC     PIC 9(5)V99.
           05 FLDB-DEC     PIC 9(5)V99.
           05 FLDB         PIC 99.
           05 FLDC         PIC 99.
           05 FLDD         PIC 99.
           05 FLDE         PIC 99.
           05 FLDF         PIC 9V9.
           05 RESULT-FLD1       PIC 9(9)V9(9).
           05 RESULT-FLD2       PIC 9(9)V9(9).
           05 RESULT-FLD3       PIC 9(9)V9(9).
           05 MONTHLY-PAYMENT   PIC 9(9)V99.
           05 PRINCIPAL         PIC 9(9)    VALUE 2000000.
           05 INT-RATE          PIC V99     VALUE .01.
           05 INT-RATE-DEC      PIC 9(5)V99 VALUE 10.01.
           05 NBR-PAYMENTS      PIC 9(3)    VALUE 120.
       PROCEDURE DIVISION.
           PERFORM INIT-RTN.
           PERFORM ROUNDING-EXAMPLES.
           PERFORM ADD-EXAMPLES.
           PERFORM INIT-RTN.
           PERFORM SUBTRACT-EXAMPLES.
           PERFORM INIT-RTN.
           PERFORM MULTIPLY-EXAMPLES.
           PERFORM INIT-RTN.
           PERFORM DIVIDE-EXAMPLES.
           PERFORM INIT-RTN.
           PERFORM COMPUTE-EXAMPLES.
           PERFORM INIT-RTN.
           PERFORM COMPUTE-PYTHAGORAS.
           PERFORM INIT-RTN.
           PERFORM COMPUTE-FAHR-CELS.
           PERFORM INIT-RTN.
           PERFORM COMPUTE-MONTHLY-PAYMENT.
           GOBACK.
       INIT-RTN.
           INITIALIZE NUMERIC-VARIABLES.
           MOVE  11 TO FLD1 FLD1-BIG FLD1-SIGNED.
           MOVE  22 TO FLD2.
           MOVE  33 TO FLD3.
           MOVE  44 TO FLD4 FLD4-DEC.
           MOVE  55 TO FLD5 FLD5-BIG.
           MOVE  66 TO FLD6.
           MOVE  77 TO FLD7.
           MOVE  88 TO FLD8.
           MOVE  99 TO FLD9 FLD9-BIG.
           MOVE  12 TO FLDA FLDA-DEC.
           MOVE  13 TO FLDB FLDB-DEC.
           MOVE  14 TO FLDC.
           MOVE  15 TO FLDD.
           MOVE  16 TO FLDE.
       ROUNDING-EXAMPLES.
      *** NOTE THE DIFFERENCE BETWEEN THESE THREE STATEMENTS
           COMPUTE int-rate     = ( (1.00 + 10 / 100) ** 5) + .5.
           COMPUTE int-rate ROUNDED = ( (1.00 + 10 / 100) ** 5) + .5.
           COMPUTE int-rate-DEC = ( (1.00 + 10 / 100) ** 5) +. 5.
       ADD-EXAMPLES.
      ***** On Size Error examples
           ADD FLD1 TO FLD2.
           ADD FLD3 TO FLD4 GIVING FLD5.
           ADD FLD3 TO FLD4 GIVING FLD5-BIG.
           ADD FLD6 TO FLD7 GIVING FLD8 FLD9.
      *     DISPLAY NUMERIC-VARIABLES.
       SUBTRACT-EXAMPLES.
           SUBTRACT FLD1 FROM FLD2.
           SUBTRACT FLD4 FROM FLD1.
           SUBTRACT FLD4 FROM FLD1-SIGNED.
           SUBTRACT 200 FROM FLDF GIVING FLDF.
           SUBTRACT FLD1 FROM 200 GIVING FLDF.
           SUBTRACT FLD3 FROM FLD4 GIVING FLD5.
           SUBTRACT FLD6 FROM FLD7 GIVING FLD8 FLD9.
           DISPLAY NUMERIC-VARIABLES.
       MULTIPLY-EXAMPLES.
      ***** On Size Error examples
           MULTIPLY FLD1 BY FLD2.
           MULTIPLY FLD1-BIG BY FLD2.
           MULTIPLY FLD1     BY NEG-1.
           MULTIPLY FLD1-BIG BY NEG-1.
           MULTIPLY FLD3 BY FLD5 GIVING FLD6
                     ON SIZE ERROR MOVE 9 TO FLD1.
           MULTIPLY FLD3 BY FLD5 GIVING FLD5-BIG.
           MULTIPLY FLD6 BY FLD7 GIVING FLD8 FLD9.
           MULTIPLY FLD6 BY FLD7 GIVING FLD8 FLD9-BIG.
           DISPLAY NUMERIC-VARIABLES.
       DIVIDE-EXAMPLES.
      ***** On Size Error & Rounded examples
           DIVIDE   FLD3 INTO FLD4.
           DIVIDE   FLD3 INTO FLD4-DEC.
           DIVIDE   FLD3 INTO FLD4 ROUNDED.
           DIVIDE   FLD5 BY FLD6 GIVING FLD7.
           DIVIDE   FLD5 BY FLD6 GIVING FLD7 ROUNDED.
           DIVIDE   FLD8 BY FLD9 GIVING FLDA FLDA-DEC.
           DIVIDE   FLD1 BY ZERO-VAL GIVING FLD1-SIGNED
               ON SIZE ERROR DISPLAY 'ATTEMPT TO DIVIDE BY ZERO'.
           DISPLAY NUMERIC-VARIABLES.
       COMPUTE-EXAMPLES.
           Compute
               RESULT-FLD1 = FLD1 + FLD2 / FLD3 ** 4
               ON SIZE ERROR DISPLAY 'ATTEMPT TO DIVIDE BY ZERO'.
           Compute
               RESULT-FLD2 = (FLD1 + FLD2) * FLD3 / (FLD3 ** 4) - FLD5
               ON SIZE ERROR DISPLAY 'ATTEMPT TO DIVIDE BY ZERO'.
       COMPUTE-PYTHAGORAS.
            COMPUTE HYPOTENUSE =
                          FUNCTION SQRT (SIDE1 ** 2 + SIDE2 ** 2).
       COMPUTE-FAHR-CELS.
            COMPUTE WS-CEL = (WS-FAHR - 32) * (0.5556).
            COMPUTE WS-CEL ROUNDED = (WS-FAHR - 32) * (0.5556).
       COMPUTE-MONTHLY-PAYMENT.
            COMPUTE MONTHLY-Payment
                    = Principal *
                      (int-rate *
                  (1 + INT-RATE) ** NBR-PAYMENTS) /
                     (((1 + INT-RATE ) ** NBR-PAYMENTS) - 1)
                     ON SIZE ERROR MOVE 9 TO FLD1.