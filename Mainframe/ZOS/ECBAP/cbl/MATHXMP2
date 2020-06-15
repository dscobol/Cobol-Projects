       IDENTIFICATION DIVISION.
      * ******* EXAMPLES OF TRUNCTION, ROUNDING ERRORS AND OVERFLOW
      * ******* ALSO NEGATIVE NUMBERS
       PROGRAM-ID. MATHXMP2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Elementary items
       01  ELEMENTARY-MATH-FIELDS.
           03  ACCUMULATORS.
                05 LINE-COUNT          PIC 9(2).
                05 COUNT-1             PIC 9(5).
                05 FINAL-COUNT         PIC 9(5).
                05 TOTAL-COUNT         PIC 9(5).
                05 GRAND-TOT           PIC 99.
                05 ONE-MONTH           PIC 99.
           03  TOTALS.
                05  TOT-ONE             PIC 9    VALUE 7.
                05  TOT-TWO             PIC 99   VALUE 2.
                05  TOT-THREE           PIC 99   VALUE 3.

           03 Tax-Amounts-add.
               05 Fed-Tax-a       PIC 9(4)V999 VALUE 5000.999.
               05 Soc-Sec-Tax-a   PIC 9(5)    VALUE 9001.
               05 state-Tax-a     PIC 9(4)V99 VALUE 8000.80.
               05 Total-Tax-a     PIC 9(5)V99 VALUE 8000.80.

           03 Tax-Amounts-subtract.
                 05  Fed-Tax-s           PIC 9(4)V999  Value 5000.111.
                 05  Soc-Sec-Tax-s       PIC 9(5)V99  Value 0.
                 05  State-Tax-s         PIC 9(4)V99  Value 9000.
                 05  Total-Tax-s         PIC 9(5)  Value 0.

           03 Tax-Amount-Mult.
               05 Fed-Tax-m             PIC 9(4)V99 VALUE 5000.90.
               05 Soc-Sec-Tax-m         PIC 9(4)V99 VALUE 8000.80.
               05 Total-Tax-m           PIC 9(5).

           03 Tax-Amounts-D.
               05 Fed-Tax-D             PIC 9(4)V99 VALUE 5000.90.
               05 One-Month-D           PIC 9(3)V99.
               05 Left-Over-D           PIC 9V99.
               05 DIV-1                 PIC 9 VALUE 9.
               05 DIV-2                 PIC 9V99 VALUE 1.34.
               05 LEFT-OVER             PIC 999.
               05 HALF-COUNT            PIC 999.
               05 REM-L                 PIC 99.
               05 COUNT-1-D             PIC 9(5) VALUE 199.
               05 COUNT-2-3             PIC 9(3)V9 VALUE ZERO.
               05 BIG-NBR               PIC S9(9)99 VALUE 1232143243.
               05 BIG-ZERO              PIC S9(9)   VALUE ZERO.


       PROCEDURE DIVISION.
           INITIALIZE ACCUMULATORS.
      * ADD xamples
      *TO Option Examples:

            ADD 1 TO LINE-COUNT.
            ADD Tot-ONE Tot-TWO TO Grand-Tot.
            ADD 500 Tot-ONE     TO Tot-THREE.
      * What happened to Tot-THREE's value?

      * ADD GIVING Examples:
            ADD Tot-ONE to Tot-TWO GIVING Grand-Tot.
      * What happened to ToT-TWO's value?
            ADD 500 Tot-ONE Giving Tot-TWO.
            ADD Fed-Tax-A Soc-Sec-Tax-A State-Tax-A
                            GIVING Total-Tax-A.
      * What does ROUNDED do?
            ADD Fed-Tax-A Soc-Sec-Tax-A State-Tax-A
                            GIVING Total-Tax-A ROUNDED.

      *    SUBTRACT Verb

            SUBTRACT 1 FROM COUNT-1.

            SUBTRACT COUNT-1 FROM 200 GIVING FINAL-COUNT.

            SUBTRACT COUNT-1 FROM TOTAL-COUNT.

      *     SUBTRACT COUNT-1 FROM 200.   (Syntax error)

            SUBTRACT Fed-Tax-S From State-Tax-S
                            Giving Soc-Sec-Tax-S.

            SUBTRACT Fed-Tax-S From State-Tax-S
                            Giving Soc-Sec-Tax-s ROUNDED.

            SUBTRACT Fed-Tax-S From State-Tax-S
                            Giving Total-Tax-s ROUNDED.

      *     MULTIPLY Verb

            MULTIPLY 2 BY COUNT-1.

            MULTIPLY COUNT-1 BY 2 GIVING FINAL-COUNT.

      *     MULTIPLY COUNT-1 BY 20.

            MULTIPLY Fed-Tax-m BY 1.7 Giving Total-Tax-m.
            MULTIPLY Fed-Tax-m BY 1.7 Giving Total-Tax-m Rounded.

      *     MULTIPY 1.7 BY Fed-Tax.

      *     DIVIDE Verb
      *     Can also add :  REMAINDER dataname4. to clause

            DIVIDE COUNT-1 INTO BIG-NBR GIVING ONE-MONTH-D
                ON SIZE ERROR DISPLAY "BIG-NBR PROBLEM".

            DIVIDE BIG-NBR BY BIG-ZERO GIVING BIG-NBR
                ON SIZE ERROR DISPLAY "ZERO-DIVIDE PROBLEM".

            DIVIDE LEFT-OVER BY BIG-NBR GIVING BIG-NBR.

            DIVIDE DIV-1 BY DIV-2 GIVING REM-L.

            DIVIDE DIV-1 BY DIV-2 GIVING REM-L REMAINDER LEFT-OVER-D.

            DIVIDE DIV-1 BY DIV-2 GIVING REM-L ROUNDED.

            DIVIDE 13 INTO COUNT-1-D GIVING LEFT-OVER
                REMAINDER REM-L

            DIVIDE 13 INTO COUNT-1-D GIVING COUNT-2-3 ROUNDED
                 REMAINDER LEFT-OVER.

            DIVIDE Fed-Tax-D BY 12
                   GIVING One-Month-d REMAINDER Left-Over-d.
            GOBACK.
