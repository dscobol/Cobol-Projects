      *****************************************************************
      * Program name:    INTEG1
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-30 dastagg        Demonstrate Integer and
      *                          Integer-Part Functions
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEG1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-Integer-Values.
           12 WS-Int-Value PIC S9(4).
           12 WS-Int-Display PIC ZZZ9+.

       PROCEDURE DIVISION.


           COMPUTE WS-Int-Value = FUNCTION INTEGER(123.456).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer 123.456 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER(-123.456).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer -123.456 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER(45.7).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer 45.7 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER(-45.7).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer -45.7 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER(45.3).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer 45.3 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER(-45.3).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer -45.3 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER(+0).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer +0 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER(-0).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer -0 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER-PART(123.456).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer-Part 123.456 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER-PART(-123.456).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer-Part -123.456 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER-PART(45.7).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer-Part 45.7 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER-PART(-45.7).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer-Part -45.7 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER-PART(45.3).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer-Part 45.3 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER-PART(-45.3).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer-Part -45.3 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER-PART(+0).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer-Part +0 : " WS-Int-Display.

           COMPUTE WS-Int-Value = FUNCTION INTEGER-PART(-0).
           MOVE WS-Int-Value TO WS-Int-Display.
           DISPLAY "Integer-Part -0 : " WS-Int-Display.


           GOBACK.

