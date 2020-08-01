       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAHR2CEL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEMPS-VARS.
           05 WS-FAHRENHEIT     PIC 999V99.
           05 WS-FAHRENHEIT-OUT PIC ZZZ.ZZ.
           05 WS-CELSIUS        PIC 999V99.
           05 WS-CELSIUS-OUT    PIC ZZZ.ZZ.

       01 WS-CONVERT-FLAG          PIC X.
           88 CONVERT-F2C              VALUE 'F'.
           88 CONVERT-C2F              VALUE 'C'.

       PROCEDURE DIVISION.
      *    SET CONVERT-F2C TO TRUE.
      *    MOVE 60 TO WS-FAHRENHEIT.
           SET CONVERT-C2F TO TRUE.
           MOVE 300 TO WS-CELSIUS.

           EVALUATE TRUE
              WHEN CONVERT-F2C
                 MOVE WS-FAHRENHEIT TO WS-FAHRENHEIT-OUT
                 COMPUTE WS-CELSIUS ROUNDED =
                  ((WS-FAHRENHEIT - 32) * 5 ) / 9
                 DISPLAY "Fahrenheit: " WS-FAHRENHEIT-OUT
                 MOVE WS-CELSIUS TO WS-CELSIUS-OUT
                 DISPLAY "   Celsius: " WS-CELSIUS-OUT

              WHEN CONVERT-C2F
                 MOVE WS-CELSIUS TO WS-CELSIUS-OUT
                 COMPUTE WS-FAHRENHEIT ROUNDED =
                    (WS-CELSIUS / 5 * 9 + 32)
                 DISPLAY "   Celsius: " WS-CELSIUS-OUT
                 MOVE WS-FAHRENHEIT TO WS-FAHRENHEIT-OUT
                 DISPLAY "Fahrenheit: " WS-FAHRENHEIT-OUT

           END-EVALUATE.

           GOBACK.

