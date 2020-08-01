       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVXMPLN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NEG-1                PIC S9 VALUE -1.
       01  EXP1                 PIC 999 VALUE 5.
       77  WS-AGE       PIC 9(03) VALUE ZEROS.
       01  NUMERIC-VARIABLES.
           05 FLD1              PIC 99.
           05 FLD1-SIGNED       PIC S99.
           05 RESULT-FLD1       PIC 9(9)V9(9).
           05 PRINCIPAL         PIC 9(9)    VALUE 2000000.
           05 INT-RATE          PIC V99     VALUE .01.
           05 INT-RATE-DEC      PIC 9(5)V99 VALUE 10.01.
           05 FLD2              PIC 9(3)V99 VALUE 23.6.
           05 FLD2-OUT          PIC $$9.99.
           05 FLD3              PIC 9(5)V99 VALUE 0002131.
           05 FLD3-OUT          PIC $ZZ,ZZ9.99.
           05 FLD4              PIC 9(6)V99 VALUE 876543.21.
           05 FLD4-OUT          PIC $**,***.99.
           05 FLD5              PIC 9(7)V99 VALUE 234.56.
           05 FLD5-OUT          PIC $$9,999.9.
           05 FLD6              PIC S9(3)V99 VALUE -123.4.
           05 FLD6A-OUT         PIC -*,***.99.
           05 FLD6B-OUT         PIC *,***.99-.
           05 FLD7              PIC S9999 VALUE -1234.
           05 FLD7-OUT          PIC -ZZZ,ZZ9.
           05 FLD8              PIC S9(5)V99 VALUE 0000.
           05 FLD8-OUT          PIC $9,999.99 BLANK WHEN ZERO.
           05 FLD9              PIC  9999.
           05 FLD9-OUT          PIC -Z,ZZ9.

      *
       PROCEDURE DIVISION.
           PERFORM MOVE-STATEMENTS.
           PERFORM DISPLAY-RESULTS.
           GOBACK.
      *
       MOVE-STATEMENTS.
           MOVE NEG-1           TO EXP1.
           MOVE EXP1            TO RESULT-FLD1.
           MOVE INT-RATE        TO PRINCIPAL.
           MOVE INT-RATE-DEC    TO WS-AGE.
           MOVE NEG-1           TO FLD1-SIGNED.
           MOVE FLD2            TO FLD2-OUT.
           MOVE FLD3            TO FLD3-OUT.
           MOVE FLD4            TO FLD4-OUT.
           MOVE FLD5            TO FLD5-OUT.
           MOVE FLD6            TO FLD6A-OUT, FLD6B-OUT.
           MOVE -1234.99        TO FLD7.
           MOVE FLD7            TO FLD7-OUT.
           MOVE FLD8            TO FLD8-OUT.
           MOVE -1234           TO FLD9.
           MOVE FLD9            TO FLD9-OUT.
      * Note that the - (minus sign) in FLD9-OUT consumes a blank

       DISPLAY-RESULTS.
           DISPLAY "EXP1 " EXP1.
           DISPLAY "RESULT-FLD1 " RESULT-FLD1.
           DISPLAY "PRINCIPAL " PRINCIPAL.
           DISPLAY "WS-AGE " WS-AGE.
           DISPLAY "FLD1-SIGNED " FLD1-SIGNED.
           DISPLAY "FLD2-OUT " FLD2-OUT.
           DISPLAY "FLD3-OUT " FLD3-OUT.
           DISPLAY "FLD4-OUT " FLD4-OUT.
           DISPLAY "FLD5-OUT " FLD5-OUT.
           DISPLAY "FLD6A-OUT " FLD6A-OUT.
           DISPLAY "FLD6B-OUT " FLD6B-OUT.
           DISPLAY "FLD7-OUT " FLD7-OUT.
           DISPLAY "FLD8-OUT " FLD8-OUT "END OF FLD8-OUT".
           DISPLAY "END".