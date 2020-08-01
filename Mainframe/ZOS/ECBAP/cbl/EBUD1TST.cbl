       ID DIVISION.
       PROGRAM-ID. EBUD1TST.
      *    DRIVER TEST STUB FOR EBUD01, EBUD02, EBUD03 DEBUGGING
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
          FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
      *
       01 W-CALL-PROGRAM       PIC X(8) VALUE 'EBUD01'.
      *
        01 INTERFACE-AREA.
          05 L-INPUT-LENGTH    PIC 9(4) COMP VALUE 94.
          05 L-INPUT-DATE.
             10 L-CCYY       PIC X(4) VALUE '2001'.
             10 L-MM         PIC X(2) VALUE '11'.
             10 L-DD         PIC X(2) VALUE '21'.
          05 DAYS-DIFF       PIC 9(8) COMP VALUE 11.
          05 RETIREMENT-DATE PIC X(80)
             VALUE 'OCTOBER 01, 2017'.
          05 RETC            PIC S9(4) COMP VALUE 0.
      *
       PROCEDURE DIVISION.

           CALL W-CALL-PROGRAM  USING INTERFACE-AREA.

           GOBACK
           .
       END-OF-SECTION.
           EXIT.