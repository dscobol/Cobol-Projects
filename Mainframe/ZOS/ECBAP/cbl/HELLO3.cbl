       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO3.
      * Comment: This program Displays a number of text strings
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  VALS.
           05  CUSTOMER-RECORD      PIC X(33).
           05  X                    PIC 99.
           05  Arg1   Pic x(10)  Value "THOMASSON ".
           05  Arg2   Pic x(10)  Value "THOMAS    ".
           05  Arg3   Pic x(10)  Value "VALLEJO   ".
       PROCEDURE DIVISION.
           Move Function Max(Arg1 Arg2 Arg3) To Customer-record(1:10).
           Compute x = Function Ord-max(Arg1 Arg2 Arg3).
           GOBACK.