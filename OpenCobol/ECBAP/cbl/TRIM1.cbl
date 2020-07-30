      *****************************************************************
      * Program name:    TRIM1
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Demonstrate Trim Function
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIM1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 some-string PIC X(32).

       PROCEDURE DIVISION.

           MOVE "    a string literal" TO some-string

           DISPLAY ":" some-string ":"
           DISPLAY ":" FUNCTION TRIM(some-string) ":"
           DISPLAY ":" FUNCTION TRIM(some-string LEADING) ":"
           DISPLAY ":" FUNCTION TRIM(some-string TRAILING) ":"

           GOBACK.

