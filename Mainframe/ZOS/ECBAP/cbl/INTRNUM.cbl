      *****************************************************************
      * Program name:    INTRNUM
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Created for ECBAP class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  INTRNUM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  COUNTERS-AND-ACCUMULATORS.
           12 X                PIC 9(2).
           12 Price1           PIC X(8)    VALUE "$8000".
           12 Price2           PIC X(8)    VALUE "$2000".

       01  Output-Record.
           12 Product-Name     PIC X(20).
           12 Product-Number   PIC 9(9).
           12 Product-Price    PIC 9(6).

       PROCEDURE DIVISION.
           COMPUTE Product-Price =
              FUNCTION MAX (
                 FUNCTION NUMVAL-C (Price1)
                 FUNCTION NUMVAL-C (Price2)
              ).
              DISPLAY "The Product-Price is: " Product-Price.
           COMPUTE X = FUNCTION LENGTH(Output-Record).
              DISPLAY "X is: " X.
           MOVE 'Socks and Stuff' TO Product-Name.
           MOVE FUNCTION UPPER-CASE (Product-Name) TO
              Product-Name.
              DISPLAY "Product-Name is: " Product-Name.
           GOBACK.
