      *****************************************************************
      * Program name:    STRING1
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Created for ECBAP class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  RCD-01.
           05  CUST-INFO.
               10  CUST-NAME    PIC X(15).
               10  CUST-ADDR    PIC X(35).
               10  CUST-PHONE.
                    15  FILLER      PIC X(1) VALUE '('.
                    15  AREA-CODE   PIC X(3).
                    15  FILLER      PIC X(1) VALUE ')'.
                    15  PREFIX      PIC X(3).
                    15  FILLER      PIC X(1) VALUE '-'.
                    15  SUFFIX      PIC X(4).
           05  BILL-INFO.
               10  INV-NO       PIC X(6).
               10  INV-AMT      PIC $$,$$$.99.
               10  AMT-PAID     PIC $$,$$$.99.
               10  DATE-PAID    PIC X(8).
               10  BAL-DUE      PIC $$,$$$.99.
               10  DATE-DUE     PIC X(8).
       77  RPT-LINE             PIC X(120).

       PROCEDURE DIVISION.
           MOVE 'J.B. SMITH' to CUST-NAME.
           MOVE '444 SPRING ST.,CHICAGO,IL.' TO CUST-ADDR.
           MOVE '(212)555-1234' TO CUST-PHONE.
           MOVE 'A14275' TO INV-NO.
           MOVE 4736.85 TO INV-AMT.
           MOVE 2400.00  TO AMT-PAID.
           MOVE '09/22/76'  TO DATE-PAID.
           MOVE 2336.85 TO BAL-DUE.
           MOVE '10/22/76' TO DATE-DUE.
           MOVE RCD-01 TO RPT-LINE.
           GOBACK.

