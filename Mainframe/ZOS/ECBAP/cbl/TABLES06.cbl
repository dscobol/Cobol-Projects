       IDENTIFICATION DIVISION.
         PROGRAM-ID. TABLES06.
      ** EXAMPLE OF TWO-DIM OCCURS -
      *  OCCURS DEPENDING ON, AND PLAIN OCCURS WIHIN AN OCCURS
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 YEARLY-SALES.
           05 MONTHLY OCCURS 12 TIMES.
              10 MONTHLY-1 OCCURS 31 TIMES.
                  20 NBR-ITEMS                   PIC 9(5).
                  20 MONTHLY-SALES               PIC 9(7)V99.
                  20 MONTHLY-RETURNS             PIC 9(5).
              10 MONTHLY-2 OCCURS 28 TIMES.
                  20 NBR-ITEMS                   PIC 9(5).
                  20 MONTHLY-SALES               PIC 9(7)V99.
                20 MONTHLY-RETURNS               PIC 9(5).

       PROCEDURE DIVISION.
           INITIALIZE YEARLY-SALES.
           DISPLAY NBR-ITEMS(1,5).
           GOBACK
           .
      *> can not initialize variable-lth table