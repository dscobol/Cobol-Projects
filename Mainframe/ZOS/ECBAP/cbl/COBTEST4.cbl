       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SALES-TAX.
           05 FILLER            PIC X(21) VALUE '280000005463968869950'.
           05 FILLER            PIC X(21) VALUE '329650006543603457350'.
           05 FILLER            PIC X(21) VALUE '557450007573101219150'.
           05 FILLER            PIC X(21) VALUE '726900008765280363750'.
           05 FILLER            PIC X(21) VALUE '902650005431500000001'.

       01 SALES-TAX-TABLE REDEFINES SALES-TAX.
      *    05 SALES-TAX-ITEM OCCURS 5 TIMES INDEXED BY TX-IDX.
      *         10 SALES-TAX-L-RANGE     PIC 9(6)V9(2).
      *         10 SALES-TAX-RATE        PIC V9(6).
      *         10 SALES-TAX-H-RANGE     PIC 9(5)V9(2).
           05 SALES-TAX-ITEM OCCURS 5 TIMES
              ASCENDING KEY IS SALES-TAX-L-RANGE INDEXED BY TX-IDX.
                   10 SALES-TAX-L-RANGE         PIC 9(6)V9(2).
                   10 SALES-TAX-RATE            PIC V9(3).
                   10 SALES-TAX-H-RANGE         PIC 9(5)V9(2).
       77  IN-ITEM-TAX-ID     PIC 9(6)V99 VALUE 333333.33.
       PROCEDURE DIVISION.
      *    SET TX-IDX TO 1.
      *    SEARCH SALES-TAX-ITEM
      *       AT END PERFORM 700-INVALID-TAX-ID
      *       WHEN SALES-TAX-L-RANGE (TX-IDX) > IN-ITEM-TAX-ID
      *            PERFORM 200-COMPUTE-WITHOLDING-TOTALS.
           SEARCH ALL SALES-TAX-ITEM
               AT END PERFORM 700-INVALID-TAX-ID
                 WHEN SALES-TAX-L-RANGE (TX-IDX) = IN-ITEM-TAX-ID
                     PERFORM 200-COMPUTE-WITHOLDING-TOTALS.
           GOBACK.
       200-COMPUTE-WITHOLDING-TOTALS.
           DISPLAY '200-COMPUTE-WITHOLDING-TOTALS',
                sales-tax-l-range(tx-idx).
       700-INVALID-TAX-ID.
           DISPLAY 'INVALID TAX ID'.