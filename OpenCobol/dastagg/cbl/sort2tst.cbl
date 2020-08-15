       ID DIVISION.
       PROGRAM-ID. SORT2TST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  HOLD-ELEM-ITEMS.
           12 FILLER PIC X(3) VALUE 'BK1'.
           12 FILLER PIC X(3) VALUE 'ALU'.
           12 FILLER PIC X(3) VALUE 'DMX'.
           12 FILLER PIC X(3) VALUE 'CN2'.
           12 FILLER PIC X(3) VALUE 'FOZ'.
           12 FILLER PIC X(3) VALUE 'EPY'.
           12 FILLER PIC X(3) VALUE 'HQW'.
           12 FILLER PIC X(3) VALUE 'GR3'.
           12 FILLER PIC X(3) VALUE 'JSV'.
           12 FILLER PIC X(3) VALUE 'IT4'.

       01 GROUP-ITEM-Storage.
           05 TABL-Element-Cnt               PIC 99 VALUE 10.
           05 TABL-SUB                       PIC 99 VALUE 0.

       01 GROUP-ITEM.
           05 TABL OCCURS 10 TIMES
                 DESCENDING KEY ELEM-ITEM3
                 INDEXED BY TABL-IDX.

              10 ELEM-ITEM1 PIC X.
              10 ELEM-ITEM2 PIC X.
              10 ELEM-ITEM3 PIC X.

       PROCEDURE DIVISION.
       MAIN.
           MOVE HOLD-ELEM-ITEMS TO GROUP-ITEM.

           DISPLAY "This is the TABL Table:".
           DISPLAY "Before SORTING".
           PERFORM VARYING TABL-SUB FROM 1 BY 1
             UNTIL TABL-SUB > TABL-Element-Cnt
             DISPLAY "TABL: " TABL(TABL-SUB)
           END-PERFORM.

           SORT TABL DESCENDING ELEM-ITEM3.

           DISPLAY "This is the TABL Table:".
           DISPLAY "After SORTING DESCENDING ELEM-ITEM3".
           PERFORM VARYING TABL-SUB FROM 1 BY 1
             UNTIL TABL-SUB > TABL-Element-Cnt
             DISPLAY "TABL: " TABL(TABL-SUB)
           END-PERFORM.

           DISPLAY "This is a SEARCH ALL of TABL Table:".
           DISPLAY "After SORTING".
           SEARCH ALL TABL
              AT END DISPLAY 'RECORD NOT FOUND'
              WHEN ELEM-ITEM3(TABL-IDX) = '2'
                 DISPLAY "Found 2"
                 DISPLAY ELEM-ITEM1(TABL-IDX)
                    " "
                       ELEM-ITEM2(TABL-IDX)
                    " "
                       ELEM-ITEM3(TABL-IDX)
                    "."
           END-SEARCH.
           GOBACK.
