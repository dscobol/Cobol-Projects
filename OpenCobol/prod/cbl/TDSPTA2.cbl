       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDSPTA2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
        FILE SECTION.
        WORKING-STORAGE SECTION.

       01 WS-TwoD.
           12 WS-First OCCURS 3 TIMES INDEXED BY I.
              15 WS-A                  PIC A.
              15 WS-Second OCCURS 2 TIMES INDEXED BY J.
                 18 WS-B               PIC X.
       01 WS-SRCH                      PIC X VALUE 'C'.

       PROCEDURE DIVISION.

           MOVE 'A12B34C56' TO WS-TwoD.
           PERFORM 2000-Display-Table VARYING I FROM 1 BY 1
              UNTIL I > 3.
           SET I J to 1.
           PERFORM 2100-Search.
           GOBACK.

       2000-Display-Table.
           DISPLAY WS-First(I).
           PERFORM 2010-Display-Table VARYING J FROM 1 BY 1
              UNTIL J > 2.       

       2010-Display-Table.
           DISPLAY WS-Second(I, J).
       
       2100-Search.
           SEARCH WS-First 
              AT END DISPLAY 'Letter C not found in table.'
           WHEN WS-A(I) = WS-SRCH 
              DISPLAY 'Letter C was found.' 
           END-SEARCH.
