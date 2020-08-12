       IDENTIFICATION DIVISION.
       PROGRAM-ID.  COBSORT2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUR-FILE ASSIGN TO RENTALS.
       DATA DIVISION.
       FILE SECTION.
       SD  OUR-FILE.
       01  OUR-SORT-REC.
           03  SORT-KEY                PIC X(10).
           03  FILLER                  PIC X(70).
      *     . . .
        WORKING-STORAGE SECTION.
        01  WS-SORT-REC                 PIC X(80).
        01  END-OF-SORT-FILE-INDICATOR  PIC X VALUE 'N'.
            88  NO-MORE-SORT-RECORDS          VALUE 'Y'.
      *. . .
       PROCEDURE DIVISION.
       A-CONTROL SECTION.
           SORT OUR-FILE ON ASCENDING KEY SORT-KEY
           INPUT PROCEDURE IS B-INPUT
           OUTPUT PROCEDURE IS C-OUTPUT.
      *. . .
       B-INPUT SECTION.
           MOVE '11111111' TO WS-SORT-REC.
           RELEASE OUR-SORT-REC FROM WS-SORT-REC.
      *. . .
       C-OUTPUT SECTION.
           DISPLAY 'STARTING READS OF SORTED RECORDS: '.
                RETURN OUR-FILE
                    AT END
                    SET NO-MORE-SORT-RECORDS TO TRUE.
                    PERFORM WITH TEST BEFORE UNTIL NO-MORE-SORT-RECORDS
                    IF SORT-RETURN = 0 THEN
                    DISPLAY 'OUR-SORT-REC = ' OUR-SORT-REC
                    RETURN OUR-FILE
                    AT END
                    SET NO-MORE-SORT-RECORDS TO TRUE
                    END-IF
                    END-PERFORM.