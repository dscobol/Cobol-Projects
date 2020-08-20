      ***********************************************************
      * Program name:    SORT1T
      * Original author: dastagg
      *
      * Description: Program to sort tables.
      *
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  --------------------------------
      * 2020-08-16 dastagg       Created for ECBAP class
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SORT1T.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-State-HOLD.
           COPY STATETBL.

       01  WS-State-Table-Storage.
           12 WS-State-Element-Cnt           PIC S9(4) COMP VALUE +51.
           12 WS-State-Table-Setup.
              15 WS-State-Table OCCURS 51 TIMES
                 ASCENDING KEY WS-State-Abbrev
                 ASCENDING KEY WS-State-Abbrev-Name
                 ASCENDING KEY WS-State-Full-Name
                 INDEXED BY WS-State-IDX.
                18 WS-State-Full-Name            PIC X(21).
                18 WS-State-Abbrev-Name          PIC X(7).
                18 WS-State-Abbrev               PIC X(2).


       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           PERFORM 1100-Load-WS-Tables.

       1100-Load-WS-Tables.
           MOVE WS-State-HOLD  TO WS-State-Table-Setup.

       2000-Process.
           PERFORM 2300-Do-Some-Searching-All.

       2300-Do-Some-Searching-All.
           DISPLAY SPACE.
           DISPLAY "Searching Tables using ALL"
           DISPLAY SPACE.

           SORT WS-State-Table ASCENDING
           WS-State-Abbrev-Name 
           WS-State-Full-Name.

           SEARCH ALL WS-State-Table
              AT END DISPLAY 'RECORD NOT FOUND'
              WHEN WS-State-Abbrev-Name(WS-State-IDX) = 'N.J.'
                 DISPLAY WS-State-Abbrev (WS-State-IDX)
                    " is also known as "
                    FUNCTION TRIM(
                       WS-State-Abbrev-Name(WS-State-IDX))
                    " and it's full name is "
                    FUNCTION TRIM(
                       WS-State-Full-Name (WS-State-IDX))
                    "."
           END-SEARCH.


       3000-End-Job.
           DISPLAY "3000-EOJ: ".
           DISPLAY "Normally, I would have something to do here".
