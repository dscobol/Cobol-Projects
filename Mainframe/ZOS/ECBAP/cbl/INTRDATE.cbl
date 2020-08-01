      *****************************************************************
      * Program name:    INTRDATE
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Created for ECBAP class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  INTRDATE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  DATE-VARS.
           12 CURRENT-YEAR             PIC X(4).
           12 CURRENT-MON              PIC X(2).
           12 CURRENT-DAY              PIC X(2).
           12 CURRENT-HOUR             PIC X(2).
           12 CURRENT-MIN              PIC X(2).
           12 CURRENT-SEC              PIC X(2).
           12 CURRENT-MSEC             PIC X(2).
           12 LOCAL-TIME.
              15 TIME-DIF              PIC X(1).
              15 TIME-DIF-H            PIC X(2).
              15 TIME-DIF-M            PIC X(2).
       01  CURRENT-WEEK-DAY            PIC 9(1).
       01  WEEKDAYS-TABLE.
           12      PIC X(9) VALUE "Monday".
           12      PIC X(9) VALUE "Tuesday".
           12      PIC X(9) VALUE "Wednesday".
           12      PIC X(9) VALUE "Thursday".
           12      PIC X(9) VALUE "Friday".
           12      PIC X(9) VALUE "Saturday".
           12      PIC X(9) VALUE "Sunday".
       01          REDEFINES WEEKDAYS-TABLE.
           12 DT-OF-WK     OCCURS 7 TIMES PIC X(9).

       PROCEDURE DIVISION.
           MOVE FUNCTION CURRENT-DATE TO DATE-VARS.
           ACCEPT CURRENT-WEEK-DAY FROM DAY-OF-WEEK.
           DISPLAY "Date: Year " CURRENT-YEAR
                       " Month " CURRENT-MON
                       " Day "   CURRENT-DAY
                             " (" DT-OF-WK(CURRENT-WEEK-DAY)
                             ")".
           DISPLAY "Time: Hour " CURRENT-HOUR
                       " Minute " CURRENT-MIN
                       " Second " CURRENT-SEC "." CURRENT-MSEC.

           IF LOCAL-TIME NOT = 0
              DISPLAY "Time difference with Greenwich mean time for this
      -               " zone. "
                 TIME-DIF TIME-DIF-H " Hours "
                 TIME-DIF-M " Minutes"
           END-IF.

           GOBACK.
