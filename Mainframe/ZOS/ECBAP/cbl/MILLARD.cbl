       IDENTIFICATION DIVISION.
       PROGRAM-ID. MILLARD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PERSONNEL-REC.
           05  NAME       PIC X(20).
           05  ADDR       PIC X(40).
           05  DATE-WS    PIC X(30).
           05  RATE       PIC 9(3)V99.
           05  BONUS-RATE PIC V99.
           05  HOURS      PIC 9(3).
           05  GROSS-PAY  PIC 9(6)V99.
           05  JOB        PIC X(14).

       01  VP-REC.
           05  VP-NAME       PIC X(20).
           05  VP-RATE       PIC 9(3)V99.
           05  VP-BONUS-RATE PIC V99.
           05  VP-HOURS      PIC 9(3).
           05  VP-GROSS-PAY  PIC 9(6)V99.
           05  VP-JOB        PIC X(14).

       PROCEDURE DIVISION.
           PERFORM ASSIGNMENT-PARAGRAPH.
           PERFORM CONDITIONAL-SELECTION.
           PERFORM DISPLAY-DATA-PARAGRAPH.
           GOBACK.

       ASSIGNMENT-PARAGRAPH.
           MOVE  "Millard Fillmore" TO NAME.
           MOVE "61 Brigham Tavern Lane, Duxbury MA" TO ADDR.
           MOVE  "Week of: February 24th, 2020" TO DATE-WS.
           MOVE 19 TO HOURS.
           MOVE 23.50 TO RATE.
           MOVE "PRESIDENT" TO JOB.

           MOVE  "Abigail Fillmore" TO VP-NAME.
           MOVE 21 TO VP-HOURS.
           MOVE 23.50 TO VP-RATE.
           MOVE "VICE PRESIDENT" TO VP-JOB.

       CONDITIONAL-SELECTION.
           IF JOB = "PRESIDENT"
                MOVE .33 TO BONUS-RATE
           END-IF.

           IF  VP-RATE > 18
               MOVE .25 TO  VP-BONUS-RATE
           ELSE
               MOVE ZERO TO VP-BONUS-RATE
           END-IF.


       DISPLAY-DATA-PARAGRAPH.
      * Display President data.
           COMPUTE GROSS-PAY = (HOURS * RATE) * (1 + BONUS-RATE).
           DISPLAY "Name: " NAME.
           DISPLAY "Job: " JOB.
           DISPLAY "Address: " ADDR.
           DISPLAY "Today's Date: " DATE-WS.
           DISPLAY "Hours Worked: " HOURS.
           DISPLAY "Hourly Rate: " RATE.
           DISPLAY "Bonus-Rate: " BONUS-RATE.
           DISPLAY "Gross Pay: " GROSS-PAY.
           DISPLAY NAME  " "  ADDR.

           DISPLAY "    ".

      * Display Vice President data.
           COMPUTE VP-GROSS-PAY =
              (VP-HOURS * VP-RATE) * (1 + VP-BONUS-RATE).
           DISPLAY "Name: " VP-NAME.
           DISPLAY "Job: " VP-JOB.
           DISPLAY "Address: " ADDR.
           DISPLAY "Today's Date: " DATE-WS.
           DISPLAY "Hours Worked: " VP-HOURS.
           DISPLAY "Hourly Rate: " VP-RATE.
           DISPLAY "Bonus-Rate: " VP-BONUS-RATE.
           DISPLAY "Gross Pay: " VP-GROSS-PAY.
           DISPLAY VP-NAME  " "  ADDR.
