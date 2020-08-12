       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPPAY.
      ***** This is an unbelievably simple COBOL program
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  REC-COUNTER              PIC 9(1).
       01  EMP-RECORD.
           05  EMP-NAME.
                10 EMP-FNAME        PIC X(15) VALUE 'FRANCISCO'.
                10 EMP-LNAME        PIC X(15).
           05  EMP-HOURLY-RATE      PIC 9(3)V99.
           05  EMP-OT-RATE          PIC V99.
           05  EMP-HOURS            PIC 9(3).
           05  EMP-PAY              PIC 9(7)V99.
       PROCEDURE DIVISION.
           MOVE "Millard"           TO EMP-FNAME.
           MOVE "Fillmore"          TO EMP-LNAME.
           MOVE 19                  TO EMP-HOURS.
           MOVE 23.50               TO EMP-HOURLY-RATE.
           IF  EMP-HOURS > 18
               MOVE .25 TO  EMP-OT-RATE
           ELSE
               MOVE ZERO TO EMP-OT-RATE.
           COMPUTE EMP-PAY =
                (EMP-HOURS * EMP-HOURLY-RATE) * (1 + EMP-OT-RATE).
           DISPLAY "Name: " EMP-NAME.
           DISPLAY "Hours Worked: " EMP-HOURS.
           DISPLAY "Hourly Rate: " EMP-HOURLY-RATE.
           DISPLAY "Bonus-Rate: " EMP-OT-RATE.
           DISPLAY "Gross Pay: " EMP-PAY.
           DISPLAY "Hi Chris - how's Loretta today?".
           GOBACK.
