       IDENTIFICATION DIVISION.
      * ******* EXAMPLES OF TRUNCTION, ROUNDING ERRORS AND OVERFLOW
      * ******* ALSO NEGATIVE NUMBERS
       PROGRAM-ID. TRUNCATE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Elementary items
       01  NUMERIC-VARIABLES.
           05   PIC-9-2             PIC 9(2).
           05   PIC-9-4             PIC 9(4).
           05   PIC-9-8             PIC 9(8).
           05   PIC-9-6-V99         PIC 9(6)V99.
           05   PIC-X-2             PIC X(2).
           05   PIC-X-6             PIC X(6).
           05   PIC-X-8             PIC X(8).

       PROCEDURE DIVISION.
      * Truncation tests.
           MOVE 99           TO PIC-9-2.
           ADD 1             TO PIC-9-2.
           MOVE 171          TO PIC-9-2.
           MOVE -171         TO PIC-9-2.
           MOVE PIC-9-2      TO PIC-9-4.
           MOVE 1000         TO PIC-9-4.
           COMPUTE PIC-9-4 = PIC-9-4 / 33.   *> SHOULD BE 30.303
           MOVE 999999.99    TO PIC-9-6-V99.
           ADD  1            TO PIC-9-6-V99.
           MOVE 1000         TO PIC-9-6-V99.
           COMPUTE PIC-9-6-V99 = PIC-9-6-V99 / 33.
           MOVE 1712         TO PIC-X-6.
           MOVE PIC-X-6      TO PIC-X-2.
           MOVE PIC-X-2      TO PIC-X-8.
           MOVE PIC-X-8      TO PIC-9-8.
           MOVE PIC-X-6      TO PIC-9-4.
           MOVE PIC-X-6      TO PIC-9-6-V99.
           COMPUTE PIC-9-6-V99 = PIC-9-6-V99 / 33.
           GOBACK.
