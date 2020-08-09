       ID DIVISION.
       PROGRAM-ID. DUMMYPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM-A PIC 9(3) VALUE 399.
       01 NUM-B PIC 9(3) VALUE 211.
       01 NUM-C PIC 9(3).
      *
       PROCEDURE DIVISION.
       MAIN.
           COMPUTE NUM-C = ((NUM-A / 100) - (NUM-B / 100)) * 100
           DISPLAY 'NUM-C IS ' NUM-C
           STOP RUN.
