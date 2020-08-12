       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVS.
      ***** This is an unbelievably simple COBOL program
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  REC-COUNTER              PIC 9(1).
       01  FAV-REC.
           05  ARTIST-NAME      PIC X(20).
           05  NUMBER-MUSICIANS PIC 9(02).
           05  GENRE                PIC X(12).
           05  EMP-HOURS            PIC 9(3).
           05  EMP-PAY              PIC 9(7)V99.
       PROCEDURE DIVISION.
           MOVE 'BLACK SABBATH' TO ARTIST-NAME.
           MOVE 4 TO NUMBER-MUSICIANS.
           MOVE 'HEAVY METAL' TO GENRE.
           MOVE 12 TO EMP-HOURS.
           MOVE 44444.99 TO EMP-PAY.
           GOBACK.
