       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO EMPROJ.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  EMP-PROJECT-TABLE-I.
           05 EMP-PROJECT-I             PIC X(4).
           05 EMP-NAME-I                PIC X(15).
           05 EMP-STATE-OFFICE-I          PIC X(02).
           05 EMP-PROJECT-POSITION-I      PIC X(20).
           05 EMP-NBR-DAYS-ON-PROJ-I      PIC 9(03).
           05 EMP-NBR-OT-HOURS-I          PIC 9(03).
           05 EMP-PER-DAY-BILLING-RATE-I  PIC 9(03)V99.
           05 EMP-PER-HOUR-OT-RATE-I      PIC 9(03)99.
           05 EMP-LANG-UAGE-CERT-I         PIC X(20).
           05 EMP-ON-CALL-I               PIC X(01).
           05 FILLER                    PIC X(02).
       WORKING-STORAGE SECTION.
       77  PROJECT-INDEX     PIC S9(4) COMP.
       77  TABLE-MAX         PIC S9(4) COMP VALUE 20.
       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.
       01  EMP-PROJECT-TABLE.
           05 EMP-PROJECT-ITEM OCCURS 20 TIMES.
                10 EMP-PROJECT            PIC X(4).
                10 EMP-NAME               PIC X(15).
                10 EMP1 REDEFINES EMP-NAME.
                    15 EMPNUM PIC 9(15).
           05 EMP-STATE-OFFICE          PIC X(02).
           05 EMP-PROJECT-POSITION      PIC X(20).
           05 EMP-NBR-DAYS-ON-PROJ      PIC 9(03).
           05 EMP-NBR-OT-HOURS          PIC 9(03).
           05 EMP-PER-DAY-BILLING-RATE  PIC 9(03)V99.
           05 EMP-PER-HOUR-OT-RATE      PIC 9(03)99.
           05 EMP-LANGUAGE-CERT         PIC X(20).
           05 EMP-ON-CALL               PIC X(01).
           05 FILLER                    PIC X(02).


       77  SUM-1   PIC 9(18) VALUE 0.
       PROCEDURE DIVISION.
           INITIALIZE EMP-PROJECT-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
           AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
              UNTIL PROJECT-INDEX = TABLE-MAX
           OR END-OF-FILE
                MOVE EMP-PROJECT-I TO  EMP-PROJECT (PROJECT-INDEX)
                MOVE EMP-NAME-I TO EMP-NAME (PROJECT-INDEX)
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
           END-PERFORM.

           GOBACK.
