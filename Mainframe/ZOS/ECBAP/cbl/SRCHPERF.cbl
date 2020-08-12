       IDENTIFICATION DIVISION.
       PROGRAM-ID. SRCHPERF.
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
           05 FILLER                    PIC X(61).
       WORKING-STORAGE SECTION.
       77  PROJECT-INDEX     PIC S9(4) COMP.
       77  TABLE-MAX         PIC S9(4) COMP VALUE 20.
       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.
       01  EMP-PROJECT-TABLE.
           05 EMP-PROJECT-ITEM OCCURS 20 TIMES.
                10 EMP-PROJECT            PIC X(4).
                10 EMP-NAME               PIC X(15).
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


