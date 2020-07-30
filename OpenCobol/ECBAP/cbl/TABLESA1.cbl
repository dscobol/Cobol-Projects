       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO EMPROJ.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  EMP-PROJECT-TABLE-I.
           05 EMP-PROJECT-I                 PIC X(4).
           05 EMP-NAME-I                    PIC X(15).
           05 EMP-STATE-OFFICE-I            PIC X(02).
           05 EMP-PROJECT-POSITION-I        PIC X(20).
           05 EMP-NBR-DAYS-ON-PROJ-I        PIC 9(03).
           05 EMP-NBR-OT-HOURS-I            PIC 9(03).
           05 EMP-PER-DAY-BILLING-RATE-I    PIC 9(03)V99.
           05 EMP-PER-HOUR-OT-RATE-I        PIC 9(03)99.
           05 EMP-LANGUAGE-CERT-I           PIC X(20).
           05 EMP-ON-CALL-I                 PIC X(01).
           05 FILLER                        PIC X(02).
       WORKING-STORAGE SECTION.
       77  PROJECT-INDEX     PIC S9(4) COMP.
       77  TABLE-MAX         PIC S9(4) COMP VALUE 20.
       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE   VALUE 'Y'.
       01  EMP-PROJECT-TABLE.
           05 EMP-PROJECT-ITEM OCCURS 20 TIMES
                ASCENDING KEY IS EMP-NAME
                INDEXED BY PROJ-IDX.
                10 EMP-PROJECT               PIC X(4).
                10 EMP-NAME                  PIC X(15).
                10 EMP-STATE-OFFICE          PIC X(02).
                10 EMP-PROJECT-POSITION      PIC X(20).
                10 EMP-NBR-DAYS-ON-PROJ      PIC 9(03).
                10 EMP-NBR-OT-HOURS          PIC 9(03).
                10 EMP-PER-DAY-BILLING-RATE  PIC 9(03)V99.
                10 EMP-PER-HOUR-OT-RATE      PIC 9(03)99.
                10 EMP-LANGUAGE-CERT         PIC X(20).
                10 EMP-ON-CALL               PIC X(01).
                10 FILLER                    PIC X(02).
       77  SUM-1   PIC 9(18) VALUE 0.
       77  MAX-OUT  PIC 9(4).
       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-PROCESS-TABLE-DATA.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE EMP-PROJECT-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
           AT END MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
              UNTIL PROJECT-INDEX = TABLE-MAX *> Load Table
           OR END-OF-FILE
                MOVE EMP-PROJECT-I TO
                        EMP-PROJECT (PROJECT-INDEX)
                MOVE EMP-NAME-I TO
                        EMP-NAME (PROJECT-INDEX)
                MOVE EMP-STATE-OFFICE-I TO
                        EMP-STATE-OFFICE  (PROJECT-INDEX)
                MOVE EMP-PROJECT-POSITION-I TO
                        EMP-PROJECT-POSITION  (PROJECT-INDEX)
                MOVE EMP-NBR-DAYS-ON-PROJ-I TO
                        EMP-NBR-DAYS-ON-PROJ (PROJECT-INDEX)
                MOVE EMP-NBR-OT-HOURS-I  TO
                        EMP-NBR-OT-HOURS (PROJECT-INDEX)
                MOVE EMP-PER-DAY-BILLING-RATE-I TO
                        EMP-PER-DAY-BILLING-RATE (PROJECT-INDEX)
                MOVE EMP-PER-HOUR-OT-RATE-I  TO
                        EMP-PER-HOUR-OT-RATE (PROJECT-INDEX)
                MOVE EMP-LANGUAGE-CERT-I  TO
                        EMP-LANGUAGE-CERT (PROJECT-INDEX)
                MOVE EMP-ON-CALL-I   TO
                        EMP-ON-CALL (PROJECT-INDEX)
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
                DISPLAY EMP-PROJECT-ITEM(PROJECT-INDEX)
           END-PERFORM.
       100-PROCESS-TABLE-DATA.
           PERFORM 200-FIND-PROJECT.
           PERFORM 300-FIND-NC-OT-SKILL.
           PERFORM 400-TOTAL-PROJ-EXPENSE.

       200-FIND-PROJECT.
      ***  Display all of the Employee names working on project 'A111'
       300-FIND-NC-OT-SKILL.
      ***  Display all of the Employee names of Programmers in NC
      ***     who are allowed to bill for On-Call work
       400-TOTAL-PROJ-EXPENSE.
      ***  Calculate and Display the total cost for the 'A111' project
       500-TOTAL-ALL-PROJECTS-EXPENSE.
      ***  Calculate & Display the total cost for all of the projects
      **   Google the COBOL Intrinsic FUNCTION SUM(<field>(ALL))
       900-WRAP-UP.
           CLOSE INPUT-FILE.
