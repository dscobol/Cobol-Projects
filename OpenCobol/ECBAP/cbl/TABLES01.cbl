       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES01.

      ***************************************************************
      * Program name:    TABLES01.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ------------------------------------
      * 2020-07-29 dastagg        Created for ECBAP class
      * 2020-07-29 dastagg        Updated to use SUM Function
      *
      ***************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
           ASSIGN TO "../../../common/data/ECBAP/emp1.proj.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
      *     ASSIGN TO DA-S-EMPROJ
      *     ORGANIZATION IS SEQUENTIAL.

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
           05 EMP-PER-HOUR-OT-RATE-I        PIC 9(03)V99.
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
                10 EMP-PER-HOUR-OT-RATE      PIC 9(03)V99.
                10 EMP-LANGUAGE-CERT         PIC X(20).
                10 EMP-ON-CALL               PIC X(01).
                10 FILLER                    PIC X(02).
       77  SUM-1   PIC 9(16)V99 VALUE 0.
       77  MAX-OUT  PIC 9(4).
       01  WS-Emp-Cost-Counters.
           12 WS-Total-Cost                  PIC $$,$$$,$$$,$$9.99.
           12 WS-Emp-Per-Day-Cost            PIC 9(7)V99.
           12 WS-Emp-Overtime-Cost           PIC 9(7)V99.

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
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
           OR END-OF-FILE
                MOVE EMP-PROJECT-I TO
                        EMP-PROJECT (PROJ-IDX)
                MOVE EMP-NAME-I TO
                        EMP-NAME (PROJ-IDX)
                MOVE EMP-STATE-OFFICE-I TO
                        EMP-STATE-OFFICE  (PROJ-IDX)
                MOVE EMP-PROJECT-POSITION-I TO
                        EMP-PROJECT-POSITION  (PROJ-IDX)
                MOVE EMP-NBR-DAYS-ON-PROJ-I TO
                        EMP-NBR-DAYS-ON-PROJ (PROJ-IDX)
                MOVE EMP-NBR-OT-HOURS-I  TO
                        EMP-NBR-OT-HOURS (PROJ-IDX)
                MOVE EMP-PER-DAY-BILLING-RATE-I TO
                        EMP-PER-DAY-BILLING-RATE (PROJ-IDX)
                MOVE EMP-PER-HOUR-OT-RATE-I  TO
                        EMP-PER-HOUR-OT-RATE (PROJ-IDX)
                MOVE EMP-LANGUAGE-CERT-I  TO
                        EMP-LANGUAGE-CERT (PROJ-IDX)
                MOVE EMP-ON-CALL-I   TO
                        EMP-ON-CALL (PROJ-IDX)
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
                DISPLAY EMP-PROJECT-ITEM(PROJ-IDX)
           END-PERFORM.

       100-PROCESS-TABLE-DATA.
           PERFORM 200-FIND-PROJECT.
           PERFORM 300-FIND-NC-OT-SKILL.
           PERFORM 400-TOTAL-PROJ-EXPENSE.
           PERFORM 500-TOTAL-ALL-PROJECTS-EXPENSE.

       200-FIND-PROJECT.
      ***  Display all of the Employee names working on project 'A111'
           DISPLAY SPACES.
           DISPLAY "All employees working on Project: A111".
           DISPLAY SPACES.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
              IF EMP-PROJECT(PROJ-IDX) = "A111"
                 DISPLAY EMP-NAME(PROJ-IDX)
              END-IF
           END-PERFORM.

       300-FIND-NC-OT-SKILL.
      ***  Display all of the Employee names of Programmers in NC
      ***     who are allowed to bill for On-Call work
           DISPLAY SPACES.
           DISPLAY "All Programmers allowed to bill On-Call work".
           DISPLAY SPACES.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
              IF EMP-ON-CALL(PROJ-IDX) = "Y"
                 DISPLAY EMP-NAME(PROJ-IDX)
              END-IF
           END-PERFORM.

       400-TOTAL-PROJ-EXPENSE.
      ***  Calculate the total cost for the 'A111' project
           INITIALIZE WS-Emp-Cost-Counters.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
              IF EMP-PROJECT(PROJ-IDX) = "A111"
                 PERFORM 410-Calculate-Employee-Expense
                 COMPUTE SUM-1 =
                    FUNCTION SUM(SUM-1
                                 WS-Emp-Per-Day-Cost
                                 WS-Emp-Overtime-Cost
                             )
                 END-COMPUTE
                 MOVE SUM-1 TO WS-Total-Cost
              END-IF
           END-PERFORM.
           DISPLAY SPACES.
           DISPLAY "Total Cost of the Project: A111".
           DISPLAY SPACES.
           DISPLAY WS-Total-Cost.

       410-Calculate-Employee-Expense.
           IF EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) > ZERO
              COMPUTE WS-Emp-Per-Day-Cost =
                 EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) *
                 EMP-PER-DAY-BILLING-RATE(PROJ-IDX)
              END-COMPUTE
           ELSE
              MOVE ZEROES TO WS-Emp-Per-Day-Cost
           END-IF.
      *     DISPLAY "Day: " WS-Emp-Per-Day-Cost.

           IF EMP-NBR-OT-HOURS(PROJ-IDX) > ZERO
              COMPUTE WS-Emp-Overtime-Cost =
                 EMP-NBR-OT-HOURS(PROJ-IDX) *
                 EMP-PER-HOUR-OT-RATE(PROJ-IDX)
              END-COMPUTE
           ELSE
              MOVE ZEROES TO WS-Emp-Overtime-Cost
           END-IF.
      *     DISPLAY "OT: " WS-Emp-Overtime-Cost.

       500-TOTAL-ALL-PROJECTS-EXPENSE.
      ***  Calculate the total cost for all of the projects
      **   Google the COBOL Intrinsic FUNCTION SUM(<field>(ALL))
           INITIALIZE WS-Emp-Cost-Counters.
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
              PERFORM 410-Calculate-Employee-Expense
              COMPUTE SUM-1 =
                 FUNCTION SUM(SUM-1
                              WS-Emp-Per-Day-Cost
                              WS-Emp-Overtime-Cost
                          )
              END-COMPUTE
              MOVE SUM-1 TO WS-Total-Cost
           END-PERFORM.

           DISPLAY SPACES.
           DISPLAY "Total cost of all Projects".
           DISPLAY SPACES.
           DISPLAY WS-Total-Cost.

       900-WRAP-UP.
           CLOSE INPUT-FILE.
