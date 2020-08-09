      ***********************************************************
      * Program name:    TABLE1
      * Original author: dastagg
      *
      * Description: Program to test loading and processing tables.
      *
      * WARNINGS:
      * RETURN-CODE = 0009
      *           This program loads an external dataset into a WS
      *           table. If the table is not large enough, the pgm
      *           will end with RETURN-CODE = 0009.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  --------------------------------
      * 2020-08-01 dastagg       Created for COBOL class
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TABLE1.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TABLOAD
      *     ASSIGN TO TABLOAD
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/hinstype.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-TL-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  TABLOAD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TL-REC                           PIC X(003).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==TL==.

       01  WS-FILE-Counters.
           12 WS-TL-Records-Read              PIC S9(4) COMP.

       01 WS-ST-HOLD.
           12 FILLER PIC X(25) VALUE 
              'ST-A1ST-A2ST-A3ST-A4ST-A5'. 

       01  WS-Simple-Table-Storage.
           12 WS-ST-SUB                       PIC 9 VALUE 0.
           12 WS-ST-Table-Setup.      
              15 WS-ST-Table OCCURS 5 TIMES.
                18 WS-ST-A                    PIC X(05).

       01 WS-STN-HOLD.
           12 FILLER PIC 9(25) VALUE 
              '0000100002000030000400005'. 

       01  WS-Simple-Num-Table-Storage.
           12 WS-STN-SUB                       PIC 9 VALUE 0.
           12 WS-STN-Table-Setup.      
              15 WS-STN-Table OCCURS 5 TIMES.
                18 WS-STN-A                    PIC 9(05).


       01 WS-DT-HOLD.
           12 WS-HOLD-DT-1                        PIC X(30) VALUES
              'DT-A1DTL11DTL12DTL13DTL14DTL15'.
           12 WS-HOLD-DT-2                        PIC X(30) VALUES
              'DT-A2DTL21DTL22DTL23DTL24DTL25'.
           12 WS-HOLD-DT-3                        PIC X(30) VALUES
              'DT-A3DTL31DTL32DTL33DTL34DTL35'.
           12 WS-HOLD-DT-4                        PIC X(30) VALUES
              'DT-A4DTL41DTL42DTL43DTL44DTL45'.           
           12 WS-HOLD-DT-5                        PIC X(30) VALUES
              'DT-A5DTL51DTL52DTL53DTL54DTL55'.

       01  WS-Two-Dim-Table-Storage.
           12 WS-DT-SUB1                       PIC 9 VALUE 0.
           12 WS-DT-SUB2                       PIC 9 VALUE 0.
           12 WS-DT-Table-Setup.      
              15 WS-DT-Table OCCURS 5 TIMES.
                 18 WS-DT-A                      PIC X(05).
                 18 WS-DT-L2 OCCURS 5 TIMES. 
                    21 WS-DT-L2-B                PIC X(05).

       01 WS-TT-HOLD.
           12 WS-HOLD-TT-1.
              15 FILLER PIC X(5) VALUE 'TT-A1'.
              15 FILLER PIC X(30) VALUE
                 'A1LB1B1LC1B1LC2B1LC3B1LC4B1LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A1LB2B2LC1B2LC2B2LC3B2LC4B2LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A1LB3B3LC1B3LC2B3LC3B3LC4B3LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A1LB4B4LC1B4LC2B4LC3B4LC4B4LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A1LB5B5LC1B5LC2B5LC3B5LC4B5LC5'.
           12 WS-HOLD-TT-2.
              15 FILLER PIC X(5) VALUE 'TT-A2'.
              15 FILLER PIC X(30) VALUE
                 'A2LB1B1LC1B1LC2B1LC3B1LC4B1LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A2LB2B2LC1B2LC2B2LC3B2LC4B2LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A2LB3B3LC1B3LC2B3LC3B3LC4B3LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A2LB4B4LC1B4LC2B4LC3B4LC4B4LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A2LB5B5LC1B5LC2B5LC3B5LC4B5LC5'.
           12 WS-HOLD-TT-3.
              15 FILLER PIC X(5) VALUE 'TT-A3'.
              15 FILLER PIC X(30) VALUE
                 'A3LB1B1LC1B1LC2B1LC3B1LC4B1LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A3LB2B2LC1B2LC2B2LC3B2LC4B2LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A3LB3B3LC1B3LC2B3LC3B3LC4B3LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A3LB4B4LC1B4LC2B4LC3B4LC4B4LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A3LB5B5LC1B5LC2B5LC3B5LC4B5LC5'.
           12 WS-HOLD-TT-4.
              15 FILLER PIC X(5) VALUE 'TT-A4'.
              15 FILLER PIC X(30) VALUE
                 'A4LB1B1LC1B1LC2B1LC3B1LC4B1LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A4LB2B2LC1B2LC2B2LC3B2LC4B2LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A4LB3B3LC1B3LC2B3LC3B3LC4B3LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A4LB4B4LC1B4LC2B4LC3B4LC4B4LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A4LB5B5LC1B5LC2B5LC3B5LC4B5LC5'.
           12 WS-HOLD-TT-5.
              15 FILLER PIC X(5) VALUE 'TT-A5'.
              15 FILLER PIC X(30) VALUE
                 'A5LB1B1LC1B1LC2B1LC3B1LC4B1LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A5LB2B2LC1B2LC2B2LC3B2LC4B2LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A5LB3B3LC1B3LC2B3LC3B3LC4B3LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A5LB4B4LC1B4LC2B4LC3B4LC4B4LC5'.
              15 FILLER PIC X(30) VALUE 
                 'A5LB5B5LC1B5LC2B5LC3B5LC4B5LC5'.

       01  WS-Three-Dim-Table-Storage.
           12 WS-TT-SUB1                       PIC 9 VALUE 0.
           12 WS-TT-SUB2                       PIC 9 VALUE 0.
           12 WS-TT-SUB3                       PIC 9 VALUE 0.
           12 WS-TT-Table-Setup.      
              15 WS-TT-Table OCCURS 5 TIMES.
                 18 WS-TT-A                      PIC X(05).
                 18 WS-TT-L2 OCCURS 5 TIMES. 
                    21 WS-TT-L2-B                PIC X(05).
                    21 WS-TT-L3 OCCURS 5 TIMES.
                       24 WS-TT-L3-C             PIC X(05).

       01  WS-Type-Table-Storage.
           12 WS-Type-Max-Element-Counter     PIC S9(4) COMP VALUE 10.
           12 WS-Type-Occurs-Dep-Counter      PIC S99 COMP-3.
           12 WS-Type-Table OCCURS 0 TO 10 TIMES
              DEPENDING ON WS-Type-Occurs-Dep-Counter
              INDEXED BY WS-Type-IDX.
              15 WS-Type     PIC X(3).
                88  HMO         VALUE 'HMO'.
                88  I-PRIVATE   VALUE 'PRI'.
                88  PPO         VALUE 'PPO'.
                88  AFFORDABLE  VALUE 'AFF'.
                88  MEDICARE    VALUE 'MED'.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           PERFORM 1010-Load-Type-Table.
           PERFORM 1100-Load-Other-Tables.

       1010-Load-Type-Table.
           OPEN INPUT TABLOAD.
           SET WS-Type-IDX TO +1.
           PERFORM 1015-Load-Type Until WS-TL-EOF.
           CLOSE TABLOAD.
           PERFORM 1099-Verify-Type-Table.

       1015-Load-Type.
           READ TABLOAD
              AT END SET WS-TL-EOF TO TRUE
           END-READ.
           IF WS-TL-Good
              ADD +1 TO
                 WS-TL-Records-Read
                 WS-Type-Occurs-Dep-Counter
             MOVE TL-REC TO WS-Type(WS-Type-IDX)
             SET WS-Type-IDX UP BY +1 
           ELSE
              IF WS-TL-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 1015-TABLOAD"
                 DISPLAY "Read TABLOAD Failed."
                 DISPLAY "File Status: " WS-TL-Status
                 MOVE +8 TO RETURN-CODE
                 GOBACK
              END-IF
           END-IF.

       1099-Verify-Type-Table.
           DISPLAY "WS-Type-Table: "
           PERFORM VARYING WS-Type-IDX FROM 1 BY 1 
              UNTIL WS-Type-IDX > WS-Type-Occurs-Dep-Counter
              DISPLAY WS-Type(WS-Type-IDX)
           END-PERFORM.
           IF WS-Type-Occurs-Dep-Counter >
              WS-Type-Max-Element-Counter
                 DISPLAY "** ERROR **: 1099-Verify-Type-Table"
                 DISPLAY "WS table size is too small for file."
                 DISPLAY "Increase WS-Type-Table-Storage variables."
                 MOVE +9 TO RETURN-CODE
                 GOBACK
           END-IF.


       1100-Load-Other-Tables.
           MOVE WS-ST-HOLD TO WS-ST-Table-Setup.
           MOVE WS-DT-HOLD TO WS-DT-Table-Setup.
           MOVE WS-TT-HOLD TO WS-TT-Table-Setup.
           MOVE WS-STN-HOLD TO WS-STN-Table-Setup.

       2000-Process.
           DISPLAY "2000-Process: ".
           DISPLAY "This is the Simple Table:"
           DISPLAY "Using numbers:"
           DISPLAY "WS-ST-Table - 1: " WS-ST-A(1). 
           DISPLAY "WS-ST-Table - 2: " WS-ST-A(2). 
           DISPLAY "WS-ST-Table - 3: " WS-ST-A(3). 
           DISPLAY "WS-ST-Table - 4: " WS-ST-A(4). 
           DISPLAY "WS-ST-Table - 5: " WS-ST-A(5).
           DISPLAY SPACES.

           DISPLAY "This is the Simple Table:"
           DISPLAY "Subscript Addition:"
           MOVE 1 TO WS-ST-SUB.
           PERFORM 5 TIMES
              DISPLAY "WS-ST-Table Entry: " WS-ST-A(WS-ST-SUB)
              ADD 1 TO WS-ST-SUB 
           END-PERFORM.
           DISPLAY SPACES.

           DISPLAY "This is the Simple Table:"
           DISPLAY "Subscript P-Varying:"
           PERFORM VARYING WS-ST-SUB FROM 1 BY 1
              UNTIL WS-ST-SUB > 5 
              DISPLAY "WS-ST-Table Entry: " WS-ST-A(WS-ST-SUB) 
           END-PERFORM.
           DISPLAY SPACES.

           DISPLAY "This is the Two Dim Table:"
           DISPLAY "Using numbers:"
           DISPLAY "WS-DT-Table 1-Level 1: " 
              WS-DT-A(1) "-" WS-DT-L2(1, 1). 
           DISPLAY "WS-DT-Table 1-Level 2: " 
              WS-DT-A(1) "-" WS-DT-L2(1, 2). 
           DISPLAY "WS-DT-Table 1-Level 3: " 
              WS-DT-A(1) "-" WS-DT-L2(1, 3). 
           DISPLAY "WS-DT-Table 1-Level 4: " 
              WS-DT-A(1) "-" WS-DT-L2(1, 4). 
           DISPLAY "WS-DT-Table 1-Level 5: " 
              WS-DT-A(1) "-" WS-DT-L2(1, 5).
           DISPLAY SPACES.
           DISPLAY "WS-DT-Table 2-Level 1: " 
              WS-DT-A(2) "-" WS-DT-L2(2, 1). 
           DISPLAY "WS-DT-Table 2-Level 2: " 
              WS-DT-A(2) "-" WS-DT-L2(2, 2). 
           DISPLAY "WS-DT-Table 2-Level 3: " 
              WS-DT-A(2) "-" WS-DT-L2(2, 3). 
           DISPLAY "WS-DT-Table 2-Level 4: " 
              WS-DT-A(2) "-" WS-DT-L2(2, 4). 
           DISPLAY "WS-DT-Table 2-Level 5: " 
              WS-DT-A(2) "-" WS-DT-L2(2, 5).
           DISPLAY SPACES.
           DISPLAY "WS-DT-Table 3-Level 1: " 
              WS-DT-A(3) "-" WS-DT-L2(3, 1). 
           DISPLAY "WS-DT-Table 3-Level 2: " 
              WS-DT-A(3) "-" WS-DT-L2(3, 2). 
           DISPLAY "WS-DT-Table 3-Level 3: " 
              WS-DT-A(3) "-" WS-DT-L2(3, 3). 
           DISPLAY "WS-DT-Table 3-Level 4: " 
              WS-DT-A(3) "-" WS-DT-L2(3, 4). 
           DISPLAY "WS-DT-Table 3-Level 5: " 
              WS-DT-A(3) "-" WS-DT-L2(3, 5).
           DISPLAY SPACES.
           DISPLAY "WS-DT-Table 4-Level 1: " 
              WS-DT-A(4) "-" WS-DT-L2(4, 1). 
           DISPLAY "WS-DT-Table 4-Level 2: " 
              WS-DT-A(4) "-" WS-DT-L2(4, 2). 
           DISPLAY "WS-DT-Table 4-Level 3: " 
              WS-DT-A(4) "-" WS-DT-L2(4, 3). 
           DISPLAY "WS-DT-Table 4-Level 4: " 
              WS-DT-A(4) "-" WS-DT-L2(4, 4). 
           DISPLAY "WS-DT-Table 4-Level 5: " 
              WS-DT-A(4) "-" WS-DT-L2(4, 5).
           DISPLAY SPACES.
           DISPLAY "WS-DT-Table 5-Level 1: " 
              WS-DT-A(5) "-" WS-DT-L2(5, 1). 
           DISPLAY "WS-DT-Table 5-Level 2: " 
              WS-DT-A(5) "-" WS-DT-L2(5, 2). 
           DISPLAY "WS-DT-Table 5-Level 3: " 
              WS-DT-A(5) "-" WS-DT-L2(5, 3). 
           DISPLAY "WS-DT-Table 5-Level 4: " 
              WS-DT-A(5) "-" WS-DT-L2(5, 4). 
           DISPLAY "WS-DT-Table 5-Level 5: " 
              WS-DT-A(5) "-" WS-DT-L2(5, 5).
           DISPLAY SPACES.

           DISPLAY "This is the Two Dim Table:"
           DISPLAY "Subscript Addition:"
           MOVE 1 TO WS-DT-SUB1.
           PERFORM 5 TIMES
              DISPLAY "WS-DT-Table Entry: " 
                 WS-DT-A(WS-DT-SUB1) 
              MOVE 1 TO WS-DT-SUB2
              PERFORM 5 TIMES
                 DISPLAY "      WS-DT-SUB Entry: " 
                    WS-DT-L2(WS-DT-SUB1, WS-DT-SUB2)
                    ADD 1 TO WS-DT-SUB2
              END-PERFORM
              ADD 1 TO WS-DT-SUB1
           END-PERFORM.
           DISPLAY SPACES.

           DISPLAY "This is the Two Dim Table:"
           DISPLAY "Subscript P-Varying:"
           PERFORM VARYING WS-DT-SUB1 FROM 1 BY 1
              UNTIL WS-DT-SUB1 > 5
              DISPLAY "WS-DT-Table Entry: " 
                 WS-DT-A(WS-DT-SUB1) 
              PERFORM VARYING WS-DT-SUB2 FROM 1 BY 1
                 UNTIL WS-DT-SUB2 > 5 
                 DISPLAY "      WS-DT-SUB Entry: " 
                    WS-DT-L2(WS-DT-SUB1, WS-DT-SUB2)
              END-PERFORM 
           END-PERFORM.
           DISPLAY SPACES.

           DISPLAY "This is the Three Dim Table:"
           DISPLAY "Using numbers:"
           DISPLAY "** Yeah, Not gonna type all that out.***" 
           DISPLAY SPACES.

           DISPLAY "This is the Three Dim Table:"
           DISPLAY "Subscript Addition:"
           MOVE 1 TO WS-TT-SUB1.
           PERFORM 5 TIMES
              DISPLAY "WS-TT-Table Entry: " 
                 WS-TT-A(WS-TT-SUB1) 
              MOVE 1 TO WS-TT-SUB2
              PERFORM 5 TIMES
                 DISPLAY "      WS-TT-SUB Entry: " 
                    WS-TT-L2-B(WS-TT-SUB1, WS-TT-SUB2)
                 MOVE 1 TO WS-TT-SUB3
                 PERFORM 5 TIMES
                    DISPLAY "           WS-TT-SUB-SUB Entry: " 
                       WS-TT-L3-C(WS-TT-SUB1, WS-TT-SUB2, WS-TT-SUB3)
                    ADD 1 TO WS-TT-SUB3
                 END-PERFORM
                 ADD 1 TO WS-TT-SUB2
              END-PERFORM
              ADD 1 TO WS-TT-SUB1
           END-PERFORM.
           DISPLAY SPACES.

           DISPLAY "This is the Three Dim Table:"
           DISPLAY "Subscript P-Varying:"
           PERFORM VARYING WS-TT-SUB1 FROM 1 BY 1
              UNTIL WS-TT-SUB1 > 5
              DISPLAY "WS-TT-Table Entry: " 
                 WS-TT-A(WS-TT-SUB1)  
              PERFORM VARYING WS-TT-SUB2 FROM 1 BY 1
                 UNTIL WS-TT-SUB2 > 5 
                 DISPLAY "      WS-TT-SUB Entry: " 
                    WS-TT-L2-B(WS-TT-SUB1, WS-TT-SUB2)
                 PERFORM VARYING WS-TT-SUB3 FROM 1 BY 1
                    UNTIL WS-TT-SUB3 > 5 
                    DISPLAY "           WS-TT-SUB-SUB Entry: " 
                       WS-TT-L3-C(WS-TT-SUB1, WS-TT-SUB2, WS-TT-SUB3)
                 END-PERFORM 
              END-PERFORM 
           END-PERFORM.
           DISPLAY SPACES.

           DISPLAY "This is the Simple Number Table:"
           DISPLAY "Using numbers:"
           DISPLAY "WS-STN-Table - 1: " WS-STN-A(1). 
           DISPLAY "WS-STN-Table - 2: " WS-STN-A(2). 
           DISPLAY "WS-STN-Table - 3: " WS-STN-A(3). 
           DISPLAY "WS-STN-Table - 4: " WS-STN-A(4). 
           DISPLAY "WS-STN-Table - 5: " WS-STN-A(5).
           DISPLAY SPACES.

           DISPLAY "This is the Simple Number Table:"
           DISPLAY "Subscript Addition:"
           MOVE 1 TO WS-STN-SUB.
           PERFORM 5 TIMES
              DISPLAY "WS-STN-Table Entry: " WS-STN-A(WS-STN-SUB)
              ADD 1 TO WS-STN-SUB 
           END-PERFORM.
           DISPLAY SPACES.

           DISPLAY "This is the Simple Number Table:"
           DISPLAY "Subscript P-Varying:"
           PERFORM VARYING WS-STN-SUB FROM 1 BY 1
              UNTIL WS-STN-SUB > 5 
              DISPLAY "WS-STN-Table Entry: " WS-STN-A(WS-STN-SUB) 
           END-PERFORM.
           DISPLAY SPACES.


      * Now, let's play with functions
           DISPLAY " Max-Salary    = " Function Max(WS-STN-A(ALL))
           DISPLAY " I             = " Function Ord-Max(WS-STN-A(ALL))
           DISPLAY " Avg-Salary    = " Function Mean(WS-STN-A(ALL))
           DISPLAY " Salary-Range  = " Function Range(WS-STN-A(ALL))
           DISPLAY " Total-Payroll = " Function Sum(WS-STN-A(ALL))
           .

       3000-End-Job.
           DISPLAY "3000-EOJ: ".
           DISPLAY "Normally, I would have something to do here".











