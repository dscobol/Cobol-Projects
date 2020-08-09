      ***********************************************************
      * Program name:    TABLE3
      * Original author: dastagg
      *
      * Description: Program to test loading and processing tables.
      *
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  --------------------------------
      * 2020-08-01 dastagg       Created for COBOL class
      * 2020-08-09 dastagg       This version will focus on SEARCH
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TABLE3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-Function-Storage.
           12 Max-Salary          PIC 999999.99+.
           12 I-Ord-Max           PIC 999999.99+.
           12 Avg-Salary          PIC 999999.99+.
           12 Salary-Range        PIC 999999.99+.
           12 Total-Payroll       PIC 999999.99+.


       01 WS-ST-HOLD.
           12 FILLER PIC X(25) VALUE
              'ST-A1ST-A2ST-A3ST-A4ST-A5'.

       01  WS-Simple-Table-Storage.
           12 WS-ST-SUB                       PIC 9 VALUE 0.
           12 WS-ST-Table-Setup.
              15 WS-ST-Table OCCURS 5 TIMES.
                18 WS-ST-A                    PIC X(05).

       01 WS-STN-HOLD.
           12 FILLER PIC 9(5) VALUE 00010.
           12 FILLER PIC 9(5) VALUE 00020.
           12 FILLER PIC 9(5) VALUE 00030.
           12 FILLER PIC 9(5) VALUE 00040.
           12 FILLER PIC 9(5) VALUE 00050.


       01  WS-Simple-Num-Table-Storage.
           12 WS-STN-SUB                       PIC 9 VALUE 0.
           12 WS-STN-Table-Setup.
              15 WS-STN-Table OCCURS 5 TIMES.
                18 WS-STN-A                    PIC 9(05).

       01  WS-SNF-Table-Storage.
           12 WS-STF-SUB                       PIC 9 VALUE 0.
       01  WS-STF-A-TAB.
           12 WS-STF-A OCCURS 5 TIMES          PIC 9(05).


       01 WS-DT-HOLD.
           12 WS-HOLD-DT-1                        PIC X(30) VALUE
              'DT-A1DTL11DTL12DTL13DTL14DTL15'.
           12 WS-HOLD-DT-2                        PIC X(30) VALUE
              'DT-A2DTL21DTL22DTL23DTL24DTL25'.
           12 WS-HOLD-DT-3                        PIC X(30) VALUE
              'DT-A3DTL31DTL32DTL33DTL34DTL35'.
           12 WS-HOLD-DT-4                        PIC X(30) VALUE
              'DT-A4DTL41DTL42DTL43DTL44DTL45'.
           12 WS-HOLD-DT-5                        PIC X(30) VALUE
              'DT-A5DTL51DTL52DTL53DTL54DTL55'.

       01  WS-Two-Dim-Table-Storage.
           12 WS-DT-SUB1                       PIC 9 VALUE 0.
           12 WS-DT-SUB2                       PIC 9 VALUE 0.
           12 WS-DT-Table-Setup.
              15 WS-DT-Table OCCURS 5 TIMES.
                 18 WS-DT-A                      PIC X(05).
                 18 WS-DT-L2 OCCURS 5 TIMES.
                    21 WS-DT-L2-B                PIC X(05).

       01 WS-DTN-HOLD.
           12 WS-HOLD-DT-1                        PIC X(30) VALUE
              'DT-A10100004000000000000000000'.
           12 WS-HOLD-DT-2                        PIC X(30) VALUE
              'DT-A20120004000000000000000000'.
           12 WS-HOLD-DT-3                        PIC X(30) VALUE
              'DT-A30150003000000000000000000'.
           12 WS-HOLD-DT-4                        PIC X(30) VALUE
              'DT-A40100002000000000000000000'.
           12 WS-HOLD-DT-5                        PIC X(30) VALUE
              'DT-A50120002000000000000000000'.

       01  WS-Two-Dim-Num-Table-Storage.
           12 WS-DTN-SUB1                       PIC 9 VALUE 0.
           12 WS-DTN-SUB2                       PIC 9 VALUE 0.
           12 WS-DTN-Table-Setup.
              15 WS-DTN-Table OCCURS 5 TIMES.
                 18 WS-DTN-A                      PIC X(05).
                 18 WS-DTN-L2 OCCURS 5 TIMES.
                    21 WS-DTN-L2-B                PIC S999v99.

       01 WS-DTHN-HOLD.
           12 WS-HOLD-DT-1                        PIC X(30) VALUE
              'DT-H10010000100001000010000000'.
           12 WS-HOLD-DT-2                        PIC X(30) VALUE
              'DT-H20020000000003000010000000'.
           12 WS-HOLD-DT-3                        PIC X(30) VALUE
              'DT-H30050000500005000050000000'.
           12 WS-HOLD-DT-4                        PIC X(30) VALUE
              'DT-H40025000210002300027000000'.
           12 WS-HOLD-DT-5                        PIC X(30) VALUE
              'DT-H50011000120001300014000000'.

       01  WS-Two-Dim-HNum-Table-Storage.
           12 WS-DTHN-SUB1                       PIC 9 VALUE 0.
           12 WS-DTHN-SUB2                       PIC 9 VALUE 0.
           12 WS-DTHN-Table-Setup.
              15 WS-DTHN-Table OCCURS 5 TIMES.
                 18 WS-DTHN-A                      PIC X(05).
                 18 WS-DTHN-L2 OCCURS 5 TIMES.
                    21 WS-DTHN-L2-B                PIC S99999.

       01 WS-3D-HOLD.
           12 WS-HOLD-3D-1.
      *    First value is Division
              15 FILLER PIC X(5) VALUE '3D-A1'.
      *    Second value is Region
              15 FILLER PIC X(5) VALUE 'A1LB1'.
      *    "Sales" for each Region
              15 FILLER PIC X(25) VALUE
                 '0010000100001000010000000'.
              15 FILLER PIC X(5) VALUE 'A1LB2'.
              15 FILLER PIC X(25) VALUE
                 '0020000200002000020000000'.
              15 FILLER PIC X(5) VALUE 'A1LB3'.
              15 FILLER PIC X(25) VALUE
                 '0030000300003000030000000'.
              15 FILLER PIC X(5) VALUE 'A1LB4'.
              15 FILLER PIC X(25) VALUE
                 '0040000400004000040000000'.
              15 FILLER PIC X(5) VALUE 'A1LB5'.
              15 FILLER PIC X(25) VALUE
                 '0050000500005000050000000'.
           12 WS-HOLD-3D-2.
              15 FILLER PIC X(5) VALUE '3D-A2'.
              15 FILLER PIC X(5) VALUE 'A2LB1'.
              15 FILLER PIC X(25) VALUE
                 '0010000100001000010000000'.
              15 FILLER PIC X(5) VALUE 'A2LB2'.
              15 FILLER PIC X(25) VALUE
                 '0020000200002000020000000'.
              15 FILLER PIC X(5) VALUE 'A2LB3'.
              15 FILLER PIC X(25) VALUE
                 '0030000300003000030000000'.
              15 FILLER PIC X(5) VALUE 'A2LB4'.
              15 FILLER PIC X(25) VALUE
                 '0040000400004000040000000'.
              15 FILLER PIC X(5) VALUE 'A2LB5'.
              15 FILLER PIC X(25) VALUE
                 '0050000500005000050000000'.
           12 WS-HOLD-3D-3.
              15 FILLER PIC X(5) VALUE '3D-A3'.
              15 FILLER PIC X(5) VALUE 'A3LB1'.
              15 FILLER PIC X(25) VALUE
                 '0010000100001000010000000'.
              15 FILLER PIC X(5) VALUE 'A3LB2'.
              15 FILLER PIC X(25) VALUE
                 '0020000200002000020000000'.
              15 FILLER PIC X(5) VALUE 'A3LB3'.
              15 FILLER PIC X(25) VALUE
                 '0030000300003000030000000'.
              15 FILLER PIC X(5) VALUE 'A3LB4'.
              15 FILLER PIC X(25) VALUE
                 '0040000400004000040000000'.
              15 FILLER PIC X(5) VALUE 'A3LB5'.
              15 FILLER PIC X(25) VALUE
                 '0050000500005000050000000'.
           12 WS-HOLD-3D-4.
              15 FILLER PIC X(5) VALUE '3D-A4'.
              15 FILLER PIC X(5) VALUE 'A4LB1'.
              15 FILLER PIC X(25) VALUE
                 '0010000100001000010000000'.
              15 FILLER PIC X(5) VALUE 'A4LB2'.
              15 FILLER PIC X(25) VALUE
                 '0020000200002000020000000'.
              15 FILLER PIC X(5) VALUE 'A4LB3'.
              15 FILLER PIC X(25) VALUE
                 '0030000300003000030000000'.
              15 FILLER PIC X(5) VALUE 'A4LB4'.
              15 FILLER PIC X(25) VALUE
                 '0040000400004000040000000'.
              15 FILLER PIC X(5) VALUE 'A4LB5'.
              15 FILLER PIC X(25) VALUE
                 '0050000500005000050000000'.
           12 WS-HOLD-3D-5.
              15 FILLER PIC X(5) VALUE '3D-A5'.
              15 FILLER PIC X(5) VALUE 'A5LB1'.
              15 FILLER PIC X(25) VALUE
                 '0010000100001000010000000'.
              15 FILLER PIC X(5) VALUE 'A5LB2'.
              15 FILLER PIC X(25) VALUE
                 '0020000200002000020000000'.
              15 FILLER PIC X(5) VALUE 'A5LB3'.
              15 FILLER PIC X(25) VALUE
                 '0030000300003000030000000'.
              15 FILLER PIC X(5) VALUE 'A5LB4'.
              15 FILLER PIC X(25) VALUE
                 '0040000400004000040000000'.
              15 FILLER PIC X(5) VALUE 'A5LB5'.
              15 FILLER PIC X(25) VALUE
                 '0050000500005000050000000'.


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

       01  WS-3D-Index-Table-Storage.
           12 WS-3D-Table-Setup.
      *    Division
              15 WS-3D-Table OCCURS 5 TIMES INDEXED BY WS-3D1-IDX.
                 18 WS-3D-A                      PIC X(05).
      *    Region
                 18 WS-3D-L2 OCCURS 5 TIMES INDEXED BY WS-3D2-IDX.
                    21 WS-3D-L2-B                PIC X(05).
      *    4 numbers for each Region, the 5th will be calculated.
                    21 WS-3D-L3 OCCURS 5 TIMES INDEXED BY WS-3D3-IDX.
                       24 WS-3D-L3-C             PIC 9(05).
       01  WS-Company-Storage.
           12 WS-Region-Total                    PIC 9(8) VALUE 0.
           12 WS-Division-Total                  PIC 9(8) VALUE 0.
           12 WS-Company-Total                   PIC 9(8) VALUE 0.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           PERFORM 1100-Load-Other-Tables.

       1100-Load-Other-Tables.
           MOVE WS-ST-HOLD TO WS-ST-Table-Setup.
           MOVE WS-DT-HOLD TO WS-DT-Table-Setup.
           MOVE WS-TT-HOLD TO WS-TT-Table-Setup.
           MOVE WS-3D-HOLD TO WS-3D-Table-Setup.
           MOVE WS-STN-HOLD TO WS-STN-Table-Setup.
           MOVE WS-STN-HOLD TO WS-STF-A-TAB.
           MOVE WS-DTN-HOLD TO WS-DTN-Table-Setup.
           MOVE WS-DTHN-HOLD TO WS-DTHN-Table-Setup.

       2000-Process.
      *    Now, let's play with SEARCHing Tables.

           DISPLAY SPACES.
           DISPLAY SPACES.
           DISPLAY SPACES.




       3000-End-Job.
           DISPLAY "3000-EOJ: ".
           DISPLAY "Normally, I would have something to do here".

