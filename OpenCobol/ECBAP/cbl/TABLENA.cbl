      ***********************************************************
      * Program name:    TABLENA
      * Original author: dastagg
      *
      * Description: Program to test loading and processing tables.
      *
      *    This is the "No ALL" version of table processing.
      *    Z COBOL functions have an ALL subscript property.
      *    gnuCobol functions do not, yet.
      *
      *    This program will process tables "the old-fashioned way".
      *    These methods will work on both gnuCobol and Z COBOL systems.
      *
      *    I've set up 1, 2 and 3 dimensional tables, with two types
      *    of each:
      *    1 with just alphanumeric data,
      *    1 with just numeric data or the first element alphanumeric,  
      *    the rest numeric to represent a load of data from an 
      *    outside source.
      *
      *    WS-1D-Table-Storage : WS-1D-HOLD
      *    WS-1DN-Table-Storage : WS-1DN-HOLD
      *    WS-2D-Table-Storage : WS-2D-HOLD
      *    WS-2DN-Table-Storage : WS-2DN-HOLD
      *    WS-3DS-Table-Storage : WS-3DS-HOLD
      *    WS-3DI-Table-Storage : WS-3DI-HOLD
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  --------------------------------
      * 2020-08-10 dastagg       Created for ECBAP class
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TABLENA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *    These two hold areas are "the same".
      *    The first is one line of data.
      *    The second is broken up for easier reading.
      *    And the first is alphanumeric, the second is numeric.
       01 WS-1D-HOLD.
           12 FILLER PIC X(25) VALUE
              'ST-A1ST-A2ST-A3ST-A4ST-A5'.

       01 WS-1DN-HOLD.
           12 FILLER PIC 9(5) VALUE 00010.
           12 FILLER PIC 9(5) VALUE 00040.
           12 FILLER PIC 9(5) VALUE 00050.
           12 FILLER PIC 9(5) VALUE 00020.
           12 FILLER PIC 9(5) VALUE 00030.

      *    Both these tables have a subscript defined for them.
      *    This table will load the values from WS-1D-HOLD.
      *    MUST keep WS-1D-Element-Cnt in sync with OCCURS #.
       01  WS-1D-Table-Storage.
           12 WS-1D-Element-Cnt               PIC 9 VALUE 5.
           12 WS-1D-SUB                       PIC 9 VALUE 0.
           12 WS-1D-Table-Setup.
              15 WS-1D-Table OCCURS 5 TIMES.
                18 WS-1D-A                    PIC X(05).

      *    This table will load the values from WS-1DN-HOLD.
      *    MUST keep WS-1DN-Element-Cnt in sync with OCCURS #.
       01  WS-1DN-Table-Storage.
           12 WS-1DN-Element-Cnt               PIC 9 VALUE 5.
           12 WS-1DN-SUB                       PIC 9 VALUE 0.
           12 WS-1DN-Table-Setup.
              15 WS-1DN-Table OCCURS 5 TIMES.
                18 WS-1DN-A                    PIC 9(05).

       01 WS-St-Crs-HOLD.
           12 FILLER        PIC X(20) VALUE 
              'ROBERT K KAHN       '.
           12 FILLER        PIC X(48) VALUE
              'ANTH101BCALC687ASOCS200CALGB12BAPHYS002AFLUT140C'.
           12 FILLER        PIC X(20) VALUE 
              'LISA CRUDUP         '.
           12 FILLER        PIC X(48) VALUE
              'BIOL201ATRIG551DSHAK213APSYC23ABBIOL002CDRUM310C'.
           12 FILLER        PIC X(20) VALUE 
              'RICHARD HILDEBRAND  '.
           12 FILLER        PIC X(48) VALUE
              'POLY555CGEOM231ARLIT560DBIOL13DBMECH002AACCO140B'.
           12 FILLER        PIC X(20) VALUE 
              'LORETTA PANTOLINE   '.
           12 FILLER        PIC X(48) VALUE
              'TUBA567ASTAT043CSHOP980ACHEM534HASTR002BVIOL610B'.
           12 FILLER        PIC X(20) VALUE 
              'SALLY HARRIS        '.
           12 FILLER        PIC X(48) VALUE
              'MEDC522CPIAN003BSPAN760AEART164BRUSS002BPIAN170A'.

       01 WS-2D-HOLD.
           12 WS-HOLD-2D-1                        PIC X(30) VALUE
              'DT-A1DTL11DTL12DTL13DTL14DTL15'.
           12 WS-HOLD-2D-2                        PIC X(30) VALUE
              'DT-A2DTL21DTL22DTL23DTL24DTL25'.
           12 WS-HOLD-2D-3                        PIC X(30) VALUE
              'DT-A3DTL31DTL32DTL33DTL34DTL35'.
           12 WS-HOLD-2D-4                        PIC X(30) VALUE
              'DT-A4DTL41DTL42DTL43DTL44DTL45'.
           12 WS-HOLD-2D-5                        PIC X(30) VALUE
              'DT-A5DTL51DTL52DTL53DTL54DTL55'.

       01 WS-2DN-HOLD.
           12 WS-HOLD-2DN-1                       PIC X(30) VALUE
              'DT-A10100004000000000000000000'.
           12 WS-HOLD-2DN-2                       PIC X(30) VALUE
              'DT-A20120004000000000000000000'.
           12 WS-HOLD-2DN-3                       PIC X(30) VALUE
              'DT-A30150003000000000000000000'.
           12 WS-HOLD-2DN-4                       PIC X(30) VALUE
              'DT-A40100002000000000000000000'.
           12 WS-HOLD-2DN-5                       PIC X(30) VALUE
              'DT-A50120002000000000000000000'.

      *    Both these tables have a subscript defined for them.
      *    This table will load the values from WS-2D-HOLD.
      *    MUST keep Element-Cnt in sync with OCCURS #.
       01  WS-2D-Table-Storage.
           12 WS-2D-Element1-Cnt               PIC 9 VALUE 5.
           12 WS-2D-Element2-Cnt               PIC 9 VALUE 5.
           12 WS-2D-SUB1                       PIC 9 VALUE 0.
           12 WS-2D-SUB2                       PIC 9 VALUE 0.
           12 WS-2D-Table-Setup.
              15 WS-2D-Table OCCURS 5 TIMES.
                 18 WS-2D-A                      PIC X(05).
                 18 WS-2D-L2 OCCURS 5 TIMES.
                    21 WS-2D-L2-B                PIC X(05).

      *    This table will load the values from WS-2DN-HOLD.
      *    MUST keep Element-Cnt in sync with OCCURS #.
       01  WS-2DN-Table-Storage.
           12 WS-2DN-Element1-Cnt               PIC 9 VALUE 5.
           12 WS-2DN-Element2-Cnt               PIC 9 VALUE 5.
           12 WS-2DN-SUB1                       PIC 9 VALUE 0.
           12 WS-2DN-SUB2                       PIC 9 VALUE 0.
           12 WS-2DN-Table-Setup.
              15 WS-2DN-Table OCCURS 5 TIMES.
                 18 WS-2DN-A                      PIC X(05).
                 18 WS-2DN-L2 OCCURS 5 TIMES.
                    21 WS-2DN-L2-B                PIC S999v99.

      *    This table will load the values from WS-St-Crs-HOLD.
      *    This table is simulate a table with Student Names and Grades.
      *    It will only use Indexes.
      *    MUST keep Element-Cnt in sync with OCCURS #.
       01  WS-Std-Crs-Table-Storage.
           12 WS-SC-Element1-Cnt               PIC 9 VALUE 5.
           12 WS-SC-Element2-Cnt               PIC 9 VALUE 6.
           12 WS-SC-Table-Setup.
              15 WS-SC-Student-Table 
                 OCCURS 5 TIMES 
                 INDEXED BY WS-SC-St-IDX.
                 18 WS-SC-Student-Name            PIC X(20).
                 18 WS-SC-Course-Table 
                    OCCURS 6 TIMES 
                    INDEXED BY WS-SC-Crs-IDX.
                    21 WS-SC-Course-Name          PIC X(07).
                    21 WS-SC-Course-Grade         PIC X(01).

       01 WS-3DS-HOLD.
           12 WS-HOLD-3DS-1.
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
           12 WS-HOLD-3DS-2.
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
           12 WS-HOLD-3DS-3.
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
           12 WS-HOLD-3DS-4.
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
           12 WS-HOLD-3DS-5.
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

       01 WS-3DI-HOLD.
           12 WS-HOLD-3DI-1.
      *    First value is Division
              15 FILLER PIC X(5) VALUE '3D-A1'.
      *    Second value is Region
              15 FILLER PIC X(5) VALUE 'A1LB1'.
      *    "Sales" for each Region
              15 FILLER PIC X(20) VALUE
                 '00100001000010000100'.
              15 FILLER PIC X(5) VALUE 'A1LB2'.
              15 FILLER PIC X(20) VALUE
                 '00200002000020000200'.
              15 FILLER PIC X(5) VALUE 'A1LB3'.
              15 FILLER PIC X(20) VALUE
                 '00300003000030000300'.
              15 FILLER PIC X(5) VALUE 'A1LB4'.
              15 FILLER PIC X(20) VALUE
                 '00400004000040000400'.
              15 FILLER PIC X(5) VALUE 'A1LB5'.
              15 FILLER PIC X(20) VALUE
                 '00500005000050000500'.
           12 WS-HOLD-3DI-2.
              15 FILLER PIC X(5) VALUE '3D-A2'.
              15 FILLER PIC X(5) VALUE 'A2LB1'.
              15 FILLER PIC X(20) VALUE
                 '00100001000010000100'.
              15 FILLER PIC X(5) VALUE 'A2LB2'.
              15 FILLER PIC X(20) VALUE
                 '00200002000020000200'.
              15 FILLER PIC X(5) VALUE 'A2LB3'.
              15 FILLER PIC X(20) VALUE
                 '00300003000030000300'.
              15 FILLER PIC X(5) VALUE 'A2LB4'.
              15 FILLER PIC X(20) VALUE
                 '00400004000040000400'.
              15 FILLER PIC X(5) VALUE 'A2LB5'.
              15 FILLER PIC X(20) VALUE
                 '00500005000050000500'.
           12 WS-HOLD-3DI-3.
              15 FILLER PIC X(5) VALUE '3D-A3'.
              15 FILLER PIC X(5) VALUE 'A3LB1'.
              15 FILLER PIC X(20) VALUE
                 '00100001000010000100'.
              15 FILLER PIC X(5) VALUE 'A3LB2'.
              15 FILLER PIC X(20) VALUE
                 '00200002000020000200'.
              15 FILLER PIC X(5) VALUE 'A3LB3'.
              15 FILLER PIC X(20) VALUE
                 '00300003000030000300'.
              15 FILLER PIC X(5) VALUE 'A3LB4'.
              15 FILLER PIC X(20) VALUE
                 '00400004000040000400'.
              15 FILLER PIC X(5) VALUE 'A3LB5'.
              15 FILLER PIC X(20) VALUE
                 '00500005000050000500'.
           12 WS-HOLD-3DI-4.
              15 FILLER PIC X(5) VALUE '3D-A4'.
              15 FILLER PIC X(5) VALUE 'A4LB1'.
              15 FILLER PIC X(20) VALUE
                 '00100001000010000100'.
              15 FILLER PIC X(5) VALUE 'A4LB2'.
              15 FILLER PIC X(20) VALUE
                 '00200002000020000200'.
              15 FILLER PIC X(5) VALUE 'A4LB3'.
              15 FILLER PIC X(20) VALUE
                 '00300003000030000300'.
              15 FILLER PIC X(5) VALUE 'A4LB4'.
              15 FILLER PIC X(20) VALUE
                 '00400004000040000400'.
              15 FILLER PIC X(5) VALUE 'A4LB5'.
              15 FILLER PIC X(20) VALUE
                 '00500005000050000500'.
           12 WS-HOLD-3DI-5.
              15 FILLER PIC X(5) VALUE '3D-A5'.
              15 FILLER PIC X(5) VALUE 'A5LB1'.
              15 FILLER PIC X(20) VALUE
                 '00100001000010000100'.
              15 FILLER PIC X(5) VALUE 'A5LB2'.
              15 FILLER PIC X(20) VALUE
                 '00200002000020000200'.
              15 FILLER PIC X(5) VALUE 'A5LB3'.
              15 FILLER PIC X(20) VALUE
                 '00300003000030000300'.
              15 FILLER PIC X(5) VALUE 'A5LB4'.
              15 FILLER PIC X(20) VALUE
                 '00400004000040000400'.
              15 FILLER PIC X(5) VALUE 'A5LB5'.
              15 FILLER PIC X(20) VALUE
                 '00500005000050000500'.

      *    This table will load the values from WS-3DS-HOLD.
      *    This table will use subscripts to navigate.
       01  WS-3DS-Table-Storage.
           12 WS-3DS-SUB1                       PIC 9 VALUE 0.
           12 WS-3DS-SUB2                       PIC 9 VALUE 0.
           12 WS-3DS-SUB3                       PIC 9 VALUE 0.
           12 WS-3DS-Table-Setup.
              15 WS-3DS-Table OCCURS 5 TIMES.
                 18 WS-3DS-A                      PIC X(05).
                 18 WS-3DS-L2 OCCURS 5 TIMES.
                    21 WS-3DS-L2-B                PIC X(05).
                    21 WS-3DS-L3 OCCURS 5 TIMES.
                       24 WS-3DS-L3-C             PIC X(05).

      *    This table will load the values from WS-3DI-HOLD.
      *    This table will use indexes to navigate.
       01  WS-3DI-Table-Storage.
           12 WS-3DI-Table-Setup.
      *    Division
              15 WS-3DI-Table OCCURS 5 TIMES INDEXED BY WS-3DI1-IDX.
                 18 WS-3DI-A                      PIC X(05).
      *    Region
                 18 WS-3DI-L2 OCCURS 5 TIMES INDEXED BY WS-3DI2-IDX.
                    21 WS-3DI-L2-B                PIC X(05).
      *    4 numbers for each Region, the 5th will be calculated.
                    21 WS-3DI-L3 OCCURS 4 TIMES INDEXED BY WS-3DI3-IDX.
                       24 WS-3DI-L3-C             PIC 9(05).

       01  WS-Company-Storage.
           12 WS-Region-Total                    PIC 9(8) VALUE 0.
           12 WS-Division-Total                  PIC 9(8) VALUE 0.
           12 WS-Company-Total                   PIC 9(8) VALUE 0.

       01  WS-Function-Storage.
           12 WS-Show-Number         PIC 999999.99+.
           12 WS-Hold-Counter        PIC S9(4) COMP VALUE ZERO.
           12 WS-Hold-Value          PIC S9(4) COMP VALUE ZERO.
           12 WS-Hold-High-Value     PIC S9(4) COMP VALUE LOW-VALUE.
           12 WS-Hold-Low-Value      PIC S9(4) COMP VALUE HIGH-VALUE.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           PERFORM 1100-Load-Other-Tables.

       1100-Load-Other-Tables.
           MOVE WS-1D-HOLD     TO WS-1D-Table-Setup.
           MOVE WS-1DN-HOLD    TO WS-1DN-Table-Setup.
           MOVE WS-2D-HOLD     TO WS-2D-Table-Setup.
           MOVE WS-St-Crs-HOLD TO WS-SC-Table-Setup.
           MOVE WS-3DS-HOLD    TO WS-3DS-Table-Setup.
           MOVE WS-3DI-HOLD    TO WS-3DI-Table-Setup.

       2000-Process.
           PERFORM 2900-Display-The-Tables.
           PERFORM 2100-Do-Some-Calculating.
           PERFORM 2200-Do-Some-Searching.


       2100-Do-Some-Calculating.
      *    Use the 1D Number table, find the MAX value.
      *    Since it is a known number of elements, this will work.
           DISPLAY "Find the Max Number: "
           COMPUTE WS-Show-Number = 
              FUNCTION MAX(
                 WS-1DN-A(1),
                 WS-1DN-A(2),
                 WS-1DN-A(3),
                 WS-1DN-A(4),
                 WS-1DN-A(5),
              )
           END-COMPUTE.
           DISPLAY "Function - Max Number - 1D table is: " 
              WS-Show-Number.

      *    But that won't work if you need to walk the table.
      *    Need to setup a Hold variable and compare instead.
           INITIALIZE WS-Hold-High-Value. 
           PERFORM VARYING WS-1DN-SUB FROM 1 BY 1 UNTIL
              WS-1DN-SUB > WS-1DN-Element-Cnt
              IF (WS-1DN-A(WS-1DN-SUB)) > WS-Hold-High-Value 
                 MOVE WS-1DN-A(WS-1DN-SUB) TO WS-Hold-High-Value
              END-IF
           END-PERFORM.
           MOVE WS-Hold-High-Value TO WS-Show-Number.
           DISPLAY " Perform - Max Number - 1D table is: " 
              WS-Show-Number.

      *    Use the 1D Number table, SUM the values.
      *    Since it is a known number of elements, this will work.
           DISPLAY "Total the values: "
           COMPUTE WS-Show-Number = 
              FUNCTION SUM(
                 WS-1DN-A(1),
                 WS-1DN-A(2),
                 WS-1DN-A(3),
                 WS-1DN-A(4),
                 WS-1DN-A(5),
              )
           END-COMPUTE.
           DISPLAY "Function - Total - 1D table is: " 
              WS-Show-Number.

      *    But that won't work if you need to walk the table.
      *    Need to setup a Hold variable and add up instead.
           INITIALIZE WS-Hold-High-Value. 
           PERFORM VARYING WS-1DN-SUB FROM 1 BY 1 UNTIL
              WS-1DN-SUB > WS-1DN-Element-Cnt
              ADD WS-1DN-A(WS-1DN-SUB) TO WS-Hold-High-Value 
              END-ADD
           END-PERFORM.
           MOVE WS-Hold-High-Value TO WS-Show-Number.
           DISPLAY " Perform - Total - 1D table is: " 
              WS-Show-Number.

      *    Use the 1D Number table, find the AVG.
      *    Since it is a known number of elements, this will work.
           DISPLAY "Average(MEAN) the values: "
           COMPUTE WS-Show-Number = 
              FUNCTION MEAN(
                 WS-1DN-A(1),
                 WS-1DN-A(2),
                 WS-1DN-A(3),
                 WS-1DN-A(4),
                 WS-1DN-A(5),
              )
           END-COMPUTE.
           DISPLAY "Function - Average of Values - 1D table is: " 
              WS-Show-Number.

      *    But that won't work if you need to walk the table.
      *    Need to setup a Hold variable and add up the values
      *    and a counter for the number of numbers then divide.
           INITIALIZE WS-Hold-Value, WS-Hold-Counter. 
           PERFORM VARYING WS-1DN-SUB FROM 1 BY 1 UNTIL
              WS-1DN-SUB > WS-1DN-Element-Cnt
              ADD +1 TO WS-Hold-Counter 
              ADD WS-1DN-A(WS-1DN-SUB) TO WS-Hold-Value 
           END-PERFORM.
           COMPUTE WS-Show-Number = 
              WS-Hold-Value / WS-Hold-Counter
           DISPLAY " Perform - Average of Values - 1D table is: " 
              WS-Show-Number.


      *    Let's move on to a 2D table and do some reporting.
      *    This is the Student-Course-Grade table.
      *    Print a line showing the student name, then follow that with
      *    a list of the courses and grades.

           PERFORM VARYING WS-SC-St-IDX FROM 1 BY 1
              UNTIL WS-SC-St-IDX > WS-SC-Element1-Cnt
              DISPLAY WS-SC-Student-Name(WS-SC-St-IDX)       
              PERFORM VARYING WS-SC-Crs-IDX FROM 1 BY 1
                 UNTIL WS-SC-Crs-IDX > WS-SC-Element2-Cnt
                 DISPLAY "Course: " 
                         WS-SC-Course-Name
                         (WS-SC-St-IDX, WS-SC-Crs-IDX)
                         "  Grade: "
                         WS-SC-Course-Grade
                         (WS-SC-St-IDX, WS-SC-Crs-IDX)
              END-PERFORM
           END-PERFORM.



       2200-Do-Some-Searching.










       2900-Display-The-Tables.
           DISPLAY "If DEBUG MODE is on, A bunch of tables will show.".   
      *    Simple Table with Alphanumeric data.
      D    DISPLAY "This is the Simple 1D Table:"
      D    DISPLAY "Using numbers as subscripts:"
      D    DISPLAY "WS-1D-Table - 1: " WS-1D-A(1). 
      D    DISPLAY "WS-1D-Table - 2: " WS-1D-A(2). 
      D    DISPLAY "WS-1D-Table - 3: " WS-1D-A(3). 
      D    DISPLAY "WS-1D-Table - 4: " WS-1D-A(4). 
      D    DISPLAY "WS-1D-Table - 5: " WS-1D-A(5).
      D    DISPLAY SPACES.

      D    DISPLAY "This is the Simple 1D Table:"
      D    DISPLAY "Using subscript addition:"
      D    MOVE 1 TO WS-1D-SUB.
      D    PERFORM 5 TIMES
      D       DISPLAY "WS-1D-Table Entry: " WS-1D-A(WS-1D-SUB)
      D       ADD 1 TO WS-1D-SUB 
      D    END-PERFORM.
      D    DISPLAY SPACES.

      D    DISPLAY "This is the Simple 1D Table:"
      D    DISPLAY "Using subscript varying:"
      D    PERFORM VARYING WS-1D-SUB FROM 1 BY 1
      D       UNTIL WS-1D-SUB > 5 
      D       DISPLAY "WS-1D-Table Entry: " WS-1D-A(WS-1D-SUB) 
      D    END-PERFORM.
      D    DISPLAY SPACES.
          
      *    Simple Table with Numbers.
      D    DISPLAY "This is the Simple 1D Number Table:"
      D    DISPLAY "Using numbers as subscripts:"
      D    DISPLAY "WS-1DN-Table - 1: " WS-1DN-A(1). 
      D    DISPLAY "WS-1DN-Table - 2: " WS-1DN-A(2). 
      D    DISPLAY "WS-1DN-Table - 3: " WS-1DN-A(3). 
      D    DISPLAY "WS-1DN-Table - 4: " WS-1DN-A(4). 
      D    DISPLAY "WS-1DN-Table - 5: " WS-1DN-A(5).
      D    DISPLAY SPACES.

      D    DISPLAY "This is the Simple 1D Number Table:"
      D    DISPLAY "Using subscript addition:"
      D    MOVE 1 TO WS-1DN-SUB.
      D    PERFORM 5 TIMES
      D       DISPLAY "WS-1DN-Table Entry: " WS-1DN-A(WS-1DN-SUB)
      D       ADD 1 TO WS-1DN-SUB 
      D    END-PERFORM.
      D    DISPLAY SPACES.

      D    DISPLAY "This is the Simple 1D Number Table:"
      D    DISPLAY "Using subscript varying:"
      D    PERFORM VARYING WS-1DN-SUB FROM 1 BY 1
      D       UNTIL WS-1DN-SUB > 5 
      D       DISPLAY "WS-1DN-Table Entry: " WS-1DN-A(WS-1DN-SUB) 
      D    END-PERFORM.
      D    DISPLAY SPACES.

      D    DISPLAY "This is the 2D Table:"
      D    DISPLAY "Using numbers as subscripts:"
      D    DISPLAY "WS-2D-Table 1-Level 1: " 
      D       WS-2D-A(1) "-" WS-2D-L2(1, 1). 
      D    DISPLAY "WS-2D-Table 1-Level 2: " 
      D       WS-2D-A(1) "-" WS-2D-L2(1, 2). 
      D    DISPLAY "WS-2D-Table 1-Level 3: " 
      D       WS-2D-A(1) "-" WS-2D-L2(1, 3). 
      D    DISPLAY "WS-2D-Table 1-Level 4: " 
      D       WS-2D-A(1) "-" WS-2D-L2(1, 4). 
      D    DISPLAY "WS-2D-Table 1-Level 5: " 
      D       WS-2D-A(1) "-" WS-2D-L2(1, 5).
      D    DISPLAY SPACES.
      D    DISPLAY "WS-2D-Table 2-Level 1: " 
      D       WS-2D-A(2) "-" WS-2D-L2(2, 1). 
      D    DISPLAY "WS-2D-Table 2-Level 2: " 
      D       WS-2D-A(2) "-" WS-2D-L2(2, 2). 
      D    DISPLAY "WS-2D-Table 2-Level 3: " 
      D       WS-2D-A(2) "-" WS-2D-L2(2, 3). 
      D    DISPLAY "WS-2D-Table 2-Level 4: " 
      D       WS-2D-A(2) "-" WS-2D-L2(2, 4). 
      D    DISPLAY "WS-2D-Table 2-Level 5: " 
      D       WS-2D-A(2) "-" WS-2D-L2(2, 5).
      D    DISPLAY SPACES.
      D    DISPLAY "WS-2D-Table 3-Level 1: " 
      D       WS-2D-A(3) "-" WS-2D-L2(3, 1). 
      D    DISPLAY "WS-2D-Table 3-Level 2: " 
      D       WS-2D-A(3) "-" WS-2D-L2(3, 2). 
      D    DISPLAY "WS-2D-Table 3-Level 3: " 
      D       WS-2D-A(3) "-" WS-2D-L2(3, 3). 
      D    DISPLAY "WS-2D-Table 3-Level 4: " 
      D       WS-2D-A(3) "-" WS-2D-L2(3, 4). 
      D    DISPLAY "WS-2D-Table 3-Level 5: " 
      D       WS-2D-A(3) "-" WS-2D-L2(3, 5).
      D    DISPLAY SPACES.
      D    DISPLAY "WS-2D-Table 4-Level 1: " 
      D       WS-2D-A(4) "-" WS-2D-L2(4, 1). 
      D    DISPLAY "WS-2D-Table 4-Level 2: " 
      D       WS-2D-A(4) "-" WS-2D-L2(4, 2). 
      D    DISPLAY "WS-2D-Table 4-Level 3: " 
      D       WS-2D-A(4) "-" WS-2D-L2(4, 3). 
      D    DISPLAY "WS-2D-Table 4-Level 4: " 
      D       WS-2D-A(4) "-" WS-2D-L2(4, 4). 
      D    DISPLAY "WS-2D-Table 4-Level 5: " 
      D       WS-2D-A(4) "-" WS-2D-L2(4, 5).
      D    DISPLAY SPACES.
      D    DISPLAY "WS-2D-Table 5-Level 1: " 
      D       WS-2D-A(5) "-" WS-2D-L2(5, 1). 
      D    DISPLAY "WS-2D-Table 5-Level 2: " 
      D       WS-2D-A(5) "-" WS-2D-L2(5, 2). 
      D    DISPLAY "WS-2D-Table 5-Level 3: " 
      D       WS-2D-A(5) "-" WS-2D-L2(5, 3). 
      D    DISPLAY "WS-2D-Table 5-Level 4: " 
      D       WS-2D-A(5) "-" WS-2D-L2(5, 4). 
      D    DISPLAY "WS-2D-Table 5-Level 5: " 
      D       WS-2D-A(5) "-" WS-2D-L2(5, 5).
      D    DISPLAY SPACES.

      D    DISPLAY "This is the Two Dim Table:"
      D    DISPLAY "Using subscript addition:"
      D    MOVE 1 TO WS-2D-SUB1.
      D    PERFORM 5 TIMES
      D       DISPLAY "WS-2D-Table Entry: " 
      D          WS-2D-A(WS-2D-SUB1) 
      D       MOVE 1 TO WS-2D-SUB2
      D       PERFORM 5 TIMES
      D          DISPLAY "      WS-2D-SUB Entry: " 
      D             WS-2D-L2(WS-2D-SUB1, WS-2D-SUB2)
      D             ADD 1 TO WS-2D-SUB2
      D       END-PERFORM
      D       ADD 1 TO WS-2D-SUB1
      D    END-PERFORM.
      D    DISPLAY SPACES.

      D    DISPLAY "This is the Two Dim Table:"
      D    DISPLAY "Using subscript varying:"
      D    PERFORM VARYING WS-2D-SUB1 FROM 1 BY 1
      D       UNTIL WS-2D-SUB1 > 5
      D       DISPLAY "WS-2D-Table Entry: " 
      D          WS-2D-A(WS-2D-SUB1) 
      D       PERFORM VARYING WS-2D-SUB2 FROM 1 BY 1
      D          UNTIL WS-2D-SUB2 > 5 
      D          DISPLAY "      WS-2D-SUB Entry: " 
      D             WS-2D-L2(WS-2D-SUB1, WS-2D-SUB2)
      D       END-PERFORM 
      D    END-PERFORM.
      D    DISPLAY SPACES.

      D    DISPLAY "This is the 3DS Subscripted Table:"
      D    DISPLAY "Using subscript varying:"
      D    PERFORM VARYING WS-3DS-SUB1 FROM 1 BY 1
      D       UNTIL WS-3DS-SUB1 > 5
      D       DISPLAY "WS-3DS-Table Entry: "
      D          WS-3DS-A(WS-3DS-SUB1)
      D       PERFORM VARYING WS-3DS-SUB2 FROM 1 BY 1
      D          UNTIL WS-3DS-SUB2 > 5
      D          DISPLAY "      WS-3DS-SUB Entry: "
      D             WS-3DS-L2-B(WS-3DS-SUB1, WS-3DS-SUB2)
      D          PERFORM VARYING WS-3DS-SUB3 FROM 1 BY 1
      D             UNTIL WS-3DS-SUB3 > 5
      D             DISPLAY "           WS-3DS-SUB-SUB Entry: "
      D              WS-3DS-L3-C(WS-3DS-SUB1, WS-3DS-SUB2, WS-3DS-SUB3)
      D          END-PERFORM
      D       END-PERFORM
      D    END-PERFORM.
      D    DISPLAY SPACES.


      D    DISPLAY "This is the 3DI Indexed Table:"
      D    DISPLAY "Using index varying:"
      D    PERFORM VARYING WS-3DI1-IDX FROM 1 BY 1
      D       UNTIL WS-3DI1-IDX > 5
      D       DISPLAY "WS-3DI-Table Entry: "
      D          WS-3DI-A(WS-3DI1-IDX)
      D       PERFORM VARYING WS-3DI2-IDX FROM 1 BY 1
      D          UNTIL WS-3DI2-IDX > 5
      D          DISPLAY "      WS-3DI-SUB Entry: "
      D             WS-3DI-L2-B(WS-3DI1-IDX, WS-3DI2-IDX)
      D          PERFORM VARYING WS-3DI3-IDX FROM 1 BY 1
      D             UNTIL WS-3DI3-IDX > 4
      D             DISPLAY "           WS-3DI-SUB-SUB Entry: "
      D              WS-3DI-L3-C(WS-3DI1-IDX, WS-3DI2-IDX, WS-3DI3-IDX)
      D          END-PERFORM
      D       END-PERFORM
      D    END-PERFORM.
      D    DISPLAY SPACES.

       3000-End-Job.
           DISPLAY "3000-EOJ: ".
           DISPLAY "Normally, I would have something to do here".