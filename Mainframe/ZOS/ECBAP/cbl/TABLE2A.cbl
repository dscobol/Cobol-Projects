      ***********************************************************
      * Program name:    TABLE2A
      * Original author: dastagg
      *
      * Description: Program to test loading and processing tables.
      *    This version is just going to focus on 3D table processing.
      *
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  --------------------------------
      * 2020-08-01 dastagg       Created for COBOL class
      *     Removed the 5 element from the 3rd level
      *     Calculate the totals just with SUM.
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TABLE2A.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-Function-Storage.
           12 Max-Salary          PIC 999999.99+.
           12 I-Ord-Max           PIC 999999.99+.
           12 Avg-Salary          PIC 999999.99+.
           12 Salary-Range        PIC 999999.99+.
           12 Total-Payroll       PIC 999999.99+.

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

       01  WS-3D-Index-Table-Storage.
           12 WS-3D-Table-Setup.
      *    Division
              15 WS-3D-Table OCCURS 5 TIMES INDEXED BY WS-3D1-IDX.
                 18 WS-3D-A                      PIC X(05).
      *    Region
                 18 WS-3D-L2 OCCURS 5 TIMES INDEXED BY WS-3D2-IDX.
                    21 WS-3D-L2-B                PIC X(05).
      *    4 numbers for each Region,
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
           MOVE WS-3D-HOLD TO WS-3D-Table-Setup.

       2000-Process.
      *    Now, let's play with functions
      D     DISPLAY "This is the 3D Indexed Table:"
      D     DISPLAY "Index-Varying:"
      D     PERFORM VARYING WS-3D1-IDX FROM 1 BY 1
      D        UNTIL WS-3D1-IDX > 5
      D        DISPLAY "WS-3D-Table Entry: "
      D           WS-3D-A(WS-3D1-IDX)
      D        PERFORM VARYING WS-3D2-IDX FROM 1 BY 1
      D           UNTIL WS-3D2-IDX > 5
      D           DISPLAY "      WS-3D-SUB Entry: "
      D              WS-3D-L2-B(WS-3D1-IDX, WS-3D2-IDX)
      D           PERFORM VARYING WS-3D3-IDX FROM 1 BY 1
      D              UNTIL WS-3D3-IDX > 5
      D              DISPLAY "           WS-3D-SUB-SUB Entry: "
      D                 WS-3D-L3-C(WS-3D1-IDX, WS-3D2-IDX, WS-3D3-IDX)
      D           END-PERFORM
      D        END-PERFORM
      D     END-PERFORM.
      D     DISPLAY SPACES.


      *    The 3D table represents a company with 5 Divisions and
      *    within those are 5 Regions with data for each region.


      *    The table is setup "like" a load where the first 4 values
      *    would be loaded from a file.
      *
      *    Step 1: Sum the 4 numbers from each region and put that total
      *    in spot 5. Note: must make sure spot 5 is ZEROES.
      *    If not, you will have to break out and add up the 4 numbers
      *    individually.

           PERFORM VARYING WS-3D1-IDX FROM 1 BY 1
              UNTIL WS-3D1-IDX > 5
              PERFORM VARYING WS-3D2-IDX FROM 1 BY 1
                 UNTIL WS-3D2-IDX > 5
                    COMPUTE WS-3D-L3-C(WS-3D1-IDX, WS-3D2-IDX, 5) =
                       FUNCTION SUM
                          (WS-3D-L3-C(WS-3D1-IDX, WS-3D2-IDX, ALL))
                    END-COMPUTE
                    DISPLAY "Region within Division Total: "
                       WS-3D-L3-C(WS-3D1-IDX, WS-3D2-IDX, 5)
              END-PERFORM
           END-PERFORM.


      *     12 WS-Region-Total                    PIC 9(8) VALUE 0.
      *     12 WS-Division-Total                  PIC 9(8) VALUE 0.
      *     12 WS-Company-Total                   PIC 9(8) VALUE 0.
      *    Create ws-region-total, division-total, and company-total
      *    Calculating for Division

           PERFORM VARYING WS-3D1-IDX FROM 1 BY 1
              UNTIL WS-3D1-IDX > 5
                 COMPUTE WS-Division-Total =
                    FUNCTION SUM
                       (WS-3D-L3-C(WS-3D1-IDX, ALL, 5))
                 END-COMPUTE
                 DISPLAY "Division within Company Total: "
                    WS-Division-Total
           END-PERFORM.

           DISPLAY SPACES.
      *    Calculate for company

           COMPUTE WS-Company-Total =
              FUNCTION SUM
                 (WS-3D-L3-C(ALL, ALL, 5))
           END-COMPUTE.
           DISPLAY "Company Total: "
              WS-Company-Total.

      *    Okay, Calculating is fine.
      *    Now, can we calculate and print subtotals in one go.
      *    Start from scratch:
           DISPLAY SPACES.
           DISPLAY SPACES.
           DISPLAY SPACES.

           MOVE WS-3D-HOLD TO WS-3D-Table-Setup.
           INITIALIZE WS-Region-Total,
                      WS-Division-Total,
                      WS-Company-Total.

           PERFORM VARYING WS-3D1-IDX FROM 1 BY 1
              UNTIL WS-3D1-IDX > 5
              PERFORM VARYING WS-3D2-IDX FROM 1 BY 1
                 UNTIL WS-3D2-IDX > 5
                    COMPUTE WS-3D-L3-C(WS-3D1-IDX, WS-3D2-IDX, 5) =
                       FUNCTION SUM
                          (WS-3D-L3-C(WS-3D1-IDX, WS-3D2-IDX, ALL))
                    END-COMPUTE
                    DISPLAY "Total for Region: "
                       WS-3D-L2-B(WS-3D1-IDX, WS-3D2-IDX) " is "
                       WS-3D-L3-C(WS-3D1-IDX, WS-3D2-IDX, 5)
              END-PERFORM
              COMPUTE WS-Division-Total =
                 FUNCTION SUM
                    (WS-3D-L3-C(WS-3D1-IDX, ALL, 5))
              END-COMPUTE
              DISPLAY "Total for Division: "
                 WS-3D-A(WS-3D1-IDX) " is "
                 WS-Division-Total
              DISPLAY SPACES
           END-PERFORM.

           COMPUTE WS-Company-Total =
              FUNCTION SUM
                 (WS-3D-L3-C(ALL, ALL, 5))
           END-COMPUTE.
           DISPLAY SPACES,
           DISPLAY "Company Total: "
              WS-Company-Total.


      *    Show me the numbers for each region for the 4th term.
      *    The table is already loaded to I just need to get the right
      *    location.

           DISPLAY SPACES.
           DISPLAY SPACES.
           DISPLAY SPACES.


           PERFORM VARYING WS-3D1-IDX FROM 1 BY 1
              UNTIL WS-3D1-IDX > 5
              PERFORM VARYING WS-3D2-IDX FROM 1 BY 1
                 UNTIL WS-3D2-IDX > 5
                   DISPLAY "Region "
                           WS-3D-L2-B(WS-3D1-IDX, WS-3D2-IDX)
                           " Total for the 4th Qtr is: "
                           WS-3D-L3-C(WS-3D1-IDX, WS-3D2-IDX, 4)
              END-PERFORM
              COMPUTE WS-Division-Total =
                 FUNCTION SUM
                    (WS-3D-L3-C(WS-3D1-IDX, ALL, 4))
              END-COMPUTE
              DISPLAY "Division "
                      WS-3D-A(WS-3D1-IDX)
                      " Total for 4th Qtr is: "
                      WS-Division-Total
              DISPLAY SPACES
           END-PERFORM.

           COMPUTE WS-Company-Total =
              FUNCTION SUM
                 (WS-3D-L3-C(ALL, ALL, 4))
           END-COMPUTE.
           DISPLAY SPACES,
           DISPLAY "Company Total for 4th Qtr is: "
              WS-Company-Total.





       3000-End-Job.
           DISPLAY "3000-EOJ: ".
           DISPLAY "Normally, I would have something to do here".

