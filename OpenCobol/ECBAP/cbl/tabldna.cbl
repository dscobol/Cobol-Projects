      ***********************************************************
      * Program name:    TABLDNA
      * Original author: dastagg
      *
      * Description: This will load files into WS.
      *    Then do stuff with them.
      *
      *    This is the "No ALL" version of table processing.
      *    Z COBOL functions have an ALL subscript property.
      *    gnuCobol functions do not, yet.
      *
      *    TABLDWA will be Z COBOL only (for now). It will use the ALL
      *    index in FUNCTIONS to process the table data.
      *
      *    This program will process tables "the old-fashioned way".
      *    These methods will work on both gnuCobol and Z COBOL systems.
      *
      *
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
      * 2020-08-09 dastagg       Created for COBOL class
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TABLDNA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE
      *     ASSIGN TO EMPLOYEE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/employee.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-EMPLOYEE-Status.

           SELECT STCOURS
      *     ASSIGN TO STCOURS
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/student-cours.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-STCOURS-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-EMPLOYEE-REC.
              15 FD-EMPNO                PIC X(006).
              15 FILLER                  PIC X(001).
              15 FD-FIRSTNME             PIC X(009).
              15 FILLER                  PIC X(001).
              15 FD-MIDINIT              PIC X(001).
              15 FILLER                  PIC X(001).
              15 FD-LASTNAME             PIC X(010).
              15 FILLER                  PIC X(001).
              15 FD-WORKDEPT             PIC X(003).
              15 FILLER                  PIC X(001).
              15 FD-PHONENO              PIC X(004).
              15 FILLER                  PIC X(001).
              15 FD-HIREDATE             PIC X(010).
              15 FILLER                  PIC X(001).
              15 FD-JOB                  PIC X(008).
              15 FILLER                  PIC X(001).
              15 FD-EDLEVEL              PIC X(002).
              15 FILLER                  PIC X(001).
              15 FD-GENDER               PIC X(001).
              15 FILLER                  PIC X(001).
              15 FD-BIRTHDATE            PIC X(010).
              15 FILLER                  PIC X(001).
              15 FD-SALARY               PIC X(008).
              15 FILLER                  PIC X(001).
              15 FD-BONUS                PIC X(007).
              15 FILLER                  PIC X(001).
              15 FD-COMM                 PIC X(008).

       FD  STCOURS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-Student-Record.
           12 FD-Student-Name             PIC X(20).
           12 FD-Student-Courses.
              15 FD-Student-Course-Table OCCURS 6 TIMES.
                 18  FD-Course-Nbr        PIC X(7).
                 18  FD-Course-Grade      PIC X(1).
           12  FILLER                     PIC X(12).


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==EMPLOYEE==.
           COPY WSFST REPLACING ==:tag:== BY ==STCOURS==.

       01  WS-FILE-Counters.
           12 FD-EMPLOYEE-Record-Cnt        PIC S9(4) COMP VALUE ZERO.
           12 FD-STCOURS-Record-Cnt         PIC S9(4) COMP VALUE ZERO.

       01  WS-Temp-Storage.
           12 WS-Display-FD-Nums           PIC Z,ZZZ,ZZ9.99.
           12 WS-Display-FD-Date           PIC 9(5).
           12 WS-Phone-Found-Flag          PIC X.
              88 WS-Phone-Found            VALUE 'Y'.
              88 WS-Phone-Not-Found        VALUE 'N'.


       01  WS-Emp-Table-Storage.
           12 WS-Emp-Max-Element-Counter     PIC 9(4) VALUE 50.
           12 WS-Emp-Occurs-Dep-Counter      PIC S99 COMP-3.
           12 WS-Emp-Table OCCURS 0 TO 50 TIMES
              DEPENDING ON WS-Emp-Occurs-Dep-Counter
              INDEXED BY WS-Emp-IDX.
              15 WS-Emp-EMPNO                PIC X(006).
              15 WS-Emp-FIRSTNME             PIC X(009).
              15 WS-Emp-MIDINIT              PIC X(001).
              15 WS-Emp-LASTNAME             PIC X(010).
              15 WS-Emp-WORKDEPT             PIC X(003).
              15 WS-Emp-PHONENO              PIC X(004).
              15 WS-Emp-HIREDATE.
                 18 WS-Emp-HD-Year           PIC X(004).
                 18 WS-Emp-HD-Month          PIC X(002).
                 18 WS-Emp-HD-Day            PIC X(002).
              15 WS-Emp-HIREDATE-Num REDEFINES
                 WS-Emp-HIREDATE             PIC 9(008).
              15 WS-Emp-JOB                  PIC X(008).
              15 WS-Emp-EDLEVEL              PIC X(002).
              15 WS-Emp-GENDER               PIC X(001).
              15 WS-Emp-BIRTHDATE.
                 18 WS-Emp-BD-Year           PIC X(004).
                 18 WS-Emp-BD-Month          PIC X(002).
                 18 WS-Emp-BD-Day            PIC X(002).
              15 WS-Emp-BIRTHDATE-Num REDEFINES 
                 WS-Emp-BIRTHDATE            PIC 9(008).
              15 WS-Emp-SALARY               PIC 9(7)V99.
              15 WS-Emp-BONUS                PIC 9(7)V99.
              15 WS-Emp-COMM                 PIC 9(7)V99.

       01  WS-Stdt-Course-Table-Storage.
           12 WS-Student-Max-Element-Counter     PIC 9(4) VALUE 5.
           12 WS-Student-Occurs-Dep-Counter      PIC S99 COMP-3.
           12 WS-Student-Course-Table  OCCURS 0 TO 5 TIMES
              DEPENDING ON WS-Student-Occurs-Dep-Counter
              INDEXED BY WS-Student-IDX.
              15 WS-Student-Name           PIC X(20).
              15 WS-Student-Courses.
                 18 WS-Course-Table 
                    OCCURS 6 TIMES INDEXED BY WS-Course-IDX.
                    21  WS-Course-Nbr      PIC X(7).
                    21  WS-Course-Grade    PIC X(1).
                       88 WS-Valid-Grade
                          VALUES "A", "B", "C", "D", "F".

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           PERFORM 1010-Load-Emp-Table.
           PERFORM 1110-Load-Student-Table.

       1010-Load-Emp-Table.
           OPEN INPUT EMPLOYEE.
           SET WS-Emp-IDX TO +1.
           PERFORM 1013-Load-Employees Until WS-EMPLOYEE-EOF.
           CLOSE EMPLOYEE.
           MOVE FD-EMPLOYEE-Record-Cnt TO WS-Emp-Occurs-Dep-Counter.
           PERFORM 1019-Verify-Emp-Table.

       1013-Load-Employees.
           READ EMPLOYEE
              AT END SET WS-EMPLOYEE-EOF TO TRUE
           END-READ.
           IF WS-EMPLOYEE-Good
              ADD +1 TO FD-EMPLOYEE-Record-Cnt
              PERFORM 1015-Emp-Load-Data
              SET WS-Emp-IDX UP BY +1 
           ELSE
              IF WS-EMPLOYEE-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 1015-EMPLOYEE"
                 DISPLAY "Read EMPLOYEE Failed."
                 DISPLAY "File Status: " WS-EMPLOYEE-Status
                 MOVE +8 TO RETURN-CODE
                 GOBACK
              END-IF
           END-IF.

       1015-Emp-Load-Data.
           MOVE FD-EMPNO     TO WS-Emp-EMPNO(WS-Emp-IDX).
           MOVE FD-FIRSTNME  TO WS-Emp-FIRSTNME(WS-Emp-IDX).
           MOVE FD-MIDINIT   TO WS-Emp-MIDINIT(WS-Emp-IDX).
           MOVE FD-LASTNAME  TO WS-Emp-LASTNAME(WS-Emp-IDX).
           MOVE FD-WORKDEPT  TO WS-Emp-WORKDEPT(WS-Emp-IDX).
           MOVE FD-PHONENO   TO WS-Emp-PHONENO(WS-Emp-IDX).

           UNSTRING FD-HIREDATE DELIMITED BY '-'
              INTO WS-Emp-HD-Year(WS-Emp-IDX) 
                   WS-Emp-HD-Month(WS-Emp-IDX) 
                   WS-Emp-HD-Day(WS-Emp-IDX) 
           END-UNSTRING.           

           MOVE FD-JOB       TO WS-Emp-JOB(WS-Emp-IDX).
           MOVE FD-EDLEVEL   TO WS-Emp-EDLEVEL(WS-Emp-IDX).
           MOVE FD-GENDER    TO WS-Emp-GENDER(WS-Emp-IDX).

           UNSTRING FD-BIRTHDATE DELIMITED BY '-'
              INTO WS-Emp-BD-Year(WS-Emp-IDX) 
                   WS-Emp-BD-Month(WS-Emp-IDX) 
                   WS-Emp-BD-Day(WS-Emp-IDX) 
           END-UNSTRING.

           MOVE FUNCTION NUMVAL-C(FD-SALARY) TO
              WS-Emp-SALARY(WS-Emp-IDX). 

           MOVE FUNCTION NUMVAL-C(FD-BONUS) TO
              WS-Emp-BONUS(WS-Emp-IDX).

           MOVE FUNCTION NUMVAL-C(FD-COMM) TO
              WS-Emp-COMM(WS-Emp-IDX).


       1019-Verify-Emp-Table.
      D    DISPLAY "WS-Emp-Table: "
      D    PERFORM VARYING WS-Emp-IDX FROM 1 BY 1 
      D       UNTIL WS-Emp-IDX > WS-Emp-Occurs-Dep-Counter
      D       DISPLAY WS-Emp-Table(WS-Emp-IDX)
      D    END-PERFORM.
           IF WS-Emp-Occurs-Dep-Counter >
              WS-Emp-Max-Element-Counter
                 DISPLAY "** ERROR **: 1019-Verify-Emp-Table"
                 DISPLAY "WS table size is too small for file."
                 DISPLAY "Increase WS-Emp-Table-Storage variables."
                 MOVE +9 TO RETURN-CODE
                 GOBACK
           END-IF.

       1110-Load-Student-Table.
           OPEN INPUT STCOURS.
           SET WS-Student-IDX TO +1.
           PERFORM 1113-Load-Students Until WS-STCOURS-EOF.
           CLOSE STCOURS.
           MOVE FD-STCOURS-Record-Cnt TO WS-Student-Occurs-Dep-Counter.
           PERFORM 1119-Verify-Student-Table.

       1113-Load-Students.
           READ STCOURS
              AT END SET WS-STCOURS-EOF TO TRUE
           END-READ.
           IF WS-STCOURS-Good
              ADD +1 TO FD-STCOURS-Record-Cnt
              PERFORM 1115-Student-Load-Data
              SET WS-Student-IDX UP BY +1 
           ELSE
              IF WS-STCOURS-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 1115-STCOURS"
                 DISPLAY "Read STCOURS Failed."
                 DISPLAY "File Status: " WS-STCOURS-Status
                 MOVE +8 TO RETURN-CODE
                 GOBACK
              END-IF
           END-IF.

       1115-Student-Load-Data.
           MOVE FD-Student-Record TO 
              WS-Student-Course-Table(WS-Student-IDX).

       1119-Verify-Student-Table.
      D    DISPLAY "WS-Student-Course-Table: "
      D    PERFORM VARYING WS-Student-IDX FROM 1 BY 1 
      D       UNTIL WS-Student-IDX > WS-Student-Occurs-Dep-Counter
      D       DISPLAY WS-Student-Course-Table(WS-Student-IDX)
      D    END-PERFORM.
           IF WS-Student-Occurs-Dep-Counter >
              WS-Student-Max-Element-Counter
                 DISPLAY "** ERROR **: 1119-Verify-Student-Table"
                 DISPLAY "WS table size is too small for file."
                 DISPLAY "Increase WS-Student-Table-Storage variables."
                 MOVE +9 TO RETURN-CODE
                 GOBACK
           END-IF.

       2000-Process.
           DISPLAY "2000-Process: ".
      *    To display the raw tables, in all their glory,
      *       activate debugging mode.
      *    otherwise 2900- doesn't do anything.
           PERFORM 2900-Display-The-Tables.
           PERFORM 2100-Do-Some-Calculating.
           PERFORM 2200-Do-Some-Searching.
           PERFORM 2300-Do-Some-Searching-All.


       2100-Do-Some-Calculating.
       2300-Do-Some-Searching-All.


       2200-Do-Some-Searching.

      *    This will find one entry and get out.
      *    Find the phone # of MONTEVERDE, ROBERT.
           SET WS-Phone-Not-Found TO TRUE.
           PERFORM VARYING WS-Emp-IDX FROM 1 BY 1
               UNTIL 
                  WS-Emp-IDX > WS-Emp-Occurs-Dep-Counter OR 
                  WS-Phone-Found
              SEARCH WS-Emp-Table
              WHEN (WS-Emp-LASTNAME(WS-Emp-IDX) = 'MONTEVERDE'
                 AND WS-Emp-FIRSTNME(WS-Emp-IDX) = 'ROBERT')
                 DISPLAY "*** Robert's Phone Number is: "
                    WS-Emp-PHONENO(WS-Emp-IDX)
                 SET WS-Phone-Found TO TRUE
           END-SEARCH
           END-PERFORM.

      *    Find all people that were born in 1948.
      *    This will go through the entire table looking for values.
           PERFORM VARYING WS-Emp-IDX FROM 1 BY 1
               UNTIL 
                  WS-Emp-IDX > WS-Emp-Occurs-Dep-Counter 
              SEARCH WS-Emp-Table
                 WHEN WS-Emp-BD-Year(WS-Emp-IDX) = '1948'
                    DISPLAY 
                       WS-Emp-FIRSTNME(WS-Emp-IDX) " " 
                       WS-Emp-LASTNAME(WS-Emp-IDX) " birthdate is "
                       WS-Emp-BIRTHDATE(WS-Emp-IDX)
              END-SEARCH
           END-PERFORM.

      *    Find all Managers
      *    This will go through the entire table looking for values.
           PERFORM VARYING WS-Emp-IDX FROM 1 BY 1
               UNTIL 
                  WS-Emp-IDX > WS-Emp-Occurs-Dep-Counter 
              SEARCH WS-Emp-Table
                 WHEN WS-Emp-JOB(WS-Emp-IDX) = 'MANAGER'
                    DISPLAY 
                       WS-Emp-FIRSTNME(WS-Emp-IDX) " " 
                       WS-Emp-LASTNAME(WS-Emp-IDX) " is a Manger."
              END-SEARCH
           END-PERFORM.

       2900-Display-The-Tables.


       3000-End-Job.
           DISPLAY "3000-EOJ: ".
           DISPLAY "Normally, I would have something to do here".
