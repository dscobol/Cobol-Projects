      ***********************************************************
      * Program name:    TABLOAD1
      * Original author: dastagg
      *
      * Description: Base pgm to read a file and load it into a table.
      *    This will act as a basic template for table loading.
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
       PROGRAM-ID.  TABLOAD1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TABLOAD
      *     ASSIGN TO TABLOAD
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../../../common/data/ECBAP/becsv.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-TABLOAD-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  TABLOAD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-REC.
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


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==TABLOAD==.

       01  WS-FILE-Counters.
           12 WS-TABLOAD-Records-Read       PIC S9(4) COMP.

       01  WS-Temp-Storage.
           12 WS-Display-FD-Nums           PIC Z,ZZZ,ZZ9.99.
           12 WS-Display-FD-Date           PIC 9(5).

       01  WS-T1-Table-Storage.
           12 WS-T1-Max-Element-Counter     PIC 9(4) VALUE 50.
           12 WS-T1-Occurs-Dep-Counter      PIC S99 COMP-3.
           12 WS-T1-Table OCCURS 0 TO 50 TIMES
              DEPENDING ON WS-T1-Occurs-Dep-Counter
              INDEXED BY WS-T1-IDX.
              15 WS-T1-EMPNO                PIC X(006).
              15 WS-T1-FIRSTNME             PIC X(009).
              15 WS-T1-MIDINIT              PIC X(001).
              15 WS-T1-LASTNAME             PIC X(010).
              15 WS-T1-WORKDEPT             PIC X(003).
              15 WS-T1-PHONENO              PIC X(004).
              15 WS-T1-HIREDATE.
                 18 WS-T1-HD-Year           PIC X(004).
                 18 WS-T1-HD-Month          PIC X(002).
                 18 WS-T1-HD-Day            PIC X(002).
              15 WS-T1-HIREDATE-Num REDEFINES
                 WS-T1-HIREDATE             PIC 9(008).
              15 WS-T1-JOB                  PIC X(008).
              15 WS-T1-EDLEVEL              PIC X(002).
              15 WS-T1-GENDER               PIC X(001).
              15 WS-T1-BIRTHDATE.
                 18 WS-T1-BD-Year           PIC X(004).
                 18 WS-T1-BD-Month          PIC X(002).
                 18 WS-T1-BD-Day            PIC X(002).
              15 WS-T1-BIRTHDATE-Num REDEFINES 
                 WS-T1-BIRTHDATE            PIC 9(008).
              15 WS-T1-SALARY               PIC 9(7)V99.
              15 WS-T1-BONUS                PIC 9(7)V99.
              15 WS-T1-COMM                 PIC 9(7)V99.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           PERFORM 1010-Load-T1-Table.

       1010-Load-T1-Table.
           OPEN INPUT TABLOAD.
           SET WS-T1-IDX TO +1.
           PERFORM 1013-Load-T1 Until WS-TABLOAD-EOF.
           CLOSE TABLOAD.
           MOVE WS-TABLOAD-Records-Read TO WS-T1-Occurs-Dep-Counter.
           PERFORM 1019-Verify-T1-Table.

       1013-Load-T1.
           READ TABLOAD
              AT END SET WS-TABLOAD-EOF TO TRUE
           END-READ.
           IF WS-TABLOAD-Good
              ADD +1 TO WS-TABLOAD-Records-Read
              PERFORM 1015-T1-Load-Data
              SET WS-T1-IDX UP BY +1 
           ELSE
              IF WS-TABLOAD-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 1015-TABLOAD"
                 DISPLAY "Read TABLOAD Failed."
                 DISPLAY "File Status: " WS-TABLOAD-Status
                 MOVE +8 TO RETURN-CODE
                 GOBACK
              END-IF
           END-IF.

       1015-T1-Load-Data.
           MOVE FD-EMPNO     TO WS-T1-EMPNO(WS-T1-IDX).
           MOVE FD-FIRSTNME  TO WS-T1-FIRSTNME(WS-T1-IDX).
           MOVE FD-MIDINIT   TO WS-T1-MIDINIT(WS-T1-IDX).
           MOVE FD-LASTNAME  TO WS-T1-LASTNAME(WS-T1-IDX).
           MOVE FD-WORKDEPT  TO WS-T1-WORKDEPT(WS-T1-IDX).
           MOVE FD-PHONENO   TO WS-T1-PHONENO(WS-T1-IDX).

           UNSTRING FD-HIREDATE DELIMITED BY '-'
              INTO WS-T1-HD-Year(WS-T1-IDX) 
                   WS-T1-HD-Month(WS-T1-IDX) 
                   WS-T1-HD-Day(WS-T1-IDX) 
           END-UNSTRING.           

           MOVE FD-JOB       TO WS-T1-JOB(WS-T1-IDX).
           MOVE FD-EDLEVEL   TO WS-T1-EDLEVEL(WS-T1-IDX).
           MOVE FD-GENDER    TO WS-T1-GENDER(WS-T1-IDX).

           UNSTRING FD-BIRTHDATE DELIMITED BY '-'
              INTO WS-T1-BD-Year(WS-T1-IDX) 
                   WS-T1-BD-Month(WS-T1-IDX) 
                   WS-T1-BD-Day(WS-T1-IDX) 
           END-UNSTRING.

           MOVE FUNCTION NUMVAL-C(FD-SALARY) TO
              WS-T1-SALARY(WS-T1-IDX). 

           MOVE FUNCTION NUMVAL-C(FD-BONUS) TO
              WS-T1-BONUS(WS-T1-IDX).

           MOVE FUNCTION NUMVAL-C(FD-COMM) TO
              WS-T1-COMM(WS-T1-IDX).



       1019-Verify-T1-Table.
      D    DISPLAY "WS-T1-Table: "
      D    PERFORM VARYING WS-T1-IDX FROM 1 BY 1 
      D       UNTIL WS-T1-IDX > WS-T1-Occurs-Dep-Counter
      D       DISPLAY WS-T1-Table(WS-T1-IDX)
      D    END-PERFORM.
           IF WS-T1-Occurs-Dep-Counter >
              WS-T1-Max-Element-Counter
                 DISPLAY "** ERROR **: 1019-Verify-T1-Table"
                 DISPLAY "WS table size is too small for file."
                 DISPLAY "Increase WS-T1-Table-Storage variables."
                 MOVE +9 TO RETURN-CODE
                 GOBACK
           END-IF.

       2000-Process.
           DISPLAY "2000-Process: ".
           DISPLAY "Normally, I would have something to do here".


       3000-End-Job.
           DISPLAY "3000-EOJ: ".
           DISPLAY "Normally, I would have something to do here".











