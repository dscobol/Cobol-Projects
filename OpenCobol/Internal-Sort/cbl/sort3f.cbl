      ***********************************************************
      * Program name:    SORT3F
      * Original author: dastagg
      *
      *    Overview: This is a series of simple examples demonstrating
      *       COBOL Internal Sorting.
      *
      *    List:
      *        SORT1F: Just USING and GIVING
      *        SORT2F: Input Procedure and GIVING
      *        SORT3F: USING and Output Procedure
      *        SORT4F: Input Procedure and Output Procedure.
      *
      *    Description: Program to use COBOL's internal sort 
      *       facility to sort and process datasets.
      *
      *    This version will sort with an USING and Output Procedure.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  --------------------------------
      * 2020-08-16 dastagg       Created to learn.
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SORT3F.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFile
           ASSIGN TO "../data/customer.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-INFile-Status.

           SELECT SORTFile
           ASSIGN TO SORTWK.

           SELECT OUTFile
           ASSIGN TO "../spool/cust-sort3f-out.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-OUTFile-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  INFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CUSTOMER REPLACING ==:tag:== BY ==INFile==.

       SD  SORTFile.
           COPY CUSTOMER REPLACING ==:tag:== BY ==SORTFile==.

       FD OUTFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CUSTOMER REPLACING ==:tag:== BY ==OUTFile==.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==INFile==.
           COPY WSFST REPLACING ==:tag:== BY ==SORTFile==.
           COPY WSFST REPLACING ==:tag:== BY ==OUTFile==.

       01  WS-File-Counters.
           12 FD-INFile-Record-Cnt         PIC S9(4) COMP VALUE ZERO.
           12 SD-SORTFile-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-OUTFile-Record-Cnt        PIC S9(4) COMP VALUE ZERO.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           DISPLAY "1000-Begin-Job: ".
           DISPLAY "Normally, I would have something to do here".

       2000-Process.
           SORT SORTFile
              ON ASCENDING KEY
                 SORTFile-Cust-State
                USING INFile
                OUTPUT PROCEDURE 2200-Output-Procedure.
           IF SORT-RETURN > 0
              DISPLAY '*** WARNING ***'
              DISPLAY '  SORT FAILED  '
           END-IF.

       2200-Output-Procedure.
      *    Note: Open output here, close at End-Job.
           OPEN OUTPUT OUTFile.
           SET WS-SORTFile-Good TO TRUE.
           PERFORM UNTIL WS-SORTFile-EOF
              PERFORM 8000-Return-SortFile
              PERFORM 2210-Process-OUTFile-Record
           END-PERFORM.

      *    Whatever processing that needs to place after the
      *       record comes back from being sorted takes place
      *       in this paragraph.
       2210-Process-OUTFile-Record.
           MOVE SORTFile-Customer-Record TO
              OUTFile-Customer-Record.
              PERFORM 6000-Write-OutFile.

       3000-End-Job.
           DISPLAY "3000-End-Job: ".
           CLOSE OUTFile.

       6000-Write-OutFile.
           WRITE OUTFile-Customer-Record.
           IF WS-OutFile-Good
              ADD +1 TO FD-OUTFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE OutFile Failed."
              DISPLAY "File Status: " WS-OutFile-Status
              GOBACK
           END-IF.

       8000-Return-SortFile.
           RETURN SORTFile
               AT END SET WS-SORTFile-EOF TO TRUE
           END-RETURN.
           IF WS-SORTFile-Good
              ADD +1 TO SD-SORTFile-Record-Cnt
           ELSE
              IF WS-SORTFile-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 8000-Return-SortFile"
                 DISPLAY "Return SORTFile Failed."
                 DISPLAY "File Status: " SORT-RETURN
                 GOBACK
              END-IF
           END-IF.
