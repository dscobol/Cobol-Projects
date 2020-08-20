      ***********************************************************
      * Program name:    SORT1F
      * Original author: dastagg
      *
      * Description: Program to sort tables.
      *    This version will just sort with USING and GIVING.
      *
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------  ------------  --------------------------------
      * 2020-08-16 dastagg       Created for ECBAP class
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SORT1F.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFile
           ASSIGN TO 
           "../../../Cobol-Projects/common/data/customer.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTFile
           ASSIGN TO SORTWK.

           SELECT OUTFile
           ASSIGN TO "../spool/cust-sort1f-out.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

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
      *    Note: no File-Status or Counters.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
      *    Because it is just USING and GIVING, file opening and 
      *    closing is not needed. The OS handles that.
           DISPLAY "1000-Begin-Job: ".
           DISPLAY "Normally, I would have something to do here".

       2000-Process.
           SORT SORTFile
              ON ASCENDING KEY
                 SORTFile-Cust-Last-Name
                USING INFile
                GIVING OUTFile.
           IF SORT-RETURN > 0
              DISPLAY '*** WARNING ***'
              DISPLAY '  SORT FAILED  '
           END-IF.

       3000-End-Job.
           DISPLAY "3000-End-Job: ".
           DISPLAY "Normally, I would have something to do here".
