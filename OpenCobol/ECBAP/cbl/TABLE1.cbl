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

       2000-Process.
           DISPLAY "2000-Process: ".
           DISPLAY "Normally, I would have something to do here".


       3000-End-Job.
           DISPLAY "3000-EOJ: ".
           DISPLAY "Normally, I would have something to do here".











