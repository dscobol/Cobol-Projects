       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BDS0804.
      * This program demonstrates how to read variable length records.
      * It also demonstrates how a file may be assigned its actual name
      * at run time rather than compile time (dynamic vs static).
      * Since the record buffer is a fixed 40 characters in size but
      * the names are variable length Reference Modification is used
      * to extract only the characters in the name from the record buffer
      * from the record buffer.
      *
      * I will not be able to run this on ZOS or MVS because of the
      * ACCEPT verb.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Long-Name-File
              ASSIGN TO Name-Of-File
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  Long-Name-File
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS V
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE
      * Had to add the next line to get it to run on gnuCOBOL
      * else it would not compile:
      * error: RECORD VARYING specified without limits,
      *        but implied limits are equal
           FROM 1 TO 40 CHARACTERS
           DEPENDING ON Name-Length.
       01  Long-Name-Rec          PIC X(40).
           88 End-Of-Names        VALUE HIGH-VALUES.


       WORKING-STORAGE SECTION.
       01  Name-Length           PIC 99.
       01  Name-Of-File           PIC X(20).
       01  StudentRec.
           05 StudentId       PIC 9(7).
           05 StudentName.
              10 Forename     PIC X(9).
              10 Surname      PIC X(12).
           05 DateOfBirth.
            08 YOB           PIC 9(4).
            08 MOBandDOB.
              10 MOB          PIC 99.
              10 DOB          PIC 99.
           05 CourseId        PIC X(5).
           05 GPA             PIC 9V99.

       PROCEDURE DIVISION.
       0000-Mainline.
           Perform 1000-BOJ.
           Perform 2000-Process.
           Perform 3000-EOJ.
           Goback.

       1000-BOJ.
           DISPLAY "Enter the name of the file :- "
              WITH NO ADVANCING
           ACCEPT Name-Of-File.
           OPEN INPUT Long-Name-File.
           Perform 5000-Read-Input-File.

       2000-Process.
           PERFORM UNTIL End-Of-Names
              DISPLAY "***" Long-Name-Rec(1:Name-Length) "***"
              Perform 5000-Read-Input-File
           END-PERFORM.

       3000-EOJ.
           CLOSE Long-Name-File.

       5000-Read-Input-File.
           READ Long-Name-File
              AT END SET End-Of-Names TO TRUE
           END-READ.
