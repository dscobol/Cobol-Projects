       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO2.
      * Comment: This program Displays a number of text strings
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE
           ASSIGN TO NEG1.
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS In-Rec.
       01  IN-REC.
           05 FLD-IN       PIC S9(5).
           05 FILLER       PIC X(75).
       WORKING-STORAGE SECTION.
       77  FLD1           PIC X(1) VALUE 'A'.
           88 A-VAL   VALUE 'A'.
       77  FLD2           PIC 9(2) VALUE 2.
       PROCEDURE DIVISION.
           EVALUATE TRUE
           WHEN A-VAL COMPUTE FLD2 = FLD2 * 10
           END-EVALUATE.

           GOBACK.







