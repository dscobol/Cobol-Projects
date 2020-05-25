       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID. BDS0702.                                             00000200
       ENVIRONMENT DIVISION.                                            00000300
       INPUT-OUTPUT SECTION.                                            00000400
       FILE-CONTROL.                                                    00000500
           SELECT EMPFILE ASSIGN TO DA-S-EMPFILE.                       00000600
                                                                        00000700
       DATA DIVISION.                                                   00000800
       FILE SECTION.                                                    00000900
       FD  EMPFILE                                                      00001000
           LABEL RECORDS ARE STANDARD                                   00001100
           RECORDING MODE IS F                                          00001200
           BLOCK CONTAINS 0 RECORDS                                     00001300
           RECORD CONTAINS 43 CHARACTERS.                               00001400
       01  EMPDETAILS.                                                  00001500
           02 EMPSSN              PIC 9(9).                             00001600
           02 EMPNAME.                                                  00001700
              03 EMPSURNAME       PIC X(15).                            00001800
              03 EMPFORENAME      PIC X(10).                            00001900
           02 EMPDATEOFBIRTH.                                           00002000
              03 EMPYOB           PIC 9(4).                             00002100
              03 EMPMOB           PIC 99.                               00002200
              03 EMPDOB           PIC 99.                               00002300
           02 EMPGENDER           PIC X.                                00002400
                                                                        00002500
       WORKING-STORAGE SECTION.                                         00002600
       01  FILE-STATUS.                                                 00002700
           15 WS-EOF              PIC X(1) VALUE 'N'.                   00002800
                                                                        00002900
       PROCEDURE DIVISION.                                              00003000
       0000-MAINLINE.                                                   00003100
           PERFORM 1000-BOJ.                                            00003200
           PERFORM 2000-PROCESS UNTIL WS-EOF = 'Y'.                     00003300
           PERFORM 3000-EOJ.                                            00003400
                                                                        00003500
       1000-BOJ.                                                        00003600
           OPEN INPUT EMPFILE.                                          00003700
           READ EMPFILE                                                 00003800
               AT END MOVE 'Y' TO WS-EOF.                               00003900
                                                                        00004000
                                                                        00004100
       2000-PROCESS.                                                    00004200
           DISPLAY EMPFORENAME SPACE EMPSURNAME SPACE '- '              00004300
      -    EMPMOB '/' EMPDOB '/' EMPYOB.                                00004400
           READ EMPFILE                                                 00004500
               AT END MOVE 'Y' TO WS-EOF.                               00004600
                                                                        00004700
                                                                        00004800
       3000-EOJ.                                                        00004900
           CLOSE EMPFILE.                                               00005000
           STOP RUN.                                                    00005100
                                                                        00005200
