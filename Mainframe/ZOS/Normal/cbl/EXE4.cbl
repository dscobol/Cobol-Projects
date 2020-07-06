      *****************************************************************
      * Program name:    EXE4
      * Original author: Ari
      *
      * Maintenence Log
      * Date      Author
      * --------- ------------  ---------------------------------------
      * 03/06/20 MYNAME  Created for COBOL class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EXE4.
       AUTHOR. Ari.
​
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * Source-Computer. IBM-PC WITH DEBUGGING MODE.
       INPUT-OUTPUT SECTION.
       File-Control.
           SELECT MyWaetherCSV
           ASSIGN to WEATIN.
           SELECT MyWaether
           ASSIGN TO WEATOUT.
​
​
​
​
​
       DATA DIVISION.
       FILE SECTION.
​
       FD  MyWaetherCSV RECORDING MODE F.
​
​
       01  MyDATA pic x(80) .
​
​
​
        FD MyWaether RECORDING MODE F.
       01  MyDATAOutput .
            05 StationNumber   pic x(10).
            05 Month           pic x(10).
            05 ADay            pic x(10).
            05 Year            pic x(10).
            05 MeanTemp        pic x(10).
            05 Fog             pic x(10).
            05 Rian            pic x(10).
            05 Snow            pic x(10).
             05 Hail           pic x(10).
            05 Thunder         pic x(10).
            05 Tornado         pic x(10).
           
​
​
       WORKING-STORAGE SECTION.
         
​
       
​
         01 FLAGS.
           05 LASTREC           PIC X VALUE SPACE.
         01 FILE-CHECK          pic x(2).
         01 ECP           pic 9(4) BINARY VALUE 1140.
         01 ACP             pic 9(4) BINARY VALUE 819.
​
         01 Header1.
           05 FILLER   pic x(5) VALUE 'W  ID'  .
           05 FILLER  pic x(3) VALUE SPACES.
           05 FILLER pic x(9) VALUE  '  P NAME'.
           05 FILLER  pic x(3) VALUE SPACES.
           05 FILLER pic x(10) VALUE  '  F NAME '.
           05 FILLER  pic x(4)  VALUE 'YEAR'.
           05 FILLER  pic x(2) VALUE SPACES.
           05 FILLER pic x(6) VALUE 'MONTH ' .
           05 FILLER  pic x(2) VALUE SPACES.
           05 FILLER  pic  x(3) VALUE 'DAY' .
           05 FILLER  pic x(1) VALUE SPACES.
           05 FILLER pic x(10) VALUE 'WORKER NUM' .
           05 FILLER  pic x(1) VALUE SPACES.
           05 FILLER  pic x(6) VALUE 'GENDER'.
​
         01 Header2.
           05 FILLER PIC x(10) value '**********'.
           05 FILLER PIC x(47) value
            '***********************************************'.
           05 FILLER PIC x(10) value '**********'.
​
​
​
       PROCEDURE DIVISION.
           
​
       Open-Files.
      D      DISPLAY 'Before OPENING'.
            OPEN INPUT MyWaetherCSV .
            OPEN OUTPUT MyWaether  .
      D      DISPLAY 'Succees OPENING'.
     
​
​
​
​
       ReadNextRecord.
            PERFORM READ-RECORD
            PERFORM  UNTIL LASTREC = 'Y'
            PERFORM Move_To_Output
            PERFORM READ-RECORD
            END-PERFORM.
       CLOSE-STOP.
           CLOSE MyWaetherCSV  .
           CLOSE MyWaether  .
​
​
           STOP RUN.
​
       READ-RECORD.
           READ MyWaetherCSV 
           AT END MOVE 'Y' TO LASTREC
           END-READ.
       Move_To_Output.
             MOVE SPACES to MyDATAOutput.
​
              UNSTRING  MyDATA  DELIMITED BY ',' 
             INTO  StationNumber  Month 
		               ADay  Year  MeanTemp  Fog 
                   Rian Snow   Hail   Thunder   Tornado  .
​
             
​
​
             WRITE MyDATAOutput .
​