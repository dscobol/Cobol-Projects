      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CALLILBO.
       AUTHOR.        ERRORPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01   MONTH-IN                PIC S9(02)   COMP.
           88 VALID-MONTH VALUES ARE 1 THRU 12.
       01   WS-USER-ABEND-CODE      PIC S9(04)   COMP.
       PROCEDURE DIVISION.
      *--- EDIT PARM ---
           IF  VALID-MONTH
               CONTINUE
           ELSE
               DISPLAY ' '
               DISPLAY '*************************************'
               DISPLAY '***   <program> INVALID PARM      ***'
               DISPLAY '***                               ***'
               DISPLAY '*** PARM MONTH (MUST BE 01 - 12).....'
                        MONTH-IN
               DISPLAY '***                               ***'
               DISPLAY '*************************************'
               MOVE  32                TO WS-USER-ABEND-CODE
               CALL 'ILBOABN0'      USING WS-USER-ABEND-CODE
           END-IF.