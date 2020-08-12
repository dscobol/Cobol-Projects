        IDENTIFICATION DIVISION.
        PROGRAM-ID. base_ibm_cics.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 OUT-MSG.
         02 FILLER PIC X(6) VALUE "Hello ".
         02 MSG    PIC X(20).
        01 INP-MSG PIC X(20) VALUE "What is your name? ".
        PROCEDURE DIVISION.
        PARA1.
            EXEC CICS
                 SEND FROM(INP-MSG)
                 ERASE
            END-EXEC

            EXEC CICS
                 RECEIVE INTO (MSG)
            END-EXEC

       *
            EXEC CICS
                 SEND FROM (OUT-MSG)
                 ERASE
            END-EXEC

            EXEC CICS
                 RETURN
            END-EXEC.

       *
        END PROGRAM base_ibm_cics.
