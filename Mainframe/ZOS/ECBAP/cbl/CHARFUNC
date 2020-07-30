      *****************************************************************
      * Program name:    CHARFUNC
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Created for ECBAP class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHARFUNC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NAME-ADDR.
           05 FILLER     PIC  X(060) VALUE
           'Jonathan Sayles, 14 Belmont Rd, Glen Rock, NJ, 07452'.
       01  OUTPUT-FLDS.
           05 WS-LENGTH-RESULT      PIC S9(09).
           05 WS-MAX-RESULT         PIC X(10).
           05 WS-MIN-RESULT         PIC X(10).
           05 WS-UPPER-RESULT       PIC X(40).
           05 WS-LOWER-RESULT       PIC X(40).
           05 WS-REVERSE-RESULT      PIC X(40).
           05 WS-NUMVAL-RESULT-1     PIC 9(09).
           05 WS-NUMVAL-RESULT-2     PIC 9(09).
           05  WS-NUMVAL-TEST-1      PIC X(10) VALUE '$9020.44'.
           05  WS-NUMVAL-TEST-2      PIC $$9,999.99 VALUE '$9020.44'.
       77  ALL-FIELDS-1   PIC X(40) VALUE 'AB*CDEF*GHI*JKLM*NO'.
       01  MISC-FIELDS.
           05   ABCD            PIC X(05) VALUE 'AB*CD'.
           05   EFGH            PIC X(05) VALUE 'EF*GH'.
           05   IJKL            PIC X(05) VALUE 'IJ*KL'.
           05   MNOP            PIC X(05) VALUE 'MN*OP'.
           05   QRST            PIC X(05) VALUE 'QR*ST'.
           05   NUM-FLD         PIC X(05) VALUE '1'.
       PROCEDURE DIVISION.
           MOVE FUNCTION MAX (ABCD, EFGH, IJKL, MNOP, QRST, NUM-FLD)
                TO WS-MAX-RESULT.
           MOVE FUNCTION MIN (ABCD, EFGH, IJKL, MNOP, QRST, NUM-FLD)
                TO WS-MIN-RESULT.
           MOVE FUNCTION UPPER-CASE(NAME-ADDR)
                TO WS-UPPER-RESULT.
           MOVE FUNCTION LOWER-CASE(NAME-ADDR)
                TO WS-LOWER-RESULT.
           MOVE FUNCTION REVERSE (FUNCTION LOWER-CASE (NAME-ADDR) )
                TO WS-REVERSE-RESULT.
           COMPUTE WS-NUMVAL-RESULT-1
              =  FUNCTION NUMVAL-C(WS-NUMVAL-TEST-1) * 2.
           COMPUTE WS-NUMVAL-RESULT-2
              =  FUNCTION NUMVAL-C(WS-NUMVAL-TEST-2) * 2.
           COMPUTE WS-LENGTH-RESULT =  FUNCTION LENGTH (NAME-ADDR).
           MOVE 0 TO WS-LENGTH-RESULT.
           INSPECT  ALL-FIELDS-1 TALLYING WS-LENGTH-RESULT
              FOR CHARACTERS BEFORE SPACE.
           GOBACK.
