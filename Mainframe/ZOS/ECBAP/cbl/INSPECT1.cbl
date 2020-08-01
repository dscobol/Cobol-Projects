      *****************************************************************
      * Program name:    INSPECT1
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Created for ECBAP class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT1.
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
       77  ALL-FIELDS-1    PIC X(40)  VALUE 'AB*CDEF*GHI*JKLM*NO'.
       77  WS-COB-CNTR        PIC S9(03) VALUE +0.
       77  WS-COB-DATA         PIC X(16)  VALUE 'COBOL-IS-SO-EAZY'.
       77  WS-CNTR1        PIC S9(03) VALUE +0.
       77  WS-CNTR2        PIC S9(03) VALUE +0.
       77  WS-CNTR3        PIC S9(03) VALUE +0.
       77  WS-DATA         PIC  X(11) VALUE '00ACADEMY00'.
       77  WS-INSP         PIC  X(11) VALUE 'WHEELAXLE'.
       01  INSPECT-TALLY-FIELDS.
           05 TALLY-1              PIC S9(05) COMP-3.
           05 TALLY-2              PIC S9(05) COMP-3.
       01  MISC-FIELDS.
           05   ABCD            PIC X(05) VALUE 'AB*CD'.
           05   EFGH            PIC X(05) VALUE 'EF*GH'.
           05   IJKL            PIC X(05) VALUE 'IJ*KL'.
           05   MNOP            PIC X(05) VALUE 'MN*OP'.
           05   QRST            PIC X(05) VALUE 'QR*ST'.
           05   NUM-FLD         PIC X(05) VALUE '1'.
           05  PRIMARY-DIAGNOSTIC-CODE PIC X(5).
       PROCEDURE DIVISION.
      **1. Use INSPECT to determine if we have a valid Diagnostic Code
      **   Bytes1 one => four must be either: DIAG or DXX9
      **   Create test data for all three scenarios
           MOVE ZERO TO TALLY-1 TALLY-2.
           INSPECT PRIMARY-DIAGNOSTIC-CODE
           TALLYING TALLY-1  FOR LEADING 'DIAG'
           TALLY-2  FOR LEADING 'DXX9'
           IF TALLY-1 NOT = 1 AND TALLY-2 NOT = 1
             DISPLAY "*** INVALID DIAGNOSTIC-CODE-PRIMARY".
      **2. Debug and Monitor WS-CNTRn - reconcile with the INSPECT
      **   statement's logic
           MOVE 0 TO WS-CNTR1, WS-CNTR2, WS-CNTR3.
           INSPECT WS-INSP TALLYING
           WS-CNTR1 FOR ALL 'E',
           WS-CNTR2 FOR LEADING 'W'
           WS-CNTR3 FOR CHARACTERS.
      **3. Same as #2 - except Monitor WS-COB-CNTR
           MOVE 0 TO WS-COB-CNTR.
           INSPECT WS-COB-DATA TALLYING WS-COB-CNTR
           FOR LEADING 'Z'
           REPLACING FIRST 'I' BY '2' AFTER INITIAL 'C'.
      **4. Same as #2 - except Monitor WS-CNTR2
           MOVE 0 TO WS-CNTR2.
           INSPECT WS-DATA TALLYING WS-CNTR2
           FOR LEADING '0' REPLACING
           FIRST 'A' BY '2' AFTER INITIAL 'C'
      **5. Same as #2 - except Monitor WS-LENGTH-RESULT
      **   Used to obtain the value of a non-space value in a field
           MOVE 0 TO WS-LENGTH-RESULT.
           INSPECT  ALL-FIELDS-1 TALLYING WS-LENGTH-RESULT
              FOR CHARACTERS BEFORE SPACE.
           GOBACK.

