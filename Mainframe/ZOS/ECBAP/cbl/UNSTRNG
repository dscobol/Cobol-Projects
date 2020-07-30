      *****************************************************************
      * Program name:    UNSTRNG
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 2020-07-29 MYNAME        Created for ECBAP class
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNSTRNG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT-NAME-ADDR.
           05 FILLER     PIC  X(060) VALUE
           'Jonathan Sayles, 14 Belmont Rd, Glen Rock, NJ, 07452'.
       01  WS-OUTPUT-UNSTR-1.
           05   NAME-O          PIC X(25).
           05   ADDR-O          PIC X(20).
           05   CITY-O          PIC X(15).
           05   STATE-O         PIC X(02).
           05   ZIP-O           PIC X(05).
       77  WS-DELIMITER1      PIC  X(01) VALUE ','.
       01  EMP-RECORD.
           05   FNAME           PIC X(10).
           05   LNAME           PIC X(10).
           05   MIDINIT         PIC X(01).
       01  EMP-FULLNAME         PIC X(21).
       77  ALL-FIELDS-1            PIC X(20) VALUE
            'AB*CDEF*GHI*JKLM*NO'.
       01  MISC-FIELDS.
           05   VAR-1            PIC X(05).
           05   VAR-2            PIC X(05).
           05   VAR-3            PIC X(05).
           05   VAR-4            PIC X(05).
           05   VAR-5            PIC X(05).
       01  MISC-FIELDS.
           05   VAR-6            PIC X(05) VALUE 'AB*CD'.
           05   VAR-7            PIC X(05) VALUE 'EF*GH'.
           05   VAR-8            PIC X(05) VALUE 'IJ*KL'.
           05   VAR-9            PIC X(05) VALUE 'MN*OP'.
           05   VAR-0            PIC X(05) VALUE 'QR*ST'.
       77  ALL-FIELDS-2          PIC X(30).
       PROCEDURE DIVISION.
           MOVE 'PHILLIP MCGRAW C' TO EMP-FULLNAME.
           UNSTRING EMP-FULLNAME DELIMITED BY SPACES
                INTO FNAME, LNAME, MIDINIT
           END-UNSTRING.

           UNSTRING WS-INPUT-NAME-ADDR DELIMITED BY WS-DELIMITER1
                    INTO NAME-O, ADDR-O, CITY-O, STATE-O, ZIP-O
           END-UNSTRING.
           UNSTRING ALL-FIELDS-1 DELIMITED BY '*'
           INTO VAR-1, VAR-2, VAR-3, VAR-4, VAR-5.
           STRING VAR-6, VAR-7, VAR-8,  VAR-9, VAR-0
                DELIMITED BY SIZE
                INTO ALL-FIELDS-2.
           GOBACK.

