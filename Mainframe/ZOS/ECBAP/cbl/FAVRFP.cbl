       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RFPIN  ASSIGN TO RFPIN.
           SELECT PROPOSAL ASSIGN TO PROPOSAL.
       DATA DIVISION.
       FILE SECTION.
       FD  RFPIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RFP-REC.
      *
       01 RFP-REC.
          05 ARTIST-ACCT-NO              PIC X(08).
          05 ARTIST-MUSICAL-GENRE        PIC X(06).
             88 ROCK   VALUE 'ROCK'.
             88 JAZZ   VALUE 'JAZZ'.
             88 FUSION VALUE 'FUSION'.
          05 MUSICIAN.
             10 MUSICIAN-LNAME           PIC X(15).
             10 MUSICIAN-FNAME           PIC X(15).
          05 MUSICIAN-INSTRUMENT-TYPE    PIC X(06).
             88 KEYBOARD       VALUE 'KEYS'.
             88 VOCALS      VALUE 'VOCALS'.
             88 GUITAR      VALUE 'GUITAR'.
             88 BASS        VALUE 'BASS'.
             88 DRUMS       VALUE 'DRUMS'.
             88 PERCUSSION  VALUE 'PERC'.
             88 KEYS        VALUE 'KEYS'.
          05 INSTRUMENT-QUALITY          PIC X(01).
             88 USED-FLAG    VALUE 'U'.
             88 NEW-FLAG     VALUE 'N'.
             88 PREMIUM-FLAG VALUE 'P'.
          05 MAX-MUSICIAN-BUDGET-AMOUNT  PIC 9(5)V99.
          05 SHIP-TO                     PIC X(03).
             88 IN-COUNTRY     VALUE 'IN'.
             88 OUT-OF-COUNTRY VALUE 'OUT'.
          05 FILLER                      PIC X(19).

      *
       FD  PROPOSAL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PROPOSAL-REC.
      *
       01 PROPOSAL-REC.
          05 ARTIST-ACCT-NO-O            PIC X(08).
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 ARTIST-MUSICAL-GENRE-O      PIC X(06).
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 MUSICIAN-NAME-O             PIC X(21).
      *      10 MUSICIAN-LNAME-O         PIC X(15).
      *      10 MUSICIAN-FNAME-O         PIC X(15).
          05 FILLER                      PIC X(01) VALUE SPACES.
          05 MUSICIAN-INSTRUMENT-TYPE-O  PIC X(12).
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 INSTRUMENT-QUALITY-O        PIC X(10).
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 SHIP-TO-O                   PIC X(03).
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 BASE-PRICE-O                PIC $Z,ZZZ,ZZ9.99.
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 ADJ-GROUP-O.
             10 ADJ-SIGN-O               PIC X(02).
      *      10 FILLER                   PIC X(01) VALUE SPACE.
             10 ADJ-AMOUNT-O             PIC $ZZZ,ZZ9.99.
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 ADJ-PRICE-O                 PIC $Z,ZZZ,ZZ9.99.
          05 FILLER                      PIC X(03).
          05 TAX-O                       PIC $ZZ,ZZ9.99.
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 SHIPPING-COST-O             PIC $ZZZ,ZZ9.99.
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 COST-PER-INSTRUMENT-O       PIC $Z,ZZZ,ZZ9.99.
          05 FILLER                      PIC X(03) VALUE SPACES.
          05 MAX-BUDGET-O                PIC $ZZ,ZZ9.99.
          05 FILLER                      PIC X(01) VALUE SPACES.
      *
       WORKING-STORAGE SECTION.
       01 WS-TEMP-VARIABLES.
          05 RFPIN-EOF           PIC X(01) VALUE SPACES.
          05 WS-TAX              PIC 9(6)V999.
          05 WS-SHIPPING-COST    PIC 9(6)V999.
          05 WS-PRICE            PIC 9(8)V99.
          05 WS-TOTAL-COST       PIC 9(8)V999.
          05 WS-ADJ-AMOUNT       PIC 9(6)V99.
          05 WS-ADJUSTED-PRICE   PIC 9(8)V999.
      *
*      01 HEADER-1-REC.
          05 FILLER PIC X(77) VALUE SPACES.
          05 FILLER PIC X(46)  VALUE
             'MUSICAL INSTRUMENT REQUEST FOR PROPOSAL REPORT'.
      *
       01 HEADER-2-REC.
          05 FILLER PIC X(77) VALUE SPACES.
          05 FILLER PIC X(46)  VALUE
             '----------------------------------------------'.
      *
*      01 HEADER-3-REC.
          05 FILLER PIC X(08) VALUE 'ACCT NO.'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(06) VALUE 'GENRE '.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(21) VALUE 'MUSICIAN NAME        '.
      *   05 FILLER PIC X(15) VALUE 'LAST NAME      '.
      *   05 FILLER PIC X(01) VALUE SPACES.
      *   05 FILLER PIC X(15) VALUE 'FIRST NAME     '.
          05 FILLER PIC X(01) VALUE SPACES.
          05 FILLER PIC X(12) VALUE 'INSTR. TYPE '.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(10) VALUE 'QUALITY   '.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(03) VALUE 'CTY'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(13) VALUE 'BASE PRICE   '.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(11) VALUE 'ADJUST AMT.'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(02) VALUE SPACES.
          05 FILLER PIC X(13) VALUE 'ADJUST PRICE '.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(10) VALUE 'TAX       '.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(11) VALUE 'SHIPPING  '.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(13) VALUE 'TOTAL PRICE  '.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(10) VALUE 'MAX BUDGET'.
      *
       01 HEADER-4-REC.
          05 FILLER PIC X(08) VALUE '--------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(06) VALUE '------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(21) VALUE '---------------------'.
      *   05 FILLER PIC X(15) VALUE '---------------'.
      *   05 FILLER PIC X(01) VALUE SPACES.
      *   05 FILLER PIC X(15) VALUE '---------------'.
          05 FILLER PIC X(01) VALUE SPACES.
          05 FILLER PIC X(12) VALUE '------------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(10) VALUE '----------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(03) VALUE '---'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(13) VALUE '-------------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(02) VALUE '--'.
          05 FILLER PIC X(11) VALUE '-----------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(13) VALUE '-------------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(10) VALUE '----------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(11) VALUE '-----------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(13) VALUE '-------------'.
          05 FILLER PIC X(03) VALUE SPACES.
          05 FILLER PIC X(10) VALUE '----------'.

      *
       01 HEADER-REC-BLANK.
          05 FILLER PIC X(200) VALUE SPACES.
      *
       01 TRAILER-REC-1.
          05 FILLER PIC X(80) VALUE
             '* ADJUSTMENT AMOUNT CALCULATED AS FOLLOWS:'.
      *
       01 TRAILER-REC-2.
          05 FILLER PIC X(92) VALUE
             ' -ADD 20% TO BASE PRICE IF INSTR. QUALITY IS PREMIUM'.
      *
       01 TRAILER-REC-3.
          05 FILLER PIC X(101) VALUE
            ' -SUBTRACT 20% FROM BASE PRICE IF INSTR. QUALITY IS USED'.
      *
       01 TRAILER-REC-4.
          05 FILLER PIC X(100) VALUE
             ' -NO ADJUSTMENT TO BASE PRICE IF INSTR. QUALITY IS NEW'.
      *
       01 TRAILER-REC-5.
          05 FILLER PIC X(90) VALUE
             '* ADJUSTED PRICE = BASE PRICE + ADJUSTMENT AMOUNT'.
      *
       01 TRAILER-REC-6.
          05 FILLER PIC X(89) VALUE
             '* TAX IS CALCUATED AS FOLLOWS:  8% * ADJUSTED PRICE'.
      *
       01 TRAILER-REC-7.
          05 FILLER PIC X(72) VALUE
             '* SHIPPING AMOUNT CALCULATED AS FOLLOWS:'.
      *
       01 TRAILER-REC-8.
          05 FILLER PIC X(93) VALUE
             ' -10% OF (ADJUSTED PRICE + TAX) IF CLIENT IS IN COUNTRY'.
      *
       01 TRAILER-REC-9.
          05 FILLER PIC X(94) VALUE
            ' -20% OF (ADJUSTED PRICE + TAX) IF CLIENT OUT OF COUNTRY'.
      *
       01 TRAILER-REC-10.
          05 FILLER PIC X(80) VALUE
             '* TOTAL PRICE = ADJUSTED PRICE + TAX + SHIPPING'.
      *
       01 TRAILER-REC-11.
          05 FILLER PIC X(92) VALUE
             '* THE MAXIMUM BUDGET FIGURE IS PROVIDED BY THE MUSICIAN'.

       01 TRAILER-REC-12.
          05 FILLER PIC X(120) VALUE
             "* NOTE:  FOR REPORTING PURPOSES, THE MUSICIAN FIRST AND LA
      -      "ST NAMES DISPLAY THE FIRST 10 CHARACTERS OF EACH FIELD".

      *
       01 TRAILER-REC-BLANK PIC X(132) VALUE SPACES.

      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PROGRAM READS AN INPUT FILE OF MUSICIAN REQUIREMENTS  *
      *    FOR MUSICAL INSTRMENTS AND CREATES AN RFP COST PROPOSAL    *
      *    REPORT, WHICH IS DISPLAYED IN THE PROPOSAL SYSOUT DD.      *
      *                                                               *
      *  INPUT FILE:                                                  *
      *      <USERID>.LEARN.FAVRPFP2                                  *
      *      INTERNAL FILE NAME:  RFPIN                               *
      *      JCL DD NAME:         RFPIN                               *
      *                                                               *
      *  OUTPUT FILE:                                                 *
      *      SYSOUT JCL SPOOL                                         *
      *      INTERNAL FILE NAME:  PROPOSAL                            *
      *      JCL DD NAME:         PROPOSAL                            *
      *****************************************************************
      *
       PROCEDURE DIVISION.
      *    DISPLAY 'ENTERING PROCEDURE DIVISION'.
      *
      *****************************************************************
      *  PERFORM HOUSEKEEPING (ONE-TIME AND INITIALIZATION FUNCTIONS),*
      *  PROCESS INPUT RECORDS UNTIL END OF FILE, WRITE OUTPUT REPORT *
      *  TRAILER RECORDS AND CLOSE FILES.                             *
      *****************************************************************
      *
           PERFORM 0000-HOUSEKEEPING.
           PERFORM 0100-MAIN
               UNTIL RFPIN-EOF = 'Y'.
           PERFORM 0950-WRITE-TRAILER-RECS.
           PERFORM 1000-CLOSE-FILES.
           GOBACK.

       0000-HOUSEKEEPING.
      *     DISPLAY ' 0000-HOUSEKEEPING'.
      *
      *****************************************************************
      *  INTIALIZE WORKING STORAGE VARIABLES, OPEN INPUT/OUPUT FILES, *
      *  WRITE HEADER RECORDS TO OUTPUT FILE AND READ 1ST RECORD FROM *
      *  INPUT FILE.                                                  *
      *****************************************************************
      *
           INITIALIZE WS-TEMP-VARIABLES.
           PERFORM 0300-OPEN-FILES.
           PERFORM 0450-WRITE-HEADER-REC.
           PERFORM 0400-READ-RFPIN.

       0100-MAIN.
      *    DISPLAY 'ENTERING PARA 0100-MAIN'.
      *
      *****************************************************************
      *  DETERMINE THE COST OF EACH INSTRUMENT, MOVE OUTPUT FIELDS TO *
      *  THE REPORT DETAIL, WRITE THE REPORT DETAIL AND READ THE NEXT *
      *  RECORD.
      *****************************************************************
      *
           PERFORM 0200-DETERMINE-INSTRUMENT-COST.
           PERFORM 0800-DATA-TO-OUTPUT-FIELDS.
           PERFORM 0900-WRITE-PROPOSAL-REC.
           PERFORM 0400-READ-RFPIN.
      *
        0200-DETERMINE-INSTRUMENT-COST.
      *    DISPLAY 'ENTERING PARA 0200-DETERMINE-INSTRUMENT-COST'.
      *
      *****************************************************************
      * THIS PARAGRAPH CONTROLS FUNCTIONALITY FOR THE FOLLOWING:      *
      *                                                               *
      * 1.  DETERMINE BASE PRICE (COST BEFORE PRICE ADJUSTEMENTS, TAX *
      *     AND SHIPPING COST)                                        *
      * 2.  DETERMINE PRICE ADJUSTMENT BASED ON QUALITY OF INSTRUMENT *
      *     (PREMIUM, NEW, USED)                                      *
      * 3.  CALCULATE TAX BASED ON ADJUSTED PRICE OF INSTRUMENT       *
      * 4.  CALCULATE SHIPPING COST BASED ON ADJUSTED PRICE + TAX     *
      *****************************************************************
      *
           PERFORM 0500-ASSIGN-BASE-INSTR-PRICE.
           PERFORM 0550-DETERMINE-QUALITY-COST.
           PERFORM 0600-APPLY-TAX.
      *
      *****************************************************************
      *  ADD TAX TO ADJUSTED PRICE TO GET TOTAL COST.                 *
      *****************************************************************
      *
           COMPUTE WS-TOTAL-COST = WS-ADJUSTED-PRICE + WS-TAX.
      *
      *****************************************************************
      *  ADD SHIPPING COST TO TOTAL COST                              *
      *****************************************************************
      *
           PERFORM 0700-CALCULATE-SHIPPING-COST.

           COMPUTE WS-TOTAL-COST = WS-TOTAL-COST + WS-SHIPPING-COST.

      *
       0300-OPEN-FILES.
      *    DISPLAY 'ENTERING PARA 0300-OPEN-FILES'.
      *
      *****************************************************************
      *  OPEN RFPIN FILE FOR INPUT AND PROPOSAL FILE FOR OUTPUT.      *
      *****************************************************************
      *
           OPEN INPUT  RFPIN.
           OPEN OUTPUT PROPOSAL.
      *
       0400-READ-RFPIN.
      *    DISPLAY 'ENTERING PARA 0400-READ-RFPIN'.
      *
      *****************************************************************
      *  READ RFPIN FILE.  WHEN THE END OF FILE IS DETECTED, MOVE A   *
      *  Y TO A SWITCH TO INDICATE END OF FILE HAS BEEN ENCOUNTERED.  *
      *****************************************************************
      *
           READ RFPIN
               AT END MOVE 'Y' TO RFPIN-EOF
           END-READ.
      *
       0450-WRITE-HEADER-REC.
      *    DISPLAY 'ENTERING PARA 0450-WRITE-HEADER-REC'.
      *
      *****************************************************************
      * WRITE THE HEADER RECORDS TO THE PROPOSAL REPORT AFTER         *
      * ADVANCING THE INDICATED NUMBER OF LINES. WRITING FROM         *
      * HEADER-REC-BLANK WRITES A BLANK LINE TO THE REPORT.           *
      *****************************************************************
      *
            WRITE PROPOSAL-REC FROM HEADER-1-REC
               AFTER ADVANCING 2 LINES.

            WRITE PROPOSAL-REC FROM HEADER-2-REC
               AFTER ADVANCING 1 LINE.

            WRITE PROPOSAL-REC FROM HEADER-REC-BLANK.

            WRITE PROPOSAL-REC FROM HEADER-3-REC
               AFTER ADVANCING 1 LINE.

            WRITE PROPOSAL-REC FROM HEADER-4-REC
               AFTER ADVANCING 1 LINE.

      *
       0500-ASSIGN-BASE-INSTR-PRICE.
      *    DISPLAY 'ENTERING PARA 0500-ASSIGN-BASE-INSTR-PRICE'.
      *
      *****************************************************************
      * THIS PARAGRAPH ASSIGNS THE BASE PRICE OF AN INSTRUMENT, BASED *
      * ON 88 LEVEL VALUES ON THE MUSICIAN-INSTRUMENT-TYPE FIELD      *
      * LISTED IN THE INPUT FILE (RFPIN).                             *
      *                                                               *
      * DURING THE EVALUATION OF THE MUSICAL INSTRUMENT TYPE, OTHER   *
      * FIELDS, WHOSE VALUES ARE DEPENDENT ON THE VALUE OF THIS       *
      * FIELD, ARE MOVED TO PROPOSAL REPORT DETAIL FIELDS, I.E.,      *
      * EXPANDED INSTRUMENT TYPE DESCRIPTIONS (I.E., KEYBOARD FOR 88  *
      * LEVEL 'KEYS') AND INSTRUMENT BASE PRICE.                      *
      *                                                               *
      * BASE PRICES ARE ASSIGNED AS FOLLOWS:                          *
      *   KEYBOARDS:                       PRICE IS   $3,017.89
      *   VOCALS (MICS, AUTO-TUNER, ETC.): PRICE IS     $599.05
      *   GUITAR RIG (GUITAR + AMP):       PRICE IS   $2,648.99
      *   BASS RIG (BASS + AMP):           PRICE IS  $18,761.00
      *   DRUM KIT:                        PRICE IS $308,722.00
      *   PERCUSSION:                      PRICE IS     $799.99
      ******************************************************************
      *
           EVALUATE TRUE
              WHEN VOCALS
                 MOVE 599.05 TO WS-PRICE, BASE-PRICE-O
                 MOVE 'VOCALS' TO MUSICIAN-INSTRUMENT-TYPE-O
              WHEN GUITAR
                 MOVE 2648.99 TO WS-PRICE, BASE-PRICE-O
                 MOVE 'GUITAR RIG' TO MUSICIAN-INSTRUMENT-TYPE-O
              WHEN KEYS
                 MOVE 3017.89 TO WS-PRICE, BASE-PRICE-O
                 MOVE 'KEYBOARD' TO MUSICIAN-INSTRUMENT-TYPE-O
              WHEN BASS
                 MOVE 18761.00 TO WS-PRICE, BASE-PRICE-O
                 MOVE 'BASS RIG' TO MUSICIAN-INSTRUMENT-TYPE-O
              WHEN DRUMS
                 MOVE 308722 TO WS-PRICE, BASE-PRICE-O
                 MOVE 'DRUM KIT' TO MUSICIAN-INSTRUMENT-TYPE-O
              WHEN PERCUSSION
                 MOVE  799.99 TO WS-PRICE, BASE-PRICE-O
                 MOVE 'PERCUSSION' TO MUSICIAN-INSTRUMENT-TYPE-O
           END-EVALUATE.
      *
       0550-DETERMINE-QUALITY-COST.
      *    DISPLAY 'ENTERING PARA 0550-DETERMINE-QUALITY-COST
      *
      *****************************************************************
      * THIS PARAGRAPH CALCULATES THE ADJUSTED INSTRUMENT PRICE BASED *
      * ON INSTRUMENT QUALITY FIELD 88 LEVELS.                        *
      *                                                               *
      *  ADJUSTED INSTRUMENT PRICES ARE CALCULATED AS FOLLOWS:        *
      *    1. "PREMIUM" INSTRUMENT QUALITY                            *
      *         ADJUSTED PRICE = BASE PRICE + (BASE PRICE * .2)       *
      *    2. "NEW" INSTRUMENT QUALITY                                *
      *         ADJUSTED PRICE = BASE PRICE (NO ADJUSTMENT)           *
      *    3. "USED" INSTRUMENT QUALITY                               *
      *         ADJUSTED PRICE = BASE PRICE - (BASE PRICE * .2)       *
      *                                                               *
      * OTHER FIELDS BASED ON THE VALUE OF THE INSTRUMENT QUALITY ARE *
      * MOVED TO PROPOSAL REPORT DETAIL FIELDS:                       *
      *    1.  INSTRUMENT-QUALITY MOVED TO REPORT DETAIL              *
      *    2.  ADJUSTED AMOUNT MOVED TO REPORT DETAIL                 *
      *         (THE AMOUNT ADDED/SUBTRACTED TO/FROM THE BASE PRICE   *
      *    3.  ADJUSTED INSTRUMENT PRICE MOVED TO REPORT DETAIL       *
      *        (PRICE AFTER ADDING/SUBTRACTING AMOUNT TO/FROM BASE    *
      *         PRICE)                                                *
      *    4. ADJUSTMENT INDICATOR                                    *
      *       a. "+ ' IF ADJUSTMENT AMOUNT ADDED TO BASE PRICE        *
      *          (INSTRUMENT QUALITY = PREMIUM)                       *
      *       b. '- ' IF ADJUSTMENT AMOUNT SUBTRACTED FROM BASE PRICE *
      *          (INSTRUMENT QUALITY = USED)                          *
      *       C. BLANK IF NO ADJUSTEMENT AMOUNT ADDED OR SUBTRACTED   *
      *          FROM BASE PRICE (INSTRUMENT QUALITY = "NEW")         *
      * ***************************************************************
      *

      *****************************************************************
      * CALCULATE ADJUSTMENT AMOUNT FOR USED AND PREMIUM INSTRUMENTS  *
      *****************************************************************
      *
           COMPUTE WS-ADJ-AMOUNT = WS-PRICE * .2.

           INITIALIZE ADJ-GROUP-O.  *> CLEAR INDICATOR FROM PRIOR WRITE

           EVALUATE TRUE
              WHEN USED-FLAG
                 COMPUTE WS-ADJUSTED-PRICE = WS-PRICE - WS-ADJ-AMOUNT
                 MOVE WS-ADJ-AMOUNT TO ADJ-AMOUNT-O
                 MOVE '- ' TO ADJ-SIGN-O
                 MOVE 'USED' TO INSTRUMENT-QUALITY-O
              WHEN NEW-FLAG
                 MOVE WS-PRICE TO WS-ADJUSTED-PRICE
                 MOVE 0 TO ADJ-AMOUNT-O
                 MOVE SPACE TO ADJ-SIGN-O
                 MOVE 'NEW' TO INSTRUMENT-QUALITY-O
              WHEN PREMIUM-FLAG
                 MOVE WS-ADJ-AMOUNT TO ADJ-AMOUNT-O
                 MOVE '+ ' TO ADJ-SIGN-O
                 MOVE 'PREMIUM' TO INSTRUMENT-QUALITY-O
                 COMPUTE WS-ADJUSTED-PRICE = WS-PRICE + WS-ADJ-AMOUNT
           END-EVALUATE.


      *
       0600-APPLY-TAX.
      *    DISPLAY 'ENTERING PARA 0600-APPLY-TAX'.
      * ***************************************************************
      *  TAX IS CALCULATED AS A FLAT 8% OF THE COST PER INSTRUMENT    *
      *  (BASE PRICE + ADJUSTMENT AMOUNT)                             *
      *****************************************************************
           COMPUTE WS-TAX ROUNDED = .08 * WS-ADJUSTED-PRICE.
      *
       0700-CALCULATE-SHIPPING-COST.
      *    DISPLAY 'ENTERING PARA 0700-CALCULATE-SHIPPING-COST'.
      *
      *****************************************************************
      * THE SHIPPPING COST IS CALCULATED BASED ON WHETHER THE         *
      * CUSTOMER IS LOCATED "IN" OR "OUT" OF COUNTRY.                 *
      *   1.  IN-COUNTRY: SHIPPING = 10% OF TOTAL COST                *
      *       (ADJUSTED PRICE + TAX)                                  *
      *   2.  OUT-OF-COUNTRY: SHIPPING = 20% OF TOTAL COST            *
      *       (AQJUSTED PRICE + TAX)                                  *
      *****************************************************************
      *
           IF IN-COUNTRY
              COMPUTE WS-SHIPPING-COST = WS-TOTAL-COST  * .10
           ELSE
              IF OUT-OF-COUNTRY
                 COMPUTE WS-SHIPPING-COST = WS-TOTAL-COST * .20
              END-IF
           END-IF.
      *
       0800-DATA-TO-OUTPUT-FIELDS.
      *    DISPLAY 'ENTERING PARA 0800-DATA-TO-OUTPUT-FIELDS'.
      *
      *****************************************************************
      * REMAINING INPUT FILE, WORKING STORAGE AND CALCUATED FIELDS    *
      * ARE MOVED TO THE PROPOSAL DETAIL LINE FIELDS.  COMMENT LINES  *
      * BELOW INDICATE FIELDS THAT WERE MOVED IN PREVIOUS PARAGRAPHS  *
      *****************************************************************
      *
           MOVE ARTIST-ACCT-NO TO ARTIST-ACCT-NO-O.
           MOVE ARTIST-MUSICAL-GENRE TO ARTIST-MUSICAL-GENRE-O.
           INITIALIZE MUSICIAN-NAME-O. *> CLEAR FIELD OF PRIOR VALUES

      *****************************************************************
      * CREATE THE MUSICIAN NAME FIELD BY CONCATENATING               *
      *   1. FIRST NAME                                               *
      *   2. " " SPACE                                                *
      *   3. LAST NAME                                                *
      * USING THE STRING FUNCTION                                     *
      *****************************************************************
           STRING MUSICIAN-FNAME DELIMITED BY SPACE
                  " "            DELIMITED BY SIZE
                 MUSICIAN-LNAME  DELIMITED BY SPACE
                   INTO MUSICIAN-NAME-O.

      *    MUSICAN-INTSTRUMENT-TYPE-O MOVED IN PRIOR PARAGRAPH
      *    INSTRUMENT-QUALITY-O MOVED IN PRIOR PARAGRAPH
      *    WS-BASE-PRICE MOVED TO BASE-PRICE-O IN PRIOR PARAGRAPH
      *    VALUE FOR ADJ-AMOUNT-O MOVED IN PRIOR PARAGRAPH

           MOVE SHIP-TO TO SHIP-TO-O.
           MOVE WS-ADJUSTED-PRICE TO ADJ-PRICE-O.
           MOVE WS-TOTAL-COST TO COST-PER-INSTRUMENT-O.
           MOVE WS-SHIPPING-COST TO SHIPPING-COST-O.
           MOVE WS-TAX TO TAX-O.
           MOVE MAX-MUSICIAN-BUDGET-AMOUNT TO MAX-BUDGET-O.
      *
       0900-WRITE-PROPOSAL-REC.
      *    DISPLAY 'ENTERING PARA 0900-WRITE-PROPOSAL-REC'.
      *
      *****************************************************************
      * WRITE A DETAIL LINE TO THE PROPOSAL OUTPUT FILE               *
      *****************************************************************
      *
           WRITE PROPOSAL-REC.
      *
       0950-WRITE-TRAILER-RECS.
      *    DISPLAY 'ENTERING PARA 0950-WRITE-TRAILER-RECS.
      *
      *****************************************************************
      * WRITE TRAILER RECORDS FOR THE PROPOSAL OUTPUT FILE            *
      *****************************************************************
      *
           WRITE PROPOSAL-REC FROM TRAILER-REC-1
              AFTER ADVANCING 10 LINES.
           WRITE PROPOSAL-REC FROM TRAILER-REC-2
              AFTER ADVANCING 1 LINES.
           WRITE PROPOSAL-REC FROM TRAILER-REC-3
              AFTER ADVANCING 1 LINES.
           WRITE PROPOSAL-REC FROM TRAILER-REC-4
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-BLANK
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-5
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-BLANK
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-6
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-BLANK
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-7
              AFTER ADVANCING 1 LINES.
           WRITE PROPOSAL-REC FROM TRAILER-REC-8
              AFTER ADVANCING 1 LINES.
           WRITE PROPOSAL-REC FROM TRAILER-REC-9
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-BLANK
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-10
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-BLANK
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-11
              AFTER ADVANCING 1 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-BLANK
              AFTER ADVANCING 2 LINES.

           WRITE PROPOSAL-REC FROM TRAILER-REC-12.


       1000-CLOSE-FILES.
      *    DISPLAY 'ENTERING PARA 1000-CLOSE-FILES'.
      *
      *****************************************************************
      * CLOSE INPUT AND OUTPUT FILES                                  *
      *****************************************************************
      *
           CLOSE RFPIN, PROPOSAL.
