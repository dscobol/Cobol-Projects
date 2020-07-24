      * Copybook wsdt
      * Basic use
      * Create 01
      * 01  CURRENT-DATE-AND-TIME.
      * replace tag with CDT.
      *    MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.

           12 :tag:-Full-Date           PIC 9(8).
           12 :tag:-Date-Sep REDEFINES :tag:-Full-Date.
              15 :tag:-Year             PIC 9(4).
              15 :tag:-Month            PIC 9(2).
              15 :tag:-Day              PIC 9(2).
           12 :tag:-Hour                PIC 9(2).
           12 :tag:-Minutes             PIC 9(2).
           12 :tag:-Seconds             PIC 9(2).
           12 :tag:-Hundredths-Of-Secs  PIC 9(2).
           12 :tag:-GMT-Diff-Hours      PIC S9(2)
                                      SIGN LEADING SEPARATE.
           12 :tag:-GMT-Diff-Minutes    PIC 9(2). *> 00 or 30
