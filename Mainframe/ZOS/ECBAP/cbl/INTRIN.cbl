       IDENTIFICATION DIVISION.
       PROGRAM-ID.  INTRINI.
       AUTHOR.  IBM.
      * A simple Report Program with straight-line reporting logic
      * Read-and-Print-Read-and-Print, but check for Page Breaks

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
      **** Report Header and Footer Records
       01  Alpha-flds.
           05  Item-1 Pic x(30) Value "Hello World!".
           05  Item-2 Pic x(30) Value Spaces.
           05  Item-result  Pic x(30) Value Spaces.
      *
       01  Orig-cust-name Pic x(20) Value "David Bean".
      *
       01  R Pic x(20) Value "- 1234.5678".
       01  S Pic x(20) Value " $12,345.67CR".
       01  Total Usage is Comp-1.
      *
       77  EBCDIC-CCSID PIC 9(4) BINARY VALUE 1140.
       77  ASCII-CCSID PIC 9(4) BINARY VALUE 819.
       77  Input-EBCDIC PIC X(80) Value Spaces.
       77  ASCII-Output PIC X(80) Value Spaces.
      *
       01  Arg1 Pic x(10) Value "THOMASSON ".
       01  Arg2 Pic x(10) Value "THOMAS ".
       01  Arg3 Pic x(10) Value "VALLEJO ".
       01  Arg-Out  Pic x(20) Value Spaces.
       01  X       Pic 9 Value 0.
      *
       01  R1 Pic x(10) value "e".
       01  R2 Pic x(05) value "f".
       01  R3 Pic x(20) value spaces.
       01  LR Pic 99.

       01  Customer-Record         Group-Usage National.
           05  Customer-Name.
               10 Last-Name        Pic n(17).
               10 Filler           Pic n.
               10 Initials         Pic nn.
           05  Part-Order.
               10 Part-Name        Pic n(15).
               10 Part-Color       Pic n(10).

       01  Orig-Customer-Name      Group-Usage National.
           05  Surname             Pic n(17).
           05  Initials            Pic n(3).
       01  Inventory-Part-Name     Pic n(15)  Usage National.


      * 01  N1 Pic n(10) national.
      * 01  N2 Pic n(05) national value "f".
      * 01  N3 Pic n(20) national value spaces.
      * 01  LN Pic 99 national.
      *
       01  Numeric-flds.
           05 a   pic s9(2) value 20.
           05 b   pic s9(2) value 20.
           05 c   pic s9(2) value 20.
           05 d   pic s9(2) value 20.
           05 res pic s9(4) value 0.

       01  FULL-COMPILED-DATE.
           05  C-DATE.
             10  C-YEAR       PIC 9(4) Value 0.
             10  C-MONTH      PIC 99 Value 0.
             10  C-DAY        PIC 99 Value 0.
           05  C-TIME.
             10  C-HOUR       PIC 99 Value 0.
             10  C-MINUTES    PIC 99 Value 0.
             10  C-SECONDS    PIC 99 Value 0.
             10  C-SEC-HUND   PIC 99 Value 0.
           05  C-TIME-DIFF.
             10  C-GMT-DIR    PIC X  Value space.
             10  C-HOUR       PIC 99 Value 0.
             10  C-MINUTES    PIC 99 Value 0.

       01  When-Compiled-Register-Item.
           05 YYYYMMDDhhmmsshh Pic x(16).
      *
       01  Tax-S            Pic 99v999 value .045.
       01  Tax-T            Pic 99v999 value .02.
       01  Tax-W            Pic 99v999 value .035.
       01  Tax-B            Pic 99v999 value .03.
       01  Ave-Tax          Pic 99v999 Value 0.
       01  Median-Tax       Pic 99v999 Value 0.
       01  Tax-Range        Pic 99v999 Value 0.
      *
       01  Loan             Pic 9(9)V99 Value 0.
       01  Payment          Pic 9(9)V99 Value 0.
       01  Interest         Pic 9(9)V99 Value 0.
       01  Number-Periods   Pic 99 Value 0.
      *
       01  Series-Amt1      Pic 9(9)V99       Value 100.
       01  Series-Amt2      Pic 9(9)V99       Value 200.
       01  Series-Amt3      Pic 9(9)V99       Value 300.
       01  Discount-Rate    Pic S9(2)V9(6)    Value .10.
       01  Todays-Value     Pic 9(9)V99.
      *
       01  DateFields.
           05 YYYYMMDD         Pic 9(8) Value 0.
           05 YYYYMMDD-RDF Redefines YYYYMMDD.
               10 YYYY         Pic 9(4).
               10 MM           Pic 9(2).
               10 DD           Pic 9(2).
       01  Integer-Form     Pic S9(9) Value 0.


       PROCEDURE DIVISION.
      ******** Alpha Intrinsic Functions
           Move Function Upper-case(Item-1) to Item-result.
           Move Function LOWER-CASE (Item-1) to Item-result.
           Move Function Reverse(Item-1) To Item-result.

      *
           Compute Total = Function Numval(R) + Function Numval-C(S).
      * Convert EBCDIC to ASCII
           Move Function Display-of
               (Function National-of (Input-EBCDIC EBCDIC-CCSID),
               ASCII-CCSID) to ASCII-output.

      ***** Max/Min Alpha
           Move Function Max(Arg1 Arg2 Arg3) To Arg-Out(1:10).
           Move Function Min(Arg1 Arg2 Arg3) To Arg-Out(1:10).
      ***** Ord-Max/Ord-Min Alpha
           Compute X = Function Ord-max (Arg1 Arg2 Arg3).
           Compute X = Function ORD-MIN (Arg1 Arg2 Arg3).
      ***** Max used as a conditional expression
           Move Function Max(R1 R2) to R3.

      ***** Length - and embedded Intrinsic Function
           Compute LR = Function Length(Function Max(R1 R2)).
           Compute LR = Function Length(Function Max(R1 R2)).

      *     Move Function Max(N1 N2) to N3.
      *     Compute LN = Function Length(Function Max(N1 N2)).
      *
           Move WHEN-COMPILED to When-Compiled-Register-Item.
           Move function WHEN-COMPILED  to Full-Compiled-Date.
      *
           Compute Ave-Tax = Function Mean (Tax-S Tax-T Tax-W Tax-B).
           Compute Median-Tax = Function Median(Tax-S Tax-T Tax-W )
           Compute Median-Tax = Function Median(Tax-S Tax-T Tax-W Tax-B)
           Compute Tax-Range = Function Range (Tax-S Tax-T Tax-W Tax-B).
      *
           Compute Loan = 15000
           Compute Interest = .12
           Compute Number-Periods = 36
           Compute Payment =
            Loan * Function Annuity((Interest / 12) Number-Periods).
      *
            Compute Todays-Value =
               Function Present-Value(Discount-Rate
                                Series-Amt1 Series-Amt2 Series-Amt3).
      *
           Move Function Current-Date(1:8) to YYYYMMDD
           Compute Integer-Form = Function Integer-of-Date(YYYYMMDD)
           Add 90 to Integer-Form
           Compute YYYYMMDD = Function Date-of-Integer(Integer-Form)
           Display 'Due Date: ' YYYYMMDD.
      *

           Move 01 to DD.
           Add 1 to MM.
           if MM > 12
               move 01 to MM
               add +1 to YYYY.
           Compute YYYYMMDD =
              function DATE-OF-INTEGER(
                function INTEGER-OF-DATE(YYYYMMDD) - 1).

      *********** Nested Functions
           Compute res = Function Sum(a b (c / d)).
           display "End of intrinsicFuncAlpha".