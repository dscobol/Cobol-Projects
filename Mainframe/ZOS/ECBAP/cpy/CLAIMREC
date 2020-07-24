      * Copybook CLAIMREC
       01  Claim-Record.
           12 Insured-Details.
              15 Insured-Policy-No        PIC 9(07).
              15 Insured-Last-Name        PIC X(15).
              15 Insured-First-Name       PIC X(10).
           12 Policy-Details.
              15 Policy-Type              PIC 9.
                 88 Private                  VALUE 1.
                 88 Medicare                 VALUE 2.
                 88 Affordable-Care          VALUE 3.
              15 Policy-Benefit-Date-Num PIC 9(08).
              15 Policy-Benefit-Date-X REDEFINES
                    Policy-Benefit-Date-Num PIC X(08).
              15 Policy-Benefit-Period REDEFINES
                    Policy-Benefit-Date-Num.
                 18 Policy-Year              PIC 9(04).
                 18 Policy-Month             PIC 9(02).
                 18 Policy-Day               PIC 9(02).
              15 Policy-Amount               PIC S9(7)V99.
              15 Policy-Deductible-Paid      PIC S9(4).
              15 Policy-Coinsurance          PIC V99.
           12 Claim-Details.
              15 Claim-Amount                PIC S9(7)V99.
              15 Claim-Amount-Paid           PIC S9(7)V99.
           12 FILLER                         PIC X(6).
