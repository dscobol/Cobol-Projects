       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FAV-RECORD.
           12 Group-Name               PIC X(30).
           12 Number-Of-Musicians      PIC 9(02).
           12 Musical-Genre            PIC X(12).
           12 Costs.
              15 CD-Cost               PIC 9(3)V99.
              15 Shipping-Cost         PIC 9(2)V99.
              15 Tax                   PIC 9(2)V99.
           12 Group-Is-Still-Together  PIC X.
       01 Total-CD-Cost                PIC 9(5)V99.

       PROCEDURE DIVISION.
           MOVE "Led Zeppelin"      TO Group-Name.
           MOVE 4                   TO Number-Of-Musicians.
           MOVE "Rock"              TO Musical-Genre.
           MOVE 'F'                 TO Group-Is-Still-Together.

           MOVE 18.95               TO CD-Cost.
           MOVE 6.00                TO Shipping-Cost.
           IF CD-Cost > 40
              Compute Tax = CD-Cost * 0.06
           ELSE
              Compute Tax = CD-Cost * 0.10
           END-IF.

           Compute Total-CD-Cost =
                CD-Cost + Shipping-Cost + Tax.

           DISPLAY "Name: " Group-Name.
           DISPLAY "Number of Musicians: " Number-Of-Musicians.
           DISPLAY "Musical Genre: " Musical-Genre.
           IF Group-Is-Still-Together = "T"
                DISPLAY "Band is still together."
           ELSE
                DISPLAY "Band is not together anymore."
           END-IF.
           DISPLAY " ".
           DISPLAY "The total cost of the CD is: ".
           DISPLAY "Cost of CD: " CD-Cost.
           DISPLAY "Shipping Cost: " Shipping-Cost.
           DISPLAY "Tax: " Tax.
           DISPLAY "Grand Total: " Total-CD-Cost.

           GOBACK.
