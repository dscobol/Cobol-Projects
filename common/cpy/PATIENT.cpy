       01  FD-:tag:-Patient-Record.
           05 FD-:tag:-Patient-Num  PIC 9(5).
           05 FD-:tag:-Patient-NAME.
              10 FD-:tag:-Patient-LName  PIC X(10).
              10 FD-:tag:-Patient-FName  PIC X(10).
           05 FD-:tag:-Patient-Phone  PIC X(10).
           05 FD-:tag:-Patient-Type  PIC X(1).
               88 FD-:tag:-Patient-IN  VALUE "I".
               88 FD-:tag:-Patient-OUT  VALUE "O".
               88 FD-:tag:-Patient-V-Type  VALUES ARE "I", "O".
           05 FD-:tag:-Patient-Bed-Id  PIC 9(4).
           05 FD-:tag:-Patient-Date-Admit  PIC X(10).
           05 FD-:tag:-Patient-Amt-PDay  PIC 9(5)V99.
           05 FD-:tag:-Patient-Diag-Code  PIC 999.
           05 FD-:tag:-Patient-Ins-Type  PIC X(3).
           05 FD-:tag:-Patient-Stay-LTH  PIC 999.
           05 FD-:tag:-Patient-Tot-Amt  PIC 9(7)V99.
           05 FD-:tag:-Patient-PCP-ID  PIC X(6).
           05 FD-:tag:-Patient-In-Out-Net  PIC X(1).
               88 FD-:tag:-Patient-In-Net  VALUE "N".
               88 FD-:tag:-Patient-Out-Net  VALUE "O".
           05 FD-:tag:-Patient-CoPay  PIC S9(3).
           05 FD-:tag:-Patient-Deduct  PIC S9(4).
