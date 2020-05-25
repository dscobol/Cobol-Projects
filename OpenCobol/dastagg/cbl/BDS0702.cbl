       IDENTIFICATION DIVISION.
       PROGRAM-ID. BDS0702.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EmployeeFile ASSIGN TO "../data/Employee.dat "
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD EmployeeFile.
       01 EmployeeDetails.
          88  Emp-EOF             VALUE "10".
          02  EmpSSN              PIC 9(9). 
          02  EmpName.
              03 EmpSurname       PIC X(15).
              03 EmpForename      PIC X(10).
          02  EmpDateOfBirth.
              03 EmpYOB           PIC 9(4).
              03 EmpMOB           PIC 99.
              03 EmpDOB           PIC 99.
          02  EmpGender           PIC X.
       
       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-BOJ.
           PERFORM 2000-Process.
           PERFORM 3000-EOJ.

       1000-BOJ.
           OPEN INPUT EmployeeFile.
           READ EmployeeFile
               AT END SET Emp-EOF TO TRUE
           END-READ.

       2000-Process.
           PERFORM UNTIL Emp-EOF
               DISPLAY EmpForename SPACE EmpSurname " - " 
                   EmpMOB "/" EmpDOB "/" EmpYOB
               READ EmployeeFile
                   AT END SET Emp-EOF TO TRUE
               END-READ
           END-PERFORM.

       3000-EOJ.
           CLOSE EmployeeFile.
           STOP RUN.
