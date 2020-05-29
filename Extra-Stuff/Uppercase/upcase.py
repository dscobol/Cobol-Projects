#!/usr/bin/env python

""" Upcase - Uppercase every word in the file.

This will accept a file name from the system, read in the file, and
UPPERCASE every letter in the file.

This was written because the COBOL programs that run on the TK4- MVS
system have to be all uppercase, no mixed case except for literals.

Example run:

$ python upcase test.cbl

"""

import sys

file_to_upcase = sys.argv[1]

print(f"I will UPPERCASE all the letter in the file: {file_to_upcase}")

with open(file_to_upcase) as f_input:
    text = f_input.read()

with open(file_to_upcase, "w") as f_output:
    f_output.write(text.upper())
