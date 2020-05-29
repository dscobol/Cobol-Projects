#!/usr/bin/env python

""" MVT-Convert - This will "mostly" convert a modern
COBOL program to something that will compile using the TK4-
MVT/MVS COBOL Compiler from 1974.

This was written because the COBOL programs that run on the TK4- MVS
system have to be all uppercase, no mixed case except for literals and
no double quotes.

Example run:

$ python mvt-convert.py test.cbl

"""

import sys

file_to_convert = sys.argv[1]

print(f"I will convert: {file_to_convert} to 'MVT' COBOL source code.")

with open(file_to_convert) as f_input:
    text = f_input.read()

text = text.replace('"', "'")

with open(file_to_convert, "w") as f_output:
    f_output.write(text.upper())
