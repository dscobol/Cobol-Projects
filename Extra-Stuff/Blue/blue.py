#!/usr/bin/env python

""" Blue - the cobol UPPERCASER. 

This is an application that reads in the keywords.txt file
which is a list of all the COBOL keywords.
It reads the COBOL file as an argument and searches for 
and replaces any COBOL keywords that are in lowercase  or 
mixed-case text.

Example run:

$ python blue test.cbl

"""

import re
import sys

file_to_uppercase = sys.argv[1]
keywords = []

with open("keywords.txt") as k_input:
    keywords = [line.strip() for line in k_input]

print(f"I will uppercase the file: {file_to_uppercase}")

uppercase = lambda x: x.group(1).upper()
re_replace = re.compile(r"\b({})\b".format("|".join(keywords)), re.IGNORECASE)

with open(file_to_uppercase) as f_input:
    text = f_input.read()

with open(file_to_uppercase, "w") as f_output:
    f_output.write(re_replace.sub(uppercase, text))
