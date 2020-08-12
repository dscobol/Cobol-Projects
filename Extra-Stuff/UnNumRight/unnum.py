#!/usr/bin/env python

""" UnNumRight - Remove the Sequence Numbers from the left and right side. 

This is an application that removes the sequence numbers from
columns 1-6 and  
columns 73-80 in a COBOL "Fixed Format" file, then removes all
the extra spaces on the right side, then adds a line return.

Example run:

$ python unnum test.cbl

"""

import sys

file_to_remove_numbers = sys.argv[1]

print(f"I will remove the numbers from the file: {file_to_remove_numbers}")

with open(file_to_remove_numbers) as f_input:
    text = f_input.readlines()

with open(file_to_remove_numbers, "w") as f_output:
    for line in text:
        new_line = line[:72]
        new_line = new_line.rstrip()
        new_line = new_line + "\n"
        new_line = "      " + line[6:]
        f_output.write(new_line)
