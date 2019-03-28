#!/usr/bin/env python
# coding: utf-8

# # Complementing a Strand of DNA
#
# More detailed introduction: http://rosalind.info/problems/revc/
#

filename_in = "rosalind_revc.txt"
filename_out = "rosalind_revc_output.txt"

with open(filename_in, "r") as infile:
    input_string = infile.read()

print("input string:", input_string)
trans_tab = str.maketrans("ATGC", "TACG")  # str.maketrans(map_in, map_out)
reverse_complement = input_string.translate(trans_tab)[::-1].strip() # fix by stripping from string newline!

print("reverse complement:", reverse_complement)

with open(filename_out, "w") as outfile:
    outfile.write(reverse_complement)
