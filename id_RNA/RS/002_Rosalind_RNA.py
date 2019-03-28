#!/usr/bin/env python3

"""
An RNA string is a string formed from the alphabet containing 'A', 'C', 'G', and 'U'.
Given a DNA string t corresponding to a coding strand, its transcribed RNA string u is formed by replacing all occurrences of 'T' in t with 'U' in u.
Given: A DNA string t having length at most 1000 nt.
Return: The transcribed RNA string of t.
"""

# Load dataset
with open('rosalind_rna.txt', 'r') as fhand:
    dna = fhand.read().replace('\n', '')

# Original code with sample data
#t = "GATGGAACTTGACTACGTAAATT"
#u = t.replace('T', 'U')

# Replace all the Thymines with Uracils
rna = dna.replace('T', 'U')

print(rna)