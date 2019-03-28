#!/usr/bin/env python3

"""
In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'.
The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, then taking the complement of each symbol (e.g., the reverse complement of "GTCA" is "TGAC").
Given: A DNA string s of length at most 1000 bp.
Return: The reverse complement sc of s.
"""
# Complement Function
# Input: String of a DNA Sequence
# Output: String of the DNA Sequence complement of the original
def dna_complement(forward):
    cipher_book = {'A':'T','C':'G', 'G':'C', 'T':'A'}
    reverse_complement = ""
    # loop throught the forward string backwards replacing the base
    for b in range(len(forward) -1, -1, -1):
        reverse_complement = reverse_complement + cipher_book[forward[b]]
        print(forward[b], cipher_book[forward[b]])
    return reverse_complement

# Load dataset
with open('rosalind_revc.txt', 'r') as fhand:
    dna = fhand.read().replace('\n', '')

# # Original sample data
# s = "AAAACCCGGT"
# # return: ACCGGGTTTT
# # Complement
# sc = dna_complement(s)
# print(sc)

print(dna_complement(dna))