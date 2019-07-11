#!/usr/bin/env python

def possible_mRNA(prot_seq):
    rev_codon_table = {"R":6,"L":6,"S":6,
                   "A":4,"G":4,"P":4,"T":4,"V":4,
                   "I":3,"*":3,
                   "D":2,"N":2,"C":2,"E":2,"Q":2,"H":2,"K":2,"F":2,"Y":2,
                   "M":1,"W":1}
    
    result=3
    for AA in prot_seq:
        # storing only the last 6 digits?; see retationship in
        result = (result * rev_codon_table[AA]) % 10**6 
        
    return(result)

if __name__ == '__main__':
    """
    1. get protein sequence from file    
    2. calculate number of possible mRNA it originated from modulo 10^6 

    TODO:
    1. add check for number of system arguments
    2. if file exists and in right format
    3. add test functions
    """
    import sys

    #filename = "rosalind_mrna.txt"
    filename = sys.argv[1]
    
    with open(filename, 'r') as file:
        prot_seq = file.readline().strip() # reading only first line

    print(possible_mRNA(prot_seq))

