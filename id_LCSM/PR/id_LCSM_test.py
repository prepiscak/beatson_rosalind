#!/usr/bin/env python
from Bio import SeqIO

def findSharedMotif(dna_strings):
    """
    LIMITATION: Assuming there must be at least one shared motif across all sequences!!!
    calculate frequencies of the motif across all other strings
    1. IF all frequencies are different from 0 (motif present): 
     * save motif as longest_shared_motif
     * add next character to motif
    2. IF NOT:
     * move to next character
    """

    first_seq = dna_strings[0] # taking only first string for iteration (see LIMITATION imposed!)
    motif = first_seq[0] # initializing to first character of first_sequence 
    longest_shared_motif = [] # initialize empty shared motif

    for char in first_seq[1:]: # character by character in first string
        motif_detected = all([True if (item.count(motif) != 0) else False for item in dna_strings]) 
    
        if motif_detected:
            if len(motif) > len(longest_shared_motif):
                longest_shared_motif = motif # need to save otherwise reseting with new cycle
            motif = motif + char
        else:
            motif = char
 
    # return longest_shared_motif
    return(longest_shared_motif)
            
with open("rosalind_lcsm.txt", "r") as fasta_handle: # test.fasta
    fasta_record = list(SeqIO.parse(fasta_handle, "fasta"))
    strings_length = [len(dna_string) for dna_string in fasta_record]
    dna_strings = [str(dna_string.seq).strip() for dna_string in fasta_record]


findSharedMotif(dna_strings)