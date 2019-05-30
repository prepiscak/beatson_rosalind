#!/usr/bin/env python
from Bio import SeqIO

record_strings={}

with open("rosalind_grph.txt", "r") as fasta_handle: # test.fasta
    fasta_record = list(SeqIO.parse(fasta_handle, "fasta"))
    #strings_length = [len(dna_string) for dna_string in fasta_record]
    #dna_strings = [str(dna_string.seq) for dna_string in fasta_record]
    #record_ids = [record.id for record in fasta_record]
    for record in fasta_record:
        record_strings[record.id]=str(record.seq)

#print(record_strings)

k=3
adjacency=[]

for s_k, s_v in record_strings.items():
    #print(s_k, s_v)
    for t_k, t_v in record_strings.items():
        if s_v != t_v:
            if s_v[-k:] == t_v[:k]:
                adjacency.append(s_k + " " + t_k)    

print(*adjacency, sep = "\n") 
