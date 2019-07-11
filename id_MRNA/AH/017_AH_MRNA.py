#!/usr/bin/python3 -W ignore 

#usage 017_AH_MRNA.py fasta.fa

#import some stuff
from Bio import SeqIO
import sys

#open the fasta file and read
seq = list(SeqIO.parse(open(str(sys.argv[1])),'fasta'))[0].seq

#set up dictionary for the codon freqs
dict = {'G': 4,'F': 2,'L': 6,'I': 3,'M': 1,'V': 4,'S': 6,'P': 4,'T': 4,'A': 4,'Y': 2,'H': 2,'Q': 2,'N': 2, 'K': 2,'D': 2,'E': 2,'C': 2,'W': 1,'R': 6}

#start with the stop codon
d=3

#for each base in seq
for base in seq:
   d=(d*dict[base]) % 1000000

print(d)

