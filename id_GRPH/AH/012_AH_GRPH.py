#!/usr/bin/python3

#usage 012_AH_GRPH.py k fasta.fa


#import some stuff
from Bio import SeqIO
import collections
import sys


k=int(sys.argv[1])

#open the fasta file
fasta_sequences = SeqIO.parse(open(str(sys.argv[2])),'fasta')
#for each seq in the fasta file
for fasta in fasta_sequences:
   #open the fasta file, again
   search_sequences = SeqIO.parse(open(str(sys.argv[2])),'fasta')
   #for each search seq in the fasta file
   for search in search_sequences:
      #if fasta and serach are not the same
      if(fasta.seq!=search.seq):
#         print("   ",fasta.seq,search.seq)
         #if the last k bases of fasta equal the first k bases of search
         if(fasta.seq[-k:]==search.seq[:k]):
            #append the fasta and search ids to the matrix
            print(str(fasta.id),str(search.id))


