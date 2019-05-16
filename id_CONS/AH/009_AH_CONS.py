#!/usr/bin/python3

#usage 009_AH_CONS.py fasta.fa


#import some stuff
from Bio import SeqIO
import collections
#from Bio.Seq import Seq
#from Bio.Alphabet import generic_rna, generic_protein
import sys

#define a matrix (list of list?)
matrix=[]

#open the fasta file
fasta_sequences = SeqIO.parse(open(str(sys.argv[1])),'fasta')
#for each seq in the fasta file
for fasta in fasta_sequences:
   #append the seq as a list to the matrix
   matrix.append(list(str(fasta.seq)))

#get the length of one seq (they're all the same)
CONSlen=len(matrix[0])

#transpose the matrix
matrix=list(zip(*matrix))

#define a consensus list
CONSseq=[]
#define a counts matrix
counts=[]

#for each position in the consensus
for i in range(0,CONSlen):
   #err! append to CONSseq list the 'key' to the 'set' with the max 'count'	
   CONSseq.append(max(set(matrix[i]), key=matrix[i].count))
   #count the basse frequencies 
   c=collections.Counter(matrix[i])
   #append them to the counts martix as strings not integers
   counts.append(str(c['A'])+str(c['C'])+str(c['G'])+str(c['T']))

#print the CONSseq
print(''.join(CONSseq))

#transpose the counts matrix
counts=list(zip(*counts))

#print the counts matrix with spaces
s=" "
print("A:", s.join(counts[0]))
print("C:", s.join(counts[1]))
print("G:", s.join(counts[2]))
print("T:", s.join(counts[3]))


#$ ./009_AH_CONS.py 009_CONS.fa 
#ATGCAACT
#A: 5 1 0 0 5 5 0 0
#C: 0 0 1 4 2 0 6 1
#G: 1 1 6 3 0 1 0 0
#T: 1 5 0 0 0 1 1 6

