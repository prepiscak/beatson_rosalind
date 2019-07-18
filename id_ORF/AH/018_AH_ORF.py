#!/usr/bin/python3

#usage 018_AH_ORF.py fasta.fa

#import some stuff
#from Bio.Seq import Seq
from Bio import SeqIO
import sys
import re

#open the fasta file and read the sequence
seq = list(SeqIO.parse(open(str(sys.argv[1])),'fasta'))[0].seq

#make an empty set 
prot_set=set()

#for forward and reverse strands
for strand, nuc in [(+1, seq), (-1, seq.reverse_complement())]:
   #for each reading frame
   for frame in range(3):
      #get the translated seq using translations table 1
      prot = str(nuc[frame:len(seq)-((len(seq)-frame) % 3)].translate(1))
      #print(prot)
      #set up the motifs in regex
      motif='M(?=\w*\*)'   #match Ms that are followed somewhere by a *
      Emotif='(\*)'        #match *s
      #for each M match in prot 
      for Mmatch in re.finditer(motif,prot):
         #for each (but I only want the first) * match in the substr M to end
         for Ematch in re.finditer(Emotif,prot[list(Mmatch.span())[0]:]):
            #get the prot substr M to first * and put in in prot_set
            #print(strand, frame, list(Mmatch.span())[0]*3, (list(Ematch.span())[0]+list(Mmatch.span())[0])*3, prot[list(Mmatch.span())[0]:list(Ematch.span())[0]+list(Mmatch.span())[0]])
            prot_set.add(prot[list(Mmatch.span())[0]:list(Ematch.span())[0]+list(Mmatch.span())[0]])
            break

for result in prot_set:
    print(result)



