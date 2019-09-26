#!/usr/bin/python3 -W ignore 


#usage 022_AH_SPLC.py fasta.fa

#import some stuff
from Bio import SeqIO
from Bio.Seq import Seq
import sys
import re

#open the fasta file and read the data into a list
seq = list(SeqIO.parse(open(str(sys.argv[1])),'fasta'))

#get the first seq into a string
DNA = str(seq[0].seq)
#get the rest of the seq objects (introns) into a list
introns = seq[1:]

for intron in introns:
   #in the DNA string - replase the intron sequence, with nothing '', once 
   #strings are immutable so I create res then copy it back to DNA
   DNA = res = DNA.replace(str(intron.seq),'',1)

#translate DNA, with table 1, without the stop, and print
print(Seq(DNA).translate(1,to_stop=True))

