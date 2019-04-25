#!/usr/bin/python2

#usage 007_AH_PROT.py AN_mRNA_STRING

#import some stuff
from Bio.Seq import Seq
from Bio.Alphabet import generic_rna, generic_protein
import sys

#Read in the mRNA from the command line
mRNA_seq = Seq(sys.argv[1], generic_rna)

#print the translation to the stop codon
print mRNA_seq.translate(to_stop=True)

