#!/usr/bin/python3

#usage 008_AH_SUBS.py STRING MOTIF

#import some stuff
#from Bio.Seq import Seq
#from Bio.Alphabet import generic_rna, generic_protein
import sys

##Read in the mRNA from the command line
#mRNA_seq = Seq(sys.argv[1], generic_rna)

##print the translation to the stop codon
#print mRNA_seq.translate(to_stop=True)

#get the command line paramerter
string=str(sys.argv[1])
motif=str(sys.argv[2])

print(len(string))
print("motif", motif)

#set up array to hold matches
my @matches;

#for each position in 'string' from 0 to only the motif length remains
for i in range(1, len(string))
	#match 'motif' to the start of a substring from position 'i' to end of string
#	if (substr(string,i,length(string))=~/^$motif/)
		#if match, store position in array
		print i+1;





#print array
#print "@matches\n";

