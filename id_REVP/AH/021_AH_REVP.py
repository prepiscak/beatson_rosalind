#!/usr/bin/python3 -W ignore 


#usage 021_AH_REVP.py fasta.fa

#import some stuff
#from Bio.Seq import Seq
from Bio import SeqIO
import sys
import re

#open the fasta file and read the sequence
seq = list(SeqIO.parse(open(str(sys.argv[1])),'fasta'))[0].seq

#set up dictionary of base pairs
dict = {'A': 'T',
'C': 'G',
'G': 'C',
'T': 'A'}

#make a list of possible windows (reverse palindrome in the string having length between 4 and 12)
windows=[12,10,8,6,4]

#for each base till 4 from the end
for base in range(0,len(seq)-3):
   #modify windows to include only those that are less than len(seq)-base (you can't have a 12 window when there are 8 bases left)
   windows = sorted((i for i in windows if i < int(len(seq)-base+1)), reverse=True)
   #for each window
   for window in windows:
      match='T'
      #for each pair in the window
      for pair in range(0,int(window/2)):
         #if the pairs don't match next window (pairs 1:end,2:end-1,,,halfLength:end-halfLength+1
         if(seq[base+pair:base+pair+1]!=dict[seq[base+window-pair-1:base+window-pair]]):
            match='F' 
            break
      #if all the pairs in the window match
      if(match=='T'):
        #print the start and the window
        print(base+1,window)

