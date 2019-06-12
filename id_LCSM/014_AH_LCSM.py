#!/usr/bin/python3

#usage 014_AH_LCSM.py fasta.fa

#find the shortest seq (e.g.ABCDEFG) then search the rest for substrings of shortest as...
#i          j  word eq 
#1-ABCDEFG -0  0:7  j:7-i+1
#2-ABCDEF  -0  0:6  j:7-i+1
#2- BCDEFG -1  1:7  j:7-i+1+j
#3-ABCDE   -0
#3- BCDEF  -1
#3-  CDEFG -2
#4-ABCD    -0  0:4  
#4- BCDE   -1  1:5
#4-  CDEF  -2  2:6
#4-   DEFG -3  3:7
#5-ABC
#5- BCD
#5-  CDE
#5-   DEF
#5-    EFG
#6-AB
#6- BC
#6-  CD
#6-   DE
#6-    EF
#6-     FG
#7-A
#7- B
#7-  C 
#7-   D
#7-    E
#7-     F
#7-      G
#stop when you find a substring in all seqs


#import some stuff
from Bio import SeqIO
import sys

#open the fasta file and read
fasta_sequences = SeqIO.parse(open(str(sys.argv[1])),'fasta')

#get the sequences into a list of strings
sequences=[str(item.seq) for item in fasta_sequences]

#get the length of the list 'sequences'
count=len(sequences)

#get the shortest seq and it's length
word=min(sequences, key=len)
length=len(word)

#print(word)
#print(length)

for i in range(1,length+1):
   for j in range(0,i):
     # print(i, j, word[j:length-i+1+j])
      if sum(word[j:length-i+1+j] in seq for seq in sequences)==count:
         print(word[j:length-i+1+j])
         break
   else:
     continue
   break

#It's realy slow!!!
#[ahedley@Y90-AHED-L-D2 my_scripts]$ time ./014_AH_LCSM.py rosalind_lcsm.fa 
#ACAGCGACAATTCGAAATCGACGATATGTTACACTTGAAGACAGTATTCTGGGCACAGCGTGAGGAATGGCATAAACTCTCTGTAATGGCGATCATACTTGT

#real	1m31.457s
#user	1m31.256s
#sys	0m0.013s



