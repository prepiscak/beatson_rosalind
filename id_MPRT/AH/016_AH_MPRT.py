#!/usr/bin/python3

#usage 016_AH_MPRT.py MPRT.txt

#import some stuff
import requests
import re
import sys

#open the file
with open(sys.argv[1]) as file:
   #for each line in the file
   for prot in file:
      #strip the line end chars
      prot=prot.strip()
      #build the url
      url = "https://www.uniprot.org/uniprot/" + prot + ".fasta"
      #get the page
      html = requests.get(url)
      #convert to text
      pagetext = html.text
      #read lines into a list
      lines=pagetext.splitlines()
      #remove the first item of the list - the header
      lines.pop(0)
      #concat lines into seq
      seq=''.join(lines)
      #set up the motif in a regex
      #(?=<your regex>) is a 'lookahead' structure. This is not part of the match
      #so the next match can start at the position after the leading N
      motif='N(?=[^P][ST][^P])'
      #an expample of overlapping motifs
#      seq='ANNTTAAAANNTTAAA'
      #open a list for the positions
      positions=[]
      #for each motif match in seq 
      for match in re.finditer(motif,seq):
          #match.span looks like (84, 88) - start and end+1 counting from zero
          positions.append(list(match.span())[0]+1)
      #if there are matches in this prot
      if len(positions) >0:
         #print the output
         print(prot)
         print(*positions)






