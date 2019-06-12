#
# This script reads in a text file containing at most 100 DNA strings (each
# of at most 1000 kbp) in Rosalind's FASTA format, and returns the longest
# substring that is common to all DNA strings.
#
# Script is run as follows:
#
# python lcsm_slow.py datfile.txt
#
# where "datfile.txt" is the data file containing the DNA strings for which
# we wish to find the longest common substring.
#
# Output is the longest common substring.
#
# The basic algorithm is as follows:
#
# (1) We first find the shortest DNA string (since it's impossible for the
#     longest common substring to be longer than the shortest DNA string).
# (2) First loop: We then create a window of characters (initially equal to
#     the number of characters in the shortest DNA string, and decrementing
#     by 1 with each iteration)...
# (3) Second loop: Starting from the first character of the shortest DNA
#     string, we shift the window one place to the right with each iteration
#     until the right-hand edge of the window reaches the final character of
#     the shortest DNA string. The characters encompassed by the window are
#     our test substring.
# (4) We then check this test substring against each DNA string. If any DNA
#     string does *not* contain the test substring, then the substring isn't
#     common to all DNA strings -- so move on to the next iteration of the
#     second loop. If all DNA strings contain the test substring, then we
#     have found a longest common substring.
#
# Here's an example of the moving window, assuming that the shortest DNA
# string comprises eight characters...
#
# First iteration  (w=8, p=1):
#    [ x x x x x x x x ]
#
# Second iteration (w=7, p=1):
#    [ x x x x x x x - ]
# Third iteration  (w=7, p=2):
#    [ - x x x x x x x ]
#
# Fourth iteration (w=6, p=1):
#    [ x x x x x x - - ]
# Fifth iteration  (w=6, p=2):
#    [ - x x x x x x - ]
# Sixth iteration  (w=6, p=3):
#    [ - - x x x x x x ]
#
# ...etc
#

# Import library to access command line arguments
from Bio import SeqIO
import collections
import sys

# Get the input filename
fname = sys.argv[1]

# Open the fasta file
fasta_sequences = SeqIO.parse(open(str(fname)),'fasta')

# Declare an empty list
strings=[]

# Loop through each fasta sequence
for fasta in fasta_sequences:
   # Append the sequence to the list of strings
   strings.append(fasta.seq)

# Get the number of strings
n = len(strings)

# Get the number of characters in each string
lengths = [len(i) for i in strings]

# Get the length of the shortest string
l = min(lengths)
# Get the index of the shortest string
n_short = lengths.index(l)

# Find the longest common substring
# width of moving window is (l+1-i)
for i in xrange(0,l):
  # Starting position (left-hand side of the window, runs from 1 to i)
  for j in xrange(0,i):
    # Cycle through each string
    for s in xrange(0,n):
      # Skip the iteration when s is the shortest string (the substring is obviously present in the shortest string!)
      if s == n_short :
        continue
      # If the substring isn't present in s, then it isn't a common substring -- so proceed to the next iteration of the 'j' loop
      if strings[n_short][j:(l+j-i)] not in strings[s]:
        break
    else:
      # We can only reach this point when the substring is common to all strings, so store the substring and break both outer loops
      substring = strings[n_short][j:(l+j-i)]
      break
    continue
  else:
    continue
  break

# Write the longest common substring to the screen
print(substring)
