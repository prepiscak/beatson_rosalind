# This script reads in a text file containing only the characters
# A, C, G and T, and calculates its reverse-complement.
#
# Script is to be run as follows:
#
#  python MN_python.py datfile.txt
#
# where "datfile.txt" is the data file containing the string whose
# reverse-complement we want to calculate.

# Import library to access command line arguments
import sys

# Import maketrans function for creating a translation table
from string import maketrans

# Define the input filename
fname = sys.argv[1]

# Read the file into a string
with open(fname, 'r') as myfile:
    str = myfile.read().replace('\n', '')

# Create a lookup table
mapping = maketrans('ATCG', 'TAGC')

# Print the reverse of the translated result
print(str.translate(mapping)[::-1])
