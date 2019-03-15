# This script takes a text file comprising a string, and replaces
# every instance of "T" with "U".
#
# Script is to be run as follows:
#
#  python MN_python.py datfile.txt
#
# where "datfile.txt" is the data file containing the string we
# want to analyse.

# Import library to access command line arguments
import sys

# Define the input filename
fname = sys.argv[1]

# Read the file into a string
with open(fname, 'r') as myfile:
    str = myfile.read().replace('\n', '')

# Replace all occurrences of 'T' with 'U'
print(str.replace('T','U'))
