#
# This script takes a text file containing only the characters
# A, C, G and T, and counts the number of occurrences of each one.
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
    string = myfile.read().replace('\n', '')

# Define the letters of interest
letters = ['A','C','G','T']

# Cycle through each letter and append the number of occurrences to the result string
result = ''
for l in letters :
    result = result + str(string.count(l)) + ' '

# Print the result string to the screen
print( result )
