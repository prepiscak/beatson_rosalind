# This script reads in a protein string and calculates its
# total weight using a monoisotopic mass table.
#
# Script is to be run as follows:
#
#  python MN_python.py protein
#
# where "protein" is the protein string of interest.

# Import library to access command line arguments
import sys

# Get the input protein
protein = sys.argv[1]

# Read the monoisotopic mass table into an associative array
lookup_table = {} 
with open('mass_table.txt', "r") as f: 
    for line in f: 
        key, value = line.strip().split('\t') 
        lookup_table[key] = float(value) 

# Initialise the total mass to zero
tot = 0.0

# Loop over characters in the protein string and add them together
for c in protein:
  tot += lookup_table[c]

# Print the total protein mass (to three decimal places, as per the example)
print("%.3f" % tot)