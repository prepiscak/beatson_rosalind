#
# This script takes a text file containing one line of six
# nonnegative integers,each of which does not exceed 20,000.
# The integers correspond to the number of couples in a population
# possessing each genotype pairing for a given factor. In order,
# the six given integers represent the number of couples having
# the following genotypes:
#
# AA-AA
# AA-Aa
# AA-aa
# Aa-Aa
# Aa-aa
# aa-aa
#
# The script returns the expected number of offspring displaying
# the dominant phenotype in the next generation, under the
# assumption that every couple has exactly two offspring.
#
# Script is to be run as follows:
#
#  python id_iev.py datfile.txt
#
# where "datfile.txt" is the data file containing the six
# nonnegative integers described above.

# Import library to access command line arguments
import sys

# Get the input filename
fname = sys.argv[1]

# Read the data into an array
with open(fname, 'r') as f:
    counts = [int(x) for x in next(f).split()]

# Calculate the expected number of offspring (assuming each pair produces two offspring)
ans = 2 * (counts[0] + counts[1] + counts[2] + 0.75*counts[3] + 0.5*counts[4])

# Print the result string to the screen
print( ans )
