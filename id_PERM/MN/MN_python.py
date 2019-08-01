# This script reads in an integer (n), calculates all of the
# permutations of the numbers 1:n, prints out the total
# number of permutations, and then prints out all of the
# permutations.
#
# Script is to be run as follows:
#
#  python MN_python.py n
#
# where "n" is the upper-bound for the sequence of numbers
# that we want to permute.

# Import libraries to access command line arguments,
# calculate permutations, and calculate factorials
import sys
import itertools 
import math

# Get the input integer
n = int(sys.argv[1])

# Create a list
numbs = range(1, n+1)

# Print the number of permutations (n!)
print(math.factorial(n))

# Calculate the permutations
perm = itertools.permutations(numbs) 

# Print the output
for i in list(perm): 
    print(' '.join(map(str,i)))
