# This script takes two integers, k and N, and calculates the probability that
# at least N Aa Bb organisms will belong to the k-th generation of the family
# tree, assuming that Mendel's second law holds for the factors.
#
# Run using the following command:
#
#  python MN_lia.py datfile.txt
#
# where "datfile.txt" is the data file containing k and N

# Import library to access command line arguments
import sys
# Import library to access the factorial function
import math

# Get the input filename
fname = sys.argv[1]

# Read the data into an array
with open(fname, 'r') as f:
    dat = [int(x) for x in next(f).split()]

# Extract the two arguments
k = dat[0]
N = dat[1]

# Calculate the total number of organisms at the kth generation
tot = 2**k
# Declare the probability that an organism is Aa Bb (same for each generation)
p = 1.0/4
# Declare the probability that an organism is *not* Aa Bb
q = 1-p
# Initialise the probability to zero
prob = 0.0
# Calculate the probability that at least N Aa Bb organisms belong to the k-th generation
for i in xrange(0,tot+1-N):
    prob = prob + (q**i)*(p**(tot-i))/(math.factorial(tot-i)*math.factorial(i))
prob = math.factorial(tot)*prob

# Print the result string to the screen
print( prob )
