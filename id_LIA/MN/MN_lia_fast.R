# This script takes two integers, k and N, and calculates the probability that
# at least N Aa Bb organisms will belong to the k-th generation of the family
# tree, assuming that Mendel's second law holds for the factors.
#
# Run using the following command:
#
#    Rscript --vanilla MN_lia_fast.R datfile.txt
#

# Get the data file from the command line
input_args <- commandArgs(trailingOnly=TRUE)
dfile <- input_args[1]

# Read data
dat <- scan(dfile,quiet=TRUE)

# Declare function for calculating the probability that at least N Aa Bb
# organisms will belong to the k-th generation of the family tree, assuming
# that Mendel's second law holds for the factors.
pcalc <- function(k,N){
  # Calculate the total number of organisms at the kth generation
  tot <- 2**k
  # Declare the probability that an organism is Aa Bb (same for each generation)
  p <- 1/4
  # Declare the probability that an organism is *not* Aa Bb
  q <- 1-p
  # Initialise the probability to zero
  prob <- 0
  # Select the faster calculation, on the basis of N
  if ( (N-1) < (tot-N) ) { 
    # Calculate the probability that less than N Aa Bb organisms belong to the k-th generation
    for(i in 0:(N-1)){
      prob <- prob + (p**i)*(q**(tot-i))/(factorial(tot-i)*factorial(i))
    }
    prob <- factorial(tot)*prob
    # Probability that at least N Aa Bb organisms belong to the k-th generation is (1-prob)
    prob <- 1-prob
  } else {
    # Calculate the probability that at least N Aa Bb organisms belong to the k-th generation
    for(i in 0:(tot-N)){
      prob <- prob + (q**i)*(p**(tot-i))/(factorial(tot-i)*factorial(i))
    }
    prob <- factorial(tot)*prob
  }
  # Return the calculated probability
  return(prob)
}

# Calculate the probability that at least N Aa Bb organisms belong to the k-th generation
# using the function declared above
prob <- pcalc(dat[1],dat[2])

# Print the result to the screen
cat(prob,sep="\n")