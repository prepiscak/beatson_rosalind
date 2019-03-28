# This script reads in a text file containing only the characters
# A, C, G and T, and counts the number of occurrences of each one.
#
# Script is to be run from the command line as follows:
#
#  Rscript --vanilla MN_R_v1.R datfile.txt
#
# where "datfile.txt" is the data file containing the string we
# want to analyse.

# Get the command line arguments
args <- commandArgs(trailingOnly=TRUE)

# Define the input filename (the first command line argument)
fname <- args[1]

# Open a connection to the input file, and read the contents of the
# file into the string variable; then close the file connection
con <- file(fname,'r')
string <- readLines(con)
close(con)

# Split the string into individual characters (one element per character)
string_split <- unlist(strsplit(string,""))

# Array containing the letters whose frequencies we want to calculate
uletters <- c("A","C","G","T")

# Initialise the result list
ans <- list()

# Cycle through the unique letters and count the occurrences of each one
for(letter in uletters){
  ans[[letter]] <- sum(string_split==letter)
}

# Print the result
cat(unlist(ans))
