# This script reads in a text file comprising a string, and replaces
# every instance of "T" with "U".
#
# Script is to be run from the command line as follows:
#
#  Rscript --vanilla MN_R.R datfile.txt
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

# Replace all occurrences of 'T' with 'U'
cat(gsub("T","U",string))

