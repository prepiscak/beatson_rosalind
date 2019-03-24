# This program reads in a text file containing only the characters
# A, C, G and T, and calculates its reverse-complement.
#
# Script is to be run from the command line as follows:
#
#  Rscript --vanilla MN_R.R datfile.txt
#
# where "datfile.txt" is the data file containing the string whose
# reverse-complement we want to calculate.

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
x <- unlist(strsplit(string,""))

# Find the elements containing each character
A_ind <- which(x=="A")
T_ind <- which(x=="T")
C_ind <- which(x=="C")
G_ind <- which(x=="G")

# Put the appropriate character in the appropriate element
x[A_ind] <- "T"
x[T_ind] <- "A"
x[C_ind] <- "G"
x[G_ind] <- "C"

# Reverse the vector, collapse to a single string, and print the result
cat(paste(rev(x),collapse=""))
