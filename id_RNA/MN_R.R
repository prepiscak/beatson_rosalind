# Define the input filename
fname <- "data_samp.txt"

# Open a connection to the input file, and read the contents of the
# file into the string variable; then close the file connection
con <- file(fname,'r')
string <- readLines(con)
close(con)

# Replace all occurrences of 'T' with 'U'
gsub("T","U",string)

