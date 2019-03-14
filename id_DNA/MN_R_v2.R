# Define the input filename
fname <- "data_samp.txt"

# Open a connection to the input file, and read the contents of the
# file into the string variable; then close the file connection
con <- file(fname,'r')
string <- readLines(con)
close(con)

# Load the stringr library
library(stringr)

# List the letters of interest
l <- c("A","C","G","T")

# Initialise the result list
ans <- list()

# Cycle through the letters of interest and count the occurrences of each one
for(letter in l){
  ans[[letter]] <- str_count(string, letter)
}

# Print the result
unlist(ans)