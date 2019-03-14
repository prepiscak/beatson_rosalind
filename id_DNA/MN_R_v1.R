# Define the input filename
fname <- "data_samp.txt"

# Open a connection to the input file, and read the contents of the
# file into the string variable; then close the file connection
con <- file(fname,'r')
string <- readLines(con)
close(con)

# Split the string into individual characters (one element per character)
string_split <- unlist(strsplit(string,""))

# Get the unique letters
uletters <- sort(unique(string_split))

# Initialise the result list
ans <- list()

# Cycle through the unique letters and count the occurrences of each one
for(letter in uletters){
  ans[[letter]] <- sum(string_split==letter)
}

# Print the result
unlist(ans)
