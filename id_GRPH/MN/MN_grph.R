# This script takes a collection of DNA strings in FASTA format, having total
# length at most 10 kbp, and prints the adjacency list corresponding to O3.
#
# Run using the following command:
#
#    Rscript --vanilla MN_grph.R dat.txt
#

# Get the data file from the command line
input_args <- commandArgs(trailingOnly=TRUE)
dfile <- input_args[1]

# Read data
dat <- readLines(dfile)

# Get the first character of each line
firstchars <- sapply(dat,function(x){ substr(x,1,1) })

# Determine which first characters are equal to ">"
newstrings <- which(firstchars==">")
# Append length(dat)+1 to newstrings, so that the final iteration
# of the loop (below) will work as intended
newstrings <- c(newstrings,length(dat)+1)

# Get the strings and string names
strings <- c()
string_names <- c()
for(i in 1:(length(newstrings)-1)) {
  # string name (the remaining characters after ">")
  string_names[i] <- substr(dat[newstrings[i]],2,nchar(dat[newstrings[i]]))
  # string (all lines between the current ">" and the next ">" -- or, 
  #         in the case of the final iteration, until the last line)
  strings[i] <- ""
  for(j in (newstrings[i]+1):(newstrings[i+1]-1)) {
    strings[i] <- paste0(strings[i],dat[j])
  }
}

# Create a data frame for the first three characters of each string
df_first <- data.frame(first = string_names,
                       first_string = strings,
                       string = sapply(strings,function(x) { substr(x,1,3) })
                      )
# Create a data frame for the last three characters of each string
df_last <- data.frame(last = string_names,
                      last_string = strings,
                      string = sapply(strings,function(x) { n <- nchar(x); substr(x,n-2,n) })
                     )

# Create an adjacency list by merging the two data frames on "string"
adj <- merge(df_last,df_first)

# Remove self-matches (a string cannot match itself)
inds <- which(adj$last_string==adj$first_string)
if(length(inds)>0){
  adj <- adj[-inds,c("last","first"),drop=FALSE]
} else {
  adj <- adj[,c("last","first")]
}

# Print the result to the screen
cat(cbind(apply(as.matrix(adj),1,function(x){ paste(x[1],x[2],sep=" ") })),sep="\n")