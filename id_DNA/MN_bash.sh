#!/bin/bash

# Define the input file
fname="data_samp.txt"

# Count the number of occurrences of the desired character in the file.
# The -o flag of grep prints each occurrence of the search-string on
# a new line. We then use wc -l to count the number of lines.
# N.B. If we wanted to pass a string, rather than a filename, we should
#      use "<<< $string" instead of "$fname". This is slower, though!
nA=`grep -o "A" $fname | wc -l`
nC=`grep -o "C" $fname | wc -l`
nG=`grep -o "G" $fname | wc -l`
nT=`grep -o "T" $fname | wc -l`

# Write the output to the screen
echo "$nA $nC $nG $nT"

