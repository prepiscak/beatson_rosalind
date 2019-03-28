#!/bin/bash
#
# This script takes a text file containing only the characters
# A, C, G and T, and counts the number of occurrences of each one.
#
# Script is to be run as follows:
#
#  ./MN_grep.sh datfile.txt
#
# where "datfile.txt" is the data file containing the string we
# want to analyse.

# Define the input file (first command line argument)
fname="$1"

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

