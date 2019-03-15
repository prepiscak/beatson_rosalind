#!/bin/bash
#
# This script takes a text file containing only the characters
# A, C, G and T, and counts the number of occurrences of each one.
#
# Script is to be run as follows:
#
#  ./MN_awk.sh datfile.txt
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
nA=`awk -v RS=A 'END{print NR-1}' $fname`
nC=`awk -v RS=C 'END{print NR-1}' $fname`
nG=`awk -v RS=G 'END{print NR-1}' $fname`
nT=`awk -v RS=T 'END{print NR-1}' $fname`

# Write the output to the screen
echo "$nA $nC $nG $nT"

