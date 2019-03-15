#!/bin/bash
#
# This script takes a text file comprising a string, and replaces
# every instance of "T" with "U".
#
# Script is to be run as follows:
#
#  ./MN_sed.sh datfile.txt
#
# where "datfile.txt" is the data file containing the string we
# want to analyse.

# Define the input file (first command line argument)
fname="$1"

# Replace all instances of 'T' with 'U'
sed 's/T/U/g' $fname
