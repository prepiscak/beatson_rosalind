#!/bin/bash
#
# This script takes a text file containing only the characters
# A, C, G and T, and calculates its reverse-complement.
#
# Script is to be run as follows:
#
#  ./MN_tr.sh datfile.txt
#
# where "datfile.txt" is the data file containing the string
# whose reverse compliment we wish to calculate.


# Define the input filename
fname=$1

# Reverse the string
#output=`rev $fname`

# Swap T and A (using Z as a temporary placeholder)
#output=`tr A Z <<< $output | tr T A | tr Z T`

# Swap C and G (using Z as a temporary placeholder)
#tr C Z <<< $output | tr G C | tr Z G

# Complete solution in one line (much faster)
rev $fname | tr A Z | tr T A | tr Z T | tr C Z | tr G C | tr Z G

