#!/bin/bash
#
# Basic logic:
#   (Number of As) = (length of string) - (length of string without As)

# Define the string
string="AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

##
# Could create a little bash function
#   $1: the input string
#   $2: the character we want to count
count_char () {
  nochar=${1//[${2}]}
  echo $(( ${#1} - ${#nochar} ))
}

# Count the number of occurrences of the desired character in $string
count_char $string "A"
count_char $string "C"
count_char $string "G"
count_char $string "T"

