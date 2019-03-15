<?php

# This script takes a text file containing only the characters
# A, C, G and T, and counts the number of occurrences of each one.
#
# Script is to be run as follows:
#
#  php MN_php.php datfile.txt
#
# where "datfile.txt" is the data file containing the string we
# want to analyse.

# Get the input filename (first command line argument)
$fname = $argv[1];

# Read the contents of the file into $str
$str = file_get_contents($fname);

# Define the letters of interest
$letters = array('A','C','G','T');

# Cycle through each letter, and count/print the number of occurrences of each one
foreach ($letters as $l) {
  echo substr_count ( $str , $l ) . " ";
}

?>

