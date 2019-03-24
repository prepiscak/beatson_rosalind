<?php

# This script reads in a text file containing only the characters
# A, C, G and T, and calculates its reverse-complement.
#
# Script is to be run as follows:
#
#  php MN_php.php datfile.txt
#
# where "datfile.txt" is the data file containing the string whose
# reverse-complement we want to calculate.

# Get the input filename (first command line argument)
$fname = $argv[1];

# Read the contents of the file into $str
$str = file_get_contents($fname);
    
# Swap 'T' and 'A', and swap 'C' and 'G'
$str = strtr($str, array("T"=>"A", "A"=>"T", "G"=>"C", "C"=>"G"));

# Print the reverse of the result
echo strrev ( $str );

?>

