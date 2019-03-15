<?php

# This script takes a text file comprising a string, and replaces
# every instance of "T" with "U".
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
    
# Replace all occurrences of 'T' with 'U'
echo str_replace ( 'T' , 'U' , $str );

?>

