<?php

# Define the input filename
$fname = 'data_samp.txt';

# Read the contents of the file into $str
$str = file_get_contents($fname);

# Define the letters of interest
$letters = array('A','C','G','T');

# Cycle through each letter, and count/print the number of occurrences of each one
foreach ($letters as $l) {
  echo substr_count ( $str , $l ) . " ";
}

?>

