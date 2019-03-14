<?php

# Define the input filename
$fname = 'data_samp.txt';

# Read the contents of the file into $str
$str = file_get_contents($fname);
    
# Replace all occurrences of 'T' with 'U'
echo str_replace ( 'T' , 'U' , $str );

?>

