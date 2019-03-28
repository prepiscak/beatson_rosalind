<?php

# This script takes two integer parameters, an and k, as command line
# arguments, and computes the Fibonacci-based result using the recurrence
# relation:
#
#     F(n,k) = F(n-1,k) + k*F(n-2,k),
#
# where F(1,k) = F(2,k) = 1.
#
# In this program, we're using a naive recursive function to solve the
# problem.
#
# Script is to be run as follows
#
#  php MN_php.php n k
#
# where "n" and "k" are the two input parameters.

function F( $n, $k ) {
  if ( $n <= 2 ) {
    return 1;
  } else {
    $result = F( $n-1, $k ) + $k*F( $n-2, $k );
    return $result;
  }
}

# Get the input parameters (first and second command line arguments)
$n = $argv[1];
$k = $argv[2];
    
# Print the result
echo F( $n, $k );

?>
