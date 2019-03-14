#!/bin/bash

# Define the input filename
fname="data_samp.txt"

# Replace all instances of 'T' with 'U'
sed 's/T/U/g' $fname
