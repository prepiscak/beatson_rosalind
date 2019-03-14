# Define the input filename
fname = 'data_samp.txt'

# Read the file into a string
with open(fname, 'r') as myfile:
    string = myfile.read().replace('\n', '')

# Define the letters of interest
letters = ['A','C','G','T']

# Cycle through each letter and append the number of occurrences to the result string
result = ''
for l in letters :
    result = result + str(string.count(l)) + ' '

# Print the result string to the screen
print( result )
