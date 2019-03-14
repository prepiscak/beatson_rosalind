# Define the input filename
fname = 'data_samp.txt'

# Read the file into a string
with open(fname, 'r') as myfile:
    str = myfile.read().replace('\n', '')

# Replace all occurrences of 'T' with 'U'
print(str.replace('T','U'))
