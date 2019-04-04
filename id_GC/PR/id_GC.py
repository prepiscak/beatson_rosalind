import sys

# Needs further cleaning and re-writing as standalone script

def calculateGC(seq):
    """
    calculateGC return percentage of GC content in the input sequence
    """
    return(((seq.count("G") + seq.count("C"))/float(len(seq)))*100)
    # Count the occurrences of a given item in the list

def readFasta(infasta):
    """
    readFasta reads in fasta file and populate dictionary 
      with identifiers as keys and sequence characters as values
    """
    data = {}
    with open(infasta, "r") as inputfile:
        while True:
            line = inputfile.readline().strip()
            #line = line  # remove trailing newline character.
            #print(line)
            if line.startswith('>'):
                header = line.replace(">","")
                data[header] = []
            elif not line:
                break
            else:
                data[header].extend(line) #.append(line)
    return(data)    
    
def getMaxGC(fasta_file):
    """
    getMaxGC return sequence with maximum GC content in a format:
        identifier + '\n' + str(GC_content)
    """
    data = readFasta(fasta_file) #read_fasta function
    
    max_GC = 0
    for identifier, sequence in data.items(): # iterate over idetifiers and sequences
        #print(identifier)
        GC_content = calculateGC(sequence) # calculateGC function on 2 element (list of sequence characters)
        #print(GC_content)
        if GC_content > max_GC:
            result = identifier + "\n" + str(GC_content)
            max_GC = GC_content
            #print(result)
    return(result)
     
if len(sys.argv) != 2:
    print("Error input needed: id_GC.py <inputfile>")
    sys.exit(2)
else:
    fasta_file = sys.argv[1]
    #print(fasta_file)
    GC_results = getMaxGC(fasta_file)
    print(GC_results)
    
    # saving results to file
    with open("rosalind_gc_output.txt", "w") as f:
        f.write(GC_results)    
