#!/usr/bin/env python

# Option 1. using requests, re (or regex)
# Option 2. using bioservices, regex

def get_seq(protein_id):
    """
    Retrieve sequence from Uniprot using requests
    Returns string with sequence
    """
    full_URL = ("http://www.uniprot.org/uniprot/" + protein_id + ".fasta")
    
    #print("obtaining fasta for " + protein_id + ":\n" + full_URL)
    
    result = requests.get(full_URL)
    # add check if responce returned (code 200)

    if result.ok:
        sequence = str(result.text)
        return(sequence.split("\n", 1)[1].replace('\n', ''))
    else:
        print("Something went wrong ", result.status_code)

def find_motif(sequence, protein_motif=r'(?=(N[^P][ST][^P]))'):
    """
    Find motif start locations sequence using re module
    NOTE: using lookahead with re module to find also overlapping
    or and using directly re or regex module
    Returns list with positions
    """
    #protein_motif=re.compile(r'(?=(N[^P][ST][^P]))')
    results = [item.start()+1 for item in re.finditer(protein_motif, sequence)]
    return(results)

if __name__ == '__main__':
    """
    1. get protein_ids from file
    2. retrieve fasta sequences from Uniprot
    3. find motif and if exists print protein_id and locations of motif in sequence
    
    TODO:
    [] fix if sys args != 1
    [] retrieve all fasta at once and iterate through list of fastas 
    [] save to results file instead of printing
    """
    import sys
    import requests
    import re
    
    #filename = "rosalind_MPRT_test.txt"
    filename = sys.argv[1]
    
    with open(filename, "r") as file:
        protein_ids=file.read().splitlines()
        
    #protein_ids=["A2Z669", "B5ZC00", "P07204_TRBM_HUMAN", "P20840_SAG1_YEAST"]

    for protein_id in protein_ids:
        sequence = get_seq(protein_id)
        # check if sequence found; otherwise print some errors
        if sequence: 
            motif_locations = find_motif(sequence)
            # print only if motif found - check if motif found
            if motif_locations:
                print(protein_id)
                print(*motif_locations)