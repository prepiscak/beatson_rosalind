#!/usr/bin/env python

# Option 1. using requests, re (or regex)
# Option 2. using bioservices, regex

from bioservices.uniprot import UniProt
u = UniProt(verbose=True)

def get_seq(protein_id):
    """
    Retrieve sequence from Uniprot using bioservices
    or directly use get_fastq_sequence from the package
    Returns string with sequence
    """
    # it is possible to retrieve all protein_ids at once!
    sequence = u.retrieve(protein_id, "fasta")
    if sequence != "404":
        sequence = sequence.split("\n", 1)[1].replace('\n', '')
    else:
        sequence=False
        print("{} not found".format(protein_id))
    return(sequence)

# needs regex module
import regex
def find_motif(sequence, protein_motif=r'N[^P][ST][^P]'):
    """
    Find motif start locations sequence using regex module
    or and using directly re or regex module
    Returns list with positions
    """
    results = [item.start()+1 for item in regex.finditer(protein_motif, sequence, overlapped=True)]
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
    
    #filename = "rosalind_MPRT_test.txt
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