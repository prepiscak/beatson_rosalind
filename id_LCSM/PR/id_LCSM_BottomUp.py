#!/usr/bin/env python

def findSharedMotif1(dna_strings):
    """
    LIMITATION: Assuming there must be at least one shared motif across all sequences!!!
    calculate frequencies of the motif across all other strings
    1. IF all frequencies are different from 0 (motif present): 
     * save motif as longest_shared_motif
     * add next character to motif
    2. IF NOT:
     * move to next character and start building from there
    """
    #shortest_seq = dna_strings[strings_length.index(min(strings_length))]
    dna_strings.sort(key=len)
    shortest_seq = dna_strings[0]
    longest_shared_motif=''

    # Optimize by reducing number of for loops and number of tests? 

    for i in range(len(shortest_seq)):
        for j in range(i+1, len(shortest_seq)+1):
            motif = shortest_seq[i:j]
            motif_detected = all([True if (item.count(motif) != 0) else False for item in dna_strings])
        
            if motif_detected:
                if len(motif) > len(longest_shared_motif):
                    longest_shared_motif = motif
                continue
            else:
                break

    return(longest_shared_motif)

if __name__ == '__main__':
    from Bio import SeqIO
    import sys
    fasta_file = sys.argv[1]

    with open(fasta_file, "r") as fasta_handle: # test.fasta
        fasta_record = list(SeqIO.parse(fasta_handle, "fasta"))
        dna_strings = [str(dna_string.seq).strip() for dna_string in fasta_record]

    print(findSharedMotif1(dna_strings))