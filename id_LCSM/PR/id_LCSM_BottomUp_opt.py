#!/usr/bin/env python

def findSharedMotif2(dna_strings):
    #print(dna_strings)
    #sortin dna_strings 1st
    dna_strings.sort(key=len) # sorting dna_strings
    #print(dna_strings)

    # take 1st argument as the shortes one
    shortest_seq = dna_strings[0]
    longest_shared_motif=''

    # going by shortest 1st and if not found skip as there is no point for longer
    for i in range(len(shortest_seq)):
        for j in range(i+1, len(shortest_seq)+1):
            motif = shortest_seq[i:j]
            #print(motif)

            # iterate through strings break if not found in short one
            detected = 0
            for string in dna_strings[1:]:
                if motif in string:
                    #print(string, motif in string)
                    detected +=1
                    continue # if motif found continue otherwise break
                else:
                    break # if it is not present in the n number of short ones, break (no point to continue)
        
            #print(detected)
            if detected == len(dna_strings[1:]): # if detected in all strings
                if len(motif) > len(longest_shared_motif): # check if longest; if not save
                    longest_shared_motif = motif
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

    print(findSharedMotif2(dna_strings))