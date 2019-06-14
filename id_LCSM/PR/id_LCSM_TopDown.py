#!/usr/bin/env python

def findSharedMotif3(dna_strings):
    # Matts function
    dna_strings.sort(key=len)
    # Get the index of the shortest string
    n_short = 0 # we know index of the shortest is 0 (sorted list!)
    l = len(dna_strings[0])
    n = len(dna_strings)
    # Find the longest common substring
    # width of moving window is (l+1-i)
    #print(dna_strings[n_short])
    for i in range(1, l+1): # BUG??? since i=0 first iteration j=0 and will skip checking full size of shortest string!!!???
    # Starting position (left-hand side of the window, runs from 1 to i)
        for j in range(0,i):
            #print("i={} j={}".format(i,j))
            #print(dna_strings[n_short][j:(l+j-i)])
            #print(j*"-" + dna_strings[n_short][j:(l-i+j+1)] + (i-j-1)*"-")
            # Cycle through each string
            for s in range(0,n):
            # Skip the iteration when s is the shortest string (the substring is obviously present in the shortest string!)
                if s == n_short:
                    continue
                # If the substring isn't present in s, then it isn't a common substring -- so proceed to the next iteration of the 'j' loop
                if dna_strings[n_short][j:(l-i+j+1)] not in dna_strings[s]:
                    break
            else:
                # We can only reach this point when the substring is common to all strings, so store the substring and break both outer loops
                substring = dna_strings[n_short][j:(l-i+j+1)]
                break
            continue
        else:
            continue
        break
    # Write the longest common substring to the screen
    return(substring)

if __name__ == '__main__':
    from Bio import SeqIO
    import sys
    fasta_file = sys.argv[1]

    with open(fasta_file, "r") as fasta_handle: # test.fasta
        fasta_record = list(SeqIO.parse(fasta_handle, "fasta"))
        dna_strings = [str(dna_string.seq).strip() for dna_string in fasta_record]

    print(findSharedMotif3(dna_strings))
