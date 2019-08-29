#!/usr/bin/env python

def rev_complement(dna_string):
    """
    ADD descriptions
    """
    trans_tab = str.maketrans("ATGC", "TACG") #str.maketrans(map_in, map_out)
    return(dna_string.translate(trans_tab)[::-1]) # reverse complement

def detect_rev_palindromes(dna_string):
    palindrome_dict = {}
    for position in range(len(dna_string)):
        """
        ADD descriptions
        BUG: fixed bug where there are more substrings from a position (list rather than single value!)
        BUG: fixed range(len(dna_string)-4) to also include last 4 characters!
        BUG: fixed removed enumerate from palindrome_list as this is not needed! 
        """
        # A: generate a list of all dna substrings of length between 4-12 characters starting from position n
        dna_substrings = [dna_string[position:sub_length] for sub_length in range(position+4, position+(12+1)) if sub_length < len(dna_string)+1] # limit length of substring with if
        #print(dna_substrings)
        
        # B: Generate reverse complements of substrings
        # B = [rev_complement(substring) for substring in dna_substrings]
        #print(B)
        
        # C: Update dictionary with new position length of detected palidrome sequences (combination of match between A, B above and position)
        # length rather than actual string
        palindrome_list = [len(substring) for substring in dna_substrings if substring == rev_complement(substring)]
        if palindrome_list:
            palindrome_dict[position+1] = palindrome_list
    return(palindrome_dict)

if __name__ == '__main__':
    """
    TODO: print to files rather than to screen
    """
    from Bio import SeqIO
    import sys
    fasta_file = sys.argv[1]

    with open(fasta_file, "r") as fasta_handle: # test.fasta
        fasta_record = list(SeqIO.parse(fasta_handle, "fasta"))
        dna_strings = [str(dna_string.seq).strip() for dna_string in fasta_record]

        if len(dna_strings) > 1:
            print("Only one string per fasta supported at the moment!")
        dna_string = dna_strings[0]

    palindrome_dict = detect_rev_palindromes(dna_string)

    # printing each key, and expanding values
    for key, values in palindrome_dict.items():
        #print("{} {}".format(key, values))
        for value in values:
            print("{} {}".format(key, value))
