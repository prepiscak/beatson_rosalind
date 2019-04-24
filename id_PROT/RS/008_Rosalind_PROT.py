#!/usr/bin/env python3

'''
Translating RNA into Protein

The 20 commonly occurring amino acids are abbreviated by using 20 letters from the English alphabet (all letters except for B, J, O, U, X, and Z). Protein strings are constructed from these 20 symbols. Henceforth, the term genetic string will incorporate protein strings along with DNA strings and RNA strings.
The RNA codon table dictates the details regarding the encoding of specific codons into the amino acid alphabet.

Given: An RNA string s corresponding to a strand of mRNA (of length at most 10 kbp). Also accept an integer corresponding to the reading frame.
Return: The protein string encoded by s.
'''
import os
import re


# Check rna sequence only contains the correct letters
def allowed_match(strg, search=re.compile(r'[^GCUA.]').search):
    return not bool(search(strg))


# check reading frame is an integer
def check_rf(read_str):
    try:
        read_str = int(read_str)
        return True
    except ValueError:
        print("Reading frame not an integer")
        return False


# Convert a rna sequence to protein. Accept string and integer for reading frame.
def translate(rna_seq, rf):
    codon_table = {'UUU' : 'F', 'UUC' : 'F', 'UUG' : 'L', 'UUA' : 'L', 'UCU' : 'S', 'UCC' : 'S', 'UCA' : 'S', 'UCG' : 'S', 'UAU' : 'Y', 'UAC' : 'Y', 'UAA' : 'Stop', 'UAG' : 'Stop', 'UGU' : 'C', 'UGC' : 'C', 'UGA' : 'Stop', 'UGG' : 'W', 'CUU' : 'L', 'CUC' : 'L', 'CUA' : 'L', 'CUG' : 'L', 'CCU' : 'P', 'CCC' : 'P', 'CCA' : 'P', 'CCG' : 'P', 'CAU' : 'H', 'CAC' : 'H', 'CAA' : 'Q', 'CAG' : 'Q', 'CGU' : 'R', 'CGC' : 'R', 'CGA' : 'R', 'CGG' : 'R', 'AUU' : 'I', 'AUC' : 'I', 'AUA' : 'I', 'AUG' : 'M', 'ACU' : 'T', 'ACC' : 'T', 'ACA' : 'T', 'ACG' : 'T', 'AAU' : 'N', 'AAC' : 'N', 'AAA' : 'K', 'AAG' : 'K', 'AGU' : 'S', 'AGC' : 'S', 'AGA' : 'R', 'AGG' : 'R', 'GUU' : 'V', 'GUC' : 'V', 'GUA' : 'V', 'GUG' : 'V', 'GCU' : 'A', 'GCC' : 'A', 'GCA' : 'A', 'GCG' : 'A', 'GAU' : 'D', 'GAC' : 'D', 'GAA' : 'E', 'GAG' : 'E', 'GGU' : 'G', 'GGC' : 'G', 'GGA' : 'G', 'GGG' : 'G'}
    #print(rna_seq)
    rf = rf - 1
    protein = ''
    
    for b in range(rf, len(rna_seq) - 1, 3):
        if len(rna_seq[b:(b+3)]) < 3:
            print(len(rna_seq[b:(b+3)]), " codon to short.")
        elif codon_table[rna_seq[b:(b+3)]] is 'Stop':
            break
        else:
            protein = protein + codon_table[rna_seq[b:(b+3)]]
            print(rna_seq[b:(b+3)], codon_table[rna_seq[b:(b+3)]])
            #print(protein)
    
    return protein


def test():
    test_s = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    print(test_s)
    print("Protein Sequence:")
    print(translate(test_s, 1))


def inputSequence():
    s_i = input("Input rna sequence ")
    rf_i = input("Input start for reading frame (1, 2, or 3) ")
        
    if s_i == 'q' or rf_i == 'q':
        print("Quiting")
        exit()
    elif allowed_match(s_i) is False:
        print("Illegal characters in sequence. Please re-enter")
        inputSequence()
    elif check_rf(rf_i) == False:
        # Check reading frame is an integer
        print("Reading frame incorrect.")
        inputSequence()
    else:
        print(translate(s_i, int(rf_i)))
          

# Main function
def main():
    fn = input("Please enter path, 'test' for test data, 'input' to input sequences or 'q' to quit\n")
    if fn == 'test':
        test()
    elif fn == 'input':
        inputSequence()
    elif fn == 'q':
        exit()
    else:
        # Check file exists and if so load in data
        exists = os.path.isfile(fn)
        if exists:
            # Store configuration file values
            print("File found")
            # Load data and split into two
            with open(fn, 'r') as fhand:
                dt = fhand.read().split('\n')
                    #check_file(dt)
                if allowed_match(dt[0]) and check_rf(dt[1]):
                    print("Protein sequence:")
                    print(translate(dt[0], dt[1]))
                else:
                    print("Sorry, problem with sequences in file")
                    main()
        else:
            print("Sorry, couldn't find the file.")
            print()
            main()


main()