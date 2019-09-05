#!/usr/bin/env python

def splice(dna_string, introns):
    pre_mDNA=dna_string
    for intron in introns:
        pre_mDNA = pre_mDNA.replace(intron, "") # default replacing all occurences
    return(pre_mDNA)

def transcribe(dna_string):
    """
    Function to transcribe DNA to RNA; reverse complementing and replacing thymine with uracil
    """
    trans_tab = str.maketrans("ATGC", "UACG") # transcribe; DNA->RNA T->U
    return(dna_string.translate(trans_tab))[::-1] # reverse complement

#def translate
# from id_ORF rosalind
def get_orf(dna_string):
    # Generating reverse complement
    trans_tab = str.maketrans("ATGC", "TACG")  # str.maketrans(map_in, map_out)
    dna_string_rev_compl = dna_string.translate(trans_tab)[::-1].strip() # fix by stripping from string newline!
    
    dna_orf1 = dna_string[0:] 
    dna_orf2 = dna_string[1:len(dna_string)-2] # to preserve codons
    dna_orf3 = dna_string[2:len(dna_string)-4] # to preserve codons
    dna_orf4 = dna_string_rev_compl[0:] # use this as template for transcription so mRNA corresponds to DNA (s string) as coding 
    dna_orf5 = dna_string_rev_compl[1:len(dna_string_rev_compl)-2] # to preserve codons
    dna_orf6 = dna_string_rev_compl[2:len(dna_string_rev_compl)-4] # to preserve codons
    
    # using above transcribe function
    mRNA_orfs = [transcribe(dna_orf1), 
            transcribe(dna_orf2), 
            transcribe(dna_orf3), 
            transcribe(dna_orf4), 
            transcribe(dna_orf5), 
            transcribe(dna_orf6)]
    return(mRNA_orfs)

def translate(orfs):
    codon_table = {"UUU":"F","CUU":"L","AUU":"I","GUU":"V",
    "UUC":"F","CUC":"L","AUC":"I","GUC":"V",
    "UUA":"L","CUA":"L","AUA":"I","GUA":"V",
    "UUG":"L","CUG":"L","AUG":"M","GUG":"V",
    "UCU":"S","CCU":"P","ACU":"T","GCU":"A",
    "UCC":"S","CCC":"P","ACC":"T","GCC":"A",
    "UCA":"S","CCA":"P","ACA":"T","GCA":"A",
    "UCG":"S","CCG":"P","ACG":"T","GCG":"A",
    "UAU":"Y","CAU":"H","AAU":"N","GAU":"D",
    "UAC":"Y","CAC":"H","AAC":"N","GAC":"D",
    "UAA":"","CAA":"Q","AAA":"K","GAA":"E",
    "UAG":"","CAG":"Q","AAG":"K","GAG":"E",
    "UGU":"C","CGU":"R","AGU":"S","GGU":"G",
    "UGC":"C","CGC":"R","AGC":"S","GGC":"G",
    "UGA":"","CGA":"R","AGA":"R","GGA":"G",
    "UGG":"W","CGG":"R","AGG":"R","GGG":"G"}

    # start codong: "AUG"
    # stop codons: "UAA", "UAG", "UGA"

    #proteins 
    # only translating coding DNA strand: orfs[3]
    proteins = []
    for mRNA_seq in orfs: 
        #print(orfs.index(mRNA_seq))
        AA_seq=[]
        start_translation=0
        for i in range(0, len(mRNA_seq), 3):
            codon=mRNA_seq[i:i+3]
            if codon == "AUG": # start codon
                start_translation=1
            if start_translation:
                AA_seq.append(codon_table[codon])
            if codon == "UAA" or codon == "UAG" or codon == "UGA":
                start_translation = 0
        protein_seq = "".join(AA_seq)
        proteins.append(protein_seq)
    
        # if printing translated proteins
        #if protein_seq:
        #    print(protein_seq)

    return(proteins)

if __name__ == '__main__':
    """
    TODO: print to file rather than to screen
    TODO: needs fixing - weird reverse complementary!?
    """
    from Bio import SeqIO
    import sys
    fasta_file = sys.argv[1] #"test.fasta" 

    with open(fasta_file, "r") as fasta_handle: # test.fasta
        fasta_record = list(SeqIO.parse(fasta_handle, "fasta"))
        dna_strings = [str(dna_string.seq).strip() for dna_string in fasta_record]
        print("There are {} dna strings in the fasta file.".format(len(dna_strings)))

        dna_string = dna_strings[0]
        print("Length of main dna string is {} bps.".format(len(dna_string)))
        introns = dna_strings[1:]


    pre_mDNA = splice(dna_string, introns)
    mRNA = transcribe(pre_mDNA)
    orfs = get_orf(pre_mDNA)
    peptides = translate(orfs)
    peptide_from_coding=peptides[3]

    print("Protein string:")
    print(peptide_from_coding)

    output_file="rosalind_splc_output.txt"
    print("Saving results into {}".format(output_file))
    with open(output_file, "w") as output_file_handle:
        output_file_handle.write(peptide_from_coding)



