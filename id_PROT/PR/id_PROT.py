with open ("rosalind_prot.txt", "r") as file:
    mRNA_seq = file.readline().rstrip()

protein_seq="".join([codon_table[mRNA_seq[i:i+3]] for i in range(0, len(mRNA_seq), 3)])    
print(protein_seq)
