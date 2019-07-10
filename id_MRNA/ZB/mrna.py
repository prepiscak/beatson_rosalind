# peptide = 'MA'
peptide = input()

bases = "UCAG"
codons = [a + b + c for a in bases for b in bases for c in bases]
amino_acids = 'FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG'
codon_table = zip(codons, amino_acids)
codon_table_freq = dict()

for c in codon_table:
    if c[1] in codon_table_freq.keys():
        seq_count = codon_table_freq.get(c[1])
        codon_table_freq[c[1]] = seq_count + 1
    else:
        codon_table_freq[c[1]] = 1

rna_string_count = 1
for i in peptide:
    rna_string_count *= codon_table_freq.get(i)

print((rna_string_count * 3) % 1000000)

# for i in range(0, len(peptide), 1):
#     codon = sequence[i: i+3]
#     amino_acid = codon_table.get(codon, '*')
#     if amino_acid != '*':
#         peptide += amino_acid
#     else:
#         break

# print(peptide)
