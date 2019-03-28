# creating complementary strand of DNA
# reverse the string
# find the complementary nucleotide
using Bio.Seq

s = read("rosalind_revc.txt", String)
s = dna(s)

t = reverse(complement(s))

println("$t")