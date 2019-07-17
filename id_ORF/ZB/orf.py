def get_revDNA(DNAstring):
    DNAletters = ('A','C','G','T')
    complementary = ('T','G','C','A')
    DNAstringComp = []
    for i in range(0,len(DNAstring)):
        for DNA,comp in zip(DNAletters,complementary):
            if DNAstring[i] == DNA:
                DNAstringComp.append(DNAstring[i].replace(DNA,comp))
    DNAstringComp.reverse()

    revComp = ''.join(DNAstringComp)
    return revComp

def get_peptide(DNA):
    bases = "TCAG"
    codons = [a + b + c for a in bases for b in bases for c in bases]
    amino_acids = 'FFLLSSSSYY||CC|WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG'
    codon_table = dict(zip(codons, amino_acids))
    peptide = ''
    for i in range(0, len(DNA), 3):
        codon = DNA[i: i+3]
        amino_acid = codon_table.get(codon, '|')
        peptide += amino_acid

    return peptide

def cut_peptide(peptide):
    indexesStart = []
    indexesStop =[]
    orfs = set()

    for i in range(0, len(peptide), 1):
        amino_acid = peptide[i]
        # print(amino_acid)
        if amino_acid == 'M':
            indexesStart.append(i)
            # print(i)
        if amino_acid == '|':
            indexesStop.append(i)

    for j in range(0,len(indexesStart),1):
        startUsed = 1
        for k in range(0,len(indexesStop),1):
            diff = indexesStop[k] - indexesStart[j]
            # print(startUsed)
            if diff > 0:
                if startUsed==1:
                    cut = peptide[indexesStart[j]:indexesStop[k]]
                    print(indexesStart[j],indexesStop[k])
                    print(diff)
                    print(cut)
                    orfs.add(cut)
                    startUsed = 0

            else:
                # print('buuu')
                break

    return orfs

def get_orf(sequenceFull):
    orfsAll = []
    for s in range(0,len(sequenceFull),1):
        sequence = sequenceFull[s:]
        peptide = get_peptide(sequence)
        orfs= cut_peptide(peptide)
        orfsAll.append(orfs)
    return orfsAll
#################################################################################################

my_dir = 'data/'

samples = []
sequences = []
sequence = ""

f=open('../'+my_dir+'rosalind_orf.txt','r')
for line in f:
    if line.startswith('>'):
        samples.append(line.strip('\n').strip('>'))
        if sequence != "":
            sequences.append(sequence)
            sequence = ""
    else:
        sequence = sequence + line.strip('\n')
f.close()
sequences.append(sequence)


orfsMixed = []

sequenceFull = sequences[0]
revSequenceFull = get_revDNA(sequenceFull)

orfsNorm = get_orf(sequenceFull)
orfsRev = get_orf(revSequenceFull)

print(orfsRev)


orfsFull = orfsNorm

for o in range(0,len(orfsRev),1):
    orfsFull.append(orfsRev[o])

orfUnique = set()
for orf in orfsFull:
    for o in orf:
        orfUnique.add(o)

for orf in orfUnique:
    print(orf)
