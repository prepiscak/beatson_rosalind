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




my_dir = 'data/'

samples = []
sequences = []
sequence = ""

f=open('../'+my_dir+'rosalind_revp.txt','r')
for line in f:
    print(line)
    if line.startswith('>'):
        samples.append(line.strip('\n').strip('>'))
        if sequence != "":
            sequences.append(sequence)
            sequence = ""
    else:
        sequence = sequence + line.strip('\n')
f.close()
sequences.append(sequence)



for i in range(0,len(sequence)): #i seeks the position
    for j in range (3,len(sequence)-3): #j seeks the length
        if i+j > len(sequence):
            continue
        seqFor = sequence[i:i+j]
        seqBack = get_revDNA(seqFor)
        if seqFor==seqBack:
            print(str(i + 1), str(j))
        # if i+j >= len(sequence):
        #     break
