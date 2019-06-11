my_dir = 'data/'
print('../'+my_dir+'rosalind12test.txt')

samples = []
sequences = []
sequence = ""

f=open('../'+my_dir+'rosalind_lcms.txt','r')
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
# print(sequences)
shortest = sequences[0]
for sequence in sequences:
    if len(sequence) < len(shortest):
        shortest = sequence
shortest_length = len(shortest)
motif = ''
motifs = []
for motif_length in range(shortest_length + 1, 1, -1):
    for i in range(1, shortest_length - motif_length + 2):
        motif = shortest[i:i+motif_length]
        shared = 1
        for sequence in sequences:
            if sequence.find(motif) == -1: #find gives -1 if not found
                shared = 0
                break
        if shared == 1:
            print(motif)
            motifs.append(motif)
            break

print(motifs[0])
