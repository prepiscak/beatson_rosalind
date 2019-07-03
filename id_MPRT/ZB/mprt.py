import re
import requests
import ssl

# IDs = ['P07204_TRBM_HUMAN']
my_dir = 'data/'
IDs = []
f=open('../'+my_dir+'rosalind_mprt.txt','r')
for line in f:
    IDs.append(line.strip('\n'))

pattern = r'N[A-O|Q-Z][S|T][A-O|Q-Z]'
# pattern = r'N[F|L|S|Y|C|W|H|Q|R|I|M|T|N|K|V|A|D|E|G][S|T][F|L|S|Y|C|W|H|Q|R|I|M|T|N|K|V|A|D|E|G]'
NglycMotif = re.compile(pattern)

bases = "UCAG"
codons = [a + b + c for a in bases for b in bases for c in bases]
amino_acids = 'FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG'
codon_table = dict(zip(codons, amino_acids))
print(codon_table)

def get_fasta_handle_300(ID):
	url_base = 'http://www.uniprot.org/uniprot/'
	url = url_base + ID + '.fasta'

	print('Requesting: ' + url)

	response = requests.get(url)
	if response.status_code == 300:
	    redirect_url = url_base + response.headers['Location']  # Get new URL.
	    response = requests.get(redirect_url)  # Make a new request.
	return response

def get_fasta(ID):
	# generate the ID
	data = ''
	f = get_fasta_handle_300(ID)
	has_started_already = False
	for line in f:
		strLine = "".join(map(chr, line))
		#  Next lines are for the redirects where multiple sequences can be found but we're interested only in the first one
		if strLine.startswith(">") and not has_started_already:
			has_started_already = True
			continue
		if strLine.startswith(">") and has_started_already:
			break

		data += strLine
	return data

results = []

for ID in IDs:
	fasta = get_fasta(ID)
	# print(fasta)
	indexes = []
	if NglycMotif.search(fasta):
		for m in NglycMotif.finditer(fasta):
			indexes.append(str(m.start()))
		results.append(ID)
		results.append(' '.join(indexes))

print("Results are following: ==========")
for row in results:
	print(row)
