#!/usr/bin/python2

from string import maketrans   # Required to call maketrans function.

intab = "ACGT"
outtab = "TGCA"
trantab = maketrans(intab, outtab)

str = "AAAACCCGGT";
print str.translate(trantab)[::-1]

# str the input string
# translate(trantab) translate it using the tran table
# [::-1] reverse it


#[ahedley@Y90-AHED-L-D2 id_DNA]$ ./003_AH_RevCompDNA.py
#ACCGGGTTTT

