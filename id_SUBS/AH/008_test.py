#!/usr/bin/python3

#Some example code
from Bio.Seq import Seq
from Bio.Alphabet import IUPAC
import re

records = Seq("WALLLLFWLGWLGMLAGAVVIIVR", IUPAC.extended_protein)

search = re.search("F.*G", str(records))
print search
# Want FWLG
# Get 
#FWLGWLGMLAG
