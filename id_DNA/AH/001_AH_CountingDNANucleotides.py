#!/usr/bin/env python 2

string="AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC";

import re

print "%s %s %s %s" %(len(re.findall("A", string)), len(re.findall("C", string)),len(re.findall("G", string)),len(re.findall("T", string)))
