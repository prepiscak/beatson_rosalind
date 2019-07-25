#!/usr/bin/python3

#usage 019_PERM.py n

#improt some stuff
import sys
from itertools import permutations as perm

#get int from command line
n=int(sys.argv[1])

#get all the permutations of the set 1:n in a list of lists
perm_list=list(perm(set(range(1,n+1))))

#print
print(len(perm_list))
for p in perm_list:
   print(*p)


