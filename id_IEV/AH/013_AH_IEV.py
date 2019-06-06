#!/usr/bin/python3

#usage 013_AH_IEV.py 1 2 3 4 5 6


#import some stuff
import sys

#the populations from the command line
popList=list(map(int, sys.argv[1:]))

#the probabilities of each genotype
probList=(1,1,1,0.75,0.5,0)

#print(popList)
#print(probList)
#[1, 0, 0, 1, 0, 1]
#(1, 1, 1, 0.75, 0.5, 0)

#multiply the lists together
#sum the results
#multiply by 2 (because there are 2 offspring per parent pair
#print
print((sum(([a*b for a,b in zip(popList,probList)])))*2)

