#!/usr/bin/python3

#usage 010_AH_FIBD.py n ls
#where n = months and ls - life span in months

#import some stuff
import sys
import numpy as np    

#read in integers from commandline
n=int(sys.argv[1])
ls=int(sys.argv[2])

#define a matrix (list of list?)
#columns are Juveniles and Adults
#rows are months
#need ls rows to start and everything set to 0
matrix=np.zeros((ls,2))

#matrix
#0 0
#0 0
#0 0

#add the first generation 
#1 J pair
matrix= np.vstack([matrix, [1,0]])

#matrix
#0.0 0.0
#0.0 0.0
#0.0 0.0
#1.0 0.0


#for each following generation
#J=#A month-1
#A=(#J month-1 + #A month-1) - #J month-ls

for i in range(1,n):
   J=matrix[-1,1]
   A=(matrix[-1,0]+matrix[-1,1]) - matrix[-ls,0]
   matrix= np.vstack([matrix, [J,A]])

#print the matrix
for row in matrix:
  print(' '.join(map(str,row)))

#matrix
#0.0 0.0
#0.0 0.0
#0.0 0.0
#1.0 0.0
#0.0 1.0
#1.0 1.0
#1.0 1.0
#1.0 2.0
#2.0 2.0

#total number of pairs is the sum of the last row of matrix
T=int(matrix[-1,0]+matrix[-1,1])
print(T)


