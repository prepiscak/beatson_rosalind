#!/usr/bin/python3

#usage 015_AH_LIA.py k n

#  AaBb x AaBb
#    AB Ab aB ab
#AB   0  0  0  1
#Ab   0  0  1  0 
#aB   0  1  0  0 
#ab   1  0  0  0
#  4/16 = 0.25 success  0.75 fail

#  AABB x AaBb
#    AB Ab aB ab
#AB   0  0  0  1
#  1/4 = 0.25 success  0.75 fail

#  AABb x AaBb
#    AB Ab aB ab
#AB   0  0  0  1
#Ab   0  0  1  0 
#  2/8 = 0.25 success  0.75 fail

#ref https://www.youtube.com/watch?v=Ctytn4a6zjw

import sys
import math

k=int(sys.argv[1])
n=int(sys.argv[2])

#number in the kth generation
N = pow(2, k)

#set the cumulative probability to zreo
Pr = 0

#for each oprtunity to get n or more
for i in range(n, N+1): 
   #if the denominator is zero make a=1 (there's only 1 way to get that combination of success and fail)
   if (math.factorial(i)*math.factorial(N-i))==0:  a=1
   #otherwise clculate a
   else: a=math.factorial(N)/(math.factorial(i)*math.factorial(N-i))
   #add the Pr for this oportunity to the cumulative Pr
   Pr += a*pow(0.25, i)*pow(0.75, N - i)

#print formatted to 3 sig figs
print('%.3f' % Pr)
