#!/usr/bin/python2

#usage 006_AH_Med_Inheritance.py AA_population Aa_population aa_population

from __future__ import division
import sys

#read AA_population Aa_population & aa_population from the command line
AA = int(sys.argv[1])
Aa = int(sys.argv[2])
aa = int(sys.argv[3])


#p of 1st parent being AA
P1_AA=AA/(AA + Aa + aa)
#ps of 2nd parent given 1st was AA
P2_AAAA=((AA-1)/(AA-1+Aa+aa))*P1_AA
P2_AAAa=(Aa/(AA-1+Aa+aa))*P1_AA
P2_AAaa=(aa/(AA-1+Aa+aa))*P1_AA

#p of 1st parent being Aa
P1_Aa=Aa/(AA+Aa+aa)
#ps of 2nd parent given 1st was Aa
P2_AaAA=(AA/(AA+Aa-1+aa))*P1_Aa
P2_AaAa=((Aa-1)/(AA+Aa-1+aa))*P1_Aa
P2_Aaaa=(aa/(AA+Aa-1+aa))*P1_Aa


#p of 1st parent being aa
P1_aa=aa/(AA+Aa+aa)
#ps of 2nd parent given 1st was aa
P2_aaAA=(AA/(AA+Aa+aa-1))*P1_aa
P2_aaAa=(Aa/(AA+Aa+aa-1))*P1_aa
P2_aaaa=((aa-1)/(AA+Aa+aa-1))*P1_aa

#sum the p of parents * p of those parents having an A offspring
#I've omitted the *1s and *0s
P_A=P2_AAAA+P2_AAAa+P2_AAaa+P2_AaAA+(P2_AaAa*0.75)+(P2_Aaaa*0.5)+P2_aaAA+(P2_aaAa*0.5)

print P_A



