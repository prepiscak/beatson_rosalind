#!/usr/bin/perl

#usage 006_AH_Med_Inheritance.pl AA_population Aa_population aa_population

use strict;
use warnings;

#read AA_population Aa_population & aa_population from the command line
my $AA=$ARGV[0];
my $Aa=$ARGV[1];
my $aa=$ARGV[2];

#p of 1st parent being AA
my $P1_AA=$AA/($AA+$Aa+$aa);
#ps of 2nd parent given 1st was AA
my $P2_AAAA=(($AA-1)/($AA-1+$Aa+$aa))*$P1_AA;
my $P2_AAAa=($Aa/($AA-1+$Aa+$aa))*$P1_AA;
my $P2_AAaa=($aa/($AA-1+$Aa+$aa))*$P1_AA;


#p of 1st parent being Aa
my $P1_Aa=$Aa/($AA+$Aa+$aa);
#ps of 2nd parent given 1st was Aa
my $P2_AaAA=($AA/($AA+$Aa-1+$aa))*$P1_Aa;
my $P2_AaAa=(($Aa-1)/($AA+$Aa-1+$aa))*$P1_Aa;
my $P2_Aaaa=($aa/($AA+$Aa-1+$aa))*$P1_Aa;


#p of 1st parent being aa
my $P1_aa=$aa/($AA+$Aa+$aa);
#ps of 2nd parent given 1st was aa
my $P2_aaAA=($AA/($AA+$Aa+$aa-1))*$P1_aa;
my $P2_aaAa=($Aa/($AA+$Aa+$aa-1))*$P1_aa;
my $P2_aaaa=(($aa-1)/($AA+$Aa+$aa-1))*$P1_aa;

#sum the p of parents * p of those parents having an A offspring
#I've omitted the *1s and *0s
my $P_A=$P2_AAAA+$P2_AAAa+$P2_AAaa+$P2_AaAA+($P2_AaAa*0.75)+($P2_Aaaa*0.5)+$P2_aaAA+($P2_aaAa*0.5);

print "$P_A\n";


