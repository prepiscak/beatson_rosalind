#!/usr/bin/perl

use strict;
use warnings;

my $string="AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC";

my $Acount = () = $string =~ /A/g;
my $Ccount = () = $string =~ /C/g;
my $Gcount = () = $string =~ /G/g;
my $Tcount = () = $string =~ /T/g;

print "$Acount $Ccount $Gcount $Tcount\n";

#my @Cs = $string =~ /C/g;
#print join(", ", @Cs)."\n";
#my $Ccout2 = (@Cs);
#print $Ccout2."\n";

