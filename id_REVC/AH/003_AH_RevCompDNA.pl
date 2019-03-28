#!/usr/bin/perl

use strict;
use warnings;

my $string="AAAACCCGGT";

#use translate reg ex.
$string =~ tr/ACGTT/TGCA/;

$string = reverse $string;

print $string."\n";

#[ahedley@Y90-AHED-L-D2 id_DNA]$ ./003_AH_RevCompDNA.pl 
#ACCGGGTTTT


