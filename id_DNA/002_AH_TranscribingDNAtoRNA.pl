#!/usr/bin/perl

use strict;
use warnings;

my $string="GATGGAACTTGACTACGTAAATT";

$string =~s/T/U/g;

print "$string\n";

