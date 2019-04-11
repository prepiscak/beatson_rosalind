#!/usr/bin/perl

use strict;
use warnings;

my @array1=split(//,"GAGCCTACTAACGGGAT");
my @array2=split(//,"CATCGTAATGACGGCCT");

my $hamm=0;
for (my $i=0; $i<(@array1); $i++){
  if($array1[$i] ne $array2[$i]){
    $hamm++;
  }
}

print "$hamm\n";


