#!/usr/bin/perl -w

#usage 008_AH_SUBS.pl STRING MOTIF

use strict;

#get the command line paramerter
(my $string, my $motif)=@ARGV;

#if you put 3 parameters on commandline the 3rd doesn't get printed
#print $string." ".$motif."\n";

#set up array to hold matches
my @matches;

#for each position in 'string' from 0 to only the motif length remains
for(my $i=0; $i<length($string)-length($motif)+1; $i++){
	#match 'motif' to the start of a substring from position 'i' to end of string
	if(substr($string,$i,length($string))=~/^$motif/){
		#if match, store position in array
		push @matches, $i+1;
	}
}
#print array
print "@matches\n";

