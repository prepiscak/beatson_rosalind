#!/usr/bin/perl -w

#usage 007_AH_PROT.pl AN_mRNA_STRING

use strict;
use Bio::Seq;
use Bio::SeqIO;

#declare 
my $mRNA_obj;

#Read in the mRNA from the command line
#if it's an existing file name
if (-f $ARGV[0]) {
	my $fasta_obj  = Bio::SeqIO->new(-file => $ARGV[0],'-format' => 'Fasta');
	$mRNA_obj = $fasta_obj->next_seq;
}
#else presume it's a seq string
else{
	$mRNA_obj = Bio::Seq->new(-seq => $ARGV[0],-alphabet => 'rna' );
}

#translte it to prot (complete=1 means upto and excluding the stop codon)
my $prot_obj = $mRNA_obj->translate(-complete => 1);

#get the seq from the prot_obj and print
print $prot_obj->seq."\n";

