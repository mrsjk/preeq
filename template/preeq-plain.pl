#!/usr/bin/perl -w

use strict;

use PreEq::EqData;

my @raw;

# START FIXME
# push real data instead of example data

push @raw,{hfc=>undef,dest=>undef,freq=>undef,eqdata=>"
DOCS-IF3-MIB::docsIf3CmStatusUsEqData.4 = Hex-STRING: 08 01 18 00
 FF D0 FF F0 00 00 00 30 FF 80 FF 80 
 00 A0 00 B0 FF 60 FE 60 01 30 02 30 FD 00 FA C0 
 7F B0 00 00 03 30 02 A0 FE D0 FE 40 00 C0 00 E0 
 FF 60 FF 30 00 30 00 50 FF 80 FF E0 00 20 FF E0 
 00 A0 00 20 00 10 00 70 00 40 FF E0 FF D0 FF D0 
 00 90 00 20 FF F0 00 20 FF C0 00 00 00 50 00 50 
 00 00 FF 90
"};

push @raw,{hfc=>undef,dest=>undef,freq=>undef,eqdata=>"
DOCS-IF3-MIB::docsIf3CmStatusUsEqData.80 = Hex-STRING: 08 01 18 00
 FF C0 00 00 01 10 00 50 FF B0 FF A0 
 00 A0 00 50 FF 20 FF 00 01 20 01 60 FC C0 FD 20 
 7F E0 00 00 01 E0 01 00 FE 70 FF 80 01 00 00 40 
 FF 90 00 10 00 00 00 30 00 20 FF D0 00 10 FF C0 
 00 40 00 20 FF D0 FF F0 FF C0 FF D0 00 20 00 10 
 FF E0 00 10 FF D0 00 10 FF E0 FF C0 FF F0 00 20 
 00 10 FF B0
"};

push @raw,{hfc=>undef,dest=>undef,freq=>undef,eqdata=>"
DOCS-IF3-MIB::docsIf3CmStatusUsEqData.81 = Hex-STRING: 08 01 18 00
 00 00 00 50 00 40 00 50 FF A0 FF D0 
 00 E0 00 10 FF 00 FF 70 01 C0 00 00 FB F0 FF 70 
 7F D0 00 00 04 30 01 10 FE F0 00 50 01 00 FF C0 
 FF 40 00 30 00 20 00 20 FF C0 FF A0 FF D0 FF D0 
 00 10 FF E0 FF C0 00 00 FF C0 00 10 00 20 FF D0 
 00 00 00 30 00 30 FF E0 FF A0 FF D0 FF F0 FF F0 
 00 10 00 30
"};

push @raw,{hfc=>undef,dest=>undef,freq=>undef,eqdata=>"
DOCS-IF3-MIB::docsIf3CmStatusUsEqData.82 = Hex-STRING: 08 01 18 00
 FF C0 FF F0 00 30 00 20 FF 90 FF D0 
 00 70 00 30 FF 60 FF B0 00 B0 00 40 FE 90 FE C0 
 7F F0 00 00 FF 70 FD D0 FF 50 FF 40 FF F0 00 20 
 FF 80 00 00 00 50 00 00 FF 50 FF F0 00 00 00 00 
 FF F0 FF E0 FF F0 00 20 00 30 FF C0 FF B0 00 30 
 FF D0 FF F0 FF E0 00 30 FF E0 FF A0 FF F0 00 10 
 FF B0 FF E0
"};

# END FIXME

my $eq = PreEq::EqData->new();
my %opt = (type=>'plain',vertical=>4);
my @cols = ('hfc','dest','freq','usid','STAT');
map { print "\n"; $eq->parsehash(%$_)->print_values(\%opt,@cols); } @raw;
print "\n";

