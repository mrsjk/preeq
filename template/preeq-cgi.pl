#!/usr/bin/perl -w

use strict;

use CGI;
use PreEq::EqHTML;

my $q = CGI->new;
my $chars = $q->param('latin') ? 'iso-8859-2' : 'utf-8';
print $q->header(-charset=>$chars);

my $hfc = $q->param('hfc');
my $dest = $q->param('dest');
my $type = $q->param('type');
my $upst = $q->param('upstream');
my @upst = split ',',$upst if defined $upst;

print $q->start_html("PreEq".($hfc?" $hfc":"").($dest?" $dest":""));

my %f = map { my $v = $q->param($_); defined $v ? ($_,$v) : () }
	qw(fralg swaph fstart pxperdb pathtype nofft);

sub nextup {
	my ($u,%r) = @_ ? shift @_ : shift @upst;
	my @u = map { s/^([\d\.]*).*/$1/;$_ } split '_',$u if $u;
	map { $r{$_} = shift @u; } ('freq','pwr','snr');
	return %r;
}

my @eq;

my @pushed = $q->multi_param('data');
map {
	my $pushed = $_;
	push @eq, map {
		my ($data,@desc) = split ',',$_;
		my %p = map { (split '=',$_)[0..1] } @desc;
		%p = &nextup(@desc) if scalar @desc eq 1 and $desc[0] !~ /=/;
		PreEq::EqHTML->new($data,%p,%f)
	} split ';',$pushed if $pushed;
} @pushed;

my @raw;

unless (@eq) {

# START FIXME
# push real data instead of example data

push @raw,"
DOCS-IF3-MIB::docsIf3CmStatusUsEqData.4 = Hex-STRING: 08 01 18 00
 FF D0 FF F0 00 00 00 30 FF 80 FF 80 
 00 A0 00 B0 FF 60 FE 60 01 30 02 30 FD 00 FA C0 
 7F B0 00 00 03 30 02 A0 FE D0 FE 40 00 C0 00 E0 
 FF 60 FF 30 00 30 00 50 FF 80 FF E0 00 20 FF E0 
 00 A0 00 20 00 10 00 70 00 40 FF E0 FF D0 FF D0 
 00 90 00 20 FF F0 00 20 FF C0 00 00 00 50 00 50 
 00 00 FF 90
";

push @raw,"
DOCS-IF3-MIB::docsIf3CmStatusUsEqData.80 = Hex-STRING: 08 01 18 00
 FF C0 00 00 01 10 00 50 FF B0 FF A0 
 00 A0 00 50 FF 20 FF 00 01 20 01 60 FC C0 FD 20 
 7F E0 00 00 01 E0 01 00 FE 70 FF 80 01 00 00 40 
 FF 90 00 10 00 00 00 30 00 20 FF D0 00 10 FF C0 
 00 40 00 20 FF D0 FF F0 FF C0 FF D0 00 20 00 10 
 FF E0 00 10 FF D0 00 10 FF E0 FF C0 FF F0 00 20 
 00 10 FF B0
";

push @raw,"
DOCS-IF3-MIB::docsIf3CmStatusUsEqData.81 = Hex-STRING: 08 01 18 00
 00 00 00 50 00 40 00 50 FF A0 FF D0 
 00 E0 00 10 FF 00 FF 70 01 C0 00 00 FB F0 FF 70 
 7F D0 00 00 04 30 01 10 FE F0 00 50 01 00 FF C0 
 FF 40 00 30 00 20 00 20 FF C0 FF A0 FF D0 FF D0 
 00 10 FF E0 FF C0 00 00 FF C0 00 10 00 20 FF D0 
 00 00 00 30 00 30 FF E0 FF A0 FF D0 FF F0 FF F0 
 00 10 00 30
";

push @raw,"
DOCS-IF3-MIB::docsIf3CmStatusUsEqData.82 = Hex-STRING: 08 01 18 00
 FF C0 FF F0 00 30 00 20 FF 90 FF D0 
 00 70 00 30 FF 60 FF B0 00 B0 00 40 FE 90 FE C0 
 7F F0 00 00 FF 70 FD D0 FF 50 FF 40 FF F0 00 20 
 FF 80 00 00 00 50 00 00 FF 50 FF F0 00 00 00 00 
 FF F0 FF E0 FF F0 00 20 00 30 FF C0 FF B0 00 30 
 FF D0 FF F0 FF E0 00 30 FF E0 FF A0 FF F0 00 10 
 FF B0 FF E0
";

# END FIXME
}

push @eq, map { PreEq::EqHTML->new($_,&nextup,hfc=>$hfc,dest=>$dest,%f) } @raw;

my $mfn = "preeq_".($hfc?$hfc:$dest?$dest:'').'_mf_'.time;
my %mfn = (id=>$mfn,exppng=>"$mfn.png");

my $mf = PreEq::SvgHTML->multifreq(@eq);
$mf->print_freqvals(width=>700,%f,%mfn);
print "<hr>";

# fancyutf only when charset utf-8
my $fancyutf = $q->charset eq 'utf-8';
map { $_->print_combo(width=>700,fancyutf=>$fancyutf,%f); print "<hr>"; } @eq;

print $q->end_html;

