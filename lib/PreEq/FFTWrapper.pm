package PreEq::FFTWrapper;

use 5.020002;
use strict;
use warnings;

BEGIN {
	require Exporter;
	our @ISA = qw(Exporter);
	our @EXPORT = qw($fun_fft_re_im);
}

our $fun_fft_re_im;

our $VERSION = '0.01';

BEGIN {
	# find FFT module
	require Module::Find;
	my @mod = Module::Find::findsubmod PDL;
	if (grep { $_ eq 'PDL::FFT' } @mod) {
		require PDL::FFT;
		$fun_fft_re_im = sub :prototype($$) {
			my $r = shift;
			my $i = shift;
			# copy list to PDL objects
			my $pdlr = PDL::FFT::pdl(@$r);
			my $pdli = PDL::FFT::pdl(@$i);
			# in place calculation of fft
			PDL::FFT::fft($pdlr,$pdli);
			# get back normal perl lists
			# we can use PDL::FFT::list for flattened 1-d array
			# otherwise we would use PDL::FFT::unpdl
			my @fr = PDL::FFT::list $pdlr;
			my @fi = PDL::FFT::list $pdli;
			return (\@fr,\@fi);
		}
	}
}

1;
__END__

=head1 NAME

PreEq::FFTWrapper - Perl wrapper for FFT when FFT lib found

=head1 SYNOPSIS

  use PreEq::FFTWrapper;
  ($fre,$fim) = &$fun_fft_re_im(\@re,\@im) if defined $fun_fft_re_im;
  # where $fre and $fim are references to lists of re and im after fft
  # do_something_with @$fre,@$fim

=head1 DESCRIPTION

PreEq::FFTWrapper exports reference $fun_fft_re_im to function calculating FFT
when some real FFT found (currently PDL::FFT)

$fun_fft_re_im is automatically exported

=head1 SEE ALSO

See documentation for PDL::FFT for details of FFT implementation,
keep in mind however that &$fun_fft_re_im takes as args and returns
plain perl list references

=head1 AUTHOR

mariusz jadczak, E<lt>mariuszj@toya.com.plE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2016 by mariusz jadczak

This library is free software; you can redistribute it and/or modify
it under the terms of BSD licence

For specific FFT calculating module's license see module's documentation

=cut
