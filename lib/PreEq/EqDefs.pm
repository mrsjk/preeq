package PreEq::EqDefs;

use 5.020002;
use strict;
use warnings;

our @ISA = qw();

our $VERSION = '0.02';

=head1 NAME

B<PreEq::EqDefs> - Perl module for use by B<PreEq::EqData>

=head1 SYNOPSIS

  # [no need to] use PreEq::EqDefs;

=head1 DESCRIPTION

Defs for use by PreEq::EqData and PreEq::EqHTML,
does not provide any functionality of it's own, only provides some default
values for B<PreEq::>... modules,
see below the list and meaning of defined variables

=head2 $num_nibbles

Number of meaningful nibbles for each value in hex string returned by
DOCSIS::DOCS-IF3-MIB::docsIf3CmStatusUsEqData.
Does not apply to header fields which are always four fields of two-nibble
values.
This value is implementation
specific, eighter 4 or 3. Default is 4, which probably suits all common
implementations.

=cut

our $num_nibbles = 4;

=head2 $min_dbc

Min value in dBc for calculated values relative to signal strength, below
which values are not significant. This is used for calculating C<dBc> field
of tap coeficients (if undefined value 1e-12 will be used for log10 arg where
that arg would be le 0).
Default -68 [dBc] is safe bet for equalization analysis (does not directly
correnspond to regular noise level).

=cut

our $min_dbc = -68;

=head2 $fralg

Defines algorithm for presenting frequency domain graph and calculating
delta_u value and alarms returned by get_value, get_values, get_alarms
methods of PreEq::EqData class object, unless overridden by arg passed to
PreEq::EqData::new or PreEq::EqData::eq_data. See below implemented values,
where B<abs> means absolute value of B<re+j*im> of each item
in frequency domain (transformed via FFT from time domain signal values).
Default is 2

=over 2

=item B<0>

10*log10(abs), returned by PreEq::EqData::get_freqval(abs)

=item B<1>

10*log10(abs**2/N**2), where N is number of elements in frequency domain,
returned by PreEq::EqData::get_freqvals(pn)

=item B<2>

20*log10(abs/QM), where QM is quadratic mean of all abs values,
returned by PreEq::EqData::get_freqvals(qm)

=back

=cut

our $fralg = 2;

=head2 $swaph

If evaluates to true, graph of frequency domain values presented by
PreEq::SvgHTML class first draws second half of values than first half,
placing first value in frequency domain (transformed via FFT from time domain)
in the middle of graph.
In PreEq::EqData object this value is applied to 'f' field of fft result
(final result in freq domain taking into account B<fralg> and this B<swaph>
values),
other freq related fields (dB,pn,qm) are unaffected by this value and need
explicit swapping if necessary.
Default is true.

=cut

our $swaph = 1;

=head2 B<fstart>

This is value used for freq graph drawing (see B<PreEq::SvgHTML>) defines
starting element in freq array (method B<get_freqvals>) from which to start
drawing graph, this literally means how many elements to skip from beginning
of freq array while drawing freq graph. Default is 1.

=cut

our $fstart = 1;

=head2 @alarms

List of alarms for preeq stat values, where list index is alarm level.
For requested alarm level indices from 0 upto respective index (inclusive)
is checked for each field, and alarm is triggered when calculated value is
greater than defined for alarm.
By default, two levels are defined, 0 for alarms and 1 for warnings.
Each level of list is a hash with alarm values for fields:
MTC, NMTER, delta_u.

=cut

our @alarms = (
	{ MTC => .45, NMTER => -10, delta_u => 6 },
	{ MTC => .15, NMTER => -15, delta_u => 3.5 }
);

=head2 @preborder

List of border values (dBc) for signal values before main tap in time domain,
applied in reverse order, that is index 0 is for main_tap-1, index 1 for
main_tap-2, etc.
If shorter than number of taps before main tap, than list is extended with
last value upto required number of taps.

=cut

our @preborder = (-10,-10,-20,-20,-30,-30);

=head2 @postborder

List of border values (dBc) for signal values after main tap in time domain,
applied in order to taps after main tap, that is index 0 for main_tap+1,
index 1 for main_tap+2, etc.
If shorter than number of taps after main tap, than list is extended with
last value upto required number of taps

=cut

our @postborder = (-10,-10,-20,-20,-30,-30,-30);

1;
__END__

=head1 SEE ALSO

See documentation for PreEq::EqData and PreEq::EqHTML

=head1 AUTHOR

Mariusz Jadczak, E<lt>mariuszj@toya.com.plE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2016 by Mariusz Jadczak

This library is free software; you can redistribute it and/or modify
it under the terms of BSD licence

=cut

