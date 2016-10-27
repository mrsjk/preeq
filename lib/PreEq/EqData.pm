
package PreEq::EqData;

use 5.020000;
use strict;
use warnings;

use PreEq::EqDefs;

our @ISA = qw(PreEq::EqDefs);

our $VERSION = '0.01';

my $log10 = sub { my $a = shift; log($a>0?$a:.000000000001)/log(10) };

my $fft_loaded;

my $coalesce = sub { map { return $_ if defined $_ } @_; return undef };

sub new :prototype($;%)
{
	my $class = shift;
	my $self = {
		taps => undef,
		main => undef,
		tps => undef,
		coef => undef,
		fft => undef,
		MTC => undef,
		NMTER => undef,
		PreMTTER => undef,
		PostMTTER => undef,
		PPESR => undef,
		delta_u => undef,
		delta_u0 => undef,
		delta_u1 => undef,
		delta_u2 => undef,
		du => undef,
		_user => {},
	};
	bless $self, $class;
	my $raw = shift;
	my %usr = @_;
	$self->parse($raw,%usr);
	return $self;
}

sub eq_data :prototype($;%)
{
	my $self = shift;
	return $self->new(@_);
}

sub parsehash :prototype($%)
{
	my $self = shift;
	my %h = (@_);
	my $data = delete $h{eqdata};
	return $self->parse($data,%h);
}

sub parse :prototype($$;%)
{
	my $self = shift;
	my $raw = shift;
	my %usr = (@_);
	$self->{raw} = $raw;
	$self->{taps} = undef;
	$self->{main} = undef;
	$self->{tps} = undef;
	$self->{coef} = undef;
	$self->{fft} = undef;
	$self->{MTC} = undef;
	$self->{NMTER} = undef;
	$self->{PreMTTER} = undef;
	$self->{PostMTTER} = undef;
	$self->{PPESR} = undef;
	$self->{delta_u} = undef;
	$self->{usid} = undef unless $raw;
	$self->{_user} = {%usr};
	map { $self->{$_} = delete $self->{_user}{$_} if exists $usr{$_} }
		('nofft','fralg','swaph');
	$raw = '' unless defined $raw;
	if ($raw =~ s/\bdocsIf3CmStatusUsEqData[\.](\d+)//) {
		$self->{usid} = $1 unless defined $self->{_user}{usid};
	}
	$raw =~ s/^(\s*\S*[^0-9a-fA-F\s:]\S*\s*)+//g;
	$raw =~ s/\s//g;
	my $mh = '([[:xdigit:]]{2})' x 4;
	if ($raw =~ s/^${mh}//) {
		$self->{main} = hex $1;
		$self->{tps} = hex $2;
		$self->{taps} = hex $3;
	} else {
		$self->{main} = 0;
		$self->{tps} = 0;
		$self->{taps} = 0;
	}
	@{$self->{coef}} = ();
	$mh = '([[:xdigit:]]{4})' x 2;
	my $nm = $PreEq::EqDefs::num_nibbles;
	$nm = abs($nm?$nm:4);
	foreach my $i (1 .. $self->{taps}) {
		my ($a,$p) = (0,0);
		if ($raw =~ s/^${mh}//) {
			($a,$p) = (hex substr($1,-$nm), hex substr($2,-$nm));
			$a = $a - hex '1'.'0'x$nm if $a >= hex '8'.'0'x($nm-1);
			$p = $p - hex '1'.'0'x$nm if $p >= hex '8'.'0'x($nm-1);
		}
		$self->{coef}[$i] = { re=>$a, im=>$p, te=>($a**2+$p**2) };
	}
	$self->calc_stat;
	$self->calc_fft($usr{fralg},$usr{swaph});;
	return $self;
}

sub calc_stat :prototype()
{
	my $self = shift;
	my $tte = 0;
	my $premtter = 0;
	my $postmtter = 0;
	my $mte = 0;
	$self->{MTC} = undef;
	$self->{PreMTTER} = undef;
	$self->{PostMTTER} = undef;
	$self->{NMTER} = undef;
	$self->{ppser} = undef;
	foreach my $i (1 .. $self->{taps}) {
		$premtter += $self->{coef}[$i]{te} if $i < $self->{main};
		$postmtter += $self->{coef}[$i]{te} if $i > $self->{main};
		$mte = $self->{coef}[$i]{te} if $i == $self->{main};
		$tte += $self->{coef}[$i]{te};
	}
	return undef unless $mte and $tte;
	my $mindbc = $PreEq::EqDefs::min_dbc;
	my $mdbc = sub { defined $mindbc?$_[0]<$mindbc?$mindbc:$_[0]:$_[0] };
	$self->{coef}[$_]{dBc} = &$mdbc(10*&$log10($self->{coef}[$_]{te}/$tte))
		foreach (1 .. $self->{taps});
	$self->{MTC} = 10*&$log10($tte/$mte) if $mte and $tte;
	$self->{PreMTTER} = 10*&$log10($premtter/$tte) if $premtter;
	$self->{PostMTTER} = 10*&$log10($postmtter/$tte) if $postmtter;
	$self->{NMTER} = 10*&$log10(($premtter+$postmtter)/$tte)
		if ($premtter+$postmtter);
	$self->{PPESR} = 10*&$log10($premtter/$postmtter)
		if $postmtter and $premtter;
	return $self->{MTC};
}

sub calc_fft :prototype($;$$)
{
	my $self = shift;
	my $fralg = shift;
	my $swaph = shift;
	$self->{$_} = undef
		foreach ('fft','du','delta_u','delta_u0','delta_u1','delta_u2');
	return undef unless $self->{taps} and not $self->get_value('nofft');
	unless (defined $fft_loaded) {
		# might be expensive so do it once when necessary
		use PreEq::FFTWrapper;
		$fft_loaded = $fun_fft_re_im;
		$fft_loaded = sub { undef } unless defined $fft_loaded;
	}
	return undef unless defined $fun_fft_re_im;
	# extend to power of 2 length
	my @a = $self->get_coeflist('re',-1);
	my @p = $self->get_coeflist('im',-1);
	my ($fa,$fp) = &$fun_fft_re_im(\@a,\@p);
	my ($lu0,$hu0);
	my ($lu1,$hu1);
	my $tmp;
	@{$self->{fft}} = ();
	foreach my $i (0 .. @$fa-1) {
		my $e = sqrt($fa->[$i]**2+$fp->[$i]**2);
		my $ne = 10*&$log10($e);
		my $se = 10*&$log10($e**2/@$fa**2);
		$self->{fft}[$i] = {
			re=>$fa->[$i],im=>$fp->[$i],abs=>$e,
			dB=>$ne,pn=>$se
		};
		($lu0,$hu0,$lu1,$hu1) = ($ne,$ne,$se,$se) if not $i;
		($lu0,$tmp,$hu0) = sort { $a <=> $b } ($lu0,$hu0,$ne);
		($lu1,$tmp,$hu1) = sort { $a <=> $b } ($lu1,$hu1,$se);
	}
	return undef unless $self->{fft};
	my $qm = 0;
	map { $qm += $_->{abs}**2/@{$self->{fft}} } @{$self->{fft}};
	$qm = sqrt($qm);
	my @qma = map { $_->{qm} = 20*&$log10($_->{abs}/$qm) } @{$self->{fft}};
	my ($lu2,$hu2) = (-(-inf),-inf);
	map { ($lu2,$hu2) = ($_<$lu2?$_:$lu2,$_>$hu2?$_:$hu2) } @qma;
	$self->{delta_u0} = $hu0-$lu0;
	$self->{delta_u1} = $hu1-$lu1;
	$self->{delta_u2} = $hu2-$lu2;
	$self->{du} = [$self->{delta_u0},$self->{delta_u1},$self->{delta_u2}];
	return $self->calc_fralg($fralg,$swaph);
}

sub calc_fralg :prototype($;$$)
{
	my $self = shift;
	my $new_fralg = &$coalesce(shift,$self->get_fralg,0);
	my $new_swaph = &$coalesce(shift,$self->get_swaph,0);
	my @fra = ('dB','pn','qm');
	my $fri;
	$new_fralg = $self->get_fralg unless defined $new_fralg;
	$self->{fralg} = $new_fralg
		if exists $self->{fralg} or $new_fralg ne $self->get_fralg;
	$new_swaph = $self->get_swaph unless defined $new_swaph;
	$self->{swaph} = $new_swaph
		if exists $self->{swaph} or $new_swaph ne $self->get_swaph;
	$fri = $fra[$self->get_fralg] if defined $fra[$self->get_fralg];
	map { $_->{f} = (defined $fri ? $_{$fri} : undef) } @{$self->{fft}};
	my $n = scalar(@{$self->{fft}});
	my $m = $self->get_swaph() ? $n/2 : 0;
	$fri = 'f' unless $fri;
	map { $self->{fft}[$_]{f} = $self->{fft}[($_+$m)%$n]{$fri} } (0..$n-1);
	$self->{delta_u} = $self->{du}[$self->get_fralg()];
	return $n;
}

sub get_values :prototype(;@)
{
	my $self = shift;
	my @args = @_;
	my %h = ();
	push @args,('MTC','NMTER','PreMTTER','PostMTTER','PPESR','delta_u');
	map { $h{$_} = $self->get_value($_) if $self->has_value($_) } @args;
	return %h;
}

sub has_value :prototype($)
{
	my $self = shift;
	my $var = shift;
	return undef unless defined $var;
	map { return $_ if /^$var$/i } keys %$self;
	return $var if exists $self->{_user}{$var};
	return undef;
}

sub set_value :prototype($$;$)
{
	my $self = shift;
	my $fld = shift;
	my $val = shift;
	my $del = shift;
	return undef if exists $self->{$fld};
	return defined delete $self->{_user}{$fld} ? $val : undef if $del;
	return $self->{_user}{$fld} = $val;
}

sub get_value :prototype($)
{
	my $self = shift;
	my $var = $self->has_value(shift);
	return undef unless defined $var;
	my $v = exists $self->{$var} ? $self->{$var} : $self->{_user}{$var};
	return scalar(@$v) if ref $v eq 'ARRAY';
	return scalar(keys %$v) if ref $v eq 'HASH';
	return $v;
}

sub get_fralg :prototype()
{
	my $self = shift;
	return defined $self->{fralg} ? $self->{fralg} : $PreEq::EqDefs::fralg;
}

sub get_swaph :prototype()
{
	my $self = shift;
	return defined $self->{swaph} ? $self->{swaph} : $PreEq::EqDefs::swaph;
}

sub get_alarms :prototype(;$$)
{
	my $self = shift;
	my $lvl = shift;
	my $brd = shift;
	my %r = ();
	my $v;
	my $al;
	return undef unless defined $self->{main};
	my $gt = sub { return defined $_[0] && defined $_[1] && $_[0]>$_[1] };
	($lvl,$al) = (undef,1) if $lvl < 0;
	$lvl = $#PreEq::EqDefs::alarms unless defined $lvl;
	foreach my $i (0..$lvl) {
		$a = $PreEq::EqDefs::alarms[$i];
		$al = $i if defined $al;
		map { 
			$v = $self->get_value($_);
			$r{$_} = &$coalesce($r{$_},$al,$v) if &$gt($v,$a->{$_})
		} keys %$a if $a;
	}
	return %r unless $brd;
	my @ne = $self->get_coeflist('dBc');
	return %r unless scalar(@ne);
	my $m = $self->{main}-1;
	my @p = reverse map { shift @ne } (1..$self->{main});
	shift @p;
	my %t;
	$al = 0 if defined $al;
	map {
		$v = (sort { $a <=> $b } ($_,$#PreEq::EqDefs::preborder))[0];
		$t{$m-$_} = &$coalesce($al,$p[$_])
			if $v >= 0 and defined $PreEq::EqDefs::preborder[$v]
			   and $p[$_] >= $PreEq::EqDefs::preborder[$v]
	} (0..$#p) if @PreEq::EqDefs::preborder;
	$m += 2;
	map {
		$v = (sort { $a <=> $b } ($_,$#PreEq::EqDefs::postborder))[0];
		$t{$m+$_} = &$coalesce($al,$ne[$_])
			if $v >= 0 and defined $PreEq::EqDefs::postborder[$v]
			   and $ne[$_] >= $PreEq::EqDefs::postborder[$v]
	} (0..$#ne) if @PreEq::EqDefs::postborder;
	$r{tap} = \%t if %t;
	return %r;
}

sub get_coeflist :prototype($;$)
{
	my $self = shift;
	my $a = shift;
	my $n = shift;
	my $b = 1;
	$b += 1 while 2**$b < $self->{taps};
	$n = 2**$b if defined $n and $n < 0;
	$n = $self->{taps} unless defined $n;
	my @l;
	push @l,0 foreach (1 .. $n-$self->{taps});
	$n = $self->{taps} if $n > $self->{taps};
	push @l,$self->{coef}[$_]{$a} foreach (1 .. $n);
	return @l;
}

sub get_freqvals :prototype($)
{
	my $self = shift;
	my $a = shift;
	$a = 'f' unless defined $a;
	return undef unless $self->{fft};
	return map { $_->{$a} } @{$self->{fft}};
}

sub format_values_list :prototype(@)
{
	my $self = shift;
	my $opt = {};
	$opt = shift if ref $_[0] eq 'HASH';
	my @cols = (@_);
	my $iscsv = &$coalesce($opt->{type},'plain') eq 'csv';
	my $itemf = &$coalesce($opt->{fmtitem},$iscsv?'%s':'%2$s: %1$s%3$s');
	my $dfi = &$coalesce($opt->{fmtval},"%.02f");
	my @s = ('MTC','NMTER','PreMTTER','PostMTTER','PPESR','delta_u');
	my $af;
	my %al = $self->get_alarms(-1,1);
	my $al = $opt->{alarms};
	$al = ['!!','!'] unless $al;
	$al = [$al] unless ref $al eq 'ARRAY';
	@cols = @s unless @cols;
	my %im = (
		'TAP' => ['TAP','coef',[1..$self->{taps}],'dBc','T'],
		'FREQ' => ['FREQ','fft',[0..$#{$self->{fft}}],'f','F'],
	);
	$im{TAP}[2] = $opt->{lstTAP} if ref $opt->{lstTAP} eq 'ARRAY';
	$im{FREQ}[2] = $opt->{lstFREQ} if ref $opt->{lstFREQ} eq 'ARRAY';
	my @res;
	foreach my $col (map { exists $im{$_} ? $im{$_} : [$_] } @cols) {
		my $c = $col->[0];
		my $cc = $col->[2];
		my $ar = $col->[1];
		my $ii = &$coalesce($opt->{'fmt'.$c},$col->[3]);
		my $hf = &$coalesce($opt->{'fmthdr'.$c},$opt->{'fmthdr'});
		my $fi;
		if (ref $ii eq 'ARRAY') {
			$ii = [@$ii];
			$fi = shift @$ii if $ii->[0] =~ /%/;
		} else {
			$ii = [$ii];
		}
		map {
			my $i = $_;
			map {
				my $h = $c;
				$h = $col->[4] if defined $col->[4];
				$h .= "_$i" if defined $i;
				$h .= "_$_" unless $fi;
				$h = sprintf $hf,$c,defined $i?$i:'',$_ if $hf;
				my $iv = $self->{$ar}[$i]{$_} if defined $i;
				my $v = $fi ? undef : $iv;
				my $f = defined $v ? $dfi : '%s';
				$v = &$coalesce($v,$opt->{nullas},'');
				my $vo = sprintf $f,$v;
				if ($fi) {
					my @av = map { 
						$iv = $self->{$ar}[$i]{$_};
						defined $iv ? $iv : 0;
					} @$ii;
					$vo = sprintf $fi,@av;
				}
				my $a = @$al[$al{$c}{$i}]
					if defined $i and exists $al{$c}{$i};
				$a = '' unless defined $a;
				my $x;
				map { $x = $opt->{$_} if /^fmtitem$h$/ }
					keys %$opt;
				$f = &$coalesce($x,$opt->{'fmtitem'.$c},$itemf);
				$v = sprintf $f,$vo,$h,$a;
				push @res,$v;
			} $fi ? ($fi) : @$ii;
		} @$cc if $cc;
		next if $cc;
		$cc = $c eq 'STAT' ? [@s] : [$c];
		map {
			$hf = &$coalesce($opt->{'fmthdr'.$_},$opt->{'fmthdr'});
			$c = $_;
			my @nc = ('MTC','NMTER','PreMTTER','PostMTTER','PPESR',
			          'delta_u','delta_u0','delta_u1','delta_u2');
			my $v = $self->get_value($_);
			my $f = defined $v ? &$coalesce($dfi,'%s') : '%s';
			$f = '%s' unless grep { $_ eq $c } @nc;
			my $isnull = not defined $v;
			$v = &$coalesce($v,$opt->{nullas},'');
			my $vo = $isnull ? $v : sprintf $f,$v;
			my $a = @$al[$al{$c}] if exists $al{$c};
			$a = '' unless defined $a;
			my $h = $hf ? sprintf $hf,$c,$c,'' : $c;
			$fi = $opt->{fmtitemSTAT} if grep /^$c$/,@s;
			$fi = &$coalesce($opt->{'fmtitem'.$c},$fi,$itemf);
			if ($isnull) {
				my $k = "[- +\\d#\\.lhV]*[csduoxefgXEGpniDUOF]";
				my $l = '([^%]|%%)';
				$fi =~ s/^(($l*%\d\$$k)*$l*)(%$k)/$1%s/;
				$fi =~ s/%1\$$k/%1\$s/g;
			}
			push @res, sprintf $fi,$vo,$h,$a;
		} @$cc;
	}
	return @res;
}

sub print_values :prototype(@)
{
	my $self = shift;
	my $opt = {};
	my @cols = @_;
	$opt = shift @cols if ref $_[0] eq 'HASH';
	my $iscsv = $opt->{type} eq 'csv' if exists $opt->{type};
	my $csvhdr = exists $opt->{csvhdr} ? $opt->{csvhdr} : 'once';
	my $sep = $opt->{sep};
	$sep = $iscsv ? ',' : "\n" unless defined $sep;
	if ($iscsv and $csvhdr) {
		my $hopt = {%$opt};
		map { delete $hopt->{$_} if /^fmtitem/ } keys %$hopt;
		$hopt->{fmtitem} = '%2$s';
		my @h = $self->format_values_list($hopt,@cols);
		print join $sep,@h;
		print "\n";
		$opt->{csvhdr} = 0 if $csvhdr eq 'once';
	}
	return 0 if $opt->{onlyhdr};
	my @v = $self->format_values_list($opt,@cols);
	print join $sep,@v;
	print "\n";
	return scalar @v;
}

1;
__END__

=pod

=head1 NAME

B<PreEq::EqData> -
Perl extension for parsing and calculating upstream preeq data

=head1 SYNOPSIS

  use PreEq::EqData;

  # object initialization - new and eq_data are synonims
  $eq1 = PreEq::EqData->new($data,%aargs);
  $eq2 = PreEq::EqData->eq_data($data,%aargs);
  $eq3 = PreEq::EqData->eq_data($data);

  # object reinitialization with new data
  $eq1->parse($data,%aargs);

  # get calculated stat fields and values of object
  %val = $eq1->get_values(@oargs);
  %val = $eq1->get_values;

  # get object field value
  $val = $eq1->get_value($fld);

  # get hash of alarms
  %alarms = $eq1->get_alarms($lvl);
  %alarms = $eq1->get_alarms;

  # get list of coeficients values of field $cfld (re,im,te,dBc)
  @coef = $eq1->get_coeflist($cfld);

  # get list of fft values of field $ffld (re,im,abs,dB,pn,qm,f)
  @freqval = $eq1->get_freqvals($ffld);

  # print values of flds with options
  $eq->print_values(\%opt,@flds);

  # print csv for multiple data in %md ( id => eqdata )
  %opt = (type=>'csv');
  $eq->parse($md{$_},id=>$_)->print_values(\%opt,'id','STAT')
  	foreach keys %md;

=head1 DESCRIPTION

module parses DOCSIS::DOCS-IF3-MIB::docsIf3CmStatusUsEqData and calculates
taps data (re and im parts of signal, energy of signal in time domain),
stats data (MTC, NMTER, PreMTTER, PostMTTER, PPESR),
alarms and values of frequency responce data (re and im parts of signal,
energy representation of signal in frequency domain for defined algorithms)
via FFT

=head2 OBJECT INITIALIZATION

eq_data object initialization, B<new> or B<eq_data> with prototype($;%)
takes 1 or more args:

  first argument is data containing preequalization data as returned by
  DOCSIS::DOCS-IF3-MIB::docsIf3CmStatusUsEqData[.<id>],
  may contain this field name, than <id> saved as usid field of object,
  begining upto (exclusive) first word containg only hex digits is ignored
  rest is interpreted as header and taps coeficient data as in DOCSIS spec

  optional arg is a hash of additional fields for object which must not be
  predefined object fields (those are silently ignored) with some special
  fields (preserved upon parse() unless passed again):
    nofft   when evaluates to true suppresses fft calculation
            which may be kind of expensive depending on available fft
            algorithm (currently only PDL::FFT is used, which is fast
            enough to not bother)
    fralg   frequency responce and it's delta_u calculation algorithm,
            with fralg values (other ignored):
            0 - for delta of values in field dB of fft result list
            1 - for delta of values in field pn of fft result list
            2 - for delta of values in field qm of fft result list
            default is value defined in $PreEq::EqDefs::fralg (which is 2),
	    fields dB,pn,qm are calculated anyway for each element of fft
            result list, also delta_u0, delta_u1, delta_u2 are calculated,
	    object's field du contains list of all calculated deltas,
	    rfalg applies to delta_u returned by get_values and get_alarms,
	    and field f of fft result list (get_freqvals('f');),
	    see also swaph below and for details of algorithm and dB,pn,qm
	    fields see method get_freqvals;
    swaph   when evaluates to true swaps halves of f field list, thus
            f field of fft result list holds and get_freqvals('f') returns
	    swapped list of graph values in freq domain

=head2 INTERNAL METHODS

those are methods called internally upon object initialization and data
calculation, need not be called directly (unless for special purposes),
and are refereced here only for completeness of documentation

=over

=item B<parse($data)>;
=cut
=item B<parse($data,%aargs)>;

parses DOCSIS::DOCS-IF3-MIB::docsIf3CmStatusUsEqData, called internally
upon object initialization, may be used for object reinitialization with
new coeficients data (and optionally with additional fields), preserving
special fields (nofft,fralg,swaph) unless passed again with new values,
calls calc_stat and calc_fft, returns object's reference

=item B<parsehash(%args)>;

same as S<parse($data,%args)>
except data is expected to be in S<$args{eqdata}>

=item B<calc_stat>;

calculates stats (MTC,NMTER,PreMTTER,PostMTTER,PPESR) of parsed data

=item B<calc_fft>;

calculates fft of signal, that is converts time domain signal values
to frequency domain signal values, calls calc_fralg with default values

=item B<calc_fralg($fralg,$swaph)>;

calculates 'f' field of frequency domain values (see get_freqvals('f'))
and delta_u based on values passed (or default values) of fralg and swaph,
when defined values for args are passed, appropriate object fields are changed
if necessary (that is, if they where defined in object or they are different
than defaults defined in PreEq::EqDefs)

=item B<get_fralg>;

get value of fralg, eighter passed to object or of last call of calc_fralg,
or default defined in $PreEq::EqDefs

=item B<get_swaph>;

get value of swaph, eighter passed to object or of last call fo calc_fralg,
or default defined in $PreEq::EqDefs

=item B<has_value($fld)>;

returns object's field name iff $fld exists in object (case insensitive)
including additional user-defined fields (case sensitive), undef otherwise

=item B<set_value($fld,$val)>;
=cut
=item B<set_value($fld,$val,$del)>;

sets user defined field $fld to value $val or deletes field $fld if $del,
$fld must not exist as object internal field

=back

=head2 DATA EXTRACTION METHODS

methods for extracting parsed and calculated values and alarms

=over

=item B<get_values>;
=cut
=item B<get_values(@oargs)>;

get hash of stat values (MTC,NMTER,PreMTTER,PostMTTER,PPESR,delta_u)
if defined and optional args if exist (nonexistent optional args are
silently ignored)
for delta_u of specific algorithm (see sections OBJECT INITIALIZATION
and method get_freqvals) pass delta_u<no> ie delta_u0, delta_u1 etc,
user-defined fields are case sensitive, builtin are case insensitive

=item B<get_value($fld)>;

get value of specific field, rules for get_values apply

=item B<get_alarms>;
=cut
=item B<get_alarms($lvl,$wborder)>;

get hash of alarm values for alarm levels $lvl and below, default level
is max (get alarms of all levels),
if optional $wborder evaluates to true, dBc fields of coeficients are checked
against @PreEq::EqDefs::preborder and @PreEq::EqDefs::postborder, field taps
of returned hash is hash of pairs idx => dBc value for taps equal or greater
than border values, number of taps are 1-based (like main tap),
if $lvl < 0 then lowest alarm levels are returned instead of values for fields
(0 as alarm level for tap dBc values)

=item B<get_coeflist($fld)>;
=cut
=item B<get_coeflist($fld,$num)>;

get list of coeficients for taps (parsed eq data in time domain),
where $fld is one of:

=over 1

 re  - real part of signal (amplitude)
 im  - imaginary part of signal
 te  - tap energy calculated as re**2+im**2
 dBc - normalized tap energy calculated as 10*log10(te/tte),
       where tte is total tap energy (energy of all taps)

=back

optional $num is number of elements to return, if greater than number
of taps than list is extended to $num length (with leading zeroes),
if -1 than list length is extended to match closest power of 2 length
with leading zeroes (needed internally for FFT), for 24 taps that makes
list with 32 elements (8 elements of 0 and 24 tap elements)

=item B<get_freqvals>;
=cut
=item B<get_freqvals($fld)>;

get list of calculated (via FFT) values of signal in frequency domain,
where $fld is one of:

=over 1

 re  - real part of signal (amplitude)
 im  - imaginary part of signal
 abs - energy as abs of complex(re,im)
 dB  - 10*log10(abs)
 pn  - 10*log10(abs**2/N**2), where N is number of elements
 qm  - 20*log10(abs/QM), where QM is quadratic mean of all abs values
 f   - final result of signal values in frequency domain,
       one of dB,pn,qm depending on fralg and swaph values

=back

for simplicity, default arg is 'f', which is special for not only selecting
default algorithm (based on fralg object field or PreEq::EqDefs::fralg), but
also for applying swaph switch (swaph object field or PreEq::EqDefs::swaph),
for other args returned lists are in order of fft result

=item B<format_values_list(@flds)>;

returns formatted list of values for fields passed in @flds argument,
requested fields need not exist and items are returned for all,
if first argument is a hash reference, than fields are formatted accordingly
to options passed in that hash, see method B<print_values> below for list
of special fields and options (except options csvhdr, onlyhdr and sep,
option type only applies here to default fmtitem)

=item B<print_values(@flds)>;

prints (to default filehandle) formatted values for fields passed
in @flds arg, requested fields need not exist within object
(for those that do not exist empty (or specified in options) values are
printed), if first argument is a hash reference than is treated as options,

requested fields may be predefined object fields or user supplied fields,
special fields are supported for which multiple values are returned:

  STAT - fields: MTC,NMTER,PreMTTER,PostMTTER,PPESR,delta_u
  TAP  - tap coeficients accordingly to fmtTAP option
  FREQ - values in frequency domain accordingly to fmtFREQ option

supported options optionally passed in first arg (as hash reference):

=over 2

=item B<type>

supported types are: plain and csv, for anything other default plain is assumed

=item B<sep>

separator for printed items, default is "\n" for type=>'plain'
and "," for type=>'csv'

=item B<csvhdr>

print csv header first if csvhdr evalueates to true, with special value 'once'
for printing csv header only once for passed options hash, that is when
csvhdr=>'once' exists in option hash, csv header is printed and value of
csvhdr within passed hash is changed to 0, suppresing printing of header for
consecutive calls to print_values method (beware of passing anonymous hash
reference for which updates do not matter), value 'once' is default;
note: this option prints csv header line and values line, to extract or
print only header set B<onlyhdr> or set S<B<< fmtitem => '%2$s' >>>
(but fmtitem do not change S<B<< csvhdr => 'once' >>>)

=item B<onlyhdr>

only header is printed, that is requested field names with special fields
parsed, for type=>'plain' nothing is printed, for type=>'csv' option B<csvhdr>
applies

=item B<fmthdr>

=item B<< fmthdr<fld> >>

format for header name of field <fld>, with argument being field name,
and optional two arguments, index and TAP or FREQ elem's field,
for list fields (TAP,FREQ), fmthdr applies to all fields for which fmthdr<fld>
is not defined, defaults to field name for common fields,
'T_<idx>_<tapfld>' for TAP or 'F_<idx>_<freqfld>' for FREQ

=item B<fmtitem>

=item B<< fmtitem<fld> >>

format for tripple (value,name,alvl) for each requested field name,
this is sprintf format string and list (value,name,alvl) is passed to
sprintf for that format, thus "%1$s" evaluates to value of requested
field and "%2$s" evaluates to name of that field, third element "%3$s" is
value of alarm mark (see option B<alarms> below and method B<get_alarms> with
negative alarm level),
set B<< fmtitem<fld> >> to override specific <fld> format,
default is S<"%2$s: %1$s%3$s"> for type 'plain' and "%s" for type 'csv'

=item B<alarms>

list of alarm marks for printed items, where list indices correspond to
alarm levels as defined in @PreEq::EqDefs::alarms, default is ['!!','!']

=item B<fmtval>

format for known numeric values, that is for all calculated fields,
all TAP fields and all FREQ fields, default is "%.02f",
for all other including user supplied fields "%s" is used

=item B<fmtTAP>
=cut
=item B<fmtFREQ>

format for each TAP or FREQ item, this may be single scalar for field name
of TAP or FREQ element (see methods get_coeflist and get_freqvals), 
or list of those fields in which case separate items are printed (with names
'T_<idx>_<fldvalue>' or 'F_<idx>_<fldvalue>), or list containing
format string (string with % char) as first element and fields
(due to user-specified formatting undefined values are converted to 0)
in which case one item is printed (list passed to sprintf function), with
detault format for each value as specified by option B<fmtval> (name
'T_<idx>' or F_<idx>), defaults are: 'dBc' for TAP and 'f' for FREQ

=item B<lstTAP>
=cut
=item B<lstFREQ>

array of TAP or FREQ indecies to print if 'TAP' or 'FREQ' list requested,
by default all printed for requested 'TAP' or 'FREQ' field

=back

=back

=head1 NOTES

This module should not be of much use for ordinary people, it's purpose is
to help in visualizing and analyzing signal distortions in return path of
DOCSIS plant, and that means it may only be of any use for people that know
what it is about.

Some aspects are covered within this documentation, for more details see
the source code. Bearing in mind that perl is mostly write-only language,
I tried to make it as readable as possible, but... well, it's perl
and nobody cares anyway.

=head1 SEE ALSO

PreEq::EqDefs, PreEq::FFTWrapper, PreEq::EqHTML

=head1 AUTHOR

Mariusz Jadczak, E<lt>mariuszj@toya.com.plE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2016 by Mariusz Jadczak

This library is free software; you can redistribute it and/or modify
it under the terms of BSD licence

=cut

