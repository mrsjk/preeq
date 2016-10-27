package PreEq::EqHTML;

use 5.020002;
use strict;
use warnings;

no warnings 'utf8';

use PreEq::EqData;
use PreEq::SvgHTML;

our @ISA = qw(PreEq::EqData PreEq::SvgHTML);

our $VERSION = '0.02';

# private utility functions

my $coalesce = sub { map { return $_ if defined $_ } @_; return undef };

my $printdiv;
$printdiv = sub {
	if (ref $_[0] eq 'CODE') {
		my $f = shift;
		my $r = &$f(@_);
		print $r if defined $r;
		return;
	}
	my $st = shift if ref $_[0] eq 'HASH';
	my %st = (%$st) if $st;
	print '<div';
	map { my $v = delete $st{$_}; print " $_='$v'" if $v }
		('name','id','class','title');
	print ' style="',join(';',map {"$_:$st{$_}"} keys %st),'"' if %st;
	print '>';
	while (@_) {
		if (ref $_[0] eq 'HASH') {
			&$printdiv(@_);
			print '</div>';
			return;
		}
		if (ref $_[0] eq 'ARRAY') {
			&$printdiv(@{$_[0]});
			shift;
			next;
		}
		map { print $_ if defined $_ } (shift);
	}
	print "</div>";
	return;
};

my $print_tag;
$print_tag = sub {
	while (@_) {
		my $i = shift;
		&$print_tag(@$i) if ref $i eq 'ARRAY';
		next unless ref $i eq 'HASH';
		my %h = (%$i);
		my $t = delete $h{tag};
		my $v = delete $h{val};
		next unless $t;
		print "<$t ".join(' ',map {"$_='$h{$_}'"} keys %h).">";
		&$print_tag(@_) if grep /^$t$/,('g','svg');
		print $v if defined $v;
		print "</$t>\n";
		return if grep /^$t$/,('g','svg');
	}
};

my $printedscriptswitchdisplay = 0;
my $makeswitchdisplay = sub {
	my $id = shift;
	my $pr = &$coalesce(shift,'');
	my $bt = &$coalesce(shift,'+');
	my $fn = &$coalesce(shift,'switchdisplay');
	my $f = shift;
	my $s = 'border:1px solid blue;font-size:10px;margin-bottom:4px;';
	$s .= 'vertical-align:middle;';
	my $ret = "<a id='$id' onclick=$fn('$id','$pr') style='$s'>$bt</a>";
	$printedscriptswitchdisplay = 0 if $f;
	return $ret if $printedscriptswitchdisplay++;
	print "<script>function $fn(i,p) { ";
	print "var e = document.getElementById(p+i); ";
	print "e.style.display = e.style.display=='none'?'block':'none'; ";
	print "var c = document.getElementById(i).childNodes[0]; ";
	print "if (c.textContent == '+' && e.style.display == 'block') ";
	print "{ c.textContent = '--'; } ";
	print "if (c.textContent == '--' && e.style.display == 'none') ";
	print "{ c.textContent = '+'; } ";
	print "}</script>\n";
	return $ret;
};

=pod

=head1 NAME

PreEq::EqHTML - Perl extension for HTML visualization of PreEq::EqData

=head1 SYNOPSIS

  use PreEq::EqHTML;
  $eq = PreEq::EqHTML->new($eqdata);
  # print combo bar,stats and freq graph below
  $eq->print_combo(\%opt);
  # print table of calculated values
  $eq->print_values(\%opt,@cols);

=head1 DESCRIPTION

See L<PreEq::EqData> module documentation. This module adds methods to
print (to defalt filehandle) HTML visualization of parsed and calculated
preequalization data. See below list of supported methods.

=cut

=head2 B<print_combo(%opt)>;

combo of bars and stats in first row and freq graph in second row
(with names of sub divs: bars, stat, freq)
with additional user supplied header, some expected good-to-see values,
hidden raw eq data (div with name 'raw') and swichable eq data table,
supported options:

=over

=item B<width>

width in px (integer), this is combo width, that is sum of bar and stat
width (where min bar width is 100, min stat width depends on font size
which in turn depends on height), and also this is width of freq graph
below bar and stat, default width is 600

=item B<height>

height in px (integer), this is bar and stat height (min is 100),
freq graph height is variable depending on signal amplitude,
default height (of bar and stat) is 200

=item B<name>

name of enclosing div tag, default 'eqcombo'

=item B<id  >

id of enclosing div tag

=item B<hdr >

header line within enclosing div above graph combo, this is literally printed
within div tag, no checking is made so make sure it does not break DOM
structure, below this is line containing values of hfc, freq, usid if defined
within object

=back

other options may alter freq graph, see B<PreEq::SvgHTML::print_freqvals>

=cut

sub print_combo :prototype($%)
{
	my $self = shift;
	my %opt = (@_) if @_;
	my $w = &$coalesce($opt{width},$self->get_value('fwidth'),600);
	my $h = &$coalesce($opt{height},200);
	$h = 100 if $h < 100;
	my $fs = $h > 138 ? 16 : int(($h/6-2)/1.33);
	my $vw = $fs*14+4;
	$w = 100+$vw if $w < 100+$vw;
	my $bw = $w-$vw-1;
	$opt{width} = $w;
	($w,$h) = ("${w}px","${h}px");
	my %bvs = (width=>$w,height=>$h,position=>'relative');
	my %cs = (name=>&$coalesce(delete $opt{name},'eqcombo'));
	$cs{id} = delete $opt{id} if $opt{id};
	my $swid = &$coalesce($cs{id},rand);
	my @hs = (&$makeswitchdisplay($swid,'eqdata'));
	push @hs,delete($opt{hdr})."<br>" if $opt{hdr};
	my @hdrv = ('id','dest','hfc','usid','freq','pwr','snr');
	map { my $v = $self->get_value($_); push @hs," $_:$v " if $v } @hdrv;
	push @hs,[{name=>'raw',display=>'none'},$self->{raw}] if $self->{raw};
	my $prtbrs = sub { (shift)->print_coefbars(@_); undef; };
	my $prtfrq = sub { (shift)->print_freqvals(@_); undef; };
	my $prtvls = sub { (shift)->print_values(@_); undef; };
	my @bd = ($prtbrs,$self,name=>'bars',width=>"${bw}px",height=>$h);
	my @vd = ($prtvls,$self,
	          {vertical=>6,tagatt=>"style='font-size:$fs'",
	           fancyutf=>$opt{fancyutf}});
	my %vs = (name=>'stat',position=>'absolute',top=>'0px',right=>'0px',
	          height=>$h,'background-color'=>'lightblue',width=>"${vw}px");
	my %ss = (height=>'1px');
	my $fn = "preeq_";
	$fn .= &$coalesce($self->get_value('hfc'),$self->get_value('dest'),
	                  $self->get_value('id'),'');
	map { $fn .= sprintf "_%.1f",$_ if $_ } ($self->get_value('freq'));
	map { $fn .= "_$_" if $_ } ($self->get_value('usid'));
	$fn .= "_".time;
	my @fd = ($prtfrq,$self,name=>'freq',id=>$fn,exppng=>"$fn.png",%opt);
	my @rd = ($prtvls,$self,
	          {border=>'1px solid grey',fontsize=>'14px',
	           fmtTAP=>['re','im','dBc'],
	           fmtFREQ=>['re','im','abs','dB','pn','qm','f'],
	           fancyutf=>$opt{fancyutf},fstart=>$opt{fstart}},
	          'tidx','TAP','fidx','FREQ');
	my %rs = (name=>'eqdata',id=>"eqdata$swid",display=>'none');
	&$printdiv(\%cs,@hs,[\%bvs,\@bd,[\%vs,\@vd]],[\%ss],\@fd,[\%rs,\@rd]);
}

=head2 B<print_values(@flds)>;

prints HTML table (unless type=>'plain' or type=>'csv' passed)
with values of fields,
with semantics, arguments, fields and options (with some differences) like
B<PreEq::EqData::print_values>,

options supported in HTML table version (some unique to HTML version and
some inherited from B<PreEq::EqData::print_values>):

=over

=item B<type>

for types 'plain' and 'csv' PreEq::EqData::print_values is directly invoked,
default type is 'html' for printing HTML table, all other are ignored and
'html' assumed

=item S<B<tag> >

puts result in HTML table within tag (with B<tagatt> - see below) if other
than 'table' or 'tr' (case insensitive), for default 'table' just HTML table
is printed, and for special 'tr' no other outer tag, no table tag and no
header row is printed, only table row (B<peridx> described below applies)

=item B<tagatt>

attributes of outer tag (that is table or outer tag defined by B<tag> option),
beware that no parsing nor checking is made on value of that option so make
sure it does not break DOM structure

=item B<border>

border for table and td, must be valid inline style attribute

=item B<fontsize>

font size for table tag

=item B<vertical>

by default, headers are in first row and values in next row
(or rows - see option B<peridx> below),
when this option value evaluates to true, headers are in first col
and values in next, additionally when this value has bit of value 2 set then
colon is appended to headers and when this value has bit of value 4 set then
additional column with known units is appended

=item B<onlyhdr>

prints only header row without any outer tag, options B<tag> and B<tagattr>
are ignored

=item B<peridx>

when multi-value fields are requested, that is 'TAP' or 'FREQ', this option
defines layout of table, if B<peridx> value evaluates to false than flat
list of field values is printed in one row (or col if B<vertical>),
if evaluates to true than as many rows (cols) are printed
as max of tap or freq list requested (B<lstTAP> and B<lstFREQ> applies),
at least one row if both lists are empty,
for non-list values (other than 'TAP','FREQ') each value is printed only
in first row (col) if B<peridx>=>1, always otherwise, default is 1

=item B<style>

defined style for S<< <td> >> elements, eighter 'css' or 'inline' (for other
values default 'inline' is assumed), applies to formatting and alarms,
for 'css' <th> (for header) elements' class is set to 'eqhdr' and
<td> (for values) elements' class is set to 'eqval' and optionally value of
matched alarm (see B<alarms> below), for 'inline' style "text-align:right" is
set for every nonempty cell (<th> and <td>) and optionally value of
matched alarm in inline-style format, note: no css is supplied by this module

=item B<alarms>

like in B<PreEq::EqData::print_values>, except values in list must be class
names if style=>'css' and valid inline-style attributes if style=>'inline',
default is ['eqalarm0','eqalarm1'] for css and
['background-color:red;','background-color:orange;'] for inline

=item B<fmtval>

=item B<fmtTAP>

=item B<fmtFREQ>

=item B<lstTAP>

=item B<lstFREQ>

those options are exactly like in B<PreEq::EqData::print_values>

=item ignored options from B<PreEq::EqData::print_values> (but see type):

sep, csvhdr, fmthdr, S<< fmthdr<fld> >>, fmtitem, S<< fmtitem<fld> >>

=back

also additional fields are recognized: B<tidx>, B<fidx>, B<nidx>
for 'TAP' index, 'FREQ' index and natural index;
they are defined and they mask user supplied fields with such names
when B<peridx> evaluates to true, tidx and fidx are indices for TAP and FREQ
values, nidx is natural index starting always from 1

=cut

sub print_values :prototype(@)
{
	my $self = shift;
	my $opt = {};
	$opt = shift if ref $_[0] eq 'HASH';
	my @cols = (@_);
	my $txt = $opt->{type} ? $opt->{type} : 'html';
	return $self->SUPER::print_values($opt,@cols)
		if grep { $_ eq $txt if defined $txt } ('plain','csv');
	my $tag = &$coalesce($opt->{tag},'table');
	my $tagatt = &$coalesce($opt->{tagatt},'');
	print "<$tag $tagatt>\n"
		unless grep /^$tag$/i,('','table','tr') or $opt->{onlyhdr};
	my @out = ();
	my $vert = $opt->{vertical};
	my $peri = &$coalesce($opt->{peridx},1);
	my $pio = $peri eq 1;
	$opt = {%$opt};
	my ($lt,$lf) = ($opt->{lstTAP},$opt->{lstFREQ});
	$lt = undef unless $peri and $lt and ref $lt eq 'ARRAY';
	$lf = undef unless $peri and $lf and ref $lf eq 'ARRAY';
	map { delete $opt->{$_} if /^fmtitem/ } keys %$opt;
	my $style = $opt->{style};
	$style = 'inline' unless defined $style and $style eq 'css';
	my $tb = $opt->{border} ? "border:$opt->{border}" : '';
	$tb .= ';padding:3px';
	my $tdatt = 'class="eqhdr"' if $style eq 'css';
	$tdatt = "style='text-align:right;$tb'" unless $style eq 'css';
	if (not $tag =~ /^tr$/i or $opt->{onlyhdr}) {
		$tagatt = '' unless grep /^$tag$/i,('','table');
		my $fs = $opt->{fontsize};
		my $tc = '';
		$tc .= 'border-collapse:collapse;' unless $tagatt =~ /collapse/;
		$tc .= "font-size:$fs" if $fs and not $tagatt =~ /font-size/;
		$tc = $tagatt =~ s/\b(style=['"]?)/$1$tc/ ? '' : "style='$tc'";
		my $m = "\N{BULLET OPERATOR}";
		#my $delta = "\N{MATHEMATICAL BOLD CAPITAL DELTA}";
		# above breaks chrome formatting, need to use greek instead
		my $delta = "\N{GREEK CAPITAL LETTER DELTA}";
		#my $l10 = "log\N{SUBSCRIPT ONE}\N{SUBSCRIPT ZERO}";
		# above ain't work in chrome on win with some std fonts
		my $l10 = "log10";
		my $pw2 = "\N{SUPERSCRIPT TWO}";
		my $sqrt = "\N{SQUARE ROOT}";
		my $sw;
		$sw = "\N{LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR}";
		my $qm = "QUADRATIC_MEAN"; # no fancy math here
		($delta,$l10,$pw2,$sqrt,$sw,$m) =
				("delta","log10","**2","sqrt","swapped","*")
			unless $opt->{fancyutf};
		print "<table $tc $tagatt>\n" unless $opt->{onlyhdr};
		$opt->{fmtitem} = "<th $tdatt>".'%2$s';
		$opt->{fmtitem} .= ':' if $vert and $vert & 2;
		$opt->{fmtitemtidx} = "<th $tdatt>T_idx";
		$opt->{fmtitemfidx} = "<th $tdatt>F_idx";
		$opt->{fmtitemnidx} = "<th $tdatt>idx";
		$opt->{fmtitemdelta_u} = "<th $tdatt>$delta u";
		my @fa = ('dB','pn','qm');
		my $fdsc = "F_$fa[$self->get_fralg]";
		$fdsc .= " $sw" if $self->get_swaph;
		my $fstr = &$coalesce($opt->{fstart},$PreEq::EqDefs::fstart,0);
		$fdsc .= " ${fstr}..".(scalar $#{$self->{fft}}) if $fstr;
		my %tit = (T_dBc=>"10$m$l10( (T_re${pw2}+T_im${pw2}) / TTE )",
		           F_dB=>"10$m$l10( |F_re+j${m}F_im| )",
		           F_pn=>"10$m$l10( |F_re+j${m}F_im|${pw2} / N${pw2} )",
		           F_qm=>"20$m$l10( |F_re+j${m}F_im| / $qm )",
		           F_abs=>"$sqrt( F_re${pw2} + F_im${pw2} )",
			   F_f=>$fdsc,
		           MTC=>"Main Tap Compression - ".
		                "not enough power for main signal",
		           NMTER=>"abs(NMTER) approximates MER",
		           PreMTTER=>"relates to group delay and cascade depth",
		           PostMTTER=>"relates to microreflections",
		           PPESR=>"dominant distortion: ".
		                  "microreflections < 0 < group delay");
		map {$opt->{'fmtitem'.$_}="<th $tdatt title='$tit{$_}'>$_";}
			keys %tit;
		map {$opt->{'fmtitem'.$_} .= ':' if $opt->{'fmtitem'.$_}}
			('tidx','fidx','nidx','delta_u',keys %tit)
			if $vert and $vert & 2;
		$opt->{lstTAP} = [undef] if $peri;
		$opt->{lstFREQ} = [undef] if $peri;
		push @out,[$self->SUPER::format_values_list($opt,@cols)];
		map { delete $opt->{'fmtitem'.$_} }
			('tidx','fidx','nidx','delta_u',keys %tit);
	}
	if ($peri) {
		$lt = [1..($self->{taps}?$self->{taps}:0)] unless $lt;
		$lf = [0..scalar @{$self->{fft}}-1] unless $lf;
		$peri = scalar @$lt if grep /^TAP$/,@cols;
		my $pf = scalar @$lf if grep /^FREQ$/,@cols;
		$peri = $pf if $pf and $pf > $peri;
	}
	my $uflds = {};
	map { $uflds->{$_} = $self->get_value($_) if $self->has_value($_) }
		('tidx','fidx','nidx');
	$tdatt = 'class="eqval %3$s"' if $style eq 'css';
	$tdatt = 'style="text-align:right;%3$s;'.$tb.'"' unless $style eq 'css';
	$opt->{fmtitem} = '<td name="%2$s" '.$tdatt.'>%1$s</td>';
	$opt->{alarms} = ['background-color:red;','background-color:orange;']
		unless defined $opt->{alarms} or $style eq 'css';
	$opt->{alarms} = ['eqalarm0','eqalarm1']
		unless defined $opt->{alarms} or $style ne 'css';
	$opt->{sep} = '';
	my @ml = ('TAP','FREQ','tidx','fidx','nidx');
	map {
		$self->set_value('nidx',$_+1) if $lt or $lf;
		$opt->{lstTAP} = [$self->set_value('tidx',shift @$lt)] if $lt;
		$opt->{lstFREQ} = [$self->set_value('fidx',shift @$lf)] if $lf;
		push @out,[$self->SUPER::format_values_list($opt,@cols)];
		map {
			my $c = $_;
			$opt->{'fmtitem'.$c} = '<td />' unless grep /^$c$/,@ml
		} @cols if $pio and $_ == 0;
	} (0..($peri?$peri-1:0)) unless $opt->{onlyhdr};
	if ($vert and $vert & 4) {
		$tdatt = 'class="equnit"' if $style eq 'css';
		$tdatt = "style='text-align:left;$tb'" unless $style eq 'css';
		my %ku = ('dBc'=>['NMTER','PreMTTER','PostMTTER','TAP'],
		          'dB'=>['MTC','FREQ','delta_u']);
		map { delete $opt->{$_} if /^fmtitem/ } keys %$opt;
		$opt->{fmtitem} = '<td/>';
		map {
			my $u = $_;
			map { $opt->{'fmtitem'.$_} = "<td $tdatt>$u</td>" }
				@{$ku{$u}};
		} keys %ku;
		push @out,[$self->SUPER::format_values_list($opt,@cols)];
	}
	map { $self->set_value($_,$uflds->{$_},not exists $uflds->{$_}) }
		('tidx','fidx','nidx');
	map { print "<tr>"; map { print $_ } @$_ } @out unless $vert;
	while ($vert and ref $out[0] eq 'ARRAY' and scalar @{$out[0]}) {
		print "<tr>";
		map { print shift @{$out[$_]} } (0..$#out);
	}
	print "</table>" unless $tag =~ /^tr$/i or $opt->{onlyhdr};
	print "</$tag>\n"
		unless grep /^$tag$/i,('','table','tr') or $opt->{onlyhdr};
	return undef if $opt->{silentret};
	return scalar @out;
}

=head2 B<print_coefbars(%opt)>;

prints bar graph (only div tags for compatibility with all browsers)
of tap 'dBc' values, with main tap blue and other taps between orange and red
depending on distance between 'dBc' value and alarm value, with lower bound
for graph based on $PreEq::EqDefs::min_dbc or default -68

supported options:

=over

=item B<barwidth>

=item B<barheight>

specify size of graph (size of div tag enclosing bar graph), accept any
unit suported by HTML browsers, graph will fit desired size
even if size is changed dynamically within browser
(by changing DOM object's style attributes),
see width,height below for defaults

=item B<width>

=item B<height>

fallback size values if above (barwidth,barheight) are not specified,
default is width=>'400px' and height=>'200px'

=item B<barBackgroundColor>

=item B<backgroudColor>

background color, barBackgroundColor takes precedence,
applies to all graph including enclosing div tag,
other colors are not modifiable (blue for main tap, orange to red for
other taps, red for alarm levels, grey for grid lines, defult text color),
default is 'lightgrey'

=item B<barhdr>

extra header fields to be shown in graph header (along with taps,main,tps),
must be reference to list of fields,
empty values are shown for fields not defined within object

=back

=cut

sub print_coefbars :prototype($;%)
{
	my $self = shift;
	my %opt = (@_);
	return undef unless $self->{taps};
	my $w = &$coalesce($opt{barwidth},$opt{width},'400px');
	my $h = &$coalesce($opt{barheight},$opt{height},'200px');
	my %cs = (width=>$w,height=>$h,position=>'relative');
	$cs{name} = $opt{name} if $opt{name};
	$cs{id} = $opt{id} if $opt{id};
	$cs{'background-color'} = &$coalesce($opt{barBackgroundColor},
	                                     $opt{backgroundColor},'lightgrey');
	my @hf = ('taps','main','tps');
	push @hf, @{$opt{barhdr}} if ref $opt{barhdr} eq 'ARRAY';
	my @hv = map {
		my $v=$self->get_value($_); sprintf "$_: %s",defined $v?$v:''
	} @hf;
	my %hs = (height=>'15%',position=>'relative','text-align'=>'center',
	          top=>'0px','font-size'=>'14px');
	my %bs = (position=>'absolute',height=>'80%',width=>'90%');
	my %gs = (position=>'absolute',height=>'1px','font-size'=>'9px',
	          'text-align'=>'right','border-top'=>'1px solid grey',
	          left=>'2%',width=>'107%');
	my $mindbc = &$coalesce($PreEq::EqDefs::min_dbc,-68);
	my @grid = map {
		my %g = (top=>($_*10/$mindbc)*100 ."%",%gs); [\%g,$_*10,"dBc"]
	} (int($mindbc/10)..0);
	my $f = sub { $_[0]->print_bardiv($_[1]); return undef };
	my @bars = map { [$f,$self,$_] } (1..$self->{taps});
	&$printdiv(\%cs,[\%hs,join(', ',@hv)],\%bs,@grid,@bars);
	return undef if $opt{silentret};
	return $self->{taps};
}

=head2 B<print_bardiv($idx)>;

used internally by print_coefbars, prints $idx'th tap bar,
which is div tag fitting height of containing tag on horizontal position
corresponding to $idx,
within which actual bar is shown with height corresponding to value 'dBc',
alarm level and $idx number, color for alarm level is red,
color of bar is between orange and red depending on distance from alarm level

=cut

sub print_bardiv :prototype($$)
{
	my $self = shift;
	my $idx = shift;
	return undef unless $self->{taps} and $idx;
	return undef unless $idx > 0 and $idx <= $self->{taps};
	my $mindbc = &$coalesce($PreEq::EqDefs::min_dbc,-68);
	my $val = $self->{coef}[$idx]{dBc};
	$val = $mindbc if $val < $mindbc;
	my $ri = $idx - $self->{main};
	my $pb = 100/($self->{taps}+1);
	my $ba = $mindbc;
	my $br = [];
	my $tit = "tap:$idx ";
	$tit .= "re:$self->{coef}[$idx]{re} ";
	$tit .= "im:$self->{coef}[$idx]{im} ";
	$tit .= sprintf "dBc:%.02f ",$self->{coef}[$idx]{dBc};
	$br = \@PreEq::EqDefs::preborder if $ri < 0;
	$br = \@PreEq::EqDefs::postborder if $ri > 0;
	my $bi = (sort {$a<=>$b} (abs($ri)-1,$#{$br}))[0];
	$ba = (defined $bi and defined $br->[$bi] and $bi >= 0) ?
		$val-$br->[$bi] : $val;
	my $bc = int((sort {$a<=>$b} (255,($ba/$mindbc)*400))[0]);
	my %al = (position=>'absolute',width=>'100%',top=>'0%',
	          'border-bottom'=>'3px solid red',
		  bottom=>(1-$br->[$bi]/$mindbc)*100-1 ."%",
	         ) if defined $bi and defined $br->[$bi] and $bi>=0;
	$tit .= sprintf "-alarm:%.02f",$val-$br->[$bi] if %al;
	my %os = (position=>'absolute',height=>'100%','font-size'=>'8px',
	          width=>"$pb%",left=>($idx-.5)*$pb."%",title=>$tit);
	my %is = (position=>'absolute',bottom=>'0px',left=>'15%',width=>'70%',
	          height=>((1-$val/$mindbc)*100 ."%"),
	          'background-color'=>($ri?"rgb(255,$bc,0)":'blue'));
	my %ts = (position=>'absolute',bottom=>'-10px',width=>'100%',
	          'text-align'=>'center');
	&$printdiv(\%os,[\%al],[\%is],\%ts,$idx);
	return 1;
}

=head2 B<print_freqvals(%opt)>;

this function is inherited from B<PreEq::SvgHTML> (see module's documentation),
only mentioned here for completeness

=cut

=head2 B<get_freqcolor>;

used internally to get externally-set color for freq graph or assign
here-defined color to multiple freq graph version in PreEq::SvgHTML

=cut

sub get_freqcolor :prototype($)
{
	my $self = shift;
	return $self->{_freqcolor} if exists $self->{_freqcolor};
	return undef;
}

=head2 B<set_freqcolor($color)>;

used internally by PreEq::SvgHTML to set freq graph color matching the one
in multiple freq graph version

=cut

sub set_freqcolor :prototype($$)
{
	my $self = shift;
	$self->{_freqcolor} = shift;
}

1;
__END__

=head1 NOTES

This module should not be of much use for ordinary people, it's purpose is
to help in visualizing and analyzing signal distortions in return path of
DOCSIS plant, and that means it may only be of any use for people that know
what it is about.

=head1 SEE ALSO

PreEq::EqData, PreEq::EqDefs, PreEq::FFTWrapper, PreEq::SvgHTML

=head1 AUTHOR

Mariusz Jadczak, E<lt>mariuszj@toya.com.plE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2016 by Mariusz Jadczak 

This library is free software; you can redistribute it and/or modify
it under the terms of BSD licence

=cut

