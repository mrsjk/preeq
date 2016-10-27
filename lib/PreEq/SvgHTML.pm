package PreEq::SvgHTML;

use 5.020002;
use strict;
use warnings;

our @ISA = qw();

our $VERSION = '0.01';

use Scalar::Util 'blessed';

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
	map { my $v = delete $st{$_}; print " $_=\"$v\"" if $v }
		('name','id','class','onclick');
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

my $jsexppngfun = 0;
our $jsexportpng = sub {
my ($sid,$fname) = @_;
print "
<script>
function exppng(svgid,name) {
	var se = document.getElementById(svgid);
	if (!se) { return; }
	if (se.tagName != 'SVG') {
		var cse = se.getElementsByTagName('svg');
		se = cse.length ? cse[0] : undefined;
	}
	if (!se) { return; }
	var w = se.width.baseVal.value;
	var h = se.height.baseVal.value;
	var sd = '<svg xmlns=\"http://www.w3.org/2000/svg\"';
	sd += ' width=\"'+w+'\"';
	sd += ' height=\"'+h+'\"';
	sd += '>'+se.innerHTML+'</svg>';
	var ce = document.createElement('canvas');
	ce.width = w;
	ce.height = h;
	var im = new Image();
	var dwnld = function () {
		var cx = ce.getContext('2d');
		cx.drawImage(im,0,0);
		var imdata = ce.toDataURL('image/png');
		var dl = document.createElement('a');
		dl.download = name;
		dl.href = imdata;
		document.body.appendChild(dl);
		dl.click();
		document.body.removeChild(dl);
	}
	im.onload = dwnld; // wait till image ready before drawing to canvas
	im.src = 'data:image/svg+xml,'+encodeURIComponent(sd);
}
</script>
" unless $jsexppngfun++;
return "exppng('$sid','$fname')" if $sid and $fname;
};

=pod

=head1 NAME

PreEq::SvgHTML - Perl extension for HTML svg for use in PreEq::EqHTML

=head1 SYNOPSIS

  use PreEq::SvgHTML;
  # create new object for multiple graphs
  $mfg = PreEq::SvgHTML->multifreq($eq1,$eq2,'blue',$eq3,'green',\@freqs);
  # merge more graphs with black color
  $mfg->cmerge('black',\@freqs2,$eq4);
  # print svg area within div tag (with id='bla' attribute), pathtype '+C'
  $mfg->print_freqvals(id=>'bla',pathtype=>'+C',width=>400);
  # ... later ...
  # $eq3 already has color 'blue' matching that set for multifreq
  $eq3->print_freqvals;

=head1 DESCRIPTION

This is helper module for L<PreEq::EqHTML>
(see L<PreEq::EqHTML> module documentation).
This module provides methods to print (to defalt filehandle) HTML svg
visualization of freq graph. Allows to generate svg tag with multiple
freq graph with common scale, or single graph when inherited by class
providing method C<get_freqvals>.
See below list of supported methods.

=cut

=head1 OBJECT INITIALIZATION

B<PreEq::SvgHTML> object allows generating svg tag with multiple freq graphs.
Accepts references to arrays of freq values or objects providing method
C<get_freqvals> and optionally methods C<get_freqcolor> and C<set_freqcolor>
(designed specifically for B<PreEq::EqHTML> but any blessed reference with
those methods will do). For each reference graph color may be specified
or will be chosen from predefined colors (inifinitely iterating predefined
list of (currently) four colors: darkorange,blueviolet,magenta,seagreen) unless
referenced object provides method C<get_freqcolor> which returns defined
value in which case that value is used. If referenced object provides method
C<set_freqcolor> this method is called with graph's color.
References passed are forgotten, this object keeps only copy of freq values
(in time of adding to this object) and passed or calculated colors for graphs.
Each graph, no matter how many elements, will be over full horizontal
scale, but may be prepended or appended with undef values to leave empty space
before or after graph.

=head2 B<multifreq(@list)>;

creates new object, accepts mixed list of colors (scalars) and references
to arrays or objects, 
each color (any scalar value) applies to following graphs until new color
or undef passed. undef resets passed color, in which case object's method
C<get_freqcolor> is used or default color scheme is resumed
(iterating predefined list or colors)

=cut

sub multifreq :prototype($;@)
{
	my $class = shift;
	my $self = {
		glist=>[],
		fcolors=>[],
		_cols=>[],
	};
	bless $self,$class;
	my $col;
	while (@_) {
		my $e = shift;
		$self->cmerge($col,$e) if ref $e;
		$col = $e unless ref $e;
	}
	return $self;
}

=head2 B<merge(@list)>;

appends graphs to object, accepts references to arrays of freq values and
references to objects providing C<get_freqvals> method, other args silently
ignored, colors based on return value of C<get_freqcolor> method or default
scheme

=cut

sub merge
{
	my $self = shift;
	return $self->cmerge(undef,@_);
}

=head2 B<cmerge($color,@list)>;

appends graphs (references as in constructor and in B<merge> method)
to object with color passed as first arg, color in first arg (if defined)
applies to all passed graphs

=cut

sub cmerge :prototype($$@)
{
	my $self = shift;
	my $col = shift;
	my @defcolors = ('darkorange','blueviolet','magenta','seagreen');
	my $ret = 0;
	while (@_) {
		my $e = shift;
		@{$self->{_cols}} = (@defcolors) unless @{$self->{_cols}};
		if (Scalar::Util::blessed($e)) {
			my @fv = $e->get_freqvals if $e->can('get_freqvals');
			next unless @fv;
			my $c = $e->get_freqcolor if $e->can('get_freqcolor');
			$c = $col if defined $col;
			$c = shift @{$self->{_cols}} unless defined $c;
			$e->set_freqcolor($c) if $e->can('set_freqcolor');
			push @{$self->{glist}},\@fv;
			push @{$self->{fcolors}},$c;
			$ret++;
		}
		if (ref $e eq 'ARRAY') {
			my $c = $col;
			$c = shift @{$self->{_cols}} unless defined $c;
			push @{$self->glist},[@$e];
			push @{$self->fcolors},$c;
			$ret++;
		}
	}
	return $ret;
}

=head1 INTERNAL METHODS

Those methods are used internally, should be no need to call them explicitely

=head2 B<get_freqcolor($idx)>;

get color assigned to graph (0-based in order of adding/merging)

=cut

sub get_freqcolor :prototype($$)
{
	my $self = shift;
	return $self->{fcolors}[$_];
}

=head2 B<calc_freqstat(@freqs)>;

calculates stats (B<len>,B<min>,B<max>,B<ref>,B<amp>) for freq values in @freqs
and returns hash with those values, where B<len> is list length (number of
elements in @freqs), B<min> and B<max> are literally min and max of values
in @freqs, B<ref> is eighter 0 if 0 is between B<min> and B<max> or first
defined value in @freqs otherwise, and B<amp> is absolute maximum distance
between B<ref> and freq values

=head2 B<calc_freqstat>;

calculates same stats as above, except it scans all freq lists added/merged
to this object, B<len> is max length of all freq graphs, B<min>,B<max>,B<ref>
are rounded to integer values, B<ref> is eighter 0 if B<ref> would be 0 for all
graphs or halfway between calculated (and rounded) B<min> and B<max> otherwise,
B<min> and B<max> with same absolute distance from B<ref>, enclosing all values
unless maximum distance between (B<min>,B<max>)-bounds and values does not
exceed 1/4 dB, and B<amp> is absolute max distance between B<ref> and
(B<min>,B<max>)-bounds or all values whichever is greater

=cut

sub calc_freqstat :prototype($;@)
{
	my $self = shift;
	my @fl;
	push @fl,[@_] if @_;
	@fl = (@{$self->{glist}}) if not @fl and exists $self->{glist};
	return undef unless @fl;
	my ($dr,$ref,$min,$max,$len,$amp);
	map {
		my $refc;
		my $l = scalar @$_ if ref $_ eq 'ARRAY';
		map {$refc = $_ if defined $_ and not defined $refc} @$_ if $l;
		$len = $l if not defined $len or defined $l and $l > $len;
		my @cf = map { defined $_ ? $_ : $refc } @$_;
		($min,$max) = ($refc,$refc) unless defined $min or defined $max;
		($min,$max) = (sort {$a<=>$b} ($min,$max,@cf))[0,-1]
			if defined $refc;
		$refc = 0 if defined $refc and $min*$max <= 0;
		$ref = $refc unless defined $ref;
		$dr = 1 if defined $refc and $ref != $refc;
	} @fl;
	return () unless defined $min and defined $max;
	$ref = ($min+$max)/2 if $dr and defined $ref;
	$amp = (sort {$a<=>$b} (abs($min-$ref),abs($max-$ref)))[-1]
		if defined $ref;
	my %s = (len=>$len,amp=>$amp,ref=>$ref,min=>$min,max=>$max);
	return %s if @_ or not defined $ref;
	my ($mx,$mn) = (int($max),int($min));
	$mx += 1 if $max > 0 and $mx != $max;
	$mn -= 1 if $min < 0 and $mn != $min;
	$ref = ($mn+$mx)/2 if $ref;
	if (not (($mx-$mn)%2)) {
		$mx -= 1 if $mx-$max > .75 and $mx > 1;
		$mn += 1 if $min-$mn > .75 and $mn < -1;
		$s{amp} = (sort {$a<=>$b} (abs($ref-$mx),abs($ref-$mn)))[-1];
		($s{min},$s{max}) = ($ref-$s{amp},$ref+$s{amp});
		$amp = (sort {$a<=>$b} (abs($ref-$min),abs($ref-$max)))[-1];
		$s{amp} = $amp if $amp > $s{amp};
		$s{ref} = $ref;
		return %s;
	}
	if (abs($mx-$max)>abs($mn-$min)) {
		my $mv = ($mx-$max>.75 and abs($ref-$mx)>1) ? \$mx : \$mn;
		$$mv -= 1;
	} else {
		my $mv = ($min-$mn>.75 and abs($ref-$mn)>1) ? \$mn : \$mx;
		$$mv += 1;
	}
	$ref = ($mn+$mx)/2;
	$s{amp} = (sort {$a<=>$b} map {abs($ref-$_)} ($mn,$min,$max,$mx))[-1];
	($s{ref},$s{min},$s{max}) = ($ref,$mn,$mx);
	return %s;
}

=head2 B<svgcurve($type,$tx,$ty,@freqs)>;

calculates signle graph svg path d attribute value for @freqs,
with type eighter 'L' (straight lines between points) or 'C' (bezier curves
with two control points) or 'x' or '+' or 'p' (x or + (+p) marks for points),
$tx being reference to function translating @freqs
indieces to absolute horizontal points within svg area, $ty being reference
to function translating values to absolute vertical points within svg area

=cut

sub svgcurve :prototype($$$$@)
{
	my $self = shift;
	my $type = shift;
	my $tx = shift;
	my $ty = shift;
	my @c;
	return @c unless ref $tx eq 'CODE' and ref $ty eq 'CODE';
	return @c unless grep /^[$type]$/,('C','L','x','+','p');
	foreach my $i (0..$#_) {
		my $b = $_[$i-2] if $i >= 2;
		my $s = $_[$i-1] if $i >= 1;
		my $e = $_[$i];
		my $a = $_[$i+1] if $i < $#_;
		if ($type eq '+' or $type eq 'p' or $type eq 'x') {
			next unless defined $e;
			my $y1 = $type eq 'x' ? 2 : 0;
			push @c,['M',int(&$tx($i))-2,int(&$ty($e))-$y1,
			         'L',int(&$tx($i))+2,int(&$ty($e))+$y1,
				 'M',int(&$tx($i))-$y1,int(&$ty($e))+2,
				 'L',int(&$tx($i))+$y1,int(&$ty($e))-2];
			next;
		}
		push @c,['M',int(&$tx($i)),int(&$ty($e))]
			if defined $e and not scalar @c;
		next unless defined $s and defined $e;
		push(@c,['L',int(&$tx($i)),int(&$ty($e))]) if $type eq 'L';
		next unless $type eq 'C';
		my $d = $e-$s;
		my $d1 = $s-$b if defined $b;
		my $d2 = $a-$e if defined $a;
		my $c1 = $s+$d/4;
		$c1 = $s+$d/16 if defined $d1 and $d*$d1 <= 0;
		$c1 = $s+$d/4+$d/4*(1-$d/$d1) if $d and $d1 and $d1/$d>1;
		$c1 = $s+$d/4-$d/4*(1-$d1/$d) if $d and $d1 and $d/$d1>1;
		my $c2 = $e-$d/4;
		$c2 = $e-$d/16 if defined $d2 and $d*$d2 <= 0;
		$c2 = $e-$d/4-$d/4*(1-$d/$d2) if $d and $d2 and $d2/$d>1;
		$c2 = $e-$d/4+$d/4*(1-$d2/$d) if $d and $d2 and $d/$d2>1;
		push @c,['C', # bezier curve with two control points
		         int(&$tx($i-.75)),int(&$ty($c1)),
		         int(&$tx($i-.25)),int(&$ty($c2)),
		         int(&$tx($i)),int(&$ty($e))];
	}
	return @c if wantarray;
	return join ' ', map { join ' ',@$_ } @c;
}

=head1 OUTPUT METHODS

Methods for generating output

=head2 B<print_freqvals(%opt)>;

prints to default filehandle div tag containing svg area with graph(s),
grey horizontal lines for B<ref>, B<min>, B<max> values of B<calc_freqstat>
return hash and vertical line in the middle of horizontal scale, behaves
little different when called via object of inheriting class providing
method B<get_freqvals> (in which case B<ref>, B<min>, B<max> are like
described in B<calc_freqstat(@freqs)> method for single graph,
with B<min> and B<max> strictly bounding all values), and when
called via object not providing B<get_freqvals> (in which case B<ref>, B<min>,
B<max> are like described in B<calc_freqstat()> method for multiple graphs
passed to this object, with symetric round B<min> and B<max> beyond or near
peaks of all values of all graphs), with vertical size of svg area depending
on calculated apmlitude, and other attributes passed in options (as hash):

=over

=item B<freqwidth>

=item B<width>

width of svg area in px, preferred option is freqwidth, when not defined
than width is used or default 600 if none defined

=item B<pxperdb>

vertical px per db, that is vertical height of 1dB (overall vertical height
depends on graph(s) apmlitude), default 100

=item B<fstart>

start drawing freq graph from index fstart, this literally means how many
elements to skip from beginning while drawing graph
(counting from first defined value),
default is value of B<$PreEq::EqDefs::fstart>,
with fallback to 0 if none defined

=item B<freqBackgroundColor>

=item B<backgroundColor>

background color for svg area, freqBackgroundColor takes precedence,
default is '#cbcaaf', does not apply to exported png

=item B<pathtype>

type of graph line, supported are 'L' for straight lines between points, 'C'
for bezier curves with two control points (smooth graph), 'x' and '+' for
discrete point marks of shape x or + (within single path tag),
multiple types may be specified (like 'LC') for each graph,
last occurence of each type determines order of paths
(last is most visible, with 'stroke-width' eq 2 for last and 1 for other in
in single graph version, or 1 for all in multiple graph version),
default is 'C'

=item B<stroke>

this option defines color or colors for graph types, may be scalar for last
color, list of colors in order of final B<pathtype> (with dups removed), or
hash with keys of pathtype and values for color, missing pathtypes are set
to 'black' with default 'red' for last pathtype (for list and scalar colors
are prepended with black values to match expected list size),
applies to each graph separately, last value will be changed accordingly
to object initialization rules in case of multiple graphs and to return of
'get_freqcolor(0)' method for single graph version if method exists and
returns defined value,
so this option does not make much sense for multiple graph version with one
pathtype for each graph, nor for single graph version with already assigned
color (note: merging object's reference sets its color via 'set_freqcolor'
method, which will be used on call from that object in case of inheritance)

=item B<id>

=item B<name>

attributes for enclosing div tag, unique id required for png export

=item B<exppng>

this option allows exporting graphs to png files within browser and sets
file name for exported file, exporting is done entirely within browser via
embded js script and is possible only within browsers supporting svg tags
and canvas elements, intermediate canvas elements are not added to DOM
structure of html file and are created on the fly,

there is no default for
this option though PreEq::EqHTML::print_combo method invoking single graph
printing sets file to C<< preeq_<ident>[_<freq>][_<usid>]_<unixtime>.png >>,
where S<ident> is eighter hfc or dest or id if those attributes are set,
optional S<freq> and S<usid> are included if set, and S<unixtime> is time
of creating graph as returned by S<time> function

=back

=cut

sub print_freqvals :prototype($%)
{
	my $self = shift;
	my %opt = @_;
	my $w = &$coalesce($opt{freqwidth},$opt{width},'600');
	my $pxdb = &$coalesce($opt{pxperdb},100);
	my $bc = &$coalesce($opt{freqBackgroundColor},$opt{backgroudColor},
	                    '#cbcaaf');
	my $pt = &$coalesce($opt{pathtype},'');
	undef while not ref $pt and $pt =~ s/(.)(.*\g1)/$2/g;
	$pt =~ s/[^+pxLC]//g;
	$pt = 'C' unless $pt;
	my @pt = split '',$pt unless ref $pt;
	@pt = (@$pt) if ref $pt eq 'ARRAY';
	my $fc = &$coalesce($opt{stroke},'red');
	my @fc = ($fc) unless ref $fc;
	@fc = (@$fc) if ref $fc eq 'ARRAY';
	@fc = map { $fc->{$_} ? $fc->{$_} : 'black' } @pt if ref $fc eq 'HASH';
	@fc = (reverse(reverse(@fc),map{'black'}(1..($#pt-$#fc))))[-1-$#pt..-1];
	my @fr = $self->get_freqvals if $self->can('get_freqvals');
	my @mf = (@{$self->{glist}}) if exists $self->{glist} and not @fr;
	return undef unless scalar @fr or scalar @mf;
	my $val = sub { defined $_[0] ? sprintf "%.2f",$_[0] : '' };
	my %v = $self->calc_freqstat(@fr);
	return undef unless %v;
	my $fs = &$coalesce($opt{fstart},$PreEq::EqDefs::fstart,0);
	$v{len} -= $fs if $v{len};
	@mf = (\@fr) if @fr;
	my $mar = 20;
	my $h = 2*$v{amp}*$pxdb + 2*$mar;
	my $ydb = sub { $mar+$pxdb*($v{amp}+$v{ref}-$_[0]) if defined $_[0] };
	my $xfi = sub { ($w-30)/(1+$v{len})*(1+$_[0]) if defined $_[0] };
	my %d = (name=>&$coalesce($opt{name},'freq'));
	$d{id} = $opt{id} if $opt{id};
	my $expfun = &$jsexportpng($opt{id},$opt{exppng}) if $opt{exppng};
	$d{onclick} = $expfun if $expfun;
	my %s = (tag=>'svg',width=>$w,height=>$h,style=>"background-color:$bc");
	my %g = (tag=>'g',name=>'grid',stroke=>'grey','stroke-width'=>.5);
	my %t = (tag=>'g',name=>'gridt','font-size'=>9);
	my (@el,@tl);
	my $sl = sub {
		my ($xs,$xe)=(int(&$xfi($_[0])),int(&$xfi($_[2])));
		my ($ys,$ye)=(int(&$ydb($_[1])),int(&$ydb($_[3])));
		return '' unless $xs and $ys and ($xe or $ye);
		"M $xs $ys ".(($xe and $ye)?"L $xe $ye":$xe?"H $xe":"V $ye");
	};
	my %p = (tag=>'path',fill=>'none');
	push @el,{%p,name=>'md',d=>&$sl(0,$v{ref},$v{len}-1,undef)};
	push @el,{%p,name=>'hi',d=>&$sl(0,$v{max},$v{len}-1,undef)};
	push @el,{%p,name=>'lo',d=>&$sl(0,$v{min},$v{len}-1,undef)};
	push @el,{%p,name=>'hm',
	          d=>&$sl(($v{len}-1)/2,$v{ref}-$v{amp},undef,$v{ref}+$v{amp})};
	my %ta = (tag=>'text',dx=>4,x=>int(&$xfi($v{len}-1)));
	my $tp = sub { (y=>int(&$ydb($_[0])),val=>&$val($_[0])) };
	push @tl,{%ta,&$tp($v{ref}),name=>'tmd',dy=>2};
	push @tl,{%ta,&$tp($v{max}),name=>'thi',dy=>-3};
	push @tl,{%ta,&$tp($v{min}),name=>'tlo',dy=>7};
	my @fg = map { 
		my @f = (@{$mf[$_]}) if ref $mf[$_] eq 'ARRAY';
		my $c = $self->get_freqcolor($_) if $self->can('get_freqcolor');
		my @u;
		push @u,shift @f until not scalar @f or defined $f[0];
		push @u,@f[$fs..$#f];
		map {
			my $d = $self->svgcurve($pt[$_],$xfi,$ydb,@u);
			my $lst = $_ eq $#pt if @fr;
			({%p,name=>$pt[$_],'stroke-width'=>$lst?2:1,
			  stroke=>($c and $_ eq $#pt)?$c:$fc[$_],d=>$d}) if $d
		} (0..$#pt);
	} (0..$#mf);
	&$printdiv(\%d,[$print_tag,\%s,[\%g,@el],[\%t,@tl],@fg]);
}

1;
__END__

=head1 SEE ALSO

PreEq::EqHTML, PreEq::EqData, PreEq::EqDefs, PreEq::FFTWrapper

=head1 AUTHOR

Mariusz Jadczak, E<lt>mariuszj@toya.com.plE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2016 by Mariusz Jadczak

This library is free software; you can redistribute it and/or modify
it under the terms of BSD licence

=cut

