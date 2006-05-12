#!/usr/bin/perl -w

=head1 NAME

bench_optree.pl - Look at different ways of storing data that transform fast.

=cut

use strict;
use Benchmark qw(cmpthese timethese);
use CGI::Ex::Dump qw(debug);

#my $obj = bless [1, 2], __PACKAGE__;
#my $struct1 = \ [ '-', 1, 2 ];
#my $struct2 = ['-', 1, 2];
#
#sub call { $_[0]->[0] - $_[0]->[1] }
#
#sub obj_meth {  $obj->call }
#sub ref_type { if (ref($struct1) eq 'REF') { if (${$struct1}->[0] eq '-') { ${$struct1}->[1] - ${$struct1}->[2] } } }
#
#print "(".obj_meth().")\n";
#print "(".ref_type().")\n";
#cmpthese timethese(-2, {
#    obj_meth => \&obj_meth,
#    ref_type => \&ref_type,
#}, 'auto');


#pauls@pslaptop:~/perl/CGI-Ex/lib$    perl -e 'my $a = "1 + 2 * (3 + (4 / 5) * 9) - 20";
#       use CGI::Ex::Template;
#       use Data::Dumper;
#       print Dumper(CGI::Ex::Template->new->parse_variable(\$a));'

my $a = "1 + 2 * (3 + (4 / 5) * 9) - 20";

###----------------------------------------------------------------###

my $A1 = [ \[ '-', [ \[ '+', '1', [ \[ '*', '2', [ \[ '+', '3', [ \[ '*', [ \[ '/', '4', '5' ], 0 ], '9' ], 0 ] ], 0 ] ], 0 ] ], 0 ], '20' ], 0 ];

my $self = bless {}, __PACKAGE__;

sub get_var {
    return $_[1] if ! ref $_[1];
    my $self = shift;
    my $var  = shift;
    my $ref  = $var->[0];
    if (ref $ref) {
        if (ref($ref) eq 'REF') {
            return $self->play_operator($$ref);
        } elsif (ref($ref) eq 'ARRAY') {
            die "No nested";
        } else {
            die "No literals";
        }
    } else {
        die "Not doing that yet";
    }
}

sub play_operator {
    my $self = shift;
    my $tree = shift;
    my $op   = $tree->[0];
    if ($op eq '-') { return $self->get_var($tree->[1]) - $self->get_var($tree->[2]) }
    if ($op eq '+') { return $self->get_var($tree->[1]) + $self->get_var($tree->[2]) }
    if ($op eq '*') { return $self->get_var($tree->[1]) * $self->get_var($tree->[2]) }
    if ($op eq '/') { return $self->get_var($tree->[1]) / $self->get_var($tree->[2]) }
}

###----------------------------------------------------------------###

sub get_var2 { ref($_[1]) ? $_[1]->call($_[0]) : $_[1] }

{
    package Num;
    sub new { my $c = shift; bless \@_, $c };
    sub call { $_[0]->[0] }
    package Op;
    sub new { my $c = shift; bless \@_, $c }
#    sub new { my $c = shift; bless [map{ref$_?$_:Num->new($_)} @_], $c }
    package Op::Minus;
    our @ISA = qw(Op);
    sub call { $_[1]->get_var2($_[0]->[0]) - $_[1]->get_var2($_[0]->[1]) }
    package Op::Plus;
    our @ISA = qw(Op);
    sub call { $_[1]->get_var2($_[0]->[0]) + $_[1]->get_var2($_[0]->[1]) }
    package Op::Mult;
    our @ISA = qw(Op);
    sub call { $_[1]->get_var2($_[0]->[0]) * $_[1]->get_var2($_[0]->[1]) }
    package Op::Div;
    our @ISA = qw(Op);
    sub call { $_[1]->get_var2($_[0]->[0]) / $_[1]->get_var2($_[0]->[1]) }
};
sub plus  ($$) { Op::Plus->new( @_) }
sub minus ($$) { Op::Minus->new(@_) }
sub mult  ($$) { Op::Mult->new( @_) }
sub div   ($$) { Op::Div->new(  @_) }

my $A2 = minus(plus(1, mult(2, plus(3, mult(div(4,5), 9)))), 20);
debug $A2;

print eval($a)."\n";
print $self->get_var($A1)."\n";
print $self->get_var2($A2)."\n";

cmpthese timethese (-2, {
    perl        => sub { eval $a },
    bare_data   => sub { $self->get_var($A1) },
    method_call => sub { $self->get_var2($A2) },
}, 'auto');

use Storable;
my $d1 = Storable::freeze($A1);
my $d2 = Storable::freeze($A2);
Storable::thaw($d1); # load lib
print length($d1)."\n";
print length($d2)."\n";

cmpthese timethese (-2, {
    freeze_bare => sub { Storable::freeze($A1) },
    freeze_meth => sub { Storable::freeze($A2) },
}, 'auto');

cmpthese timethese (-2, {
    thaw_bare => sub { Storable::thaw($d1) },
    thaw_meth => sub { Storable::thaw($d2) },
}, 'auto');


print $d2;
