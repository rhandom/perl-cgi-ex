package CGI::Ex::Template::Context;

=head1 NAME

CGI::Ex::Template::Context - Provide a TT style context

=head1 DESCRIPTION

=head1 AUTHOR

Paul Seamons <paul at seamons dot com>

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut

use strict;
use warnings;
use base qw(Exporter);

our $VERSION   = '2.13';

use vars qw($AUTOLOAD);

sub new {
    my $class = shift;
    my $self  = shift || {};
    die "Missing _template" if ! $self->{'_template'};
    return bless $self, $class;
}

sub _template { shift->{'_template'} || die "Missing _template" }

sub template {
    my ($self, $name) = @_;
    return $self->_template->{'BLOCKS'}->{$name} || $self->_template->load_template($name);
}

sub config { shift->_template }

sub stash {
    my $self = shift;
    return $self->{'stash'} ||= bless {_template => $self->_template}, 'CGI::Ex::Template::_Stash';
}

sub insert { shift->_template->_insert(@_) }

sub eval_perl { shift->_template->{'EVAL_PERL'} }

sub process {
    my $self = shift;
    my $ref  = shift;
    my $args = shift || {};

    $self->_template->set_variable($_, $args->{$_}) for keys %$args;

    my $out  = '';
    $self->_template->_process($ref, $self->_template->_vars, \$out);
    return $out;
}

sub include {
    my $self = shift;
    my $ref  = shift;
    my $args = shift || {};

    my $t = $self->_template;

    my $swap = $t->{'_vars'};
    local $t->{'_vars'} = {%$swap};

    $t->set_variable($_, $args->{$_}) for keys %$args;

    my $out = ''; # have temp item to allow clear to correctly clear
    eval { $t->_process($ref, $t->_vars, \$out) };
    if (my $err = $@) {
        die $err if ref($err) !~ /Template::Exception$/ || $err->type !~ /return/;
    }

    return $out;
}

sub define_filter {
    my ($self, $name, $filter, $is_dynamic) = @_;
    $filter = [ $filter, 1 ] if $is_dynamic;
    $self->define_vmethod('filter', $name, $filter);
}

sub filter {
    my ($self, $name, $args, $alias) = @_;
    my $t = $self->_template;

    my $filter;
    if (! ref $name) {
        $filter = $t->{'FILTERS'}->{$name} || $CGI::Ex::Template::FILTER_OPS->{$name} || $CGI::Ex::Template::SCALAR_OPS->{$name};
        $t->throw('filter', $name) if ! $filter;
    } elsif (UNIVERSAL::isa($name, 'CODE') || UNIVERSAL::isa($name, 'ARRAY')) {
        $filter = $name;
    } elsif (UNIVERSAL::can($name, 'factory')) {
        $filter = $name->factory || $t->throw($name->error);
    } else {
        $t->throw('undef', "$name: filter not found");
    }

    if (UNIVERSAL::isa($filter, 'ARRAY')) {
        $filter = ($filter->[1]) ? $filter->[0]->($t->context, @$args) : $filter->[0];
    } elsif ($args && @$args) {
        my $sub = $filter;
        $filter = sub { $sub->(shift, @$args) };
    }

    $t->{'FILTERS'}->{$alias} = $filter if $alias;

    return $filter;
}

sub define_vmethod { shift->_template->define_vmethod(@_) }

sub throw {
    my ($self, $type, $info) = @_;

    if (ref($type) =~ /Template::Exception$/) {
	die $type;
    } elsif (defined $info) {
	$self->_template->throw($type, $info);
    } else {
	$self->_template->throw('undef', $type);
    }
}

sub AUTOLOAD { shift->_template->throw('not_implemented', "The method $AUTOLOAD has not been implemented") }

sub DESTROY {}

###----------------------------------------------------------------###

package CGI::Ex::Template::_Stash;

use vars qw($AUTOLOAD);

sub _template { shift->{'_template'} || die "Missing _template" }

sub get {
    my ($self, $var) = @_;
    if (! ref $var) {
        if ($var =~ /^\w+$/) {  $var = [$var, 0] }
        else {                  $var = $self->_template->parse_expr(\$var, {no_dots => 1}) }
    }
    return $self->_template->play_expr($var, {no_dots => 1});
}

sub set {
    my ($self, $var, $val) = @_;
    if (! ref $var) {
        if ($var =~ /^\w+$/) {  $var = [$var, 0] }
        else {                  $var = $self->_template->parse_expr(\$var, {no_dots => 1}) }
    }
    $self->_template->set_variable($var, $val, {no_dots => 1});
    return $val;
}

sub AUTOLOAD { shift->_template->throw('not_implemented', "The method $AUTOLOAD has not been implemented") }

sub DESTROY {}

###----------------------------------------------------------------###

1;
