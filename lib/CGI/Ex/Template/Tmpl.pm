package CGI::Ex::Template::Tmpl;

=head1 NAME

CGI::Ex::Template::Tmpl - provide Text::Tmpl support

=head1 DESCRIPTION

Provides for extra or extended features that may not be as commonly used.
This module should not normally be used by itself.

See the CGI::Ex::Template documentation for configuration and other parameters.

=head1 AUTHOR

Paul Seamons <paul at seamons dot com>

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut

use strict;
use warnings;

our $VERSION = '2.13';

sub parse_tree_tmpl {
    my $self = shift;

    local @{ $CGI::Ex::Template::ALIASES }{qw(ECHO INCLUDE IFN    LOOP   ENDCOMMENT ENDIF ENDIFN ENDLOOP)}
                                         = qw(GET  PROCESS UNLESS REPEAT END        END   END    END);
    local $self->{'ABSOLUTE'}   = 1;
    local $self->{'RELATIVE'}   = 1;
    local $self->{'ANYCASE'}    = 1;
    local $self->{'V1DOLLAR'}   = 1;
    local $self->{'TAG_STYLE'}  = $self->{'TAG_STYLE'} || 'html';

    return $self->parse_tree_tt3(@_);
}

###----------------------------------------------------------------###
### a few HTML::Template and HTML::Template::Expr routines

sub set_dir {
    my $self = shift;
    $self->{'INCLUDE_PATHS'} = [shift, './'];
}

sub set_values {
    my $self = shift;
    my $args;
    if (@_ == 1) {
        my $key = shift;
        if (ref($key) ne 'HASH') {
            return $self->{'_vars'}->{$key};
        }
        $args = [%$key];
    } else {
        $self->throw('param', "Odd number of parameters") if @_ % 2;
        $args = \@_;
    }
    while (@$args) {
        my $key = shift @$args;
        $self->{'_vars'}->{$key} = shift @$args;
    }
    return;
}

sub parse_string {
    my $self = shift;

    my $content = \ $_[0];

    my $vars = $self->{'_vars'} || {};

    local $self->{'SYNTAX'} = $self->{'SYNTAX'} || 'tmpl';
    local $CGI::Ex::Template::QR_PRIVATE = undef;

    my $out = '';
    $self->process_simple($content, $vars, \$out) || die $self->error;
    return $out;
}

###----------------------------------------------------------------###

1;
