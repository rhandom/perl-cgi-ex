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
our $error;

sub parse_tree_tmpl {
    my $self = shift;

    local @{ $CGI::Ex::Template::ALIASES }{qw(ECHO INCLUDE IFN    ENDCOMMENT ENDIF ENDIFN ENDLOOP)}
                                         = qw(GET  PROCESS UNLESS END        END   END    END);
    local $self->{'ABSOLUTE'}   = 1;
    local $self->{'RELATIVE'}   = 1;
    local $self->{'ANYCASE'}    = 1;
    local $self->{'V1DOLLAR'}   = 1;
    local $self->{'TAG_STYLE'}  = $self->{'TAG_STYLE'} || 'html';

    return $self->parse_tree_tt3(@_);
}

###----------------------------------------------------------------###

sub set_dir {
    my $self = shift;
    $self->{'INCLUDE_PATHS'} = [shift, './'];
}

sub parse_file {
    my ($self, $content) = @_;

    my $vars = $self->{'_vars'} || {};

    local $self->{'SYNTAX'} = $self->{'SYNTAX'} || 'tmpl';
    local $CGI::Ex::Template::QR_PRIVATE = undef;

    $error = undef;

    my $out = '';
    $self->process_simple($content, $vars, \$out)
        || ($error = $self->error);
    return $out;
}

sub loop_iteration {
    my $self = shift;
    my $name = shift;
    my $ref  = $self->{'_vars'}->{$name} ||= [];
    my $vars;

    $self->throw('loop', "Variable $name is not an arrayref during loop_iteration") if ref($ref) ne 'ARRAY';
    if (defined(my $index = shift)) {
        $vars = $ref->[$index] || $self->throw('loop', "Index $index is not yet defined on loop $name");
    } else {
        $vars = {};
        push @$ref, $vars;
    }

    return ref($self)->new('_vars' => $vars);
}

###----------------------------------------------------------------###

1;
