package Cea;

###---------------------###
#  Copyright 2003-2013 - Paul Seamons
#  Distributed under the Perl Artistic License without warranty

use base qw(CGI::Ex::App);
our $VERSION = '2.39';

1;

__END__

=head1 NAME

Cea - Shortcut for accessing CGI::Ex::App

=head1 SYNOPSIS

    use base qw(Cea);

=head1 DESCRIPTION

Cea is a barebones subclass of CGI::Ex::App useful as a namespace,
and also as a lazy way for accessing CGI::Ex::App.

Pronounced See-ah.

See L<CGI::Ex::App> for a full listing of features.

=head1 LICENSE

This module is distributed under the Artistic License.

=head1 AUTHOR

Paul Seamons <paul@seamons.com>

=cut
