# -*- Mode: Perl; -*-

=head1 NAME

3_conf_00_base.t - Test for the basic functionality of CGI::Ex::Conf

=cut

use strict;
use Test::More tests => 2;

use_ok('CGI::Ex::Conf');

my $obj = CGI::Ex::Conf->new;
ok($obj);

### TODO - re-enable more fileside tests
