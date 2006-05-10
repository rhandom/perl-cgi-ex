# -*- Mode: Perl; -*-

=head1 NAME

1_validate_06_groups.t - Test CGI::Ex::Validate's ability to use groups of validation

=cut

use strict;
use Test::More tests => 7;

use_ok('CGI::Ex::Validate');

my ($v, $e);

sub validate { scalar CGI::Ex::Validate::validate(@_) }

###----------------------------------------------------------------###

### three groups, some with validate_if's
$v = [{
  'group validate_if' => 'foo',
  bar => {required => 1},
},
{
  'group validate_if' => 'hem',
  haw => {required => 1},
},
{
  raspberry => {required => 1},
}];

$e = validate({}, $v);
ok($e);

$e = validate({
  raspberry => 'tart',
}, $v);
ok(! $e);

$e = validate({
  foo => 1,
  raspberry => 'tart',
}, $v);
ok($e);

$e = validate({
  foo => 1,
  bar => 1,
  raspberry => 'tart',
}, $v);
ok(! $e);

$e = validate({
  foo => 1,
  bar => 1,
  hem => 1,
  raspberry => 'tart',
}, $v);
ok($e);

$e = validate({
  foo => 1,
  bar => 1,
  hem => 1,
  haw => 1,
  raspberry => 'tart',
}, $v);
ok(! $e);

