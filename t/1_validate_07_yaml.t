# -*- Mode: Perl; -*-

=head1 NAME

1_validate_07_yaml.t - Check for CGI::Ex::Validate's ability to use YAML.

=cut

use strict;
use Test::More tests => 15;

SKIP: {

skip("Missing YAML.pm", 15) if ! eval { require 'YAML.pm' };

use_ok('CGI::Ex::Validate');

my $N = 0;
my $v;
my $e;

sub validate { scalar CGI::Ex::Validate::validate(@_) }

###----------------------------------------------------------------###

### single group
$v = '
user:
  required: 1
foo:
  required_if: bar
';

$e = validate({}, $v);
ok($e);
$e = validate({user => 1}, $v);
ok(! $e);
$e = validate({user => 1, bar => 1}, $v);
ok($e);
$e = validate({user => 1, bar => 1, foo => 1}, $v);
ok(! $e);


### three groups, some with validate_if's - using arrayref
$v = '
- group validate_if: foo
  bar:
    required: 1
- group validate_if: hem
  haw: { required: 1 }
- raspberry:
    required: 1
';

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


### three groups, some with validate_if's - using documents
$v = '---
group validate_if: foo
bar:
  required: 1
---
group validate_if: hem
haw: { required: 1 }
---
raspberry:
  required: 1
';

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

} # end of SKIP
