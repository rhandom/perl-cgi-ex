# -*- Mode: Perl; -*-

use Test::More tests => 21;
use strict;

$^W = 1;

use_ok('CGI::Ex::Validate');

my ($v, $e);

sub validate { CGI::Ex::Validate::validate(@_) }

###----------------------------------------------------------------###

### test single group for extra fields
$v = [
{
  'general no_extra_fields' => 'all',
  foo => {max_len => 10},
},
];

$e = validate({}, $v);
ok(! $e);

$e = validate({foo => "foo"}, $v);
ok(! $e);

$e = validate({foo => "foo", bar => "bar"}, $v);
ok($e);

$e = validate({bar => "bar"}, $v);
ok($e);


### test on failed validate if
$v = [
{
  'general no_extra_fields' => 'all',
  'group validate_if' => 'baz',
  foo => {max_len => 10},
},
];

$e = validate({}, $v);
ok(! $e);

$e = validate({foo => "foo"}, $v);
ok(! $e);

$e = validate({foo => "foo", bar => "bar"}, $v);
ok(! $e);

$e = validate({bar => "bar"}, $v);
ok(! $e);

### test on successful validate if
$v = [
{
  'general no_extra_fields' => 'all',
  'group validate_if' => 'baz',
  foo => {max_len => 10},
  baz => {max_len => 10},
},
];

$e = validate({baz => 1}, $v);
ok(! $e);

$e = validate({baz => 1, foo => "foo"}, $v);
ok(! $e);

$e = validate({baz => 1, foo => "foo", bar => "bar"}, $v);
ok($e);

$e = validate({baz => 1, bar => "bar"}, $v);
ok($e);

### test on multiple groups, some with validate if
$v = [
{
  'general no_extra_fields' => 'all',
  'group validate_if' => 'baz',
  foo => {max_len => 10},
  baz => {max_len => 10},
},
{
  'group validate_if' => 'hem',
  haw => {max_len => 10},
},
];

$e = validate({haw => 1, baz => 1}, $v);
ok(! $e);

$e = validate({haw => 1, baz => 1, foo => "foo"}, $v);
ok(! $e);

$e = validate({haw => 1, baz => 1, foo => "foo", bar => "bar"}, $v);
ok($e);

$e = validate({haw => 1, baz => 1, bar => "bar"}, $v);
ok($e);


### test on multiple groups, some with validate if
$v = [
{
  'general no_extra_fields' => 'used',
  'group validate_if' => 'baz',
  foo => {max_len => 10},
  baz => {max_len => 10},
},
{
  'group validate_if' => 'hem',
  haw => {max_len => 10},
},
];

$e = validate({haw => 1, baz => 1}, $v);
ok($e);

$e = validate({haw => 1, baz => 1, foo => "foo"}, $v);
ok($e);

$e = validate({haw => 1, baz => 1, foo => "foo", bar => "bar"}, $v);
ok($e);

$e = validate({haw => 1, baz => 1, bar => "bar"}, $v);
ok($e);
