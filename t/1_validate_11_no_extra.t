# -*- Mode: Perl; -*-

use strict;

$^W = 1;

### determine number of tests
seek(DATA,0,0);
my $prog  = join "", <DATA>;
my @tests = ($prog =~ /&print_ok\(/g);
my $tests = @tests;
print "1..$tests\n";

require CGI::Ex::Validate;

my ($N, $v, $e, $ok) = (0);

sub validate {
  return scalar &CGI::Ex::Validate::validate(@_);
}
sub print_ok {
  my $ok = shift;
  $N ++;
  warn "Test failed at line ".(caller)[2]."\n" if ! $ok;
  print "" . ($ok ? "" : "not ") . "ok $N\n";
}
&print_ok(1);

###----------------------------------------------------------------###

### test single group for extra fields
$v = [
{
  'general no_extra_fields' => 'all',
  foo => {max_len => 10},
},
];

$e = &validate({}, $v);
&print_ok(! $e);

$e = &validate({foo => "foo"}, $v);
&print_ok(! $e);

$e = &validate({foo => "foo", bar => "bar"}, $v);
&print_ok($e);

$e = &validate({bar => "bar"}, $v);
&print_ok($e);


### test on failed validate if
$v = [
{
  'general no_extra_fields' => 'all',
  'group validate_if' => 'baz',
  foo => {max_len => 10},
},
];

$e = &validate({}, $v);
&print_ok(! $e);

$e = &validate({foo => "foo"}, $v);
&print_ok(! $e);

$e = &validate({foo => "foo", bar => "bar"}, $v);
&print_ok(! $e);

$e = &validate({bar => "bar"}, $v);
&print_ok(! $e);

### test on successful validate if
$v = [
{
  'general no_extra_fields' => 'all',
  'group validate_if' => 'baz',
  foo => {max_len => 10},
  baz => {max_len => 10},
},
];

$e = &validate({baz => 1}, $v);
&print_ok(! $e);

$e = &validate({baz => 1, foo => "foo"}, $v);
&print_ok(! $e);

$e = &validate({baz => 1, foo => "foo", bar => "bar"}, $v);
&print_ok($e);

$e = &validate({baz => 1, bar => "bar"}, $v);
&print_ok($e);

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

$e = &validate({haw => 1, baz => 1}, $v);
&print_ok(! $e);

$e = &validate({haw => 1, baz => 1, foo => "foo"}, $v);
&print_ok(! $e);

$e = &validate({haw => 1, baz => 1, foo => "foo", bar => "bar"}, $v);
&print_ok($e);

$e = &validate({haw => 1, baz => 1, bar => "bar"}, $v);
&print_ok($e);


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

$e = &validate({haw => 1, baz => 1}, $v);
&print_ok($e);

$e = &validate({haw => 1, baz => 1, foo => "foo"}, $v);
&print_ok($e);

$e = &validate({haw => 1, baz => 1, foo => "foo", bar => "bar"}, $v);
&print_ok($e);

$e = &validate({haw => 1, baz => 1, bar => "bar"}, $v);
&print_ok($e);

__DATA__
