# -*- Mode: Perl; -*-

use Test::More tests => 22;
use strict;
$^W = 1;

use_ok('CGI::Ex::Validate');

my ($v, $e);

sub validate {
  return scalar CGI::Ex::Validate::validate(@_);
}
sub print_ok {
  my ($ok, $msg) = @_;
  ok($ok, $msg);
  warn "Test failed at line ".(caller)[2]."\n" if ! $ok;
}

print_ok(1, 'Compiles');

###----------------------------------------------------------------###

### where are my samples
my $dir = __FILE__;
$dir =~ tr|\\|/|; # should probably use File::Spec
$dir =~ s|[^/]+$|../samples| || die "Couldn't determine dir";
$dir =~ s|^t/|./t/|; # to satisfy conf

### single group
$v = "$dir/yaml1.val";

$e = validate({}, $v);
print_ok($e, 'nothing passed');
$e = validate({user => 1}, $v);
print_ok(! $e, 'user passed');
$e = validate({user => 1, bar => 1}, $v);
print_ok($e, 'user and bar passed');
$e = validate({user => 1, bar => 1, foo => 1}, $v);
print_ok(! $e, 'user and bar and foo passed');


### single group - default extension
$v = "$dir/yaml1";

$e = validate({}, $v);
print_ok($e);
$e = validate({user => 1}, $v);
print_ok(! $e);
$e = validate({user => 1, bar => 1}, $v);
print_ok($e);
$e = validate({user => 1, bar => 1, foo => 1}, $v);
print_ok(! $e);


### three groups, some with validate_if's - using arrayref
$v = "$dir/yaml2.val";

$e = validate({}, $v);
print_ok($e);

$e = validate({
  raspberry => 'tart',
}, $v);
print_ok(! $e);

$e = validate({
  foo => 1,
  raspberry => 'tart',
}, $v);
print_ok($e);

$e = validate({
  foo => 1,
  bar => 1,
  raspberry => 'tart',
}, $v);
print_ok(! $e);

$e = validate({
  foo => 1,
  bar => 1,
  hem => 1,
  raspberry => 'tart',
}, $v);
print_ok($e);

$e = validate({
  foo => 1,
  bar => 1,
  hem => 1,
  haw => 1,
  raspberry => 'tart',
}, $v);
print_ok(! $e);


### three groups, some with validate_if's - using documents
$v = "$dir/yaml3.val";

$e = validate({}, $v);
print_ok($e);

$e = validate({
  raspberry => 'tart',
}, $v);
print_ok(! $e);

$e = validate({
  foo => 1,
  raspberry => 'tart',
}, $v);
print_ok($e);

$e = validate({
  foo => 1,
  bar => 1,
  raspberry => 'tart',
}, $v);
print_ok(! $e);

$e = validate({
  foo => 1,
  bar => 1,
  hem => 1,
  raspberry => 'tart',
}, $v);
print_ok($e);

$e = validate({
  foo => 1,
  bar => 1,
  hem => 1,
  haw => 1,
  raspberry => 'tart',
}, $v);
print_ok(! $e);

