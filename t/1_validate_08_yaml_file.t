# -*- Mode: Perl; -*-

=head1 NAME

1_validate_08_yaml_file.t - Check for CGI::Ex::Validate's ability to load YAML conf files.

=cut

use strict;
use Test::More tests => 22;

SKIP: {

skip("Missing YAML.pm", 22) if ! eval { require 'YAML' };

local $^W = 1;

use_ok('CGI::Ex::Validate');

my ($v, $e);

sub validate { scalar CGI::Ex::Validate::validate(@_) }

###----------------------------------------------------------------###

### where are my samples
my $dir = __FILE__;
$dir =~ tr|\\|/|; # should probably use File::Spec
$dir =~ s|[^/]+$|../samples| || die "Couldn't determine dir";
$dir =~ s|^t/|./t/|; # to satisfy conf

### single group
$v = "$dir/yaml1.val";

$e = validate({}, $v);
ok($e, 'nothing passed');
$e = validate({user => 1}, $v);
ok(! $e, 'user passed');
$e = validate({user => 1, bar => 1}, $v);
ok($e, 'user and bar passed');
$e = validate({user => 1, bar => 1, foo => 1}, $v);
ok(! $e, 'user and bar and foo passed');


### single group - default extension
$v = "$dir/yaml1";

$e = validate({}, $v);
ok($e);
$e = validate({user => 1}, $v);
ok(! $e);
$e = validate({user => 1, bar => 1}, $v);
ok($e);
$e = validate({user => 1, bar => 1, foo => 1}, $v);
ok(! $e);


### three groups, some with validate_if's - using arrayref
$v = "$dir/yaml2.val";

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
$v = "$dir/yaml3.val";

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
