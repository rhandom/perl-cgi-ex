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

### where are my samples
my $dir = __FILE__;
$dir =~ tr|\\|/|; # should probably use File::Spec
$dir =~ s|[^/]+$|samples| || die "Couldn't determine dir";
$dir =~ s|^t/|./t/|; # to satisfy conf

### single group
$v = "$dir/perl1.pl";

$e = &validate({}, $v);
&print_ok($e);
$e = &validate({user => 1}, $v);
&print_ok(! $e);
$e = &validate({user => 1, bar => 1}, $v);
&print_ok($e);
$e = &validate({user => 1, bar => 1, foo => 1}, $v);
&print_ok(! $e);


### three groups, some with validate_if's - using arrayref
$v = "$dir/perl2.pl";

$e = &validate({}, $v);
&print_ok($e);

$e = &validate({
  raspberry => 'tart',
}, $v);
&print_ok(! $e);

$e = &validate({
  foo => 1,
  raspberry => 'tart',
}, $v);
&print_ok($e);

$e = &validate({
  foo => 1,
  bar => 1,
  raspberry => 'tart',
}, $v);
&print_ok(! $e);

$e = &validate({
  foo => 1,
  bar => 1,
  hem => 1,
  raspberry => 'tart',
}, $v);
&print_ok($e);

$e = &validate({
  foo => 1,
  bar => 1,
  hem => 1,
  haw => 1,
  raspberry => 'tart',
}, $v);
&print_ok(! $e);

__DATA__
