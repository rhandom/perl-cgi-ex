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
$v = "$dir/storable1.storable";

### don't use the included binary - write our own - for portable tests
my $val = {
  user => {
    required => 1,
  },
  foo => {
    required_if => 'bar',
  },
};
&print_ok(eval {require Storable});
&print_ok(&Storable::store($val, $v));

$e = &validate({}, $v);
&print_ok($e);
$e = &validate({user => 1}, $v);
&print_ok(! $e);
$e = &validate({user => 1, bar => 1}, $v);
&print_ok($e);
$e = &validate({user => 1, bar => 1, foo => 1}, $v);
&print_ok(! $e);

__DATA__
