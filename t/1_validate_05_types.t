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


### required
$v = {foo => {required => 1}};
$e = &validate({}, $v);
&print_ok($e);

$e = &validate({foo => 1}, $v);
&print_ok(! $e);

### validate_if
$v = {foo => {required => 1, validate_if => 'bar'}};
$e = &validate({}, $v);
&print_ok(! $e);

$e = &validate({bar => 1}, $v);
&print_ok($e);

### required_if
$v = {foo => {required_if => 'bar'}};
$e = &validate({}, $v);
&print_ok(! $e);

$e = &validate({bar => 1}, $v);
&print_ok($e);

### max_values
$v = {foo => {required => 1}};
$e = &validate({foo => [1,2]}, $v);
&print_ok($e);

$v = {foo => {max_values => 2}};
$e = &validate({}, $v);
&print_ok(! $e);

$e = &validate({foo => "str"}, $v);
&print_ok(! $e);

$e = &validate({foo => [1]}, $v);
&print_ok(! $e);

$e = &validate({foo => [1,2]}, $v);
&print_ok(! $e);

$e = &validate({foo => [1,2,3]}, $v);
&print_ok($e);

### min_values
$v = {foo => {min_values => 3, max_values => 10}};
$e = &validate({foo => [1,2,3]}, $v);
&print_ok(! $e);

$e = &validate({foo => [1,2,3,4]}, $v);
&print_ok(! $e);

$e = &validate({foo => [1,2]}, $v);
&print_ok($e);

$e = &validate({foo => "str"}, $v);
&print_ok($e);

$e = &validate({}, $v);
&print_ok($e);

### enum
$v = {foo => {enum => [1, 2, 3]}, bar => {enum => "1 || 2||3"}};
$e = &validate({}, $v);
&print_ok($e);

$e = &validate({foo => 1, bar => 1}, $v);
&print_ok(! $e);

$e = &validate({foo => 1, bar => 2}, $v);
&print_ok(! $e);

$e = &validate({foo => 1, bar => 3}, $v);
&print_ok(! $e);

$e = &validate({foo => 1, bar => 4}, $v);
&print_ok($e);

# equals
$v = {foo => {equals => 'bar'}};
$e = &validate({}, $v);
&print_ok(! $e);

$e = &validate({foo => 1}, $v);
&print_ok($e);

$e = &validate({bar => 1}, $v);
&print_ok($e);

$e = &validate({foo => 1, bar => 2}, $v);
&print_ok($e);

$e = &validate({foo => 1, bar => 1}, $v);
&print_ok(! $e);

$v = {foo => {equals => '"bar"'}};
$e = &validate({foo => 1, bar => 1}, $v);
&print_ok($e);

$e = &validate({foo => 'bar', bar => 1}, $v);
&print_ok(! $e);

### min_len
$v = {foo => {min_len => 10}};
$e = &validate({}, $v);
&print_ok($e);

$e = &validate({foo => ""}, $v);
&print_ok($e);

$e = &validate({foo => "123456789"}, $v);
&print_ok($e);

$e = &validate({foo => "1234567890"}, $v);
&print_ok(! $e);

### max_len
$v = {foo => {max_len => 10}};
$e = &validate({}, $v);
&print_ok(! $e);

$e = &validate({foo => ""}, $v);
&print_ok(! $e);

$e = &validate({foo => "1234567890"}, $v);
&print_ok(! $e);

$e = &validate({foo => "12345678901"}, $v);
&print_ok($e);

### match
$v = {foo => {match => qr/^\w+$/}};
$e = &validate({foo => "abc"}, $v);
&print_ok(! $e);

$e = &validate({foo => "abc."}, $v);
&print_ok($e);

$v = {foo => {match => [qr/^\w+$/, qr/^[a-z]+$/]}};
$e = &validate({foo => "abc"}, $v);
&print_ok(! $e);

$e = &validate({foo => "abc1"}, $v);
&print_ok($e);

$v = {foo => {match => 'm/^\w+$/'}};
$e = &validate({foo => "abc"}, $v);
&print_ok(! $e);

$e = &validate({foo => "abc."}, $v);
&print_ok($e);

$v = {foo => {match => 'm/^\w+$/ || m/^[a-z]+$/'}};
$e = &validate({foo => "abc"}, $v);
&print_ok(! $e);

$e = &validate({foo => "abc1"}, $v);
&print_ok($e);

$v = {foo => {match => '! m/^\w+$/'}};
$e = &validate({foo => "abc"}, $v);
&print_ok($e);

$e = &validate({foo => "abc."}, $v);
&print_ok(! $e);

$v = {foo => {match => 'm/^\w+$/'}};
$e = &validate({}, $v);
&print_ok($e);

$v = {foo => {match => '! m/^\w+$/'}};
$e = &validate({}, $v);
&print_ok(! $e);

### compare
$v = {foo => {compare => '> 0'}};
$e = &validate({}, $v);
&print_ok($e);
$v = {foo => {compare => '== 0'}};
$e = &validate({}, $v);
&print_ok(! $e);
$v = {foo => {compare => '< 0'}};
$e = &validate({}, $v);
&print_ok($e);

$v = {foo => {compare => '> 10'}};
$e = &validate({foo => 11}, $v);
&print_ok(! $e);
$e = &validate({foo => 10}, $v);
&print_ok($e);

$v = {foo => {compare => '== 10'}};
$e = &validate({foo => 11}, $v);
&print_ok($e);
$e = &validate({foo => 10}, $v);
&print_ok(! $e);

$v = {foo => {compare => '< 10'}};
$e = &validate({foo => 9}, $v);
&print_ok(! $e);
$e = &validate({foo => 10}, $v);
&print_ok($e);

$v = {foo => {compare => '>= 10'}};
$e = &validate({foo => 10}, $v);
&print_ok(! $e);
$e = &validate({foo => 9}, $v);
&print_ok($e);

$v = {foo => {compare => '!= 10'}};
$e = &validate({foo => 10}, $v);
&print_ok($e);
$e = &validate({foo => 9}, $v);
&print_ok(! $e);

$v = {foo => {compare => '<= 10'}};
$e = &validate({foo => 11}, $v);
&print_ok($e);
$e = &validate({foo => 10}, $v);
&print_ok(! $e);


$v = {foo => {compare => 'gt ""'}};
$e = &validate({}, $v);
&print_ok($e);
$v = {foo => {compare => 'eq ""'}};
$e = &validate({}, $v);
&print_ok(! $e);
$v = {foo => {compare => 'lt ""'}};
$e = &validate({}, $v);
&print_ok($e); # 68

$v = {foo => {compare => 'gt "c"'}};
$e = &validate({foo => 'd'}, $v);
&print_ok(! $e);
$e = &validate({foo => 'c'}, $v);
&print_ok($e);

$v = {foo => {compare => 'eq c'}};
$e = &validate({foo => 'd'}, $v);
&print_ok($e);
$e = &validate({foo => 'c'}, $v);
&print_ok(! $e);

$v = {foo => {compare => 'lt c'}};
$e = &validate({foo => 'b'}, $v);
&print_ok(! $e);
$e = &validate({foo => 'c'}, $v);
&print_ok($e);

$v = {foo => {compare => 'ge c'}};
$e = &validate({foo => 'c'}, $v);
&print_ok(! $e);
$e = &validate({foo => 'b'}, $v);
&print_ok($e);

$v = {foo => {compare => 'ne c'}};
$e = &validate({foo => 'c'}, $v);
&print_ok($e);
$e = &validate({foo => 'b'}, $v);
&print_ok(! $e);

$v = {foo => {compare => 'le c'}};
$e = &validate({foo => 'd'}, $v);
&print_ok($e);
$e = &validate({foo => 'c'}, $v);
&print_ok(! $e); # 80

### sql
### can't really do anything here without prompting for a db connection

### custom
my $n = 1;
$v = {foo => {custom => $n}};
$e = &validate({}, $v);
&print_ok(! $e);
$e = &validate({foo => "str"}, $v);
&print_ok(! $e);

$n = 0;
$v = {foo => {custom => $n}};
$e = &validate({}, $v);
&print_ok($e);
$e = &validate({foo => "str"}, $v);
&print_ok($e);

$n = sub { my ($key, $val) = @_; return defined($val) ? 1 : 0};
$v = {foo => {custom => $n}};
$e = &validate({}, $v);
&print_ok($e);
$e = &validate({foo => "str"}, $v);
&print_ok(! $e);

### type checks
$v = {foo => {type => 'ip'}};
$e = &validate({foo => '209.108.25'}, $v);
&print_ok($e);
$e = &validate({foo => '209.108.25.111'}, $v);
&print_ok(! $e);

### min_in_set checks
$v = {foo => {min_in_set => '2 of foo bar baz', max_values => 5}};
$e = &validate({foo => 1}, $v);
&print_ok($e);
$e = &validate({foo => 1, bar => 1}, $v);
&print_ok(! $e);
$e = &validate({foo => 1, bar => ''}, $v); # empty string doesn't count as value
&print_ok($e);
$e = &validate({foo => 1, bar => 0}, $v);
&print_ok(! $e);
$e = &validate({foo => [1, 2]}, $v);
&print_ok(! $e);
$e = &validate({foo => [1]}, $v);
&print_ok($e);
$v = {foo => {min_in_set => '2 foo bar baz', max_values => 5}};
$e = &validate({foo => 1, bar => 1}, $v);
&print_ok(! $e);

### max_in_set checks
$v = {foo => {max_in_set => '2 of foo bar baz', max_values => 5}};
$e = &validate({foo => 1}, $v);
&print_ok(! $e);
$e = &validate({foo => 1, bar => 1}, $v);
&print_ok(! $e);
$e = &validate({foo => 1, bar => 1, baz => 1}, $v);
&print_ok($e);
$e = &validate({foo => [1, 2]}, $v);
&print_ok(! $e);
$e = &validate({foo => [1, 2, 3]}, $v);
&print_ok($e);

__DATA__
