# -*- Mode: Perl; -*-

use strict;

$^W = 1;

print "1..7\n";

use CGI::Ex::Validate;
print "ok 1\n";

my ($v, $e, $ok);
sub validate {
  return scalar &CGI::Ex::Validate::validate(@_);
}

### required
$v  = {foo => {required => 1}};
$e  = &validate({}, $v);
$ok = $e;
print "" . ($ok ? "" : "not ") . "ok 2\n";

$e  = &validate({foo => 1}, $v);
$ok = ! $e;
print "" . ($ok ? "" : "not ") . "ok 3\n";

### validate_if
$v  = {foo => {required => 1, validate_if => 'bar'}};
$e  = &validate({}, $v);
$ok = ! $e;
print "" . ($ok ? "" : "not ") . "ok 4\n";

$e  = &validate({bar => 1}, $v);
$ok = $e;
print "" . ($ok ? "" : "not ") . "ok 5\n";

### required_if
$v  = {foo => {required_if => 'bar'}};
$e  = &validate({}, $v);
$ok = ! $e;
print "" . ($ok ? "" : "not ") . "ok 6\n";

$e  = &validate({bar => 1}, $v);
$ok = $e;
print "" . ($ok ? "" : "not ") . "ok 7\n";

