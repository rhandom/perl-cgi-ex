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

$v = [
{
  foo => {
    max_len => 10,
    replace => 's/[^\d]//g',
  },
},
];

$e = &validate({
  foo => '123-456-7890',
}, $v);
&print_ok(! $e);


my $form = {
  key1 => 'Bu-nch @of characte#rs^',
  key2 => '123 456 7890',
};


$v = {
  key1 => {
    replace => 's/[^\s\w]//g',
  },
};

$e = &validate($form, $v);
&print_ok(! $e && $form->{key1} eq 'Bunch of characters');

$v = {
  key2 => {
    replace => 's/(\d{3})\D*(\d{3})\D*(\d{4})/($1) $2-$3/g',
  },
};

$e = &validate($form, $v);
&print_ok(! $e && $form->{key2} eq '(123) 456-7890');


$v = {
  key2 => {
    replace => 's/.+//g',
    required => 1,
  },
};

$e = &validate($form, $v);
&print_ok($e && $form->{key2} eq '');

__DATA__
