# -*- Mode: Perl; -*-

=head1 NAME

1_validate_12_change.t - Test CGI::Ex::Validate's ability to modify form fields

=cut

use strict;
use Test::More tests => 5;
use strict;

use_ok('CGI::Ex::Validate');
my $e;
my $v;
sub validate { scalar CGI::Ex::Validate::validate(@_) }


###----------------------------------------------------------------###

$v = {
  foo => {
    max_len => 10,
    replace => 's/[^\d]//g',
  },
};

$e = validate({
  foo => '123-456-7890',
}, $v);
ok(! $e, "Didn't get error");


my $form = {
  key1 => 'Bu-nch @of characte#rs^',
  key2 => '123 456 7890',
};


$v = {
  key1 => {
    replace => 's/[^\s\w]//g',
  },
};

$e = validate($form, $v);
ok(! $e && $form->{key1} eq 'Bunch of characters', "No error and key1 updated");

$v = {
  key2 => {
    replace => 's/(\d{3})\D*(\d{3})\D*(\d{4})/($1) $2-$3/g',
  },
};

$e = validate($form, $v);
ok(! $e && $form->{key2} eq '(123) 456-7890', "No error and phone updated");

$v = {
  key2 => {
    replace => 's/.+//g',
    required => 1,
  },
};

$e = validate($form, $v);
ok($e && $form->{key2} eq '', "Error with all replaced");

