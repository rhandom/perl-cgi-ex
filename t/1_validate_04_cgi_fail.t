# -*- Mode: Perl; -*-

use strict;

$^W = 1;

print "1..2\n";

use CGI::Ex;
use CGI;

print "ok 1\n";

my $form = CGI->new({
  user => 'abc',
#  pass => '123',
});
my $val = {
  user => {
    required => 1,
  },
  pass => {
    required => 1,
  },
};

my $err_obj = CGI::Ex->new->validate($form,$val);

if ($err_obj) {
  print "ok 2\n";
} else {
  print "not ok 2\n";
}
