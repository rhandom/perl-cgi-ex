# -*- Mode: Perl; -*-

use Test;

BEGIN {plan tests => 2};

use CGI::Ex::Die;
ok(1);

ok(eval {
  import CGI::Ex::Die register => 1;
  $SIG{__DIE__} eq \&CGI::Ex::Die::die_handler;
});
