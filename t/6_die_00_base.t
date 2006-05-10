# -*- Mode: Perl; -*-

use Test::More tests => 2;

use_ok('CGI::Ex::Die');

ok(eval {
  import CGI::Ex::Die register => 1;
  $SIG{__DIE__} eq \&CGI::Ex::Die::die_handler;
});
