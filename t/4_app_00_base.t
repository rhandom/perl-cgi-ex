# -*- Mode: Perl; -*-

use Test;

BEGIN {plan tests => 2};

use CGI::Ex::App;
ok(1);

my $obj = CGI::Ex::App->new({
});
ok($obj);
