# -*- Mode: Perl; -*-

use Test;

BEGIN {plan tests => 2};

use CGI::Ex::Wrap;
ok(1);

my $obj = CGI::Ex::Wrap->new;
my $str = '[( hello )][( there )][( how )][( are )][( you )]';
$obj->wrap(\$str);
print "{$str}\n";
ok(1);
