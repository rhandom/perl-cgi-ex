# -*- Mode: Perl; -*-

use Test;

BEGIN {plan tests => 4};

use CGI::Ex;
ok(1);

my $cgix = CGI::Ex->new;
my $form = {foo => 'bar', this => {is => {nested => ['wow', 'wee']}}};

ok('bar' eq $cgix->swap_template("[% foo %]", $form));

ok('wee' eq $cgix->swap_template("[% this.is.nested.1 %]", $form));

my $str = "[% this.is.nested.0 %]";
$cgix->swap_template(\$str, $form);
ok('wow' eq $str);

$cgix = CGI::Ex->new;
$cgix->set_form({
  foo => 'bar',
  baz => 'wow',
  this => 'wee',
});
$str = "<html>([% foo %]) <br>
([% baz %]) <br>
([% this %]) </html>";
$cgix->swap_template(\$str);
print $str;
ok($str eq "<html>(bar) <br>
(wow) <br>
(wee) </html>");
