# -*- Mode: Perl; -*-

use strict;

$^W = 1;

print "1..2\n";

use CGI::Ex;

print "ok 1\n";

my $string = qq{
<input attr="<br value='waw'>
<br>" type="hidden" name="foo1">
};

my %fdat = (foo1 => 'bar1');


my $cgix = new CGI::Ex;
$cgix->fill(text => \$string,
            form => \%fdat,
            );

if ($string =~ m/ value="bar1"/) {
  print "ok 2\n";
} else {
  print "not ok 2\n";
}
