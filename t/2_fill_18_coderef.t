# -*- Mode: Perl; -*-

use strict;

$^W = 1;

print "1..4\n";

use CGI::Ex;

print "ok 1\n";

my $ok2 = 0;
my $ok3 = 0;

my $hidden_form_in = qq{<input type="hidden" name="foo1">
<input type="hidden" name="foo2" value="ack">};

my %fdat = (foo1 => sub { $ok2 ++; return 'bar1' },
            );
my $cdat = sub {
  $ok3 ++;
  my $key = shift;
  return ($key eq 'foo2') ? 'bar2' : '';
};

my $fif = new CGI::Ex;
my $output = $fif->fill(scalarref => \$hidden_form_in,
			fdat => [\%fdat, $cdat]);

print "" . ($ok2 ? "" : "not ") . "ok 2\n";
print "" . ($ok3 ? "" : "not ") . "ok 3\n";

if ($output =~ m/^<input( (type="hidden"|name="foo1"|value="bar1")){3}>\s*<input( (type="hidden"|name="foo2"|value="bar2")){3}>$/){
  print "ok 4\n";
} else {
  print "Got unexpected out for hidden form:\n$output\n";
  print "not ok 4\n";
}
