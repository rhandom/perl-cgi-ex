# -*- Mode: Perl; -*-

use strict;

$^W = 1;

print "1..2\n";

use CGI::Ex;

print "ok 1\n";

my $hidden_form_in = '
<INPUT TYPE="TEXT" NAME="foo1" value="nada">
<input type="hidden" name="foo2"/>
';

my %fdat = (foo1 => 'bar1',
            foo2 => '"bar2"');


my $fif = new CGI::Ex;
my $output = $fif->fill(scalarref => \$hidden_form_in,
			fdat      => \%fdat);
if ($output =~ m/^\s*<input( (type="TEXT"|name="foo1"|value="bar1")){3}>\s*<input( (type="hidden"|name="foo2"|value="&quot;bar2&quot;")){3}\s*\/>\s*$/i){
	print "ok 2\n";
} else {
	print "Got unexpected out for $hidden_form_in:\n$output\n";
	print "not ok 2\n";
}
