# -*- Mode: Perl; -*-

use strict;

$^W = 1;

print "1..2\n";

use HTML::Form;

print "ok 1\n";

my $hidden_form_in = qq{<INPUT TYPE="radio" NAME="foo1" value="bar1">
<input type="radio" name="foo1" value="bar2">
<input type="radio" name="foo1" value="bar3">
<input type="radio" name="foo1" checked value="bar4">};

my %fdat = (foo1 => 'bar2');

my $fif = new HTML::Form;
my $output = $fif->fill(scalarref => \$hidden_form_in,
			fdat => \%fdat);
my $is_checked = join(" ",map { m/checked/ ? "yes" : "no" } split ("\n",$output));
if ($is_checked eq 'no yes no no'){
	print "ok 2\n";
} else {
	print "Got unexpected is_checked:\n$is_checked\n";
	print "not ok 2\n";
}
