# -*- Mode: Perl; -*-

use strict;

$^W = 1;

print "1..3\n";

use HTML::Form;

print "ok 1\n";

my $hidden_form_in = qq{<TEXTAREA NAME="foo">blah</TEXTAREA>};

my %fdat = (foo => 'bar>bar');

my $fif = new HTML::Form;
my $output = $fif->fill(scalarref => \$hidden_form_in,
			fdat => \%fdat);
if ($output eq '<TEXTAREA NAME="foo">bar&gt;bar</TEXTAREA>'){
	print "ok 2\n";
} else {
	print "Got unexpected out for $hidden_form_in:\n$output\n";
	print "not ok 2\n";
}

# empty fdat test

%fdat = (foo => '');

$fif = new HTML::Form;
$output = $fif->fill(scalarref => \$hidden_form_in,
			fdat => \%fdat);
if ($output eq '<TEXTAREA NAME="foo"></TEXTAREA>'){
	print "ok 3\n";
} else {
	print "Got unexpected out for $hidden_form_in:\n$output\n";
	print "not ok 3\n";
}
