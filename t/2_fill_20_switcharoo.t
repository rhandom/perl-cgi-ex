# -*- Mode: Perl; -*-

use strict;
$^W = 1;
print "1..2\n";
use CGI::Ex;
print "ok 1\n";


my $string;
my %fdat = (foo1 => 'bar1');
my $cgix = new CGI::Ex;
my $n    = 1;
my $dook = sub {
  $n ++;
  print "$n - ($string)\n";
  my @a;
  if ($string =~ m/ value=([\"\'])bar1\1/i
      && 1 == scalar(@a=$string =~ m/(value)/gi)) {
    print "ok $n\n";
  } else {
    print "not ok $n\n";
  }
};

###----------------------------------------------------------------###

$string = qq{<input name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input name=foo1>};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input name=foo1 />};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input value name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input value value name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input value value="" name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input grrr name="foo1" value="">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input value= name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input type=hidden value= name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input value= type="hidden" name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input value="" name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input value='' name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input value='one' name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input Value="one" name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input VALUE="one" name="foo1">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<input name="foo1" value="one">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE="one">};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE="one" >};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE="" >};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE= >};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE >};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE />};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE= />};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE="" />};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE="one" />};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();

$string = qq{<INPUT NAME="foo1" VALUE="one" />};
$cgix->fill(text => \$string, form => \%fdat);
&$dook();


