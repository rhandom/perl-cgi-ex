#!/usr/bin/perl -w

# [pauls@localhost lib]$ perl ../t/samples/bench_cgix_hfif.pl
# Benchmark: timing 1000 iterations of cgix_func, cgix_meth, hfif...
#  cgix_func:  1 wallclock secs ( 1.05 usr +  0.00 sys =  1.05 CPU) @ 952.38/s (n=1000)
#  cgix_meth:  2 wallclock secs ( 1.09 usr +  0.01 sys =  1.10 CPU) @ 909.09/s (n=1000)
#  hfif:  8 wallclock secs ( 7.96 usr +  0.02 sys =  7.98 CPU) @ 125.31/s (n=1000)
#            Rate      hfif cgix_meth cgix_func
# hfif      125/s        --      -86%      -87%
# cgix_meth 909/s      625%        --       -5%
# cgix_func 952/s      660%        5%        --
# [pauls@localhost lib]$

use strict;

use Benchmark qw(cmpthese);
use HTML::FillInForm;
use CGI::Ex;

my $n = 1000;

my $t = q{

<!-- This is another thing -->
<html>
<form name=foo>

<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>

<input type=text name=foo value="wow">

<input type=password name="pass" value="">

<select name=garbage>
  <option value=lid>Lid</option>
  <option value=can>Can</option>
  <option value=wheel>Wheel</option>
  <option value=truck>Truck</option>
</select>

<!-- </form> -->

<textarea name=Mighty></textarea>

</form>

</html>
};

my $form = {
  foo => "bar",
  pass => "word",
  garbage => ['can','lid'],
  Mighty  => 'ducks',
};


my $fif = HTML::FillInForm->new;
my $fo  = CGI::Ex->new;
$fo->{remove_comments} = 1;

my $x = $fo->fill(scalarref => \$t,
                  fdat => $form,
                  target => 'foo',
                  );
#print $x;
#exit;

cmpthese($n, {
  hfif => sub {
    my $copy = $t;
    my $new = $fif->fill(scalarref => \$copy,
                         fdat => $form,
                         target => 'foo',
                         );
  },
  cgix_meth => sub {
    my $copy = $t;
    $fo->fill(scalarref => \$copy,
              fdat => $form,
              target => 'foo',
              );
  },
  cgix_func => sub {
    my $copy = $t;
    &CGI::Ex::Fill::form_fill(\$copy, $form, 'foo');
  },
});
