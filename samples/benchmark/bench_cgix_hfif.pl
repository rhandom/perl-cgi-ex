#!/usr/bin/perl -w

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
  one => sub {
    my $copy = $t;
    my $new = $fif->fill(scalarref => \$copy,
                         fdat => $form,
                         target => 'foo',
                         );
  },
  two => sub {
    my $copy = $t;
    $fo->fill(scalarref => \$copy,
              fdat => $form,
              target => 'foo',
              );
  },
  tyree => sub {
    my $copy = $t;
    &CGI::Ex::Fill::form_fill(\$copy, $form, 'foo');
  },
});
