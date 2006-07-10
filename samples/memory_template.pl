#!/usr/bin/perl -w

my $swap = {
    one   => "ONE",
    two   => "TWO",
    three => "THREE",
    a_var => "a",
    hash  => {a => 1, b => 2},
    code  => sub {"($_[0])"},
};

my $txt  = "[% one %][% two %][% three %][% hash.keys.join %] [% code(one).length %] [% hash.\$a_var %]\n";
#$txt = hello2000();

###----------------------------------------------------------------###

my $module;
if (! fork) {
    $module = 'CGI::Ex::Template';
    $0 = "perl $module";
} elsif (! fork) {
    $module = 'Template';
    $0 = "perl $module";
}

if ($module) {
    my $pm = "$module.pm";
    $pm =~ s|::|/|g;
    require $pm;

    my $t = $module->new(ABSOLUTE => 1);
    my $out = '';
    $t->process(\$txt, $swap, \$out);
    print $out;
}

sleep 15; # go and check the 'ps fauwx|grep perl'


###----------------------------------------------------------------###

sub hello2000 {
    my $hello2000 = "<html><head><title>[% title %]</title></head><body>
[% array = [ \"Hello\", \"World\", \"2000\", \"Hello\", \"World\", \"2000\" ] %]
[% sorted = array.sort %]
[% multi = [ sorted, sorted, sorted, sorted, sorted ] %]
<table>
[% FOREACH row = multi %]
  <tr bgcolor=\"[% loop.count % 2 ? 'gray' : 'white' %]\">
  [% FOREACH col = row %]
    <td align=\"center\"><font size=\"+1\">[% col %]</font></td>
  [% END %]
  </tr>
[% END %]
</table>
[% param = integer %]
[% FOREACH i = [ 1 .. 10 ] %]
  [% var = i + param %]"
  .("\n  [%var%] Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World <br/>"x20)."
[% END %]
</body></html>
";
}
