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
