#!/usr/bin/perl -w

use strict;
use Benchmark qw(cmpthese);
use POSIX qw(tmpnam);
use File::Path qw(rmtree);
use CGI::Ex::Template;
use Template;

my $tt_cache_dir = tmpnam;
END { rmtree $tt_cache_dir };
mkdir $tt_cache_dir, 0755;

my $tt1 = Template->new;
my $tt2 = Template->new(CACHE_DIR => $tt_cache_dir);

my $cet = CGI::Ex::Template->new;

###----------------------------------------------------------------###

my $swap = {one => "ONE", two => "TWO", three => "THREE"};

my $txt = "[% one %][% two %][% three %]\n";

sub tt1 {
    my $out = '';
    $tt1->process(\$txt, $swap, \$out);
    return $out;
}

sub tt2 {
    my $out = '';
    $tt2->process(\$txt, $swap, \$out);
    return $out;
}

sub cet1 {
    my $out = '';
    $cet->process(\$txt, $swap, \$out);
    return $out;
}

sub cet2 {
    my $out = $txt;
    $cet->swap(\$out, $swap);
    return $out;
}

### check out put - and also allow for caching
die "tt2  didn't match" if tt1()  ne tt1();
die "cet1 didn't match" if cet1() ne tt1();
die "cet2 didn't match" if cet2() ne tt1();

###----------------------------------------------------------------###

cmpthese (-2, {
    tt_nocache  => \&tt1,
    tt_cache    => \&tt2,
    cet_process => \&cet1,
    cet_swap    => \&cet2,
});
