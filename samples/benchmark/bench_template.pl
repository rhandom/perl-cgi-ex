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

my $tt1 = Template->new(ABSOLUTE => 1);
my $tt2 = Template->new(ABSOLUTE => 1, COMPILE_DIR => $tt_cache_dir, COMPILE_EXT => 'ttc');

my $cet = CGI::Ex::Template->new(ABSOLUTE => 1);

###----------------------------------------------------------------###

my $swap = {
    one   => "ONE",
    two   => "TWO",
    three => "THREE",
    a_var => "a",
    hash  => {a => 1, b => 2},
    array => [qw(A B C D E a A)],
    code  => sub {"($_[0])"},
    cet   => $cet,
};

my $txt  = "[% cet.undefinedd %]\n";#[% foo %][% one %][% two %][% three %][% DUMP hash %]\n";

#my $file = \$txt;

my $file = $tt_cache_dir .'/template.txt';
open(my $fh, ">$file") || die "Couldn't open $file: $!";
print $fh $txt;
close $fh;


###----------------------------------------------------------------###

sub tt1 {
    my $out = '';
    $tt1->process($file, $swap, \$out);
    return $out;
}

sub tt2 {
    my $out = '';
    $tt2->process($file, $swap, \$out);
    return $out;
}

sub cet {
    my $out = '';
    $cet->process($file, $swap, \$out);
    return $out;
}

print tt1(), tt2(), cet(); exit;

### check out put - and also allow for caching
die "tt2 didn't match" if tt1() ne tt1();
die "cet didn't match" if cet() ne tt1();

###----------------------------------------------------------------###

cmpthese (-2, {
    tt_nocache  => \&tt1,
    tt_cache    => \&tt2,
    cet_process => \&cet,
});
