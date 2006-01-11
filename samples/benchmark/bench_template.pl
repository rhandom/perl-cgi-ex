#!/usr/bin/perl -w

use strict;
use Benchmark qw(cmpthese timethese);
use POSIX qw(tmpnam);
use File::Path qw(rmtree);
use CGI::Ex::Template;
#use CGI::Ex::Template2;
use CGI::Ex::Dump qw(debug);
use Template;

my $tt_cache_dir = tmpnam;
END { rmtree $tt_cache_dir };
mkdir $tt_cache_dir, 0755;

my $tt1 = Template->new(ABSOLUTE => 1);
my $tt2 = Template->new(ABSOLUTE => 1, COMPILE_DIR => $tt_cache_dir, COMPILE_EXT => 'ttc');

my $cet = CGI::Ex::Template->new(ABSOLUTE => 1);
#my $cet2 = CGI::Ex::Template2->new(ABSOLUTE => 1);


###----------------------------------------------------------------###

my $swap = {
    one   => "ONE",
    two   => "TWO",
    three => "THREE",
    a_var => "a",
    hash  => {a => 1, b => 2, c => { d => ["hmm"] }},
    array => [qw(A B C D E a A)],
    code  => sub {"($_[0])"},
    cet   => $cet,
};

#my $txt  = ((" "x1000)."[% one %]\n")x10;
#my $txt  = ((" "x1000)."[% one %]\n")x100;
my $txt = ((" "x10)."[% one %]\n")x1000;
#my $txt  = "[% one %]"x20;
#my $txt  = "([% 1 + 2 %])";
#my $txt   = "[% one %]";
#my $txt   = "[% SET one = 2 %]";
#my $txt   = "[% c.d.0 %]";

my $file  = \$txt;
my $file2 = $tt_cache_dir .'/template.txt';
open(my $fh, ">$file2") || die "Couldn't open $file2: $!";
print $fh $txt;
close $fh;

###----------------------------------------------------------------###

sub file_TT_new {
    my $out = '';
    my $t = Template->new(ABSOLUTE => 1);
    $t->process($file2, $swap, \$out);
    return $out;
}

sub str_TT_new {
    my $out = '';
    my $t = Template->new(ABSOLUTE => 1);
    $t->process($file, $swap, \$out);
    return $out;
}

sub file_TT {
    my $out = '';
    $tt1->process($file2, $swap, \$out);
    return $out;
}

sub str_TT {
    my $out = '';
    $tt1->process($file, $swap, \$out);
    return $out;
}

sub file_TT_cache {
    my $out = '';
    $tt2->process($file2, $swap, \$out);
    return $out;
}

sub str_TT_cache {
    my $out = '';
    $tt2->process($file, $swap, \$out);
    return $out;
}

###----------------------------------------------------------------###

sub file_CET_new {
    my $out = '';
    my $t = CGI::Ex::Template->new(ABSOLUTE => 1);
    $t->process($file2, $swap, \$out);
    return $out;
}

sub str_CET_new {
    my $out = '';
    my $t = CGI::Ex::Template->new(ABSOLUTE => 1);
    $t->process($file, $swap, \$out);
    return $out;
}

sub file_CET {
    my $out = '';
    $cet->process($file2, $swap, \$out);
    return $out;
}

sub str_CET {
    my $out = '';
    $cet->process($file, $swap, \$out);
    return $out;
}

sub str_CET_swap {
    my $txt = $cet->swap($$file, $swap);
    return $txt;
}

#sub str_CET_old {
#    my $out = '';
#    $cet2->process($file, $swap, \$out);
#    return $out;
#}

#debug file_CET(), file_CET(), str_TT();

### check out put - and also allow for caching
for (1..10) {
    die "file_CET didn't match"     if file_CET()     ne str_TT();
    die "str_CET didn't match"      if str_CET()      ne str_TT();
    die "str_CET_swap didn't match" if str_CET_swap() ne str_TT();
#    die "str_CET_old  didn't match" if str_CET_old()  ne str_TT();
}

###----------------------------------------------------------------###

cmpthese timethese (-2, {
    file_TT_n   => \&file_TT_new,
    str_TT_n    => \&str_TT_new,
    file_TT     => \&file_TT,
    str_TT      => \&str_TT,
#    file_TT_cch => \&file_TT_cache,
#    str_TT_cch  => \&str_TT_cache,

    file_CET_n   => \&file_CET_new,
    str_CET_n    => \&str_CET_new,
    file_CET     => \&file_CET,
    str_CET      => \&str_CET,
    str_CET_sw   => \&str_CET_swap,
#    str_CET_o    => \&str_CET_old,
});


#  Benchmark: running file_CET, file_CET_n, file_TT, file_TT_n, str_CET, str_CET_n, str_CET_o, str_CET_sw, str_TT, str_TT_n for at least 2 CPU seconds...
#    file_CET:  2 wallclock secs ( 2.00 usr +  0.00 sys =  2.00 CPU) @ 46.00/s (n=92)
#  file_CET_n:  2 wallclock secs ( 2.07 usr +  0.01 sys =  2.08 CPU) @ 16.83/s (n=35)
#    file_TT:  3 wallclock secs ( 2.25 usr +  0.01 sys =  2.26 CPU) @ 49.12/s (n=111)
#    file_TT_n:  2 wallclock secs ( 2.24 usr +  0.00 sys =  2.24 CPU) @  2.68/s (n=6)
#    str_CET:  2 wallclock secs ( 2.06 usr +  0.01 sys =  2.07 CPU) @ 16.43/s (n=34)
#    str_CET_n:  2 wallclock secs ( 2.10 usr +  0.01 sys =  2.11 CPU) @ 16.59/s (n=35)
#    str_CET_o:  2 wallclock secs ( 2.13 usr +  0.00 sys =  2.13 CPU) @ 21.60/s (n=46)
#  str_CET_sw:  2 wallclock secs ( 2.07 usr +  0.00 sys =  2.07 CPU) @ 16.43/s (n=34)
#    str_TT:  2 wallclock secs ( 2.23 usr +  0.00 sys =  2.23 CPU) @  2.69/s (n=6)
#    str_TT_n:  2 wallclock secs ( 2.23 usr +  0.01 sys =  2.24 CPU) @  2.68/s (n=6)
#               Rate str_TT_n file_TT_n str_TT str_CET_sw str_CET str_CET_n file_CET_n str_CET_o file_CET file_TT
#  str_TT_n   2.68/s       --       -0%    -0%       -84%    -84%      -84%       -84%      -88%     -94%    -95%
#  file_TT_n  2.68/s       0%        --    -0%       -84%    -84%      -84%       -84%      -88%     -94%    -95%
#  str_TT     2.69/s       0%        0%     --       -84%    -84%      -84%       -84%      -88%     -94%    -95%
#  str_CET_sw 16.4/s     513%      513%   510%         --     -0%       -1%        -2%      -24%     -64%    -67%
#  str_CET    16.4/s     513%      513%   510%         0%      --       -1%        -2%      -24%     -64%    -67%
#  str_CET_n  16.6/s     519%      519%   517%         1%      1%        --        -1%      -23%     -64%    -66%
#  file_CET_n 16.8/s     528%      528%   525%         2%      2%        1%         --      -22%     -63%    -66%
#  str_CET_o  21.6/s     706%      706%   703%        31%     31%       30%        28%        --     -53%    -56%
#  file_CET   46.0/s    1617%     1617%  1610%       180%    180%      177%       173%      113%       --     -6%
#  file_TT    49.1/s    1734%     1734%  1725%       199%    199%      196%       192%      127%       7%      --
