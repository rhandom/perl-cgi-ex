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
my $cetc = CGI::Ex::Template->new(ABSOLUTE => 1, COMPILE_DIR => $tt_cache_dir, COMPILE_EXT => 'ttc');
#my $cet2 = CGI::Ex::Template2->new(ABSOLUTE => 1);


###----------------------------------------------------------------###

my $swap = {
    one   => "ONE",
    two   => "TWO",
    three => "THREE",
    a_var => "a",
    hash  => {a => 1, b => 2, c => { d => [{hee => ["hmm"]}] }},
    array => [qw(A B C D E a A)],
    code  => sub {"($_[0])"},
    cet   => $cet,
};
#$swap->{$_} = $_ for (1 .. 1000);

my $txt;
                                                        # This percent is compiled (in memory) CET vs TT
#$txt = ((" "x1000)."[% one %]\n")x10;                  #   69%
#$txt = ((" "x1000)."[% one %]\n")x100;                 #   42%
$txt = ((" "x10)."[% one %]\n")x1000;                  #   -6%
#$txt = "[% one %]";                                    #  253%
#$txt = "[% one %]"x20;                                 #   44%
#$txt = "([% 1 + 2 %])";                                #   49%
#$txt = "[% 1 + 2 + 3 + 5 + 6 + 8 %]";                  #  118%
#$txt = "[% SET one = 2 %]";                            #  196%
#$txt = "[% SET one = [0..30] %]";                      #   42%
#$txt = "[% c.d.0.hee.0 %]";                            #  280%
#$txt = ((" "x10)."[% c.d.0.hee.0 %]\n")x1000;          #   62%
#$txt = "[% t = 1 || 0 ? 0 : 1 || 2 ? 2 : 3 %][% t %]"; #  109%
#$txt = "[% IF 1 %]Two[% END %]";                       #  180%
#$txt = "[% FOREACH i = [0..10] %][% i %][% END %]";    #    8%
#$txt = "[%i=1 ; FOREACH i = [0..100] ; i ; END ; i%]"; #  -12%
#$txt = "[%i=1 ; FOREACH [0..100] ; i ; END ; i %]";    #   11%
#$txt = "[%f=10%][%WHILE f%][%f=f- 1%][%f%][% END %]";  #  -16%
#$txt = "[%f=10; WHILE f ; f = f - 1 ; f ; END %]";     #  -16%
#$txt = "[%f=10; WHILE (g=f) ; f = f - 1 ; f ; END %]"; #   -9%
#$txt = "[% BLOCK foo %]Hi[% END %][% PROCESS foo %]";  #  321%
#$txt = "[% BLOCK foo %]Hi[% END %][% INCLUDE foo %]";  #  288%

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
    $tt1->process($file, $swap, \$out) || debug $tt1->error;
    return $out;
}

sub file_TT_cache_new {
    my $out = '';
    my $t = Template->new(ABSOLUTE => 1, COMPILE_DIR => $tt_cache_dir, COMPILE_EXT => 'ttc');
    $t->process($file2, $swap, \$out);
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
    my $txt = $cet->swap($file, $swap);
    return $txt;
}

sub file_CET_cache_new {
    my $out = '';
    my $t = CGI::Ex::Template->new(ABSOLUTE => 1, COMPILE_DIR => $tt_cache_dir, COMPILE_EXT => 'ttc');
    $t->process($file2, $swap, \$out);
    return $out;
}

#sub str_CET_old {
#    my $out = '';
#    $cet2->process($file, $swap, \$out);
#    return $out;
#}

#debug file_CET(), str_TT();
#debug $cet->parse_tree($file);

### check out put - and also allow for caching
for (1..2) {
    if (file_CET() ne str_TT()) {
        debug $cet->parse_tree($file);
        debug file_CET(), str_TT();
        die "file_CET didn't match";
    }
    die "file_TT didn't match "            if file_TT()      ne str_TT();
    die "str_CET didn't match "            if str_CET()      ne str_TT();
    die "str_CET_swap didn't match "       if str_CET_swap() ne str_TT();
    die "file_CET_cache_new didn't match " if file_CET_cache_new() ne str_TT();
    die "file_TT_cache_new didn't match " if file_TT_cache_new() ne str_TT();
}

###----------------------------------------------------------------###

cmpthese timethese (-2, {
    file_TT_n   => \&file_TT_new,
    str_TT_n    => \&str_TT_new,
    file_TT     => \&file_TT,
    str_TT      => \&str_TT,
    file_TT_c_n => \&file_TT_cache_new,

    file_CT_n   => \&file_CET_new,
    str_CT_n    => \&str_CET_new,
    file_CT     => \&file_CET,
    str_CT      => \&str_CET,
    str_CT_sw   => \&str_CET_swap,
    file_CT_c_n => \&file_CET_cache_new,
#    str_CT_o    => \&str_CET_old,
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
