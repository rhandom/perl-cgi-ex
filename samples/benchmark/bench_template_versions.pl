#!/usr/bin/perl -w

use strict;
use Benchmark qw(cmpthese timethese);
use POSIX qw(tmpnam);
use File::Path qw(rmtree);
use CGI::Ex::Template;
#use CGI::Ex::Template2;
use CGI::Ex::Template_60;
#use CGI::Ex::Template_65;
#use CGI::Ex::Template_70;
#use CGI::Ex::Template_75;
use CGI::Ex::Dump qw(debug);
use Template;

### This is with CGI::Ex::Template at CVS revision 1.106 which has most of TT's features
#   file_CET:  2 wallclock secs ( 2.07 usr +  0.00 sys =  2.07 CPU) @ 31.88/s (n=66)
#  file_CET_60:  2 wallclock secs ( 2.08 usr +  0.02 sys =  2.10 CPU) @ 33.33/s (n=70)
#  file_CET_n:  3 wallclock secs ( 2.18 usr +  0.01 sys =  2.19 CPU) @ 10.05/s (n=22)
#    file_TT:  3 wallclock secs ( 2.03 usr +  0.00 sys =  2.03 CPU) @ 33.99/s (n=69)
#    file_TT_n:  2 wallclock secs ( 2.10 usr +  0.01 sys =  2.11 CPU) @  1.90/s (n=4)
#                Rate   file_TT_n  file_CET_n    file_CET file_CET_60     file_TT
#  file_TT_n   1.90/s          --        -81%        -94%        -94%        -94%
#  file_CET_n  10.0/s        430%          --        -68%        -70%        -70%
#  file_CET    31.9/s       1582%        217%          --         -4%         -6%
#  file_CET_60 33.3/s       1658%        232%          5%          --         -2%
#  file_TT     34.0/s       1693%        238%          7%          2%          --

my $tt_cache_dir = tmpnam;
END { rmtree $tt_cache_dir };
mkdir $tt_cache_dir, 0755;

my $tt1 = Template->new(ABSOLUTE => 1);

my $cet = CGI::Ex::Template->new(ABSOLUTE => 1);
#my $cet_old = CGI::Ex::Template2->new(ABSOLUTE => 1);
my $cet_60 = CGI::Ex::Template_60->new(ABSOLUTE => 1);
#my $cet_65 = CGI::Ex::Template_65->new(ABSOLUTE => 1);
#my $cet_70 = CGI::Ex::Template_70->new(ABSOLUTE => 1);
#my $cet_75 = CGI::Ex::Template_75->new(ABSOLUTE => 1);

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
#my $txt   = "[% t = 1 || 0 ? 0 : 1 || 2 ? 2 : 3 %][% t %]";

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

sub file_TT {
    my $out = '';
    $tt1->process($file2, $swap, \$out);
    return $out;
}

###----------------------------------------------------------------###

sub file_CET_new {
    my $out = '';
    my $t = CGI::Ex::Template->new(ABSOLUTE => 1);
    $t->process($file2, $swap, \$out);
    return $out;
}

sub file_CET {
    my $out = '';
    $cet->process($file2, $swap, \$out);
    return $out;
}

#sub file_CET_old {
#    my $out = '';
#    $cet_old->process($file2, $swap, \$out);
#    return $out;
#}

sub file_CET_60 {
    my $out = '';
    $cet_60->process($file2, $swap, \$out);
    return $out;
}

#sub file_CET_65 {
#    my $out = '';
#    $cet_65->process($file2, $swap, \$out);
#    return $out;
#}
#
#sub file_CET_70 {
#    my $out = '';
#    $cet_70->process($file2, $swap, \$out);
#    return $out;
#}
#
#sub file_CET_75 {
#    my $out = '';
#    $cet_75->process($file2, $swap, \$out);
#    return $out;
#}

#debug file_CET(), file_CET(), str_TT();

### check out put - and also allow for caching
for (1..10) {
    die "file_CET didn't match"     if file_CET()     ne file_TT();
    die "file_CET_60 didn't match"  if file_CET_60()  ne file_TT();
}

###----------------------------------------------------------------###

cmpthese timethese (-2, {
    file_TT_n   => \&file_TT_new,
    file_TT     => \&file_TT,
    file_CET_n  => \&file_CET_new,
    file_CET    => \&file_CET,
#    file_CET_o  => \&file_CET_old,
    file_CET_60 => \&file_CET_60,
#    file_CET_65 => \&file_CET_65,
#    file_CET_70 => \&file_CET_70,
#    file_CET_75 => \&file_CET_75,
});

