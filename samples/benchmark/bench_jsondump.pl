#!/usr/bin/perl -w

# Benchmark: timing 1000 iterations of cgix_func, cgix_meth, hfif...
#  cgix_func:  1 wallclock secs ( 1.41 usr +  0.01 sys =  1.42 CPU) @ 704.23/s (n=1000)
#  cgix_meth:  2 wallclock secs ( 1.47 usr +  0.00 sys =  1.47 CPU) @ 680.27/s (n=1000)
#  hfif:  8 wallclock secs ( 8.34 usr +  0.04 sys =  8.38 CPU) @ 119.33/s (n=1000)
#            Rate      hfif cgix_meth cgix_func
# hfif      119/s        --      -82%      -83%
# cgix_meth 680/s      470%        --       -3%
# cgix_func 704/s      490%        4%        --

use strict;

use Benchmark qw(cmpthese timethese);
use JSON;
use CGI::Ex::JSONDump;

my $json = JSON->new(pretty => 0, keysort => 0);
my $cejd = CGI::Ex::JSONDump->new({pretty => 0, no_sort => 1});


my $data = {
    one   => 'two',
    three => [qw(a b c)],
    four  => 1,
    five  => '1.0',
    six   => undef,
};

print "JSON\n--------------------\n". $json->objToJson($data)."\n----------------------------\n";
print "CEJD\n--------------------\n". $cejd->dump($data)     ."\n----------------------------\n";

cmpthese timethese(-2, {
    json => sub { my $a = $json->objToJson($data) },
    cejd => sub { my $a = $cejd->dump($data) },
    zejd => sub { my $a = $cejd->dump($data) },
});

###----------------------------------------------------------------###

$json = JSON->new(pretty => 1, keysort => 1);
$cejd = CGI::Ex::JSONDump->new({pretty => 1});

$data = {
    one   => 'two',
    three => [qw(a b c)],
    four  => 1,
    five  => '1.0',
    six   => '12345678901234567890',
    seven => undef,
};

print "JSON\n--------------------\n". $json->objToJson($data)."\n----------------------------\n";
print "CEJD\n--------------------\n". $cejd->dump($data)     ."\n----------------------------\n";

cmpthese timethese(-2, {
    json => sub { my $a = $json->objToJson($data) },
    cejd => sub { my $a = $cejd->dump($data) },
});

###----------------------------------------------------------------###

$json = JSON->new(pretty => 1);
$cejd = CGI::Ex::JSONDump->new({pretty => 1});

$data = ["foo\n<script>\nThis is sort of \"odd\"\n</script>"];

print "JSON\n--------------------\n". $json->objToJson($data)."\n----------------------------\n";
print "CEJD\n--------------------\n". $cejd->dump($data)     ."\n----------------------------\n";

cmpthese timethese(-2, {
    json => sub { my $a = $json->objToJson($data) },
    cejd => sub { my $a = $cejd->dump($data) },
});
