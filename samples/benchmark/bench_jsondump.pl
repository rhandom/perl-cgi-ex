#!/usr/bin/perl -w

# Benchmark: running cejd, json, zejd for at least 2 CPU seconds...
#       cejd:  4 wallclock secs ( 2.18 usr +  0.00 sys =  2.18 CPU) @ 7045.87/s (n=15360)
#       json:  3 wallclock secs ( 2.16 usr +  0.00 sys =  2.16 CPU) @ 6634.26/s (n=14330)
#       zejd:  3 wallclock secs ( 2.16 usr +  0.00 sys =  2.16 CPU) @ 6634.26/s (n=14330)
#        Rate zejd json cejd
# zejd 6634/s   --   0%  -6%
# json 6634/s   0%   --  -6%
# cejd 7046/s   6%   6%   --
#
# Benchmark: running cejd, json for at least 2 CPU seconds...
#       cejd:  3 wallclock secs ( 2.04 usr +  0.00 sys =  2.04 CPU) @ 5690.20/s (n=11608)
#       json:  2 wallclock secs ( 2.06 usr +  0.00 sys =  2.06 CPU) @ 5291.75/s (n=10901)
#        Rate json cejd
# json 5292/s   --  -7%
# cejd 5690/s   8%   --
#
# Benchmark: running cejd, json for at least 2 CPU seconds...
#       cejd:  4 wallclock secs ( 2.21 usr +  0.00 sys =  2.21 CPU) @ 24320.81/s (n=53749)
#       json:  3 wallclock secs ( 2.14 usr +  0.00 sys =  2.14 CPU) @ 10048.13/s (n=21503)
#         Rate json cejd
# json 10048/s   -- -59%
# cejd 24321/s 142%   --

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
