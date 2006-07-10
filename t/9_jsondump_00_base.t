# -*- Mode: Perl; -*-

=head1 NAME

9_jsondump_00_base.t - Testing of the CGI::Ex::JSONDump module.

=cut

use strict;
use Test::More tests => 1;

use_ok('CGI::Ex::JSONDump');

ok(eval { CGI::Ex::JSONDump->import('JSONDump'); 1 }, "Import JSONDump");

ok(&JSONDump, "Got the sub");

my $obj = CGI::Ex::JSONDump->new;

ok(JSONDump({a => 1}) eq $obj->dump({a => 1}), "Function and OO Match");

sub test_dump {
    my $data = shift;
    my $str  = shift;
    my $args = shift || {};
    my ($sub, $file, $line) = caller;

    my $out = JSONDump($data, $args);

    if ($out eq $str) {
        ok(1, "Dump matched at line $line");
    } else {
        ok(0, "Didn't match at line $line - shouldv'e been"
           ."\n---------------------\n"
           . $str
           ."\n---------------------\n"
           ."Was"
           ."\n---------------------\n"
           . $out
           ."\n---------------------\n"
           );
    }
}

###----------------------------------------------------------------###

test_dump({a => 1}, "{\n  \"a\" : 1\n}", {pretty => 1});
test_dump({a => 1}, "{\"a\":1}", {pretty => 0});
