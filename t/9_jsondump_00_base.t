# -*- Mode: Perl; -*-

=head1 NAME

9_jsondump_00_base.t - Testing of the CGI::Ex::JSONDump module.

=cut

use strict;
use Test::More tests => 37;

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

test_dump([1, 2, 3], "[\n  1,\n  2,\n  3\n]", {pretty => 1});
test_dump([1, 2, 3], "[1,2,3]", {pretty => 0});

test_dump({a => sub {}}, "{}", {pretty => 0});
test_dump({a => sub {}}, "{\"a\":\"CODE\"}", {handle_unknown_types => sub {my $self=shift;return $self->js_escape(ref shift)}, pretty => 0});

test_dump({a => 1}, "{}", {skip_keys => ['a']});
test_dump({a => 1}, "{}", {skip_keys => {a=>1}});

test_dump({2 => 1, _a => 1}, "{2:1,\"_a\":1}", {pretty=>0});
test_dump({2 => 1, _a => 1}, "{2:1}", {pretty=>0, skip_key_qr => qr/^_/});

test_dump({a => 1}, "{\n  \"a\" : 1\n}", {pretty => 1});
test_dump({a => 1}, "{\n  \"a\" : 1\n}", {pretty => 1, hash_nl => "\n", hash_sep => " : ", indent => "  "});
test_dump({a => 1}, "{\n\"a\" : 1\n}", {pretty => 1, hash_nl => "\n", hash_sep => " : ", indent => ""});
test_dump({a => 1}, "{\"a\" : 1}", {pretty => 1, hash_nl => "", hash_sep => " : ", indent => ""});
test_dump({a => 1}, "{\"a\":1}", {pretty => 1, hash_nl => "", hash_sep => ":", indent => ""});
test_dump({a => 1}, "{\"a\":1}", {pretty => 0, hash_nl => "\n", hash_sep => " : "});

test_dump(['a' => 1], "[\n  \"a\",\n  1\n]", {pretty => 1});
test_dump(['a' => 1], "[\n  \"a\",\n  1\n]", {pretty => 1, array_nl => "\n", indent => "  "});
test_dump(['a' => 1], "[\n\"a\",\n1\n]", {pretty => 1, array_nl => "\n", indent => ""});
test_dump(['a' => 1], "[\"a\",1]", {pretty => 1, array_nl => "", indent => ""});
test_dump(['a' => 1], "[\"a\",1]", {pretty => 0, array_nl => "\n"});



test_dump(1, "1");
test_dump('1.0', '"1.0"');
test_dump('123456789012345', '"123456789012345"');
test_dump('a', '"a"');
test_dump("\n", '"\\n"');
test_dump("\\", '"\\\\"');
test_dump('<script>', '"<scrip"+"t>"');
test_dump('<html>', '"<htm"+"l>"');
test_dump('<!--', '"<!-"+"-"');
test_dump('"', '"\\""');

test_dump("Foo\n".("Bar"x30), "\"Foo\\n\"\n  +\"".("Bar"x30)."\"", {pretty => 1});
test_dump("Foo\n".("Bar"x30), "\"Foo\\n\"\n\n  +\"".("Bar"x30)."\"", {pretty => 1, str_nl => "\n\n"});
