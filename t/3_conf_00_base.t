# -*- Mode: Perl; -*-

use Test;

BEGIN {plan tests => 24};

use CGI::Ex::Conf;
ok(1);

my $dir = $0;
$dir =~ s|/[^/]+$||;
$dir = '.' if ! length $dir;
$dir .= '/samples';
my $obj = CGI::Ex::Conf->new({
  paths => ["$dir/conf_path_1", "$dir/conf_path_3"],
});

ok($obj);

my $hash = $obj->read('apples.pl');
ok($hash);
ok($hash->{quantity});

$hash = $obj->read('apples.pl');
ok($hash);
ok($hash->{quantity});


local $CGI::Ex::Conf::DIRECTIVE = 'FIRST';
$hash = $obj->read('apples.pl');
ok($hash);
ok($hash->{quantity} == 20);
ok($hash->{foo} eq 'file1');

local $CGI::Ex::Conf::DIRECTIVE = 'LAST';
$hash = $obj->read('apples.pl');
ok($hash);
ok($hash->{quantity} == 30);
ok($hash->{foo} eq 'file2');

local $CGI::Ex::Conf::DIRECTIVE = 'MERGE';
$hash = $obj->read('apples.pl');
ok($hash);
ok($hash->{quantity} == 30);
ok($hash->{foo} eq 'file1'); # has immutable value


local $obj->{directive} = 'FIRST';
$hash = $obj->read('oranges.pl');
ok($hash);
ok($hash->{quantity} == 20);
ok($hash->{foo} eq 'file1');

local $obj->{directive} = 'LAST';
$hash = $obj->read('oranges.pl');
ok($hash);
ok($hash->{quantity} == 30);
ok($hash->{foo} eq 'file2');

local $obj->{directive} = 'MERGE';
$hash = $obj->read('oranges.pl');
ok($hash);
ok($hash->{quantity} == 20); # has immutable key so all values are immutable
ok($hash->{foo} eq 'file1'); # has immutable key so all values are immutable


