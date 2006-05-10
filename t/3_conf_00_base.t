# -*- Mode: Perl; -*-

=head1 NAME

3_conf_00_base.t - Test for the basic functionality of CGI::Ex::Conf

=cut

use strict;
use Test::More tests => 24;

use_ok('CGI::Ex::Conf');

my $dir = __FILE__;
$dir =~ tr|\\|/|; # should probably use File::Spec
$dir =~ s|[^/]+$|../samples| || die "Couldn't determine dir";
$dir =~ s|^t/|./t/|; # to satisfy conf

my $obj = CGI::Ex::Conf->new({
  paths => ["$dir/conf_path_1", "$dir/conf_path_3"],
});

### most test for the reading of files
### are actually performed in the validation tests

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

$hash = $obj->read('apples.pl', {directive => 'MERGE'});
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


