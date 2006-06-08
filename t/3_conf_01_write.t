# -*- Mode: Perl; -*-

=head1 NAME

3_conf_01_write.t - Test CGI::Ex::Conf's ability to write and read the various file types.

=cut

use strict;
use Test::More tests => 18;

use_ok('CGI::Ex::Conf');

my $dir = __FILE__;
$dir =~ tr|\\|/|; # should probably use File::Spec
$dir =~ s|[^/]+$|../samples| || die "Couldn't determine dir";
$dir =~ s|^t/|./t/|; # to satisfy conf

my $obj = CGI::Ex::Conf->new({
  paths => ["$dir/conf_path_1", "$dir/conf_path_3"],
});

my $tmpfile = "$obj->{paths}->[0]/write_test";
### most test for the reading of files
### are actually performed in the validation tests

ok($obj);

my $hash = {
  one => 1,
  two => 2,
  three => {
    foo => 'Foo',
    bar => 'Bar',
  },
};
my $file;
my $in;

$file = $tmpfile .'.pl';
ok( eval { $obj->write_ref($file, $hash) } );
$in = $obj->read_ref($file);
ok($in->{'three'}->{'foo'} eq 'Foo');
unlink $file;

SKIP: {
    skip("YAML.pm not found", 2) if ! eval { require YAML };
    my $file = $tmpfile .'.yaml';
    ok( eval { $obj->write_ref($file, $hash) } );
    my $in = $obj->read_ref($file);
    ok($in->{'three'}->{'foo'} eq 'Foo');
    unlink $file;
};

SKIP: {
    skip("JSON.pm not found", 2) if ! eval { require JSON };
    my $file = $tmpfile .'.json';
    ok( eval { $obj->write_ref($file, $hash) } );
    my $in = $obj->read_ref($file);
    ok($in->{'three'}->{'foo'} eq 'Foo');
    unlink $file;
};

SKIP: {
    skip("Storable.pm not found", 2) if ! eval { require Storable };
    $file = $tmpfile .'.sto';
    ok( eval { $obj->write_ref($file, $hash) } );
    $in = $obj->read_ref($file);
    ok($in->{'three'}->{'foo'} eq 'Foo');
    unlink $file;
};

SKIP: {
    skip("XML::Simple not found", 2) if ! eval { require XML::Simple };
    $file = $tmpfile .'.xml';
    ok( eval { $obj->write_ref($file, $hash) } );
    $in = $obj->read_ref($file);
    ok($in->{'three'}->{'foo'} eq 'Foo');
    unlink $file;
};

SKIP: {
    skip("Config::IniHash not found", 2) if ! eval { require Config::IniHash };
    ### ini likes hash O' hashes
    $hash->{'one'} = {};
    $hash->{'two'} = {};
    $file = $tmpfile .'.ini';
    ok( eval { $obj->write_ref($file, $hash) } );
    $in = $obj->read_ref($file);
    ok($in->{'three'}->{'foo'} eq 'Foo');
    unlink $file;
};

SKIP: {
    skip('YAML.pm still not found', 4) if ! eval { require YAML };
    ok (eval { $obj->write('FooSpace', $hash) });
    ok (unlink $obj->{'paths'}->[1] . '/FooSpace.conf');

    ok (eval { $obj->write('FooSpace', $hash, {directive => 'FIRST'}) });
    ok (unlink $obj->{'paths'}->[0] . '/FooSpace.conf');
};
