#!perl -T
# -*- Mode: Perl; -*-

use strict;

$^W = 1;

### Set up taint checking
sub is_tainted { local $^W = 0; ! eval { eval("#" . substr(join("", @_), 0, 0)); 1 } }

my $taint = join(",", $0, %ENV, @ARGV);
if (! is_tainted($taint) && open(my $fh, "/dev/urandom")) {
  sysread($fh, $taint, 1);
}
$taint = substr($taint, 0, 0);
if (! is_tainted($taint)) {
  print "1..1\nok 1 # skip Couldn't get any tainted data or we aren't in taint mode\n";
  exit;
}

### make sure tainted hash values don't bleed into other values
my $form = {};
$form->{'foo'} = "123$taint";
$form->{'bar'} = "456$taint";
$form->{'baz'} = "789";
if (!  is_tainted($form->{'foo'})
    || is_tainted($form->{'baz'})) {
  # untaint checking doesn't really work
  print "1..1\nok 1 # skip Hashes with mixed taint don't work right (older perls ?)\n";
  exit;
}

###----------------------------------------------------------------###
### Looks good - here we go

### determine number of tests
seek(DATA,0,0);
my $prog  = join "", <DATA>;
my @tests = ($prog =~ /print_ok\(/g);
my $tests = @tests;
print "1..$tests\n";

require CGI::Ex::Validate;

my ($N, $v, $e, $ok) = (0);


print_ok(is_tainted($taint));
print_ok(is_tainted($form->{'foo'}));
print_ok(! is_tainted($form->{'baz'}));
print_ok(! is_tainted($form->{'non_existent_key'}));

sub validate {
  return scalar &CGI::Ex::Validate::validate(@_);
}
sub print_ok {
  my $ok = shift;
  $N ++;
  warn "Test failed at line ".(caller)[2]."\n" if ! $ok;
  print "" . ($ok ? "" : "not ") . "ok $N\n";
}
&print_ok(1);

###----------------------------------------------------------------###

$e = &validate($form, {
  foo => {
    match   => 'm/^\d+$/',
    untaint => 1,
  },
});

print_ok(! $e);
print_ok(! is_tainted($form->{foo}));

###----------------------------------------------------------------###

$e = &validate($form, {
  bar => {
    match   => 'm/^\d+$/',
  },
});

print_ok(! $e);
print_ok(is_tainted($form->{bar}));

###----------------------------------------------------------------###

$e = &validate($form, {
  bar => {
    untaint => 1,
  },
});

print_ok($e);
#print $e if $e;
print_ok(is_tainted($form->{bar}));

###----------------------------------------------------------------###

print_ok(!is_tainted($form->{foo}));
print_ok( is_tainted($form->{bar}));
print_ok(!is_tainted($form->{baz}));

__DATA__
