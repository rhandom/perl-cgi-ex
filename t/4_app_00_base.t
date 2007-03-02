# -*- Mode: Perl; -*-

=head1 NAME

4_app_00_base.t - Check for the basic functionality of CGI::Ex::App.

=head1 NOTE

These tests are extremely stripped down to test the basic path flow.  Normally
unit tests are useful for garnering information about a module.  For CGI::Ex::App
it is suggested to stick to live use cases or the CGI::Ex::App perldoc.

=cut

use Test::More tests => 9;
use strict;

{
  package Foo;

  use base qw(CGI::Ex::App);
  use vars qw($test_stdout);

  sub ready_validate { 1 }

  sub print_out {
    my $self = shift;
    my $step = shift;
    $test_stdout = shift;
  }

  sub swap_template {
    my ($self, $step, $file, $swap) = @_;
    my $out = ref($file) ? $$file : "No filenames allowed during test mode";
    $self->cgix->swap_template(\$out, $swap);
    return $out;
  }

  ###----------------------------------------------------------------###

  sub main_info_complete { 0 }

  sub main_file_print { return \ "Main Content" }

  sub step2_hash_validation { return {wow => {required => 1, required_error => 'wow is required'}} }

  sub step2_path_info_map { [[qr{^/step2/(\w+)$}, 'wow']] }

  sub step2_file_print { return \ "Some step2 content ([% foo %], [% one %]) <input type=text name=wow>[% wow_error %]" }

  sub step2_hash_swap { return {foo => 'bar', one => 'two'} }

  sub step2_hash_fill { return {wow => 'wee'} }

  sub step2_finalize { shift->append_path('step3') }

  sub step3_info_complete { 0 }

  sub step3_file_print { return \ "All good" }


}

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = '';

Foo->new({
  form => {},
})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = 'step=step2';

Foo->new({
  form => {step => 'step2'},
})->navigate;
ok($Foo::test_stdout eq "Some step2 content (bar, two) <input type=text name=wow value=\"wee\">wow is required", "Got the right output");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = 'step=step2&wow=something';

Foo->new({
  form=> {step => 'step2', wow => 'something'},
})->navigate;
ok($Foo::test_stdout eq "All good", "Got the right output");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = '';
local $ENV{'PATH_INFO'} = '/step2';

Foo->new({
  form=> {},
})->navigate;
ok($Foo::test_stdout eq "Some step2 content (bar, two) <input type=text name=wow value=\"wee\">wow is required", "Got the right output");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = 'wow=something';
local $ENV{'PATH_INFO'} = '/step2';

my $f = Foo->new({
  form=> {wow => 'something'},
})->navigate;
ok($Foo::test_stdout eq "All good", "Got the right output");
ok($f->form->{'step'} eq 'step2', "Got the right variable set in form");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = '';
local $ENV{'PATH_INFO'} = '/step2/something';

$f = Foo->new({
  form => {},
})->navigate;
ok($Foo::test_stdout eq "All good", "Got the right output");
ok($f->form->{'step'} eq 'step2',     "Got the right variable set in form");
ok($f->form->{'wow'}  eq 'something', "Got the right variable set in form");

###----------------------------------------------------------------###
