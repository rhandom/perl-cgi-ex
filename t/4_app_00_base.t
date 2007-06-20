# -*- Mode: Perl; -*-

=head1 NAME

4_app_00_base.t - Check for the basic functionality of CGI::Ex::App.

=head1 NOTE

These tests are extremely stripped down to test the basic path flow.  Normally
unit tests are useful for garnering information about a module.  For CGI::Ex::App
it is suggested to stick to live use cases or the CGI::Ex::App perldoc - though
we do try to put it through most paces.

=cut

use Test::More tests => 20;
use strict;

{
    package Foo;

    use base qw(CGI::Ex::App);
    use vars qw($test_stdout);

    sub init { $test_stdout = '' }

    sub ready_validate { 1 }

    sub print_out {
        my $self = shift;
        my $step = shift;
        my $str  = shift;
        $test_stdout = ref($str) ? $$str : $str;
    }

    sub swap_template {
        my ($self, $step, $file, $swap) = @_;
        my $out = ref($file) ? $$file : "No filenames allowed during test mode";
        $self->cgix->swap_template(\$out, $swap);
        return $out;
    }

    sub auth_args { {login_template => \q{Login Form}} }

    ###----------------------------------------------------------------###

    sub main_info_complete { 0 }

    sub main_file_print { return \ "Main Content" }

    sub step2_hash_validation { return {wow => {required => 1, required_error => 'wow is required'}} }

    sub step2_path_info_map { [[qr{^/step2/(\w+)$}x, 'wow']] }

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

local $ENV{'PATH_INFO'}   = '';
local $ENV{'SCRIPT_NAME'} = '';

Foo->new({
    form => {},
    require_auth => 1,
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output");

###----------------------------------------------------------------###

Foo->new({
    form => {},
})->navigate_authenticated;
ok($Foo::test_stdout eq "Login Form", "Got the right output");

###----------------------------------------------------------------###

{
    package Bar;
    @Bar::ISA = qw(Foo);
    sub require_auth { 1 }
}

Bar->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output for Bar");

###----------------------------------------------------------------###

{
    package Bar1;
    @Bar1::ISA = qw(Foo);
    sub require_auth { 1 }
}

my $ok = eval { Bar1->new({
    form => {},
})->navigate_authenticated; 1 }; # can't call navigate_authenticated with overwritten require_auth
ok(! $ok, "Got the right output for Bar1");

###----------------------------------------------------------------###

{
    package Bar2;
    @Bar2::ISA = qw(Foo);
    sub main_require_auth { 1 }
}

Bar2->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output for Bar2");

###----------------------------------------------------------------###

{
    package Bar3;
    @Bar3::ISA = qw(Foo);
    sub require_auth { 1 }
    sub main_require_auth { 0 }
}

Bar3->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Bar3");

###----------------------------------------------------------------###

Foo->new({
    form => {},
    require_auth => {main => 0},
})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output");

###----------------------------------------------------------------###

Foo->new({
    form => {},
    require_auth => {main => 1},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output");

###----------------------------------------------------------------###

{
    package Bar4;
    @Bar4::ISA = qw(Foo);
    sub pre_navigate { shift->require_auth(0); 0 }
}

Bar4->new({
    form => {},
})->navigate_authenticated;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Bar4");

###----------------------------------------------------------------###

{
    package Bar5;
    @Bar5::ISA = qw(Foo);
    sub pre_navigate { shift->require_auth(1); 0 }
}

Bar5->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output for Bar5 ($@)");

###----------------------------------------------------------------###

{
    package Bar6;
    @Bar6::ISA = qw(Foo);
    sub pre_navigate { shift->require_auth({main => 1}); 0 }
}

Bar6->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output for Bar6 ($@)");
