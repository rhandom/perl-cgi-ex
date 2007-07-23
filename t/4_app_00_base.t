# -*- Mode: Perl; -*-

=head1 NAME

4_app_00_base.t - Check for the basic functionality of CGI::Ex::App.

=head1 NOTE

These tests are extremely stripped down to test the basic path flow.  Normally
unit tests are useful for garnering information about a module.  For CGI::Ex::App
it is suggested to stick to live use cases or the CGI::Ex::App perldoc - though
we do try to put it through most paces.

=cut

use Test::More tests => 25;
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

    sub auth_args { {login_template => \q{Login Form}, key_user => 'user', key_pass => 'pass', key_cookie => 'user', set_cookie => sub {}} }

    sub get_pass_by_user { '123qwe' }

    ###----------------------------------------------------------------###

    sub main_info_complete { 0 }

    sub main_file_print { return \ "Main Content" }

    sub main_path_info_map { shift->{'main_path_info_map'} }

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
###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
print "### Test some basic returns ###\n";

ok(! eval { CGI::Ex::App::new()  }, "Invalid new");
ok(! eval { CGI::Ex::App::new(0) }, "Invalid new");

my $app = CGI::Ex::App->new({script_name => '/cgi-bin/foo_bar'});
ok($app->script_name eq '/cgi-bin/foo_bar', "Can pass in script_name");
ok($app->name_module eq 'foo_bar', "Can pass in script_name");

$app = CGI::Ex::App->new({script_name => '/cgi-bin/foo_bar.pl'});
ok($app->script_name eq '/cgi-bin/foo_bar.pl', "Can pass in script_name");
ok($app->name_module eq 'foo_bar', "Can pass in script_name");

ok($app->morph_package('foo') eq 'CGI::Ex::App::Foo',        "Got a good morph_package");
ok($app->morph_package('foo_bar') eq 'CGI::Ex::App::FooBar', "Got a good morph_package");

ok(ref($app->path), "Got a good path");
ok(@{ $app->path } == 0, "Got a good path");
ok($app->default_step   eq 'main',        "Got a good default_step");
ok($app->login_step     eq '__login',     "Got a good login_step");
ok($app->error_step     eq '__error',     "Got a good error_step");
ok($app->forbidden_step eq '__forbidden', "Got a good forbidden_step");
ok($app->js_step        eq 'js',          "Got a good js_step");

###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
print "### Test basic step selection/form input/validation/filling/template swapping methods ###\n";

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = '';

Foo->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo");

{
    package Foo2;
    our @ISA = qw(Foo);
    sub form { {} }
}
Foo2->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo2");

###----------------------------------------------------------------###

{
    package Foo2_1;
    our @ISA = qw(Foo);
    sub pre_navigate { 1 }
}
Foo2_1->navigate;
ok($Foo::test_stdout eq "", "Got the right output for Foo2_1");

Foo2_1->new({_no_pre_navigate => 1})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo2_1");

{
    package Foo2_2;
    our @ISA = qw(Foo);
    sub pre_loop { 1 }
}
Foo2_2->navigate;
ok($Foo::test_stdout eq "", "Got the right output for Foo2_2");

{
    package Foo2_3;
    our @ISA = qw(Foo);
    sub post_loop { 1 }
}
Foo2_3->navigate;
ok($Foo::test_stdout eq "", "Got the right output for Foo2_3");

{
    package Foo2_4;
    our @ISA = qw(Foo);
    sub post_navigate { $Foo::test_stdout .= " post"; 1 }
}
Foo2_4->navigate;
ok($Foo::test_stdout eq "Main Content post", "Got the right output for Foo2_4");

Foo2_4->new({_no_post_navigate => 1})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo2_4");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = 'step=step2';

Foo->new({
    form => {step => 'step2'},
})->navigate;
ok($Foo::test_stdout eq "Some step2 content (bar, two) <input type=text name=wow value=\"wee\">wow is required", "Got the right output for Foo");

{
    package Foo3;
    our @ISA = qw(Foo);
    sub main_info_complete { 1 }
}
eval { Foo3->navigate };
ok($Foo::test_stdout =~ /recurse_limit \(15\)/, "Got the right output for Foo3");

eval { Foo3->new({recurse_limit => 10})->navigate };
ok($Foo::test_stdout =~ /recurse_limit \(10\)/, "Got the right output for Foo3");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = 'step=step2&wow=something';

Foo->new({
    form=> {step => 'step2', wow => 'something'},
})->navigate;
ok($Foo::test_stdout eq "All good", "Got the right output for Foo");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = 'step=step2&wow=something';

Foo->new({
    form=> {step => '_bling'},
})->navigate;
ok($Foo::test_stdout =~ /Denied/i, "Got the right output for Foo");

{
    package Foo4;
    our @ISA = qw(Foo);
    sub path { shift->{'path'} ||= ['3foo'] }
}
Foo4->new({form => {}})->navigate;
ok($Foo::test_stdout =~ /Denied/i, "Got the right output for Foo4");

###----------------------------------------------------------------###

#$ENV{'REQUEST_METHOD'} = 'GET';
#$ENV{'QUERY_STRING'}   = '';
local $ENV{'PATH_INFO'} = '/step2';

Foo->new({
    form=> {},
})->navigate;
ok($Foo::test_stdout eq "Some step2 content (bar, two) <input type=text name=wow value=\"wee\">wow is required", "Got the right output");

Foo->new({
    path_info_map_base => [],
})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo ($Foo::test_stdout)");

Foo->new({
    path_info_map_base => [[qr{(?!)}, 'foo']],
})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo ($Foo::test_stdout)");

eval { Foo->new({
    path_info_map_base => {},
})->navigate };
ok($Foo::test_stdout eq "", "Got the right output for Foo");

eval { Foo->new({
    path_info_map_base => [{}],
})->navigate };
ok($Foo::test_stdout eq "", "Got the right output for Foo");

{
    package Foo5;
    our @ISA = qw(Foo);
    sub path_info_map_base {}
}
Foo5->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo5");

local $ENV{'PATH_INFO'} = '/blah';

eval { Foo->new({
    path_info_map_base => [],
    main_path_info_map => {},
})->navigate };
ok($Foo::test_stdout =~ /fatal error.+path_info_map/, "Got the right output for Foo");

eval { Foo->new({
    path_info_map_base => [],
    main_path_info_map => [{}],
})->navigate };
ok($Foo::test_stdout =~ /fatal error.+path_info_map/, "Got the right output for Foo");

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

local $ENV{'PATH_INFO'} = '';

{
    package Foo6;
    our @ISA = qw(Foo);
    sub valid_steps { {step2 => 1} }
    sub js_run_step { $Foo::test_stdout = 'JS' }
}
Foo6->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo6");

Foo6->new({form => {step => 'main'}})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo6");

Foo6->new({form => {step => 'step3'}})->navigate;
ok($Foo::test_stdout =~ /denied/i, "Got the right output for Foo6");

Foo6->new({form => {step => 'step2'}})->navigate;
ok($Foo::test_stdout =~ /step2/i, "Got the right output for Foo6");

Foo6->new({form => {step => Foo6->new->js_step}})->navigate;
ok($Foo::test_stdout eq 'JS', "Got the right output for Foo6");



###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
print "### Test Authorization Methods ###\n";

local $ENV{'PATH_INFO'}   = '';
local $ENV{'SCRIPT_NAME'} = '';

Foo->new({
    form => {},
    require_auth => 1,
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output");

Foo->new({
    form => {},
    cookies => {user => 'foo/123qwe'},
    require_auth => 1,
})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo ($Foo::test_stdout)");

Foo->new({
    form => {},
    auth_data => {user => 'foo'},
    require_auth => 1,
})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo ($Foo::test_stdout)");

###----------------------------------------------------------------###

Foo->new({
    form => {},
})->navigate_authenticated;
ok($Foo::test_stdout eq "Login Form", "Got the right output");

###----------------------------------------------------------------###

{
    package Bar;
    our @ISA = qw(Foo);
    sub require_auth { 1 }
}

Bar->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output for Bar");

###----------------------------------------------------------------###

{
    package Bar1;
    our @ISA = qw(Foo);
    sub require_auth { 1 }
}

my $ok = eval { Bar1->new({
    form => {},
})->navigate_authenticated; 1 }; # can't call navigate_authenticated with overwritten require_auth
ok(! $ok, "Got the right output for Bar1");

###----------------------------------------------------------------###

{
    package Bar2;
    our @ISA = qw(Foo);
    sub main_require_auth { 1 }
}

Bar2->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output for Bar2");

###----------------------------------------------------------------###

{
    package Bar3;
    our @ISA = qw(Foo);
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
    our @ISA = qw(Foo);
    sub pre_navigate { shift->require_auth(0); 0 }
}

Bar4->new({
    form => {},
})->navigate_authenticated;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Bar4");

###----------------------------------------------------------------###

{
    package Bar5;
    our @ISA = qw(Foo);
    sub pre_navigate { shift->require_auth(1); 0 }
}

Bar5->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output for Bar5 ($@)");

###----------------------------------------------------------------###

{
    package Bar6;
    our @ISA = qw(Foo);
    sub pre_navigate { shift->require_auth({main => 1}); 0 }
}

Bar6->new({
    form => {},
})->navigate;
ok($Foo::test_stdout eq "Login Form", "Got the right output for Bar6 ($@)");

###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
print "### Test Configuration methods ###\n";

{
    package Conf1;
    our @ISA = qw(Foo);
    sub name_module { 'conf_1' }
}

my $file = Conf1->new->conf_file;
ok($file && $file eq 'conf_1.pl', "Got a conf_file ($file)");

$file = Conf1->new({ext_conf => 'ini'})->conf_file;
ok($file && $file eq 'conf_1.ini', "Got a conf_file ($file)");

eval { Conf1->new({
    load_conf => 1,
})->navigate };
my $err = $@;
ok($err, "Got an error");
chomp $err;
ok($Foo::test_stdout eq "", "Got the right output for Conf1 ($err)");

Conf1->new({
    load_conf => 1,
    conf => {
        form => {step => 'step3'},
    },
})->navigate;
ok($Foo::test_stdout eq "All good", "Got the right output for Conf1");

###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
print "### Various other coverage tests\n";

{
    package Foo7;
    our @ISA = qw(Foo);
    sub hash_base {}
    sub hash_common {}
    sub hash_form {}
    sub hash_fill {}
    sub hash_swap {}
    sub hash_errors {}
    sub find_hook { my ($self, $hook, $step) = @_; return $self->SUPER::find_hook($hook, $step) if $step eq 'main'; return ["non_code",1] }
}
Foo7->new({no_history => 1})->navigate;
ok($Foo::test_stdout eq "Main Content", "Got the right output for Foo7 ($Foo::test_stdout)");

ok(  eval {  Foo->new->run_hook('hash_base', 'main') }, "Can run_hook main hash_base on Foo");
ok(! eval {  Foo->new->run_hook('bogus',     'main') }, "Can't run_hook main bogus on Foo ($@)");
ok(! eval { Foo7->new->run_hook('hash_base', 'bogus') }, "Can't run_hook bogus hash_base on Foo7 for other reasons ($@)");

###----------------------------------------------------------------###
