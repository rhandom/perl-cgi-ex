# -*- Mode: Perl; -*-

=head1 NAME

4_app_00_base.t - Check for the basic functionality of CGI::Ex::App.

=head1 NOTE

These tests are extremely stripped down to test the basic path flow.  Normally
unit tests are useful for garnering information about a module.  For CGI::Ex::App
it is suggested to stick to live use cases or the CGI::Ex::App perldoc - though
we do try to put it through most paces.

=cut

use Test::More tests => 214;
use strict;
use warnings;

{
    package Foo;

    use base qw(CGI::Ex::App);
    use vars qw($test_stdout);

    sub init { $test_stdout = '' }

    sub print_out {
        my $self = shift;
        my $step = shift;
        my $str  = shift;
        $test_stdout = ref($str) ? $$str : $str;
    }

    sub swap_template {
        my ($self, $step, $file, $swap) = @_;
        die "No filenames allowed during test mode" if ! ref($file);
        return $self->SUPER::swap_template($step, $file, $swap);
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

    sub step4_file_val { return {wow => {required => 1, required_error => 'wow is required'}} }

    sub step4_path_info_map { [[qr{^/step4/(\w+)$}x, 'wow']] }

    sub step4_file_print { return \ "Some step4 content ([% foo %], [% one %]) <form><input type=text name=wow>[% wow_error %]</form>[% js_validation %]" }

    sub step4_hash_swap { return {foo => 'bar', one => 'two'} }

    sub step4_hash_fill { return {wow => 'wee'} }

    sub step4_finalize { shift->append_path('step3') }

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

ok(Foo->new(name_module => 'foo')->name_module eq 'foo', "Got the name_module");
ok(! eval { Foo->new(script_name => '%####$')->name_module } && $@, "Bad script_name");
ok(! eval { Foo->new(script_name => '%####$')->name_module('foo') } && $@, "Bad script_name");

ok(! eval { $app->morph_package } && $@,                     "Can't get a good morph_package");
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

local $ENV{'REQUEST_METHOD'} = 'POST';
#$ENV{'QUERY_STRING'}   = 'step=step2';

Foo->new({
    form => {step => 'step2'},
})->navigate;
ok($Foo::test_stdout eq "Some step2 content (bar, two) <input type=text name=wow value=\"wee\">wow is required", "Got the right output for Foo");

Foo->new({
    form => {step => 'step4'},
})->navigate;
ok($Foo::test_stdout =~ /Some step4 content.*wow is required.*<script>/s, "Got the right output for Foo (step4)");

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
local $ENV{'SCRIPT_NAME'} = '/foo';

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

ok(Foo->new({
    form => {},
    cookies => {user => 'foo/123qwe'},
})->check_valid_auth, "Ran check_valid_auth");

my $cva = Foo->new({form => {}, cookies => {user => 'foo/123qwe'}});
ok($cva->check_valid_auth && $cva->check_valid_auth, "Can run twice");



ok(! Foo->new({
    form => {},
})->check_valid_auth, "Ran check_valid_auth");

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
    sub name_module { my $self = shift; defined($self->{'name_module'}) ? $self->{'name_module'} : 'conf_1' }
}

my $file = Conf1->new->conf_file;
ok($file && $file eq 'conf_1.pl', "Got a conf_file ($file)");

ok(! eval { Conf1->new(name_module => '')->conf_file } && $@, "Couldn't get conf_file");

$file = Conf1->new({ext_conf => 'ini'})->conf_file;
ok($file && $file eq 'conf_1.ini', "Got a conf_file ($file)");

eval { Conf1->new({
    load_conf => 1,
})->navigate };
my $err = $@;
ok($err, "Got an error");
chomp $err;
ok($Foo::test_stdout eq "", "Got the right output for Conf1");

Conf1->new({
    load_conf => 1,
    conf => {
        form => {step => 'step3'},
    },
})->navigate;
ok($Foo::test_stdout eq "All good", "Got the right output for Conf1");

Conf1->new({
    load_conf => 1,
    conf_file => {form => {step => 'step3'}},
})->navigate;
ok($Foo::test_stdout eq "All good", "Got the right output for Conf1");

Conf1->new({
    load_conf => 1,
    conf_file => {form => {step => 'step3'}},
    conf_validation => {form => {required => 1}},
})->navigate;
ok($Foo::test_stdout eq "All good", "Got the right output for Conf1");

eval { Conf1->new({
    load_conf => 1,
    conf_file => {},
    conf_validation => {form => {required => 1}},
})->navigate };
ok($Foo::test_stdout eq "" && $@, "Got a conf_validation error");

###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
###----------------------------------------------------------------###
print "### Various other coverage tests\n";

ok(Conf1->new->conf_obj, "Got a conf_obj");
ok(Conf1->new(conf_args => {paths => './', directive => 'merge'})->conf_obj, "Got a conf_obj");
ok(Conf1->new->val_obj, "Got a val_obj");
ok(Conf1->new(val_args => {cgix => Conf1->new->cgix})->val_obj, "Got a val_obj");
ok(Conf1->new->load_conf(1), "Ran load_conf");

ok(Foo2->navigate->clear_app, "clear_app works");

my $dh = Foo2->navigate;
push @{ $dh->history }, "A string", ['A non ref'], {key => 'No elapsed key'};
push @{ $dh->history }, {step => 'foo', meth => 'bar', found => 'bar', elapsed => 2, response => {}};
push @{ $dh->history }, {step => 'foo', meth => 'bar', found => 'bar', elapsed => 2, response => {hi => 'there'}};
push @{ $dh->history }, {step => 'foo', meth => 'bar', found => 'bar', elapsed => 1, response => []};
push @{ $dh->history }, {step => 'foo', meth => 'bar', found => 'bar', elapsed => 1, response => ['hi']};
push @{ $dh->history }, {step => 'foo', meth => 'bar', found => 'bar', elapsed => 1, response => 'a'};
push @{ $dh->history }, {step => 'foo', meth => 'bar', found => 'bar', elapsed => 1, response => 'a'x100};
ok($dh->dump_history, "Can call dump_history");
ok($dh->dump_history('all'), "Can call dump_history");
$dh->{'history_max'} = 10;
ok($dh->dump_history('all'), "Can call dump_history");

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
ok(! eval {  Foo->new->run_hook('bogus',     'main') }, "Can't run_hook main bogus on Foo");
ok(! eval { Foo7->new->run_hook('hash_base', 'bogus') }, "Can't run_hook bogus hash_base on Foo7 for other reasons");

foreach my $meth (qw(auth_args conf_args template_args val_args)) {
    ok(! CGI::Ex::App->new->$meth, "Got a good $meth");
    ok(CGI::Ex::App->new($meth => {a=>'A'})->$meth->{'a'} eq 'A', "Got a good $meth");
}

### test read only
foreach my $meth (qw(charset
                     conf_die_on_fail
                     conf_obj
                     conf_path
                     conf_validation
                     default_step
                     error_step
                     forbidden_step
                     js_step
                     login_step
                     mimetype
                     path_info
                     path_info_map_base
                     script_name
                     step_key
                     template_obj
                     template_path
                     val_obj
                     val_path
                     )) {
    ok(CGI::Ex::App->new($meth => 'blah')->$meth eq 'blah', "I can set $meth");
}

### test read/write
foreach my $meth (qw(base_dir_abs
                     base_dir_rel
                     cgix
                     conf
                     conf_file
                     cookies
                     ext_conf
                     ext_print
                     ext_val
                     form
                     )) {
    ok(CGI::Ex::App->new($meth => 'blah')->$meth eq 'blah', "I can set $meth");
    my $c = CGI::Ex::App->new;
    $c->$meth('blah');
    ok($c->$meth eq 'blah', "I can set $meth");
}

foreach my $type (qw(base
                     common
                     errors
                     fill
                     form
                     swap
                     )) {
    my $meth = "hash_$type";
    ok(CGI::Ex::App->new("hash_$type" => {bing => 'bang'})->$meth->{'bing'} eq 'bang', "Can initialize $meth")
        if $type ne 'form';

    my $meth2 = "add_to_$type";
    my $c = CGI::Ex::App->new;
    $c->$meth2({bing => 'bang'});
    $c->$meth2(bong => 'beng');

    if ($type eq 'errors') {
        $c->$meth2({bing => "wow"});
        ok($c->$meth->{"bing_error"} eq "bang<br>wow", "$meth2 works");
        ok($c->$meth->{"bong_error"} eq 'beng', "$meth2 works");

        ok($c->has_errors, "has_errors works") if $type eq 'errors';
    } else {
        ok($c->$meth->{'bing'} eq 'bang', "$meth2 works");
        ok($c->$meth->{'bong'} eq 'beng', "$meth2 works");
    }
}

ok(! eval { CGI::Ex::App->new->get_pass_by_user } && $@, "Got a good error for get_pass_by_user");
ok(! eval { CGI::Ex::App->new->find_hook } && $@, "Got a good error for find_hook");

###----------------------------------------------------------------###
print "### Some morph tests ###\n";

{
    package Foo8;
    our @ISA = qw(Foo);

    sub blah1_pre_step { $Foo::test_stdout = 'blah1_pre'; 1 }
    sub blah2_skip { 1 }
    sub blah3_info_complete { 1 }
    sub blah3_post_step { $Foo::test_stdout = 'blah3_post'; 1 }

    sub blah4_prepare { 0 }
    sub blah4_file_print { \ 'blah4_file_print' }

    sub blah5_finalize { 0 }
    sub blah5_info_complete { 1 }
    sub blah5_file_print { \ 'blah5_file_print' }

    sub blah8_morph_package { 'Foo8' }
    sub blah8_info_complete { 0 }
    sub blah8_file_print { \ 'blah8_file_print' }

    sub blah6_allow_morph { 1 }
    package Foo8::Blah6;
    our @ISA = qw(Foo8);
    sub info_complete { 0 }
    sub file_print { \ 'blah6_file_print' }
    sub early_exit_run_step { $Foo::test_stdout = 'early'; shift->exit_nav_loop }

    sub blah7_allow_morph { 1 }
    package Foo8::Blah6::Blah7;
    our @ISA = qw(Foo8::Blah6);
    sub info_complete { 0 }
    sub file_print { \ 'blah7_file_print' }
}

Foo8->new({form => {step => 'blah1'}})->navigate;
ok($Foo::test_stdout eq 'blah1_pre', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah1'}, allow_morph => 1})->navigate;
ok($Foo::test_stdout eq 'blah1_pre', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah2'}})->navigate;
ok($Foo::test_stdout eq 'Main Content', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah3'}})->navigate;
ok($Foo::test_stdout eq 'blah3_post', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah4'}})->navigate;
ok($Foo::test_stdout eq 'blah4_file_print', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah5'}})->navigate;
ok($Foo::test_stdout eq 'blah5_file_print', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah5'}, allow_morph => 1})->navigate;
ok($Foo::test_stdout eq 'blah5_file_print', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah5'}, allow_morph => 0})->navigate;
ok($Foo::test_stdout eq 'blah5_file_print', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah5'}, allow_morph => {}})->navigate;
ok($Foo::test_stdout eq 'blah5_file_print', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah5'}, allow_morph => {blah5 => 1}})->navigate;
ok($Foo::test_stdout eq 'blah5_file_print', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah6'}})->navigate;
ok($Foo::test_stdout eq 'blah6_file_print', "Got the right output for Foo8");

Foo8->new({form => {step => 'blah8'}, allow_morph => 1})->navigate;
ok($Foo::test_stdout eq 'blah8_file_print', "Got the right output for Foo8 ($Foo::test_stdout)");

my $foo8 = Foo8->new({form => {step => 'blah7'}, allow_nested_morph => 1});
$foo8->morph('blah6');
$foo8->navigate;
ok($Foo::test_stdout eq 'blah7_file_print', "Got the right output for Foo8");

$foo8 = Foo8->new({form => {step => 'blah7'}, allow_nested_morph => {blah7 => 1}});
$foo8->morph('blah6');
$foo8->navigate;
ok($Foo::test_stdout eq 'blah7_file_print', "Got the right output for Foo8");

$foo8 = Foo8->new({form => {step => 'blah7'}, allow_nested_morph => {blah9 => 1}});
$foo8->morph('blah6');
$foo8->navigate;
ok($Foo::test_stdout eq 'blah6_file_print', "Got the right output for Foo8");

$foo8 = Foo8->new({form => {step => 'blah7'}, allow_nested_morph => 0});
$foo8->morph('blah6');
$foo8->navigate;
ok($Foo::test_stdout eq 'blah6_file_print', "Got the right output for Foo8");

$foo8 = Foo8->new({form => {step => 'early_exit'}, no_history => 1});
$foo8->morph('blah6');
$foo8->navigate;
ok($Foo::test_stdout eq 'early', "Got the right output for Foo8");
ok(ref($foo8) eq 'Foo8::Blah6', 'Still is unmorphed right');

$foo8 = Foo8->new;
$foo8->morph;
ok(ref($foo8) eq 'Foo8', 'Got the right class');
$foo8->morph('blah6');
eval { $foo8->exit_nav_loop }; # coverage
ok($@, "Got the die from exit_nav_loop");

###----------------------------------------------------------------###
print "### Some path tests tests ###\n";

{
    package Foo9;
    our @ISA = qw(Foo);
    sub file_print {
        my $self = shift;
        my $str = "First(".$self->first_step.") Previous(".$self->previous_step.") Current(".$self->current_step.") Next(".$self->next_step.") Last(".$self->last_step.")";
        return \$str;
    }
    sub one_skip { 1 }
    sub two_skip { 1 }
    sub info_complete { 0 }
    sub invalid_run_step { shift->jump('::') }
}
ok(Foo9->new->previous_step eq '', 'No previous step if not navigating');

my $c = Foo9->new(form => {step => 'one'});
$c->add_to_path('three', 'four', 'five');
$c->insert_path('one', 'two');
$c->navigate;
ok($Foo::test_stdout eq 'First(one) Previous(two) Current(three) Next(four) Last(five)', "Got the right content for Foo9");
ok(! eval { $c->set_path("more") }, "Can't call set_path after nav started");

$c = Foo9->new(form => {step => 'five'});
$c->set_path('one', 'two', 'three', 'four', 'five');
$c->navigate;
ok($Foo::test_stdout eq 'First(one) Previous(two) Current(three) Next(four) Last(five)', "Got the right content for Foo9");

$c = Foo9->new;
$c->append_path('one');
eval { $c->jump('FIRST') };
ok($Foo::test_stdout eq '', "Can't jump without nav_loop");

eval { Foo9->new(form => {step => 'invalid'})->navigate };
ok($Foo::test_stdout =~ /fatal.*invalid jump index/si, "Can't jump with invalid step");

###----------------------------------------------------------------###

{
    package Foo10;
    our @ISA = qw(Foo);

    sub join_path {
        my $self = shift;
        my $s = join "", @{ $self->path };
        substr($s, $self->{'path_i'}, 0, '(');
        substr($s, $self->{'path_i'} + 2, 0, ')');
        return $s;
    }

    #sub run_hook {
    #    my ($self, $hook, $step) = @_;
    #    print "Into $step: ".$self->join_path."\n" if $hook eq 'run_step';
    #    return $self->SUPER::run_hook($hook, $step);
    #}

    sub a_run_step {
        my $self = shift;
        if ($self->join_path eq '(a)') {
            $self->append_path('b', 'c', 'd', 'e');
            $self->jump('CURRENT');
        } elsif ($self->join_path eq 'a(a)bcde') {
            $self->jump('NEXT');
        } elsif ($self->join_path eq 'aab(a)bcde') {
            $self->jump(1);
        } elsif ($self->join_path eq 'aabab(a)ababcde') {
            $self->jump('c');
        } elsif ($self->join_path eq 'aababacd(a)ababacde') {
            $self->jump('LAST');
        } else {
            die "Shouldn't get here";
        }
    }

    sub b_run_step {
        my $self = shift;
        if ($self->join_path eq 'aa(b)cde') {
            $self->jump('PREVIOUS');
        } elsif ($self->join_path eq 'aaba(b)cde') {
            $self->jump(-10);
        } else {
            die "Shouldn't get here";
        }
    }

    sub c_run_step { 0 }

    sub d_run_step { shift->jump('FIRST') }

    sub e_run_step {
        my $self = shift;
        $self->replace_path(); # truncate
        $self->jump(1);
    }

    sub default_step { 'z' }

    sub z_run_step { 1 }

    sub __error_run_step { 1 }
}

my $Foo10 = Foo10->new(form => {step => 'a'});
$Foo10->navigate;
ok($Foo10->join_path eq 'aababacdae(z)', 'Followed good path: '.$Foo10->join_path);

###----------------------------------------------------------------###

{
    package Foo11;
    our @ISA = qw(Foo);
    sub step1_skip { 1 }
    sub step1_next_step { 'step6' }
    sub step6_file_print { \ 'step6_file_print' }
    sub step2_name_step { '' }
    sub step3_name_step { 'foo.htm' }

    package Foo12;
    our @ISA = qw(Foo11);
    sub val_path { '' }
}

local $ENV{'SCRIPT_NAME'} = '/cgi/ralph.pl';
ok(Foo11->new->file_print("george") eq 'ralph/george.html', 'file_print: '. Foo11->new->file_print("george"));
ok(Foo11->new->file_val("george") =~ m|\Q/ralph/george.val\E|, 'file_val: '. Foo11->new->file_val("george"));
ok(ref(Foo12->new->file_val("george")) eq 'HASH', 'file_val: no such path');
ok(Foo11->new(val_path => '../'        )->file_val("george") eq '../ralph/george.val', 'file_val');
ok(Foo11->new(val_path => sub {'../'}  )->file_val("george") eq '../ralph/george.val', 'file_val');
ok(Foo11->new(val_path => ['../']      )->file_val("george") eq '../ralph/george.val', 'file_val');
ok(Foo11->new(val_path => ['../', './'])->file_val("george") eq '../ralph/george.val', 'file_val');

ok(! eval { Foo11->new->file_print("step2") } && $@, 'Bad name_step');
ok(! eval { Foo11->new->file_val("step2") } && $@, 'Bad name_step');

ok(Foo11->new->file_print("step3") eq 'ralph/foo.htm', 'file_print: '. Foo11->new->file_print("step3"));
ok(Foo11->new->file_val("step3") =~ m|\Q/ralph/foo.val\E|, 'file_val: '. Foo11->new->file_val("step3"));


local $ENV{'REQUEST_METHOD'} = 'POST';

Foo11->new(form => {step => 'step1'})->navigate;
ok($Foo::test_stdout eq 'step6_file_print', "Refine Path and set_ready_validate work ($Foo::test_stdout)");

Foo11->set_ready_validate(1);
ok(Foo11->ready_validate, "Is ready to validate");
Foo11->set_ready_validate(0);
ok(! Foo11->ready_validate, "Not ready to validate");
Foo11->set_ready_validate(1);
ok(Foo11->ready_validate, "Is ready to validate");
Foo11->set_ready_validate('somestep', 0);
ok(! Foo11->ready_validate, "Not ready to validate");

###----------------------------------------------------------------###

{
    package Foo13;
    our @ISA = qw(Foo);
    sub step0_ready_validate { 1 }
    sub step0_hash_validation { {foo => {required => 1}} }

    sub step1_ready_validate { 1 }
    sub step1_form_name       { shift->{'step1_form_name'} }
    sub step1_hash_validation { shift->{'step1_hash_validation'} }
    sub step1_file_print { \ 'step1_file_print [% has_errors %]' }
}

ok(Foo13->new(ext_val => 'html')->navigate->js_validation('step0') eq '', 'Got right validation');
ok($Foo::test_stdout eq 'Main Content', "Got the right content on Foo13 ($Foo::test_stdout)");

Foo13->new(form => {step => 'step1'})->navigate->js_validation('step1');
ok($Foo::test_stdout eq 'Main Content', "Got the right content on Foo13");

ok(Foo13->new->js_validation('step1')            eq '', "No validation found");
ok(Foo13->new->js_validation('step1', 'foo')     eq '', "No validation found");
ok(Foo13->new->js_validation('step1', 'foo', {}) eq '', "No validation found");
ok(Foo13->new->js_validation('step1', 'foo', {foo => {required => 1}}), "Validation found");

###----------------------------------------------------------------###

{
    package CGIX;
    sub new { bless {}, __PACKAGE__ }
    sub get_form { {} }
    sub print_js {
        my ($self, $file) = @_;
        $Foo::test_stdout = "Print JS: $file";
    }
    sub print_content_type {
        my $self = shift;
        my $mime = shift || 'text/html';
        my $char = shift || '';
        $mime .= "; charset=$char" if $char && $char =~ m|^[\w\-\.\:\+]+$|;
        $Foo::test_stdout = "Print: $mime";
    }
}

CGI::Ex::App->new(cgix => CGIX->new)->js_run_step;
ok($Foo::test_stdout eq 'Print JS: ', "Ran js_run_step: $Foo::test_stdout");

CGI::Ex::App->new(cgix => CGIX->new, form => {js => 'CGI/Ex/validate.js'})->js_run_step;
ok($Foo::test_stdout eq 'Print JS: CGI/Ex/validate.js', "Ran js_run_step: $Foo::test_stdout");

CGI::Ex::App->new(cgix => CGIX->new, path_info => '/js/CGI/Ex/validate.js')->js_run_step;
ok($Foo::test_stdout eq 'Print JS: CGI/Ex/validate.js', "Ran js_run_step: $Foo::test_stdout");

CGI::Ex::App->new(cgix => CGIX->new)->print_out('foo', "# the output\n");
ok($Foo::test_stdout eq 'Print: text/html', "Got right header: $Foo::test_stdout");
CGI::Ex::App->new(cgix => CGIX->new, mimetype => 'img/gif')->print_out('foo', "# the output\n");
ok($Foo::test_stdout eq 'Print: img/gif', "Got right header: $Foo::test_stdout");
CGI::Ex::App->new(cgix => CGIX->new, charset => 'ISO-foo')->print_out('foo', "# the output\n");
ok($Foo::test_stdout eq 'Print: text/html; charset=ISO-foo', "Got right header: $Foo::test_stdout");

CGI::Ex::App->new(cgix => CGIX->new)->print_out('foo', \ "# the output\n");
ok($Foo::test_stdout eq 'Print: text/html', "Got right header: $Foo::test_stdout");

###----------------------------------------------------------------###\
