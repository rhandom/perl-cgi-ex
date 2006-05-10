# -*- Mode: Perl; -*-

=head1 NAME

8_auth_00_base.t - Testing of the CGI::Ex::Auth module.

=cut

use strict;
use Test::More tests => 29;

use_ok('CGI::Ex::Auth');

{
    package Auth;
    use base qw(CGI::Ex::Auth);
    use strict;
    use vars qw($printed $set_cookie $deleted_cookie);

    sub login_print      { $printed = 1 }
    sub set_cookie       { $set_cookie = 1 }
    sub delete_cookie    { $deleted_cookie = 1 }
    sub get_pass_by_user { '123qwe' }
    sub script_name      { $0 }
    sub no_cookie_verify { 1 }
    sub secure_hash_keys { ['aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', 'bbbbbbbbbbbbbbbbbbbbbbbbbbb', 'ccc'] }
}

{
    package Aut2;
    use base qw(Auth);
    use vars qw($crypt);
    BEGIN { $crypt = crypt('123qwe', 'SS') };
    sub use_crypt { 1 }
    sub get_pass_by_user { $crypt }
}

my $token = Auth->new->generate_token({user => 'test', real_pass => '123qwe', use_base64 => 1});

my $form_bad     = { cea_user => 'test',   cea_pass => '123qw'  };
my $form_good    = { cea_user => 'test',   cea_pass => '123qwe' };
my $form_good2   = { cea_user => $token };
my $form_good3   = { cea_user => 'test/123qwe' };
my $cookie_bad   = { cea_user => 'test/123qw'  };
my $cookie_good  = { cea_user => 'test/123qwe' };
my $cookie_good2 = { cea_user => $token };

sub form_good    { Auth->get_valid_auth({form => {%$form_good},  cookies => {}              }) }
sub form_good2   { Auth->get_valid_auth({form => {%$form_good2}, cookies => {}              }) }
sub form_good3   { Aut2->get_valid_auth({form => {%$form_good3}, cookies => {}              }) }
sub form_bad     { Auth->get_valid_auth({form => {%$form_bad},   cookies => {}              }) }
sub cookie_good  { Auth->get_valid_auth({form => {},             cookies => {%$cookie_good} }) }
sub cookie_good2 { Auth->get_valid_auth({form => {},             cookies => {%$cookie_good2}}) }
sub cookie_bad   { Auth->get_valid_auth({form => {},             cookies => {%$cookie_bad}  }) }

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
ok(form_good(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "Delete_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
ok(form_good2(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "Delete_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
ok(form_good3(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "Delete_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
ok(! form_bad(), "Got bad auth");
ok($Auth::printed, "Printed was set");
ok(! $Auth::set_cookie, "set_cookie called");
ok(! $Auth::deleted_cookie, "Delete_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
ok(cookie_good(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "Delete_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
ok(cookie_good2(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "Delete_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
ok(! cookie_bad(), "Got bad auth");
ok($Auth::printed, "Printed was set");
ok(! $Auth::set_cookie, "Set_cookie was not called");
ok($Auth::deleted_cookie, "delete_cookie was not called");

