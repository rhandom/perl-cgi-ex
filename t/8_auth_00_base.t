# -*- Mode: Perl; -*-

=head1 NAME

8_auth_00_base.t - Testing of the CGI::Ex::Auth module.

=cut

use strict;
use Test::More tests => 35;

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
    sub failed_sleep     { 0 }
}

{
    package Aut2;
    use base qw(Auth);
    use vars qw($crypt);
    BEGIN { $crypt = crypt('123qwe', 'SS') };
    sub use_crypt { 1 }
    sub get_pass_by_user { {password => $crypt, foobar => 'baz'} }
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

$Auth::printed = $Auth::set_cookie = $Auth::deleted_cookie = 0;
ok(form_good(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "deleted_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::deleted_cookie = 0;
ok(form_good2(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "deleted_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::deleted_cookie = 0;
ok(form_good3(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "deleted_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::deleted_cookie = 0;
ok(! form_bad(), "Got bad auth");
ok($Auth::printed, "Printed was set");
ok(! $Auth::set_cookie, "set_cookie called");
ok(! $Auth::deleted_cookie, "deleted_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::deleted_cookie = 0;
ok(cookie_good(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "deleted_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::deleted_cookie = 0;
ok(cookie_good2(), "Got good auth");
ok(! $Auth::printed, "Printed was not set");
ok($Auth::set_cookie, "Set_cookie called");
ok(! $Auth::deleted_cookie, "deleted_cookie was not called");

$Auth::printed = $Auth::set_cookie = $Auth::deleted_cookie = 0;
ok(! cookie_bad(), "Got bad auth");
ok($Auth::printed, "Printed was set");
ok(! $Auth::set_cookie, "Set_cookie was not called");
ok($Auth::deleted_cookie, "deleted_cookie was not called");


my $auth = Aut2->get_valid_auth({form => {%$form_good3}});
my $data = $auth->last_auth_data;
ok($auth && $data, "Aut2 worked again");
ok($data->{'foobar'} eq 'baz', 'And it contained the correct value');

SKIP: {
    skip("Crypt::Blowfish not found", 4) if ! eval { require Crypt::Blowfish };

    {
        package Aut3;
        use base qw(Auth);
        sub use_blowfish  { "This_is_my_key" }
        sub use_base64    { 0 }
        sub use_plaintext { 1 }
    }

    my $token2 = Aut3->new->generate_token({user => 'test', real_pass => '123qwe'});
    my $form_good4   = { cea_user => $token2 };

    sub form_good4   { Aut3->get_valid_auth({form => {%$form_good4}, cookies => {}              }) }

    $Auth::printed = $Auth::set_cookie = $Auth::deleted_cookie = 0;
    ok(form_good4(), "Got good auth");
    ok(! $Auth::printed, "Printed was not set");
    ok($Auth::set_cookie, "Set_cookie called");
    ok(! $Auth::deleted_cookie, "deleted_cookie was not called");
};
