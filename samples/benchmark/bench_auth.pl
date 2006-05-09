#!/usr/bin/perl -w

=head1 NAME

bench_auth.pl - Test relative performance of CGI::Ex::Auth

=head1 SAMPLE OUTPUT

  Benchmark: running cookie_bad, cookie_good, cookie_good2, form_bad, form_good, form_good2, form_good3 for at least 2 CPU seconds...
  cookie_bad:  2 wallclock secs ( 2.01 usr +  0.00 sys =  2.01 CPU) @ 6982.59/s (n=14035)
  cookie_good:  3 wallclock secs ( 2.15 usr +  0.06 sys =  2.21 CPU) @ 6484.16/s (n=14330)
  cookie_good2:  3 wallclock secs ( 2.02 usr +  0.09 sys =  2.11 CPU) @ 5422.27/s (n=11441)
    form_bad:  3 wallclock secs ( 2.16 usr +  0.01 sys =  2.17 CPU) @ 6603.23/s (n=14329)
   form_good:  3 wallclock secs ( 2.07 usr +  0.06 sys =  2.13 CPU) @ 6304.69/s (n=13429)
  form_good2:  3 wallclock secs ( 2.00 usr +  0.12 sys =  2.12 CPU) @ 5396.70/s (n=11441)
  form_good3:  2 wallclock secs ( 2.14 usr +  0.00 sys =  2.14 CPU) @ 7728.50/s (n=16539)
                 Rate form_good2 cookie_good2 form_good cookie_good form_bad cookie_bad form_good3
  form_good2   5397/s         --          -0%      -14%        -17%     -18%       -23%       -30%
  cookie_good2 5422/s         0%           --      -14%        -16%     -18%       -22%       -30%
  form_good    6305/s        17%          16%        --         -3%      -5%       -10%       -18%
  cookie_good  6484/s        20%          20%        3%          --      -2%        -7%       -16%
  form_bad     6603/s        22%          22%        5%          2%       --        -5%       -15%
  cookie_bad   6983/s        29%          29%       11%          8%       6%         --       -10%
  form_good3   7729/s        43%          43%       23%         19%      17%        11%         --

=cut

use strict;
use Benchmark qw(cmpthese timethese);
use CGI::Ex::Auth;
use CGI::Ex::Dump qw(debug);

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
die "Didn't get good auth"         if ! form_good();
die "printed was set"              if $Auth::printed;
die "set_cookie not called"        if ! $Auth::set_cookie;
die "delete_cookie was called"     if $Auth::deleted_cookie;

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
debug form_good2(), (my $e = $@);
die "Didn't get good auth"         if ! form_good2();
die "printed was set"              if $Auth::printed;
die "set_cookie not called"        if ! $Auth::set_cookie;
die "delete_cookie was called"     if $Auth::deleted_cookie;

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
die "Didn't get good auth"         if ! form_good3();
die "printed was set"              if $Auth::printed;
die "set_cookie not called"        if ! $Auth::set_cookie;
die "delete_cookie was called"     if $Auth::deleted_cookie;

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
die "Didn't get bad auth"          if form_bad();
die "printed was not set"          if ! $Auth::printed;
die "set_cookie called"            if $Auth::set_cookie;
die "delete_cookie was called"     if $Auth::deleted_cookie;

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
die "Didn't get good auth"         if ! cookie_good();
die "printed was set"              if $Auth::printed;
die "set_cookie not called"        if ! $Auth::set_cookie;
die "delete_cookie was called"     if $Auth::deleted_cookie;

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
die "Didn't get good auth"         if ! cookie_good2();
die "printed was set"              if $Auth::printed;
die "set_cookie not called"        if ! $Auth::set_cookie;
die "delete_cookie was called"     if $Auth::deleted_cookie;

$Auth::printed = $Auth::set_cookie = $Auth::delete_cookie = 0;
die "Didn't get bad auth"          if cookie_bad();
die "printed was not set"          if ! $Auth::printed;
die "set_cookie called"            if $Auth::set_cookie;
die "delete_cookie was not called" if ! $Auth::deleted_cookie;

print "Ready\n";

my $r = eval { timethese (-2, {
    form_good    => \&form_good,
    form_good2   => \&form_good2,
    form_good3   => \&form_good3,
    form_bad     => \&form_bad,
    cookie_good  => \&cookie_good,
    cookie_good2 => \&cookie_good2,
    cookie_bad   => \&cookie_bad,
}) };
if (! $r) {
    debug "$@";
    next;
}
eval { cmpthese $r };
