#!/usr/bin/perl -w

=head1 NAME

bench_auth.pl - Test relative performance of CGI::Ex::Auth

=head1 SAMPLE OUTPUT

  Benchmark: running cookie_bad, cookie_good, cookie_good2, form_bad, form_good, form_good2, form_good3 for at least 2 CPU seconds...
  cookie_bad:  3 wallclock secs ( 2.05 usr +  0.00 sys =  2.05 CPU) @ 6390.73/s (n=13101)
  cookie_good:  3 wallclock secs ( 2.12 usr +  0.06 sys =  2.18 CPU) @ 6573.39/s (n=14330)
  cookie_good2:  2 wallclock secs ( 2.05 usr +  0.11 sys =  2.16 CPU) @ 5851.39/s (n=12639)
    form_bad:  2 wallclock secs ( 2.11 usr +  0.00 sys =  2.11 CPU) @ 6791.00/s (n=14329)
   form_good:  2 wallclock secs ( 2.03 usr +  0.06 sys =  2.09 CPU) @ 6397.13/s (n=13370)
  form_good2:  2 wallclock secs ( 1.96 usr +  0.12 sys =  2.08 CPU) @ 5800.48/s (n=12065)
  form_good3:  3 wallclock secs ( 2.11 usr +  0.00 sys =  2.11 CPU) @ 7838.39/s (n=16539)
                 Rate form_good2 cookie_good2 cookie_bad form_good cookie_good form_bad form_good3
  form_good2   5800/s         --          -1%        -9%       -9%        -12%     -15%       -26%
  cookie_good2 5851/s         1%           --        -8%       -9%        -11%     -14%       -25%
  cookie_bad   6391/s        10%           9%         --       -0%         -3%      -6%       -18%
  form_good    6397/s        10%           9%         0%        --         -3%      -6%       -18%
  cookie_good  6573/s        13%          12%         3%        3%          --      -3%       -16%
  form_bad     6791/s        17%          16%         6%        6%          3%       --       -13%
  form_good3   7838/s        35%          34%        23%       23%         19%      15%         --

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
