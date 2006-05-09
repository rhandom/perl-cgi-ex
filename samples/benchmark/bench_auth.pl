#!/usr/bin/perl -w

=head1 NAME

bench_auth.pl - Test relative performance of CGI::Ex::Auth

=head1 SAMPLE OUTPUT

  Benchmark: running cookie_bad, cookie_good, cookie_good2, form_bad, form_good, form_good2 for at least 2 CPU seconds...
  cookie_bad:  2 wallclock secs ( 2.02 usr +  0.00 sys =  2.02 CPU) @ 5922.77/s (n=11964)
  cookie_good:  1 wallclock secs ( 2.16 usr +  0.08 sys =  2.24 CPU) @ 7199.55/s (n=16127)
  cookie_good2:  3 wallclock secs ( 2.01 usr +  0.13 sys =  2.14 CPU) @ 6280.37/s (n=13440)
    form_bad:  3 wallclock secs ( 2.14 usr +  0.00 sys =  2.14 CPU) @ 6695.79/s (n=14329)
    form_good:  2 wallclock secs ( 2.03 usr +  0.06 sys =  2.09 CPU) @ 7010.53/s (n=14652)
  form_good2:  3 wallclock secs ( 1.91 usr +  0.14 sys =  2.05 CPU) @ 6253.17/s (n=12819)
                 Rate cookie_bad form_good2 cookie_good2 form_bad form_good cookie_good
  cookie_bad   5923/s         --        -5%          -6%     -12%      -16%        -18%
  form_good2   6253/s         6%         --          -0%      -7%      -11%        -13%
  cookie_good2 6280/s         6%         0%           --      -6%      -10%        -13%
  form_bad     6696/s        13%         7%           7%       --       -4%         -7%
  form_good    7011/s        18%        12%          12%       5%        --         -3%
  cookie_good  7200/s        22%        15%          15%       8%        3%          --

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

my $token = Auth->new->generate_token({user => 'test', real_pass => '123qwe', use_base64 => 1});

my $form_bad     = { cea_user => 'test',   cea_pass => '123qw'  };
my $form_good    = { cea_user => 'test',   cea_pass => '123qwe' };
my $form_good2   = { cea_user => $token };
my $cookie_bad   = { cea_user => 'test/123qw' };
my $cookie_good  = { cea_user => 'test/123qwe' };
my $cookie_good2 = { cea_user => $token };

sub form_good    { Auth->get_valid_auth({form => {%$form_good},  cookies => {}              }) }
sub form_good2   { Auth->get_valid_auth({form => {%$form_good2}, cookies => {}              }) }
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
debug(my $e = $@) if ! form_good2();
die "Didn't get good auth"         if ! form_good2();
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
