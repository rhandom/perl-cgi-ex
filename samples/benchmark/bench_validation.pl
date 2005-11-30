#!/usr/bin/perl -w

use Benchmark qw(timethese cmpthese countit timestr);
use CGI::Ex::Validate;
use Data::FormValidator;

my $form = {
  username  => "++foobar++",
  password  => "123",
  password2 => "1234",
};

my $val_hash_ce = {
    username => {
        required => 1,
        match    => 'm/^\w+$/',
        match_error => '$name may only contain letters and numbers',
        untaint  => 1,
    },
    password => {
        required => 1,
        match    => 'm/^[ -~]{6,30}$/',
#        min_len  => 6,
#        max_len  => 30,
#        match    => 'm/^[ -~]+$/',
        untaint  => 1,
    },
    password2 => {
        validate_if => 'password',
        equals      => 'password',
    },
    email => {
        required => 1,
        match    => 'm/^[\w\.\-]+\@[\w\.\-]+$/',
        untaint  => 1,
    },
};

my $val_hash_df = {
    required => [qw(username password email)],
    dependencies => {
        password => [qw(password2)],
    },
    constraints => {
        email    => qr/^[\w\.\-]+\@[\w\.\-]+$/,
        password => qr/^[ -~]{6,30}$/,
        username => qr/^\w+$/,
    },
    untaint_all_constraints => 1,
    msgs => {
        format => '%s',
        prefix => 'error_',
    },
};

sub check_form {
  my $form = shift;
  my $hash = {};
  if (! exists $form->{'username'}) {
    push @{ $hash->{'username_error'} }, 'Username required';
  } elsif ($form->{'username'} !~ m/^(\w+)$/) {
    push @{ $hash->{'username_error'} }, 'Username may only contain letters and numbers';
  } else {
    $form->{'username'} = $1;
  }
  if (! exists $form->{'password'}) {
    push @{ $hash->{'password_error'} }, 'Password required';
  } else {
    if ($form->{'password'} !~ m/^([ -~]+)$/) {
      push @{ $hash->{'password_error'} }, 'Password contained bad characters';
    } else {
      $form->{'password'} = $1;
    }
    if (length($form->{'password'}) < 6) {
      push @{ $hash->{'password_error'} }, 'Password must be more than 6 characters';
    } elsif (length($form->{'password'}) > 30) {
      push @{ $hash->{'password_error'} }, 'Password must be less than 30 characters';
    }

    if (! defined($form->{'password2'})
        || $form->{'password2'} ne $form->{'password'}) {
      push @{ $hash->{'password2_error'} }, 'Password2 and password must be the same';
    }
  }

  if (! exists $form->{'email'}) {
    push @{ $hash->{'email_error'} }, 'Email required';
  } elsif ($form->{'email'} !~ m/^[\w\.\-]+\@[\w\.\-]+$/) {
    push @{ $hash->{'email_error'} }, 'Please type a valid email address';
  }

  return $hash;
}


cmpthese (-2,{
  cgi_ex    => sub { my $t = CGI::Ex::Validate->validate($form, $val_hash_ce) },
  data_val  => sub { my $t = Data::FormValidator->check($form, $val_hash_df) },
  homegrown => sub { my $t = scalar keys %{ check_form($form) } },
},'auto');

cmpthese (-2,{
  cgi_ex    => sub { my $t = CGI::Ex::Validate->validate($form, $val_hash_ce)->as_hash },
  data_val  => sub { my $t = Data::FormValidator->check($form, $val_hash_df)->msgs },
  homegrown => sub { my $t = check_form($form) },
},'auto');


### Home grown solution blows the others away - but lacks features
#
# Benchmark: running cgi_ex, data_val, homegrown for at least 2 CPU seconds...
#     cgi_ex:  2 wallclock secs ( 2.12 usr +  0.00 sys =  2.12 CPU) @ 1430.66/s (n=3033)
#   data_val:  2 wallclock secs ( 2.01 usr +  0.00 sys =  2.01 CPU) @ 2588.56/s (n=5203)
#  homegrown:  2 wallclock secs ( 2.19 usr +  0.01 sys =  2.20 CPU) @ 54733.18/s (n=120413)
#              Rate    cgi_ex  data_val homegrown
# cgi_ex     1431/s        --      -45%      -97%
# data_val   2589/s       81%        --      -95%
# homegrown 54733/s     3726%     2014%        --
# Benchmark: running cgi_ex, data_val, homegrown for at least 2 CPU seconds...
#     cgi_ex:  2 wallclock secs ( 2.10 usr +  0.00 sys =  2.10 CPU) @ 1218.57/s (n=2559)
#   data_val:  2 wallclock secs ( 2.14 usr +  0.00 sys =  2.14 CPU) @ 2092.99/s (n=4479)
#  homegrown:  2 wallclock secs ( 2.14 usr +  0.00 sys =  2.14 CPU) @ 56267.76/s (n=120413)
#              Rate    cgi_ex  data_val homegrown
# cgi_ex     1219/s        --      -42%      -98%
# data_val   2093/s       72%        --      -96%
# homegrown 56268/s     4518%     2588%        --
