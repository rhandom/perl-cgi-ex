#!/usr/bin/perl -w

=head1 NAME

cgi_ex_2.cgi - Rewrite of cgi_ex_1.cgi using CGI::Ex::App

=cut

if (__FILE__ eq $0) {
  handler();
}

sub handler {
  MyCGI->navigate;
}

###----------------------------------------------------------------###

package MyCGI;

use strict;
use base CGI::Ex::App;
use CGI::Ex::Dump qw(debug);

sub pre_loop {
  my $self = shift;
  my $path = shift;
  if ($#$path == -1) {
    push @$path, 'userinfo';
  }
}

### this will work for both userinfo_hash_common and success_hash_common
sub hash_common {
  my $self = shift;
  return {
    title   => 'My Application',
    script  => $ENV{SCRIPT_NAME},
    color   => ['#ccf', '#aaf'],
    history => $self->history,
  }
}

sub ready_validate {
  my $self = shift;
  return $self->form->{processing} ? 1 : 0;
}

###----------------------------------------------------------------###

sub userinfo_hash_validation {
  return {
    'group order' => ['username', 'password'],
    username => {
      required => 1,
      min_len  => 3,
      max_len  => 30,
      match    => 'm/^\w+$/',
      # could probably all be done with match => 'm/^\w{3,30}$/'
    },
    password => {
      required => 1,
      max_len  => 20,
    },
    password_verify => {
      validate_if => 'password',
      equals      => 'password',
    },
  };
}

sub userinfo_hash_swap {
  my $self = shift;
  my $hash = $self->form;
  $hash->{form_name} = 'formfoo';
  $hash->{js_val}    = $self->vob->generate_js($self->userinfo_hash_validation(),
                                               $hash->{form_name},
                                               "$ENV{SCRIPT_NAME}/js");
  return $hash;
}

###----------------------------------------------------------------###

sub userinfo_file_print {
  return \ qq {
    <html>
    <head>
      <title>[% title %]</title>
      <style>
      .error {
        display: block;
        color: red;
        font-weight: bold;
      }
      </style>
    </head>
    <body>
    <h1 style='color:blue'>Please Enter information</h1>
    <span style='color:red'>[% error_header %]</span>
    <br>

    <form name="[% form_name %]">
    <input type=hidden name=processing value=1>

    <table>
    <tr bgcolor=[% color.0 %]>
      <td>Username:</td>
      <td>
        <input type=text size=30 name=username>
        <span class=error id=username_error>[% username_error %]</span></td>
    </tr>
    <tr bgcolor=[% color.1 %]>
      <td>Password:</td>
      <td><input type=password size=20 name=password>
        <span class=error id=password_error>[% password_error %]</span></td>
    </tr>
    <tr bgcolor=[% color.0 %]>
      <td>Password Verify:</td>
      <td><input type=password size=20 name=password_verify>
        <span class=error id=password_verify_error>[% password_verify_error %]</span></td>
    </tr>
    <tr bgcolor=[% color.1 %]>
      <td colspan=2 align=right><input type=submit value=Submit></td>
    </tr>

    </table>

    </form>

    [% js_validation %]
    </body>
    </html>
  };
}

sub success_file_print {
  return \ qq{
    <html>
    <head><title>[% title %]</title></head>
    <body>
    <h1 style='color:green'>Success</h1>
    <br>
    print "I can now continue on with the rest of my script!";
    </body>
    </html>
  };
}


1;
