#!/usr/bin/perl -w

if (__FILE__ eq $0) {
  &handler();
}

sub handler {
  MyCGI->navigate();
}

###----------------------------------------------------------------###

package MyCGI;

use strict;
use base CGI::Ex::App;
use CGI::Ex::Dump qw(debug);

###----------------------------------------------------------------###

sub pre_loop {
  my $self = shift;
  my $path = shift;
  if ($#$path == -1) {
    push @$path, 'userinfo';
  }
}

sub userinfo_ready_validate {
  my $self = shift;
  return $self->form->{processing} ? 1 : 0;
}

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

sub userinfo_hash_form {
  my $self = shift;
  my $hash = $self->form;
  $hash->{form_name} = 'formfoo';
  $hash->{js_val}    = $self->vob->generate_js($self->userinfo_hash_validation(),
                                               $hash->{form_name},
                                               "$ENV{SCRIPT_NAME}/js");
  return $hash;
}

sub hash_common {
  return {
    title  => 'My Application',
    script => $ENV{SCRIPT_NAME},
    color  => ['#ccf', '#aaf'],
  }
}

sub print {
  my $self = shift;
  my $step = shift;
  my $form = shift;
  my $fill = shift;

  my $content = ($step eq 'userinfo') ? &get_content_form()
    : ($step eq 'main') ? &get_content_success()
    : "Don't have content for step \"$step\"";

  $self->cgix->swap_template(\$content, $form);
  $self->cgix->fill(\$content, $fill);

  $self->cgix->print_content_type();
  print $content;
}

### this works because we added /js onto $ENV{SCRIPT_NAME} above near js_val
sub js_pre_step {
  my $self = shift;
  my $info = $ENV{PATH_INFO} || '';
  if ($info =~ m|^/js(/\w+)+.js$|) {
    $info =~ s|^/+js/+||;
    $self->cgix->print_js($info);
    return 1;
  }
  return 0;
}


###----------------------------------------------------------------###

sub get_content_form {
  return qq{
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

    [% js_val %]
    </body>
    </html>
  };
}

sub get_content_success {
  return qq{
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
