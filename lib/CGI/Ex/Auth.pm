package CGI::Ex::Auth;

### CGI Extended Application

###----------------------------------------------------------------###
#  Copyright 2004 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom


use strict;
use vars qw($USE_PLAINTEXT
            $CHECK_CRYPTED
            $EXPIRE_LOGINS
            $FAILED_SLEEP
            );

use CGI::Ex::Dump qw(debug);
use MIME::Base64 qw(encode_base64 decode_base64);

BEGIN {
  $CHECK_CRYPTED = 1        if ! defined $CHECK_CRYPTED;
  $FAILED_SLEEP  = 2        if ! defined $FAILED_SLEEP;
  $EXPIRE_LOGINS = 6 * 3600 if ! defined $EXPIRE_LOGINS;
  #if ($ENV{MOD_PERL}) {
  #  require Digest::SHA1;
  #  require Digest::MD5;
  #}
}

###----------------------------------------------------------------###

sub new {
  my $class = shift || __PACKAGE__;
  my $self  = ref($_[0]) ? shift : {@_};
  bless $self, $class;
  $self->init();
  return $self;
}

sub init {}

###----------------------------------------------------------------###

sub require_auth {
  my $self = shift;
  $self = __PACKAGE__->new($self) if ! UNIVERSAL::isa($self, __PACKAGE__);

  ### shortcut that will print a js file as needed
  if ($ENV{PATH_INFO} && $ENV{PATH_INFO} =~ m|^/js/(CGI/Ex/\w+\.js)$|) {
    $self->cgix->print_js($1);
    return 0;
  }

  my $form    = $self->form;
  my $cookies = $self->cookies;
  my $key_l   = $self->key_logout;
  my $key_c   = $self->key_cookie;
  my $key_u   = $self->key_user;
  my $key_p   = $self->key_pass;
  my $key_chk = $self->key_cookie_check;
  my $had_form_info = 0;

  ### if they've passed us information - try and use it
  if ($form->{$key_l}) {
    $self->delete_cookie;

  } elsif (exists($form->{$key_u}) && exists($form->{$key_p})) {
    if ($self->verify_userpass($form->{$key_u}, $form->{$key_p})) {
      my $has_cookies = scalar keys %$cookies;
      my $user  = $form->{$key_u};
      my $str   = encode_base64(join ":", delete($form->{$key_u}), delete($form->{$key_p}));
      my $key_s = $self->key_save;
      $self->set_cookie($str, delete($form->{$key_s}));
      if ($has_cookies) {
        return $self->success($user); # assuming if they have cookies - the one we set will work
      } else {
        $form->{$key_chk} = time();
        my $key_r = $self->key_redirect;
        if (! $form->{$key_r}) {
          my $script = $ENV{SCRIPT_NAME} || die "Missing SCRIPT_NAME";
          my $info   = $ENV{PATH_INFO} || '';
          my $query  = $self->cgix->make_form($form);
          $form->{$key_r} = $script . $info . ($query ? "?$query" : "");
        }
        $self->location_bounce($form->{$key_r});
        return 0;
      }
    } else {
      $had_form_info = 1;
      $self->delete_cookie;
    }

  ### otherwise look for an already set cookie
  } elsif ($cookies->{$key_c}) {
    my ($user, $pass) = split /:/, decode_base64($cookies->{$key_c}), 2;
    return $self->success($user) if $self->verify_userpass($user, $pass);
    $self->delete_cookie;

  ### cases to handle no cookies
  } elsif ($form->{$key_chk}) {
    my $value = delete $form->{$key_chk};
    if ($self->allow_htauth) {
      die "allow_htauth is not implemented - yet";
    } elsif (abs(time() - $value) < 3600) {
      # fail down to below where we ask for auth
      # this is assuming that all webservers in the cluster are within 3600 of each other
    } else {
      $self->hook_print("no_cookies", $form);
      return 0;
    }
  }

  ### oh - you're still here - well then - ask for login credentials
  my $key_r = $self->key_redirect;
  if (! $form->{$key_r}) {
    my $script = $ENV{SCRIPT_NAME} || die "Missing SCRIPT_NAME";
    my $info   = $ENV{PATH_INFO} || '';
    my $query  = $self->cgix->make_form($form);
    $form->{$key_r} = $script . $info . ($query ? "?$query" : "");
  }
  $form->{login_error} = $had_form_info;
  $self->hook_print("get_login_info", $form);
  return 0;
}

###----------------------------------------------------------------###

sub hook_print {
  my $self = shift;
  my $page = shift;
  my $form = shift;
  my $meth;
  if ($meth = $self->{hook_print}) {
    $self->$meth($page, $form);
    return 0;
  }

  ### no hook - give basic functionality
  my $content;
  if ($page eq 'no_cookies') {
    $content = qq{<div style="border: 2px solid black;background:red;color:white">You do not appear to have cookies enabled.</div>};
  } elsif ($page eq 'get_login_info') {
    $content = $self->basic_login_page($form);
  } else {
    $content = "No content for page \"$page\"";
  }

  $self->cgix->print_content_type();
  print $content;
  return 0;
}

###----------------------------------------------------------------###

sub success {
  my $self = shift;
  my $user = shift;
  $self->{user} = $ENV{REMOTE_USER} = $user;
  $self->hook_success($user);
  return 1;
}

sub user {
  my $self = shift;
  return $self->{user};
}

sub hook_success {
  my $self = shift;
  my $user = shift;
  my $meth;
  if ($meth = $self->{hook_success}) {
    $self->$meth($user);
  }
}

###----------------------------------------------------------------###

sub delete_cookie {
  my $self  = shift;
  my $key_c = $self->key_cookie;
  $self->cgix->set_cookie({
    -name    => $key_c,
    -value   => '',
    -expires => '-10y',
    -path    => '/',
  });
}      

sub set_cookie {
  my $self  = shift;
  my $key_c = $self->key_cookie;
  my $value = shift || '';
  my $save_pass = shift;
  $self->cgix->set_cookie({
    -name    => $key_c,
    -value   => $value,
    ($save_pass ? (-expires => '+10y') : ()),
    -path    => '/',
  });
}

sub location_bounce {
  my $self = shift;
  my $url = shift;
  return $self->cgix->location_bounce($url);
}

###----------------------------------------------------------------###

sub key_logout {
  my $self = shift;
  $self->{key_logout} = shift if $#_ != -1;
  return $self->{key_logout} ||= 'logout';
}

sub key_cookie {
  my $self = shift;
  $self->{key_cookie} = shift if $#_ != -1;
  return $self->{key_cookie} ||= 'ce_auth';
}

sub key_cookie_check {
  my $self = shift;
  $self->{key_cookie_check} = shift if $#_ != -1;
  return $self->{key_cookie_check} ||= 'ccheck';
}

sub key_user {
  my $self = shift;
  $self->{key_user} = shift if $# != -1;
  return $self->{key_user} ||= 'ce_user';
}

sub key_pass {
  my $self = shift;
  $self->{key_pass} = shift if $# != -1;
  return $self->{key_pass} ||= 'ce_pass';
}

sub key_save {
  my $self = shift;
  $self->{key_save} = shift if $# != -1;
  return $self->{key_save} ||= 'ce_save';
}

sub key_redirect {
  my $self = shift;
  $self->{key_redirect} = shift if $# != -1;
  return $self->{key_redirect} ||= 'redirect';
}

sub allow_htauth {
  my $self = shift;
  $self->{allow_htauth} = shift if $# != -1;
  return $self->{allow_htauth} ||= 0;
}

sub payload {
  my $self = shift;
  my $user = shift;
  my $meth;
  my @payload = (time());
  if ($meth = $self->{hook_payload}) {
    push @payload, $self->$meth($user);
  }
  return join "/", @payload;
}

###----------------------------------------------------------------###

sub verify_userpass {
  my $self = shift;
  my $user = shift;
  my $pass = shift;
  my $host = shift || $self->host;
  my $meth;
  if ($meth = $self->{hook_verify_userpass}) {
    return $self->$meth($user, $pass, $host);
  } else {
    return $self->hook_verify_userpass($user, $pass, $host);
  }
}

sub hook_verify_userpass {
  my $self = shift;
  my $user = shift;
  my $pass_test = shift;
  my $host = shift || $self->host;

  return undef if ! defined $user;
  return undef if ! defined $pass_test;
  my $pass_real = $self->hook_get_pass_by_user($user, $host);
  return undef if ! defined $pass_real;

  my $type_real = ($pass_real =~ m/^(md5|sha1)\((.+)\)$/) ? $1 : 'plainorcrypt';
  my $hash_real = $2;
  my $type_test = ($pass_test =~ m/^(md5|sha1)\((.+)\)$/) ? $1 : 'plainorcrypt';
  my $hash_test = $2;

  ### if both types were plaintext - check if the equal
  if ($type_real eq 'plainorcrypt'
      && $type_test eq 'plainorcrypt') {
    return 1 if $pass_real eq $pass_test;
    if ($CHECK_CRYPTED && $pass_real =~ m|^([./0-9A-Za-z]{2})(.{,11})$|) {
      return 1 if crypt($pass_test, $1) eq $pass_real;
    }
    return 0;

  } else {
    ### if test type is plaintext - then hash it and compare it alone
    if ($type_test eq 'plainorcrypt') {
      $pass_test = $self->enc_func($type_real, $pass_test); # encode same as real
      $pass_test = "$type_real($pass_test)";
      return $pass_test eq $pass_real;

    ### if real type is plaintext - then hash it to get ready for test
    } elsif ($type_real eq 'plainorcrypt') {
      $pass_real = $self->enc_func($type_test, $pass_real);
      $pass_real = "$type_test($pass_real)";
      $type_real = $type_test;
    }
    
    ### the types should be the same (unless a system stored sha1 and md5 passwords)
    if ($type_real ne $type_test) {
      warn "Test types for user \"$user\" are of two different types - very bad";
      return 0;
    }

    ### no payload - compre directly
    if ($hash_test !~ m|^(.+)/([^/]+)$|) {
      return lc($pass_test) eq lc($pass_real);

    ### and finally - check the payload (allows for expiring login)
    } else {
      my $payload = $1; # payload can be anything
      my $compare = $2; # a checksum which is the enc of the payload + '/' + enc of password
      return 0 if $self->enc_func($type_test, "$payload/$hash_real") ne $compare;
      if ($EXPIRE_LOGINS && $payload =~ m/^(\d+)/) {
        return 0 if time() > $1 + $EXPIRE_LOGINS;
      }
      return 1;
    }
  }
  return 0; # nothing should make it this far
}

sub enc_func {
  my $self = shift;
  my $type = shift;
  my $str  = shift;
  if ($type eq 'md5') {
    require Digest::MD5;
    return &Digest::MD5::md5_hex($str);
  } elsif ($type eq 'sha1') {
    require Digest::SHA1;
    return &Digest::SHA1::sha1_hex($str);
  }
}

sub set_hook_get_pass_by_user {
  my $self = shift;
  $self->{hook_get_pass_by_user} = shift;
}

sub hook_get_pass_by_user {
  my $self = shift;
  my $user = shift;
  my $host = shift || $self->host;
  my $meth;
  if ($meth = $self->{hook_get_pass_by_user}) {
    return $self->$meth($user, $host);
  }
  die "get_pass_by_user is a virtual method - please override - or use set_hook_get_pass_by_user";
}

###----------------------------------------------------------------###

sub cgix {
  my $self = shift;
  $self->{cgix} = shift if $#_ != -1;
  return $self->{cgix} ||= do {
    require CGI::Ex;
    CGI::Ex->new(); # return of the do
  };
}

sub form {
  my $self = shift;
  if ($#_ != -1) {
    $self->{form} = shift || die "Invalid form";
  }
  return $self->{form} ||= $self->cgix->get_form;
}

sub cookies {
  my $self = shift;
  if ($#_ != -1) {
    $self->{cookies} = shift || die "Invalid cookies";
  }
  return $self->{cookies} ||= $self->cgix->get_cookies;
}

sub host {
  my $self = shift;
  return $self->{host} = shift if $#_ != -1;
  return $self->{host} ||= do {
    my $host = $ENV{HTTP_HOST} || die "Missing \$ENV{HTTP_HOST}";
    $host = lc($host);
    $host =~ s/:\d*$//;      # remove port number
    $host =~ s/\.+$//;       # remove qualified dot
    $host =~ s/[^\w\.\-]//g; # remove odd characters
    $host; # return of the do
  };
}

###----------------------------------------------------------------###

sub basic_login_page {
  my $self = shift;
  my $form = shift;

  $form->{payload}      = $self->payload;
  $form->{error}        = ($form->{login_error}) ? "Login Failed" : "";
  $form->{key_user}     = $self->key_user;
  $form->{key_pass}     = $self->key_pass;
  $form->{key_save}     = $self->key_save;
  $form->{key_redirect} = $self->key_redirect;
  $form->{form_name}    = 'ce_form';
  $form->{script}       = $self->basic_login_javascript($form);
  delete $form->{$form->{key_pass}};
 
  my $text = $self->basic_login_template();
  $self->cgix->swap_template(\$text, $form);
  $self->cgix->fill(\$text, $form);

  return $text;
}

sub basic_login_template {
  return qq{
    [% header %]
    <span class="error" style="color:red">[% error %]</span>
    <form name="[% form_name %]" method="get" action="[% script_name %]">
    <table border="0" class="login_table">
    <tr>
      <td>Username:</td>
      <td><input name="[% key_user %]" type="text" size="30" value=""></td>
    </tr>
    <tr>
      <td>Password:</td>
      <td><input name="[% key_pass %]" type="password" size="30" value=""></td>
    </tr>
    <tr>
      <td colspan="2">
        <input type="checkbox" name="[% key_save %]" value="1"> Save Password ?
      </td>
    </tr>
    <tr>
      <td colspan="2" align="right">
        <input type="hidden" name="[% key_redirect %]"
        <input type="hidden" name="[% ce_payload %]"
        <input type="submit" value="Submit">
      </td>
    </tr>
    [% extra_table %]
    </table>
    </form>
    [% script %]
    [% footer %]
  };
}

sub basic_login_javascript {
  my $self = shift;
  my $form = shift;
  my $type;
  if ($USE_PLAINTEXT) {
    return '';
  } elsif (eval {require Digest::SHA1}) {
    $type = 'sha1';
  } elsif (eval {require Digest::MD5}) {
    $type = 'md5';
  } else {
    return "";
  }

  return qq{
    <script src="$ENV{SCRIPT_NAME}/js/CGI/Ex/$type.js"></script>
    <script>
    function send_it () {
      var u = document.$form->{form_name}.$form->{key_user}.value;
      var p = document.$form->{form_name}.$form->{key_pass}.value;
      var s = (document.$form->{form_name}.$form->{key_save}.checked) ? 1 : 0;
      var r = document.$form->{form_name}.$form->{key_redirect}.value;
      var l = document.$form->{form_name}.ce_payload.value;
      var q = document.$form->{form_name}.action;
      q += '?$form->{key_user}='+escape(u);
      q += '?$form->{key_save}='+escape(s);
      q += '?$form->{key_pass}='+escape(document.${type}_hex(l+'/'+document.${type}_hex(p)));
      alert(q);
      location.href = q;
      return false;
    }
    if (document.${type}_hex) document.$form->{form_name}.onsubmit = send_it;
    </script>
  };
}

###----------------------------------------------------------------###

1;
