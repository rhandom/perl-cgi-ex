package CGI::Ex::Auth;

=head1 NAME

CGI::Ex::Auth - Handle logins nicely.

=cut

###----------------------------------------------------------------###
#  Copyright 2006 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

use strict;
use vars qw($VERSION);

use MIME::Base64 qw(encode_base64 decode_base64);
use Digest::MD5 qw(md5_hex);
use CGI::Ex;

$VERSION = '2.00';

###----------------------------------------------------------------###

sub new {
    my $class = shift || __PACKAGE__;
    my $args  = shift || {};
    return bless {%$args}, $class;
}

sub require_auth {
    my $self = shift;
    $self = $self->new(@_) if ! ref $self;

    use Debug;

    ### shortcut that will print a js file as needed (such as the md5.js)
    if ($self->script_name . $self->path_info eq $self->js_path . "/CGI/Ex/md5.js") {
        $self->cgix->print_js('CGI/Ex/md5.js');
        return 0;
    }

    my $form    = $self->form;
    my $cookies = $self->cookies;
    my $has_cookies = scalar %$cookies;

    ### allow for logout
    my $key_l   = $self->key_logout;
    if ($form->{$key_l}) {
        $self->delete_cookie;
        $self->location_bounce($self->logout_redirect);
        return 0;
    }

    my $had_form_info;
    foreach ([$form,    $self->key_user,   1],
             [$cookies, $self->key_cookie, 0],
             ) {
        my ($hash, $key, $is_form) = @$_;
        next if ! defined $hash->{$key};
        $had_form_info ++ if $is_form;

        if ($hash->{$key} !~ m|^[^/]+/| && $is_form) { # if it looks like a bare username - add on /password
            my $p = delete $hash->{ $self->key_pass };
            my $s = delete($hash->{ $self->key_save }) ? -1 : $self->expires_min;
            my $l = delete $hash->{ $self->key_payload } || '';
            my $t = $self->server_time;
            $hash->{$key} .= "$t/$s/$l/" . (defined $p ? $p : '');
        }

        eval { $self->verify_token($hash->{$key}) } || next;
        my $token = $self->last_token || next;

        ### generate a fresh cookie each time
        $self->set_cookie($self->generate_token, $token->{'save'});
        #return $self->success($user); # assume that cookies will work - if not next page will cause login
        #### this may actually be the nicer thing to do in the common case - except for the nasty looking
        #### url - all things considered - should really get location boucing to work properly while being
        #### able to set a cookie at the same time

        if ($has_cookies) {
#            $self->success($token->{'user'}); # assuming if they have cookies - the one we set will work
            return $self;
        } elsif ($is_form) {
            delete $form->{$self->key_user};      # remove token from the form
            $form->{$self->key_verify} = time();  # add verify token
            my $key_r = $self->key_redirect;
            if (! $form->{$key_r}) {              # make sure there is a redirect value
                my $query = $self->cgix->make_form($form);
                $form->{$key_r} = $self->script_name . $self->path_info . ($query ? "?$query" : "");
            }
            $self->location_bounce($form->{$key_r});
            return 0;
        }
    }
    debug $self->last_token;

    ### nothing found - see if they have cookies
    if (my $value = delete $form->{$self->key_verify}) {
        if (abs(time() - $value) < 3600) {
            $self->no_cookies_print;
            return 0;
        }
    }

    ### oh - you're still here - well then - ask for login credentials
    my $key_r = $self->key_redirect;
    if (! $form->{$key_r}) {
        my $query = $self->cgix->make_form($form);
        $form->{$key_r} = $self->script_name . $self->path_info . ($query ? "?$query" : "");
    }

    $form->{'login_error'} = $had_form_info;
    $self->login_print;
    return 0;
}

###----------------------------------------------------------------###

sub script_name { $ENV{'SCRIPT_NAME'} || die "Missing SCRIPT_NAME" }

sub path_info { $ENV{'PATH_INFO'} || '' }

sub server_time { time }

sub cgix {
    my $self = shift;
    $self->{'cgix'} = shift if $#_ != -1;
    return $self->{'cgix'} ||= CGI::Ex->new;
}

sub form {
    my $self = shift;
    $self->{'form'} = shift if $#_ != -1;
    return $self->{'form'} ||= $self->cgix->get_form;
}

sub cookies {
    my $self = shift;
    $self->{'cookies'} = shift if $#_ != -1;
    return $self->{'cookies'} ||= $self->cgix->get_cookies;
}

sub delete_cookie {
  my $self = shift;
  my $key  = $self->key_cookie;
  delete $self->cookies->{$key};
  $self->cgix->set_cookie({
    -name    => $key,
    -value   => '',
    -expires => '-10y',
    -path    => '/',
  });
}

sub set_cookie {
  my $self = shift;
  my $key  = $self->key_cookie;
  my $val  = shift || '';
  my $save_pass = shift;
  $self->cgix->set_cookie({
    -name    => $key,
    -value   => $val,
    ($save_pass ? (-expires => '+10y') : ()),
    -path    => '/',
  });
}

sub location_bounce {
    my $self = shift;
    my $url  = shift;
    return $self->cgix->location_bounce($url);
}

###----------------------------------------------------------------###

sub key_logout      { shift->{'key_logout'}      ||= 'logout'       }
sub key_cookie      { shift->{'key_cookie'}      ||= 'cea_cookie'   }
sub key_user        { shift->{'key_user'}        ||= 'cea_user'     }
sub key_pass        { shift->{'key_pass'}        ||= 'cea_pass'     }
sub key_time        { shift->{'key_time'}        ||= 'cea_time'     }
sub key_save        { shift->{'key_save'}        ||= 'cea_save'     }
sub form_name       { shift->{'form_name'}       ||= 'cea_form'     }
sub key_verify      { shift->{'key_verify'}      ||= 'cea_verify'   }
sub use_plaintext   { shift->{'use_plaintext'}   ||= 0              }
sub expires_min     { shift->{'expires_min'}     ||= 6 * 60         }
sub key_redirect    { shift->{'key_redirect'}    ||= 'cea_redirect' }
sub key_payload     { shift->{'key_payload'}     ||= 'cea_payload'  }

sub logout_redirect {
    my $self = shift;
    return $self->{'logout_redirect'} || $self->script_name ."?loggedout=1";
}

sub js_path {
    my $self = shift;
    return $self->{'js_path'} ||= $self->script_name ."/js";
}

###----------------------------------------------------------------###

sub no_cookies_print {
    my $self = shift;
    $self->cgix->print_content_type;
    print qq{<div style="border: 2px solid black;background:red;color:white">You do not appear to have cookies enabled.</div>};
    return 1;
}

sub login_print {
    my $self = shift;
    my $hash = $self->login_hash_swap;

    ### allow for a hooked override
    if (my $meth = $self->{'hook_print'}) {
        $self->$meth($hash);
        return 0;
    }

    ### get, fill, and print a basic login template
    my $text = ""
        . $self->login_header
        . $self->login_form
        . $self->login_script
        . $self->login_footer;

    ### process the document
    require CGI::Ex::Template;
    my $t   = CGI::Ex::Template->new($self->template_args);
    my $out = '';
    my $status = $t->process(\$text, $hash, \$out) || die $Template::ERROR;

    ### fill in form fields
    require CGI::Ex::Fill;
    CGI::Ex::Fill::form_fill(\$out, $hash);

    ### print it
    $self->cgix->print_content_type;
    print $out;

    return 0;
}

sub template_args {
    my $self = shift;
    return $self->{'template_args'} ||= {
        INCLUDE_PATH => $self->template_include_path,
    };
}

sub template_include_path { shift->{'template_include_path'} || '' }

sub login_hash_swap {
    my $self = shift;
    my $form = $self->form;

    return {
        %$form,
        error           => ($form->{'login_error'}) ? "Login Failed" : "",
        key_user        => $self->key_user,
        key_pass        => $self->key_pass,
        key_time        => $self->key_time,
        key_save        => $self->key_save,
        key_payload     => $self->key_payload,
        key_redirect    => $self->key_redirect,
        form_name       => $self->form_name,
        script_name     => $self->script_name,
        path_info       => $self->path_info,
        md5_js_path     => $self->js_path ."/CGI/Ex/md5.js",
        use_plaintext   => $self->use_plaintext,
        expires_min     => $self->expires_min,
        $self->key_user => $self->last_token->{'user'} || '',
        $self->key_pass => '', # don't allow for this to get filled into the form
        $self->key_time => $self->server_time,
    };
}

###----------------------------------------------------------------###

sub hook_success {
  my $self = shift;
  my $user = shift;
  $self->{'user'} = $ENV{'REMOTE_USER'} = $user;

  if (my $meth = $self->{'hook_success'}) {
    $self->$meth($user);
  }

  return 1;
}

sub user {
  my $self = shift;
  return $self->{'user'};
}

###----------------------------------------------------------------###

sub last_token { shift->{'_last_token'} }

sub verify_token {
    my $self  = shift;
    my $token = shift;

    ### parse token for info
    my $secure_hash = ($token =~ s| /sh\.(\d+\.\d+) $ ||x) ? $1 : '';
    if ($token =~ m|^ ([^/]+) / (\d+) / (-?\d+) / ([^/]*) / (.+) $|x) {
        my $h = $self->{'_last_token'} = {
            type  => 'cram',
            user  => $1,
            time  => $2,
            save  => ($3 <= 0 ? 1 : 0),
            exp   => $3,
            load  => $4,
            pass  => $5,
            token => $token,
        };
    } else {
        $self->{'_last_token'} = {
            fail  => 'Invalid token',
            token => $token,
        };
        return 0;
    }

    ### verify the user
    my $token = $self->{'_last_token'};
    if (! defined($token->{'user'}) || ! $self->verify_user($token->{'user'})) {
        $token->{'fail'} = 'Invalid User';
        return 0;
    }

    ### get the pass
    my $pass = eval { $self->get_pass_by_user($token) };
    if (! defined $pass || $@) {
        $token->{'fail'} = 'Could not get pass';
        $token->{'fail_error'} = $@;
        return 0;
    }
    $token->{'pass_real'} = ($pass =~ /^[a-f0-9]{32}$/) ? lc($pass) : md5_hex($pass);

    if ($secure_hash) {
        $self->{'_last_token'}->{'fail'} = 'secure_hash not implemented';
        return 0;

    } elsif ($token->{'type'} eq 'cram') {
        my $str = join("/", @{$token}{qw(user time exp load)});
        my $sum = md5_hex($str .'/'. $token->{'pass_real'});
        if ($token->{'pass'} ne $sum) {
            $token->{'fail'} = "Did not match";
        } elsif (! $token->{'save'}
                 && ($self->server_time - $token->{'time'}) > $token->{'exp'} * 60) {
            $token->{'fail'} = "Login Expired";
        } else {
            return 1;
        }

    } elsif ($self->use_plaintext && $pass =~ m|^([./0-9A-Za-z]{2})(.{,11})$|) {
        $token->{'type'} = 'used_crypt';
        return 1 if crypt($token->{'pass'}, $1) eq $pass;
        $token->{'fail'} = "Did not match";

    } else {
        $token->{'type'} = 'plain_or_md5';
        my $test_pass = ($token->{'pass'} =~ /^[a-f0-9]{32}$/i) ? lc($token->{'pass'}) : md5_hex($token->{'pass'});
        return 1 if $test_pass eq $token->{'pass_real'};
        $token->{'fail'} = "Did not match";
    }

    return 0;
}

sub generate_token {
    my $self  = shift;
    my $token = shift || $self->last_token;
    if ($token->{'type'} eq 'cram') {
        my $str = join("/", $token->{'user'}, $self->server_time, ($token->{'save'} ? -1 : $token->{'exp'}), $token->{'load'});
        my $sum = md5_hex($str .'/'. $token->{'pass_real'});
        return $str .'/'. $sum;
    } else {
        die "Unsupported type $token->{type}";
    }
}

sub verify_user {
    my $self = shift;
    my $user = shift;
    if (my $meth = $self->{'verify_user'}) {
        return $self->$meth($user);
    }
    return 1;
}

sub get_pass_by_user {
    my $self = shift;
    my $user = shift;
    if (my $meth = $self->{'get_pass_by_user'}) {
        return $self->$meth($user);
    }

    die "Please override get_pass_by_user";
}

###----------------------------------------------------------------###

sub login_header {
    return shift->{'login_header'} || qq {
    [%~ TRY ; PROCESS 'login_header' ; CATCH %]<!-- [% error %] -->[% END ~%]
    };
}

sub login_footer {
    return shift->{'login_footer'} || qq {
    [%~ TRY ; PROCESS 'login_footer' ; CATCH %]<!-- [% error %] -->[% END ~%]
    };
}

sub login_form {
    return shift->{'login_form'} || qq {
    <div class="login_chunk">
    <span class="login_error">[% error %]</span>
    <form class="login_form" name="[% form_name %]" method="post" action="[% script_name %]">
    <input type="hidden" name="[% key_redirect %]" value="">
    <input type="hidden" name="[% key_payload %]" value="">
    <input type="hidden" name="[% key_time %]" value="">
    <table class="login_table">
    <tr class="login_username">
      <td>Username:</td>
      <td><input name="[% key_user %]" type="text" size="30" value=""></td>
    </tr>
    <tr class="login_password">
      <td>Password:</td>
      <td><input name="[% key_pass %]" type="password" size="30" value=""></td>
    </tr>
    <tr class="login_save">
      <td colspan="2">
        <input type="checkbox" name="[% key_save %]" value="1"> Save Password ?
      </td>
    </tr>
    <tr class="login_submit">
      <td colspan="2" align="right">
        <input type="submit" value="Submit">
      </td>
    </tr>
    </table>
    </form>
    </div>
};
}

sub login_script {
  return qq {
    [%~ IF ! use_plaintext %]
    <script src="[% md5_js_path %]"></script>
    <script>
    if (document.md5_hex) document.[% form_name %].onsubmit = function () {
      var f = document.[% form_name %];
      var u = f.[% key_user %].value;
      var p = f.[% key_pass %].value;
      var t = f.[% key_time %].value;
      var s = f.[% key_save %].checked ? -1 : [% expires_min %];
      var l = f.[% key_payload %].value;

      var str = u+'/'+t+'/'+s+'/'+l;
      var sum = document.md5_hex(str +'/' + document.md5_hex(p));
      var loc = f.action + '?[% key_user %]='+escape(str +'/'+ sum);

      location.href = loc;
      return false;
    }
    </script>
    [% END ~%]
  };
}

###----------------------------------------------------------------###

1;

__END__

=head1 SYNOPSIS

  ### authorize the user
  my $auth = $self->auth({
    hook_get_pass_by_user => \&get_pass_by_user,
  });


  sub get_pass_by_user {
    my $auth = shift;
    my $username = shift;
    my $host = shift;
    my $password = some_way_of_getting_password;
    return $password;
  }

  sub my_print {
    my $auth = shift;
    my $step = shift;
    my $form = shift; # form includes login_script at this point
    my $content = get_content_from_somewhere;
    $auth->cgix->swap_template(\$content, $form);
    $auth->cgix->print_content_type;
    print $content;
  }

=head1 DESCRIPTION

CGI::Ex::Auth allows for autoexpiring, safe logins.  Auth uses
javascript modules that perform SHA1 and MD5 encoding to encode
the password on the client side before passing them through the
internet.

If SHA1 is used the storage of the password can be described by
the following code:

  my $pass = "plaintextpassword";
  my $save = ($save_the_password) ? 1 : 0;
  my $time = time;
  my $store = sha1_hex("$time/$save/" . sha1_hex($pass));

This allows for passwords to be stored as sha1 in a database.
Passwords stored in the database this way are still susceptible to bruteforce
attack, but are much more secure than storing plain text.

If MD5 is used, the above procedure is replaced with md5_hex.

A downside to this module is that it does not use a session to preserve state
so authentication has to happen on every request.  A plus is that you don't
need to use a session.  With later releases, a method will be added to allow
authentication to look inside of a stored session somewhat similar to
CGI::Session::Auth.

=head1 METHODS

=over 4

=item C<new>

Constructor.  Takes a hash or hashref of properties as arguments.

=item C<init>

Called automatically near the end of new.

=item C<require_auth>

Performs the core logic.  Returns true on successful login.
Returns false on failed login.  If a false value is returned,
execution of the CGI should be halted.  require_auth WILL
NOT automatically stop execution.

  $auth->require_auth || exit;

=item C<print_login_page>

Called if login failed.  Defaults to printing a very basic page
loaded from login_template.  The basic login template can be updated.

You will want to override it with a template from your own system.
The hook that is called will be passed the step to print (currently
only "get_login_info" and "no_cookies"), and a hash containing the
form variables as well as the following:

  error        - The error that occurred (if any)
  key_user     - $self->key_user;
  key_pass     - $self->key_pass;
  key_save     - $self->key_save;
  key_redirect - $self->key_redirect;
  form_name    - $self->form_name;
  script_name  - $ENV{SCRIPT_NAME}
  path_info    - $ENV{PATH_INFO} || ''

=item C<success>

Method called on successful login.  Sets $self->user as well as $ENV{REMOTE_USER}.

=item C<user>

Returns the user that was successfully logged in (undef if no success).

=item C<key_logout>

If a key is passed the form hash that matches this key, the current user will
be logged out.  Default is "logout".

=item C<key_cookie>

The name of the auth cookie.  Default is "ce_auth".

=item C<key_cookie_check>

A field name used during a bounce to see if cookies exist.  Default is "ccheck".

=item C<key_user>

The form field name used to pass the username.  Default is "ce_user".

=item C<key_pass>

The form field name used to pass the password.  Default is "ce_pass".

=item C<key_save>

The form field name used to pass whether they would like to save the cookie for
a longer period of time.  Default is "ce_save".  The value of this form field
should be 1 or 0.  If it is zero, the cookie installed will be a session cookie
and will expire in expires_min seconds (default of 6 hours).

=item C<form_name>

The name of the html login form to attach the javascript to.  Default is "ce_form".

=item C<verify_userpass>

Called to verify the passed form information or the stored cookie.  Calls hook_verify_userpass.

=item C<hook_verify_userpass>

Called by verify_userpass.  Arguments are the username, cookie or info to be tested,
and the hostname.  Default method calls hook_get_pass_by_user to get the real password.
Then based upon how the real password is stored (sha1, md5, plaintext, or crypted) and
how the login info was passed from the html form (or javascript), will attempt to compare
the two and return success or failure.  It should be noted that if the javascript method
used is SHA1 and the password is stored crypted or md5'ed - the comparison will not work
and the login will fail.  SHA1 logins require either plaintext password or sha1 stored passwords.
MD5 logins require either plaintext password or md5 stored passwords.  Plaintext logins
allow for SHA1 or MD5 or crypted or plaintext storage - but should be discouraged because
they are plaintext and the users password can be discovered.

=item C<hook_get_pass_by_user>

Called by hook_verify_userpass.  Arguments are the username and hostname.  Should return
a sha1 password, md5 password, plaintext password, or crypted password depending
upon which system is being used to get the information from the user.

=item C<set_hook_get_pass_by_user>

Allows for setting the subref used by hook_get_pass_by_user.x

=item C<cgix>

Returns a CGI::Ex object.

=item C<form>

A hash of passed form info.  Defaults to CGI::Ex::get_form.

=item C<cookies>

The current cookies.  Defaults to CGI::Ex::get_cookies.

=item C<host>

What host are we on.  Defaults to a cleaned $ENV{HTTP_HOST}.

=item C<login_page>

Calls the basic_login_template, swaps in the form variables (including
form name, login_script, etc).  Then prints content_type, the content, and
returns.

=item C<login_template>

Returns a bare essentials form that will handle the login.  Has place
holders for all of the form name, and login variables, and errors and
login javascript.  Variable place holders are of the form
[% form_name %] which should work with Template::Toolkit or CGI::Ex::swap_template.

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut
