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

sub get_valid_auth {
    my $self = shift;
    $self = $self->new(@_) if ! ref $self;

    ### shortcut that will print a js file as needed (such as the md5.js)
    if ($self->script_name . $self->path_info eq $self->js_path . "/CGI/Ex/md5.js") {
        $self->cgix->print_js('CGI/Ex/md5.js');
        eval { die "Printed Javascript" };
        return;
    }

    my $form    = $self->form;
    my $cookies = $self->cookies;
    my $has_cookies = scalar %$cookies;

    ### allow for logout
    my $key_l   = $self->key_logout;
    if ($form->{$key_l}) {
        $self->delete_cookie;
        $self->location_bounce($self->logout_redirect);
        eval { die "Logging out" };
        return;
    }

    my $had_form_info;
    foreach ([$form,    $self->key_user,   1],
             [$cookies, $self->key_cookie, 0],
             ) {
        my ($hash, $key, $is_form) = @$_;
        next if ! defined $hash->{$key};
        $had_form_info ++ if $is_form;

        ### if it looks like a bare username (as in they didn't have javascript)- add in other items
        if ($hash->{$key} !~ m|^[^/]+/| && $is_form) {
            $hash->{$key} = {
                user        => $hash->{$key},
                test_pass   => delete $hash->{ $self->key_pass },
                expires_min => delete($hash->{ $self->key_save }) ? -1 : delete($hash->{ $self->key_expires_min }) || $self->expires_min,
                payload     => delete $hash->{ $self->key_payload } || '',
            };
        }

        my $data = $self->verify_token({token => $hash->{$key}, from => ($is_form ? 'form' : 'cookie')}) || next;

        ### generate a fresh cookie if they submitted info - or if it is a non plaintext type
        if ($is_form
            || (! $data->{'use_plaintext'} && ! $data->{'type'} eq 'plaintext_crypt')) {
            $self->set_cookie($self->generate_token($data), $data->{'expires_min'});
        }

        #return $self->success($user); # assume that cookies will work - if not next page will cause login
        #### this may actually be the nicer thing to do in the common case - except for the nasty looking
        #### url - all things considered - should really get location boucing to work properly while being
        #### able to set a cookie at the same time

        if ($has_cookies) {
#            $self->success($data->{'user'}); # assuming if they have cookies - the one we set will work
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
            eval { die "Success login - bouncing to test cookie" };
            return;
        }
    }

    ### make sure the cookie is gone
    $self->delete_cookie if $cookies->{ $self->key_cookie };

    ### nothing found - see if they have cookies
    if (my $value = delete $form->{$self->key_verify}) {
        if (abs(time() - $value) < 15) {
            $self->no_cookies_print;
            return;
        }
    }

    ### oh - you're still here - well then - ask for login credentials
    my $key_r = $self->key_redirect;
    if (! $form->{$key_r}) {
        my $query = $self->cgix->make_form($form);
        $form->{$key_r} = $self->script_name . $self->path_info . ($query ? "?$query" : "");
    }

    $form->{'had_form_data'} = $had_form_info;
    $self->login_print;
    my $data = $self->last_auth_data;
    eval { die defined($data) ? $data : "Requesting credentials" };
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
    return $self->{'form'} ||= $self->cgix->get_form || {};
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
  $self->cgix->set_cookie({
    -name    => $key,
    -value   => $val,
    -expires => '+20y', # let the expires time take care of things
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
sub key_expires_min { shift->{'key_expires_min'} ||= 'cea_expires_min' }
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
    if (my $meth = $self->{'login_print'}) {
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
    $t->process_simple(\$text, $hash, \$out) || die $t->error;

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
    my $data = $self->last_auth_data;
    $data = {} if ! defined $data;

    return {
        %$form,
        error           => ($form->{'had_form_data'}) ? "Login Failed" : "",
        login_data      => $data,
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
        $self->key_user => $data->{'user'} || '',
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

sub new_auth {
    my $self = shift;
    return CGI::Ex::Auth::Data->new(@_);
}

sub last_auth_data { shift->{'_last_auth_data'} }

sub verify_token {
    my $self  = shift;
    my $args  = shift;
    my $token = delete $args->{'token'} || die "Missing token";
    my $data  = $self->{'_last_auth_data'} = $self->new_auth({token => $token, use_plaintext => $self->use_plaintext, %$args});

    ### token already parsed
    if (ref $token) {
        $data->add_data({%$token});

    ### parse token for info
    } else {
        if ($token =~ m|^ ([^/]+) / (\d+) / (-?\d+) / (.*) / ([a-fA-F0-9]{32}) (?: / (sh\.\d+\.\d+))? $|x) {
            $data->add_data({
                user        => $1,
                cram_time   => $2,
                expires_min => $3,
                payload     => $4,
                test_pass   => $5,
                secure_hash => $6 || '',
            });
        } elsif ($token =~ m|^ ([^/]+) / (.*) $|x) {
            $data->add_data({
                user      => $1,
                test_pass => $2,
            });
        } else {
            $data->error('Invalid token');
            return $data;
        }
    }


    ### verify the user and get the pass
    my $pass;
    if (! defined($data->{'user'})) {
        $data->error('Missing user');

    } elsif (! $self->verify_user($data->{'user'})) {
        $data->error('Invalid user');

    } elsif (! defined($pass = eval { $self->get_pass_by_user($data->{'user'}) })) {
        $data->add_data({details => $@});
        $data->error('Could not get pass');
    }
    return $data if $data->error;


    ### store - to allow generate_token to not need to relookup the pass
    $data->add_data({real_pass => $pass});


    ### looks like a secure_hash cram
    if ($data->{'secure_hash'}) {
        $data->add_data(type => 'secure_hash_cram');
        my $array = eval {$self->secure_hash_keys };
        if (! $array) {
            $data->error('secure_hash_keys not implemented');
        } elsif (! @$array) {
            $data->error('secure_hash_keys empty');
        } elsif ($data->{'use_plaintext'}) {
            $data->error('Found secure_hash_cram during use_plaintext');
        } else {
            $data->error('secure_hash not implemented yet');
        }

    ### looks like a normal cram
    } elsif ($data->{'cram_time'}) {
        $data->add_data(type => 'cram');
        my $real = $data->{'real_pass'} =~ /^[a-f0-9]{32}$/ ? lc($data->{'real_pass'}) : md5_hex($data->{'real_pass'});
        my $str  = join("/", @{$data}{qw(user cram_time expires_min payload)});
        my $sum  = md5_hex($str .'/'. $real);

        if ($data->{'use_plaintext'}) {
            $data->error('Found cram during use_plaintext');

        } elsif ($data->{'expires_min'} > 0
                 && ($self->server_time - $data->{'cram_time'}) > $data->{'expires_min'} * 60) {
            $data->error('Login expired');

        } elsif (lc($data->{'test_pass'}) ne $sum) {
            $data->error('Invalid login');

        }

    ### plaintext_crypt
    } elsif ($data->{'real_pass'} =~ m|^([./0-9A-Za-z]{2})([./0-9A-Za-z]{,11})$|
             && crypt($data->{'test_pass'}, $1) eq $data->{'real_pass'}) {
        $data->add_data(type => 'plaintext_crypt');

    ### plaintext_plaintext, plaintext_md5, md5_plaintext, md5_md5
    } else {
        my $is_md5_t = $data->{'test_pass'} =~ /^[a-f0-9]{32}$/;
        my $is_md5_r = $data->{'real_pass'} =~ /^[a-f0-9]{32}$/;
        my $test = $is_md5_t ? lc($data->{'test_pass'}) : md5_hex($data->{'test_pass'});
        my $real = $is_md5_r ? lc($data->{'real_pass'}) : md5_hex($data->{'real_pass'});
        my $type = ($is_md5_t ? 'md5' : 'plaintext') .'_'. ($is_md5_r ? 'md5' : 'plaintext');
        $data->add_data(type => $type);
        $data->error('Invalid login')
            if $test ne $real;
    }

    ### check the payload
    if (! $data->error && ! $self->verify_payload($data->{'payload'})) {
        $data->error('Invalid payload');
    }

    return $data;
}

sub generate_token {
    my $self  = shift;
    my $data  = shift || $self->last_auth_data;
    die "Can't generate a token off of a failed auth" if ! $data;
    if ($data->{'use_plaintext'} || $data->{'type'} eq 'plaintext_crypt') {
        return $data->{'user'} .'/'. $data->{'real_pass'};

    } elsif ($data->{'type'} eq 'secure_hash_cram') {
        die "Unsupported type $data->{type}";

    } else { # cram, plaintext_plaintext, md5_plaintext, plaintext_md5, md5_md5
        my $real = $data->{'real_pass'} =~ /^[a-f0-9]{32}$/ ? lc($data->{'real_pass'}) : md5_hex($data->{'real_pass'});
        my $str  = join("/", $data->{'user'}, $self->server_time, $data->{'expires_min'}, $data->{'payload'});
        my $sum  = md5_hex($str .'/'. $real);
        return $str .'/'. $sum;
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

sub verify_payload {
    my $self    = shift;
    my $payload = shift;
    if (my $meth = $self->{'verify_payload'}) {
        return $self->$meth($payload);
    }
    return 1;
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

package CGI::Ex::Auth::Data;

use strict;
use overload
    'bool'   => sub { ! shift->error },
    '0+'     => sub { 1 },
    '""'     => sub { shift->as_string },
    fallback => 1;

sub new {
    my ($class, $args) = @_;
    return bless {%{ $args || {} }}, $class;
}

sub add_data {
    my $self = shift;
    my $args = @_ == 1 ? shift : {@_};
    @{ $self }{keys %$args} = values %$args;
}

sub error {
    my $self = shift;
    if (@_ == 1) {
        $self->{'error'} = shift;
        $self->{'error_caller'} = [caller];
    }
    return $self->{'error'};
}

sub as_string {
    my $self = shift;
    return $self->error || ($self->{'user'} && $self->{'type'}) ? "Valid auth data" : "Unverified auth data";
}

###----------------------------------------------------------------###

1;

__END__

=head1 SYNOPSIS

  ### authorize the user
  my $auth = $self->get_valid_auth({
    get_pass_by_user => \&get_pass_by_user,
  });


  sub get_pass_by_user {
    my $auth = shift;
    my $user = shift;
    my $pass = some_way_of_getting_password($user);
    return $pass;
  }

=head1 DESCRIPTION

CGI::Ex::Auth allows for auto-expiring, safe logins.  Auth uses
javascript modules that perform MD5 encoding to encode
the password on the client side before passing them through the
internet.

A downside to this module is that it does not use a session to preserve state
so authentication has to happen on every request.  A plus is that you don't
need to use a session.  It is up to the interested reader to add session caching
to the get_pass_by_user method.

=head1 METHODS

=over 4

=item C<new>

Constructor.  Takes a hash or hashref of properties as arguments.

=item C<get_valid_auth>

Performs the core logic.  Returns an auth object on successful login.
Returns false on errored login (with the details of the error stored in
$@).  If a false value is returned, execution of the CGI should be halted.
get_valid_auth WILL NOT automatically stop execution.

  $auth->get_valid_auth || exit;

=item C<login_print>

Called if login errored.  Defaults to printing a very basic page
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
the two and return success or errorure.  It should be noted that if the javascript method
used is SHA1 and the password is stored crypted or md5'ed - the comparison will not work
and the login will fail.  MD5 logins require either plaintext password or md5 stored passwords.  Plaintext logins
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
