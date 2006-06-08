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

$VERSION = '2.02';

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
    if ($self->script_name . $self->path_info eq $self->js_uri_path . "/CGI/Ex/md5.js") {
        $self->cgix->print_js('CGI/Ex/md5.js');
        eval { die "Printed Javascript" };
        return;
    }

    my $form    = $self->form;
    my $cookies = $self->cookies;
    my $key_l   = $self->key_logout;
    my $key_c   = $self->key_cookie;
    my $has_cookies = scalar %$cookies;

    ### allow for logout
    if ($form->{$key_l}) {
        $self->delete_cookie({key => $key_c});;
        $self->location_bounce($self->logout_redirect);
        eval { die "Logging out" };
        return;
    }

    my $had_form_info;
    foreach ([$form,    $self->key_user, 1],
             [$cookies, $key_c,          0],
             ) {
        my ($hash, $key, $is_form) = @$_;
        next if ! defined $hash->{$key};
        $had_form_info ++ if $is_form;

        ### if it looks like a bare username (as in they didn't have javascript)- add in other items
        my $data;
        if ($is_form
            && $hash->{$key} !~ m|^[^/]+/|
            && defined $hash->{ $self->key_pass }) {
            $data = $self->verify_token({
                token => {
                    user        => delete $hash->{$key},
                    test_pass   => delete $hash->{ $self->key_pass },
                    expires_min => delete($hash->{ $self->key_save }) ? -1 : delete($hash->{ $self->key_expires_min }) || $self->expires_min,
                    payload     => delete $hash->{ $self->key_payload } || '',
                },
                from => 'form',
            }) || next;

        } else {
            $data = $self->verify_token({token => $hash->{$key}, from => ($is_form ? 'form' : 'cookie')}) || next;
            delete $hash->{$key} if $is_form;
        }

        ### generate a fresh cookie if they submitted info on plaintext types
        if ($self->use_plaintext || ($data->{'type'} && $data->{'type'} eq 'crypt')) {
            $self->set_cookie({
                key        => $key_c,
                val        => $self->generate_token($data),
                no_expires => ($data->{ $self->key_save } ? 0 : 1), # make it a session cookie unless they ask for saving
            }) if $is_form; # only set the cookie if we found info in the form - the cookie will be a session cookie after that

        ### always generate a cookie on types that have expiration
        } else {
            $self->set_cookie({
                key        => $key_c,
                val        => $self->generate_token($data),
                no_expires => 0,
            });
        }

        ### successful login

        ### bounce to redirect
        if (my $redirect = $form->{ $self->key_redirect }) {
            $self->location_bounce($redirect);
            eval { die "Success login - bouncing to redirect" };
            return;

        ### if they have cookies we are done
        } elsif ($has_cookies || $self->no_cookie_verify) {
            return $self;

        ### need to verify cookies are set-able
        } elsif ($is_form) {
            $form->{$self->key_verify} = $self->server_time;
            my $query = $self->cgix->make_form($form);
            my $url   = $self->script_name . $self->path_info . ($query ? "?$query" : "");

            $self->location_bounce($url);
            eval { die "Success login - bouncing to test cookie" };
            return;
        }
    }

    ### make sure the cookie is gone
    $self->delete_cookie({key => $key_c}) if $cookies->{$key_c};

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

    ### allow for a sleep to help prevent brute force
    sleep($self->failed_sleep) if defined($data) && $data->error ne 'Login expired';

    return;
}

###----------------------------------------------------------------###

sub script_name { shift->{'script_name'} || $ENV{'SCRIPT_NAME'} || die "Missing SCRIPT_NAME" }

sub path_info { shift->{'path_info'} || $ENV{'PATH_INFO'} || '' }

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
    my $args = shift;
    my $key  = $args->{'key'};
    $self->cgix->set_cookie({
        -name    => $key,
        -value   => '',
        -expires => '-10y',
        -path    => '/',
    });
    delete $self->cookies->{$key};
}

sub set_cookie {
    my $self = shift;
    my $args = shift;
    my $key  = $args->{'key'};
    my $val  = $args->{'val'};
    $self->cgix->set_cookie({
        -name    => $key,
        -value   => $val,
        ($args->{'no_expires'} ? () : (-expires => '+20y')), # let the expires time take care of things for types that self expire
        -path    => '/',
    });
    $self->cookies->{$key} = $val;
}

sub location_bounce {
    my $self = shift;
    my $url  = shift;
    return $self->cgix->location_bounce($url);
}

###----------------------------------------------------------------###

sub key_logout       { shift->{'key_logout'}       ||= 'cea_logout'   }
sub key_cookie       { shift->{'key_cookie'}       ||= 'cea_user'     }
sub key_user         { shift->{'key_user'}         ||= 'cea_user'     }
sub key_pass         { shift->{'key_pass'}         ||= 'cea_pass'     }
sub key_time         { shift->{'key_time'}         ||= 'cea_time'     }
sub key_save         { shift->{'key_save'}         ||= 'cea_save'     }
sub key_expires_min  { shift->{'key_expires_min'}  ||= 'cea_expires_min' }
sub form_name        { shift->{'form_name'}        ||= 'cea_form'     }
sub key_verify       { shift->{'key_verify'}       ||= 'cea_verify'   }
sub key_redirect     { shift->{'key_redirect'}     ||= 'cea_redirect' }
sub key_payload      { shift->{'key_payload'}      ||= 'cea_payload'  }
sub secure_hash_keys { shift->{'secure_hash_keys'} ||= []             }
sub no_cookie_verify { shift->{'no_cookie_verify'} ||= 0              }
sub use_crypt        { shift->{'use_crypt'}        ||= 0              }
sub use_blowfish     { shift->{'use_blowfish'}     ||= ''             }
sub use_plaintext    { my $s = shift; $s->use_crypt || ($s->{'use_plaintext'} ||= 0) }
sub use_base64       { my $s = shift; $s->{'use_base64'}  = 1      if ! defined $s->{'use_base64'};  $s->{'use_base64'}  }
sub expires_min      { my $s = shift; $s->{'expires_min'} = 6 * 60 if ! defined $s->{'expires_min'}; $s->{'expires_min'} }
sub failed_sleep     { shift->{'failed_sleep'}     ||= 3              }

sub logout_redirect {
    my $self = shift;
    return $self->{'logout_redirect'} || $self->script_name ."?loggedout=1";
}

sub js_uri_path {
    my $self = shift;
    return $self->{'js_uri_path'} ||= $self->script_name ."/js";
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
    my $hash = $self->login_hash_common;
    my $template = $self->login_template;

    ### allow for a hooked override
    if (my $meth = $self->{'login_print'}) {
        $meth->($self, $template, $hash);
        return 0;
    }

    ### process the document
    require CGI::Ex::Template;
    my $cet = CGI::Ex::Template->new($self->template_args);
    my $out = '';
    $cet->process_simple($template, $hash, \$out) || die $cet->error;

    ### fill in form fields
    require CGI::Ex::Fill;
    CGI::Ex::Fill::fill({text => \$out, form => $hash});

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

sub login_hash_common {
    my $self = shift;
    my $form = $self->form;
    my $data = $self->last_auth_data;
    $data = {} if ! defined $data;

    return {
        %$form,
        error              => ($form->{'had_form_data'}) ? "Login Failed" : "",
        login_data         => $data,
        key_user           => $self->key_user,
        key_pass           => $self->key_pass,
        key_time           => $self->key_time,
        key_save           => $self->key_save,
        key_expires_min    => $self->key_expires_min,
        key_payload        => $self->key_payload,
        key_redirect       => $self->key_redirect,
        form_name          => $self->form_name,
        script_name        => $self->script_name,
        path_info          => $self->path_info,
        md5_js_path        => $self->js_uri_path ."/CGI/Ex/md5.js",
        use_plaintext      => $self->use_plaintext,
        $self->key_user    => $data->{'user'} || '',
        $self->key_pass    => '', # don't allow for this to get filled into the form
        $self->key_time    => $self->server_time,
        $self->key_payload => $self->generate_payload({%$data, login_form => 1}),
        $self->key_expires_min => $self->expires_min,

    };
}

###----------------------------------------------------------------###

sub verify_token {
    my $self  = shift;
    my $args  = shift;
    my $token = delete $args->{'token'} || die "Missing token";
    my $data  = $self->{'_last_auth_data'} = $self->new_auth_data({token => $token, %$args});

    ### token already parsed
    if (ref $token) {
        $data->add_data({%$token, armor => 'none'});

    ### parse token for info
    } else {
        my $found;
        my $key;
        for my $armor ('none', 'base64', 'blowfish') { # try with and without base64 encoding
            my $copy = ($armor eq 'none')           ? $token
                     : ($armor eq 'base64')         ? decode_base64($token)
                     : ($key = $self->use_blowfish) ? decrypt_blowfish($token, $key)
                     : next;
            if ($copy =~ m|^ ([^/]+) / (\d+) / (-?\d+) / (.*) / ([a-fA-F0-9]{32}) (?: / (sh\.\d+\.\d+))? $|x) {
                $data->add_data({
                    user         => $1,
                    cram_time    => $2,
                    expires_min  => $3,
                    payload      => $4,
                    test_pass    => $5,
                    secure_hash  => $6 || '',
                    armor        => $armor,
                });
                $found = 1;
                last;
            } elsif ($copy =~ m|^ ([^/]+) / (.*) $|x) {
                $data->add_data({
                    user         => $1,
                    test_pass    => $2,
                    armor        => $armor,
                });
                $found = 1;
                last;
            }
        }
        if (! $found) {
            $data->error('Invalid token');
            return $data;
        }
    }


    ### verify the user and get the pass
    my $pass;
    if (! defined($data->{'user'})) {
        $data->error('Missing user');

    } elsif (! defined $data->{'test_pass'}) {
        $data->error('Missing test_pass');

    } elsif (! $self->verify_user($data->{'user'} = $self->cleanup_user($data->{'user'}))) {
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
            $data->error('secure_hash_keys not found');
        } elsif (! @$array) {
            $data->error('secure_hash_keys empty');
        } elsif ($data->{'secure_hash'} !~ /^sh\.(\d+)\.(\d+)$/ || $1 > $#$array) {
            $data->error('Invalid secure hash');
        } else {
            my $rand1 = $1;
            my $rand2 = $2;
            my $real  = $data->{'real_pass'} =~ /^[a-f0-9]{32}$/ ? lc($data->{'real_pass'}) : md5_hex($data->{'real_pass'});
            my $str  = join("/", @{$data}{qw(user cram_time expires_min payload)});
            my $sum = md5_hex($str .'/'. $real .('/sh.'.$array->[$rand1].'.'.$rand2));
            if ($data->{'expires_min'} > 0
                && ($self->server_time - $data->{'cram_time'}) > $data->{'expires_min'} * 60) {
                $data->error('Login expired');
            } elsif (lc($data->{'test_pass'}) ne $sum) {
                $data->error('Invalid login');
            }
        }

    ### looks like a normal cram
    } elsif ($data->{'cram_time'}) {
        $data->add_data(type => 'cram');
        my $real = $data->{'real_pass'} =~ /^[a-f0-9]{32}$/ ? lc($data->{'real_pass'}) : md5_hex($data->{'real_pass'});
        my $str  = join("/", @{$data}{qw(user cram_time expires_min payload)});
        my $sum  = md5_hex($str .'/'. $real);
        if ($data->{'expires_min'} > 0
                 && ($self->server_time - $data->{'cram_time'}) > $data->{'expires_min'} * 60) {
            $data->error('Login expired');
        } elsif (lc($data->{'test_pass'}) ne $sum) {
            $data->error('Invalid login');
        }

    ### plaintext_crypt
    } elsif ($data->{'real_pass'} =~ m|^([./0-9A-Za-z]{2})([./0-9A-Za-z]{11})$|
             && crypt($data->{'test_pass'}, $1) eq $data->{'real_pass'}) {
        $data->add_data(type => 'crypt', was_plaintext => 1);

    ### failed plaintext crypt
    } elsif ($self->use_crypt) {
        $data->error('Invalid login');
        $data->add_data(type => 'crypt', was_plaintext => ($data->{'test_pass'} =~ /^[a-f0-9]{32}$/ ? 0 : 1));

    ### plaintext and md5
    } else {
        my $is_md5_t = $data->{'test_pass'} =~ /^[a-f0-9]{32}$/;
        my $is_md5_r = $data->{'real_pass'} =~ /^[a-f0-9]{32}$/;
        my $test = $is_md5_t ? lc($data->{'test_pass'}) : md5_hex($data->{'test_pass'});
        my $real = $is_md5_r ? lc($data->{'real_pass'}) : md5_hex($data->{'real_pass'});
        $data->add_data(type => ($is_md5_r ? 'md5' : 'plaintext'), was_plaintext => ($is_md5_t ? 0 : 1));
        $data->error('Invalid login')
            if $test ne $real;
    }

    ### check the payload
    if (! $data->error && ! $self->verify_payload($data->{'payload'})) {
        $data->error('Invalid payload');
    }

    return $data;
}

sub new_auth_data {
    my $self = shift;
    return CGI::Ex::Auth::Data->new(@_);
}

sub last_auth_data { shift->{'_last_auth_data'} }

sub generate_token {
    my $self  = shift;
    my $data  = shift || $self->last_auth_data;
    die "Can't generate a token off of a failed auth" if ! $data;

    my $token;

    ### do kinds that require staying plaintext
    if (   (defined($data->{'use_plaintext'}) ?  $data->{'use_plaintext'} : $self->use_plaintext) # ->use_plaintext is true if ->use_crypt is
        || (defined($data->{'use_crypt'})     && $data->{'use_crypt'})
        || (defined($data->{'type'})          && $data->{'type'} eq 'crypt')) {
        $token = $data->{'user'} .'/'. $data->{'real_pass'};

    ### all other types go to cram - secure_hash_cram, cram, plaintext and md5
    } else {
        my $user = $data->{'user'} || die "Missing user";
        my $real = defined($data->{'real_pass'})   ? ($data->{'real_pass'} =~ /^[a-f0-9]{32}$/ ? lc($data->{'real_pass'}) : md5_hex($data->{'real_pass'}))
                                                   : die "Missing real_pass";
        my $exp  = defined($data->{'expires_min'}) ? $data->{'expires_min'} : $self->expires_min;
        my $load = $self->generate_payload($data);
        die "Payload can not contain a \"/\.  Please escape it in generate_payload." if $load =~ m|/|;
        die "User can not contain a \"/\."                                           if $user =~ m|/|;

        my $array;
        if (! $data->{'prefer_cram'}
            && ($array = eval { $self->secure_hash_keys })
            && @$array) {
            my $rand1 = int(rand @$array);
            my $rand2 = int(rand 100000);
            my $str = join("/", $user, $self->server_time, $exp, $load);
            my $sum = md5_hex($str .'/'. $real .('/sh.'.$array->[$rand1].'.'.$rand2));
            $token  = $str .'/'. $sum . '/sh.'.$rand1.'.'.$rand2;
        } else {
            my $str = join("/", $user, $self->server_time, $exp, $load);
            my $sum = md5_hex($str .'/'. $real);
            $token  = $str .'/'. $sum;
        }
    }

    if (my $key = $data->{'use_blowfish'} || $self->use_blowfish) {
        $token = encrypt_blowfish($token, $key);

    } elsif (defined($data->{'use_base64'}) ? $data->{'use_base64'} : $self->use_base64) {
        $token = encode_base64($token, '');
    }

    return $token;
}

sub generate_payload {
    my $self = shift;
    my $args = shift;
    return defined($args->{'payload'}) ? $args->{'payload'} : '';
}

sub verify_user {
    my $self = shift;
    my $user = shift;
    if (my $meth = $self->{'verify_user'}) {
        return $meth->($self, $user);
    }
    return 1;
}

sub cleanup_user {
    my $self = shift;
    my $user = shift;
    if (my $meth = $self->{'cleanup_user'}) {
        return $meth->($self, $user);
    }
    return $user;
}

sub get_pass_by_user {
    my $self = shift;
    my $user = shift;
    if (my $meth = $self->{'get_pass_by_user'}) {
        return $meth->($self, $user);
    }

    die "Please override get_pass_by_user";
}

sub verify_payload {
    my $self    = shift;
    my $payload = shift;
    if (my $meth = $self->{'verify_payload'}) {
        return $meth->($self, $payload);
    }
    return 1;
}

###----------------------------------------------------------------###

sub encrypt_blowfish {
    my ($str, $key) = @_;

    require Crypt::Blowfish;
    my $cb = Crypt::Blowfish->new($key);

    $str .= (chr 0) x (8 - length($str) % 8); # pad to multiples of 8

    my $enc = '';
    $enc .= unpack "H16", $cb->encrypt($1) while $str =~ /\G(.{8})/g; # 8 bytes at a time

    return $enc;
}

sub decrypt_blowfish {
    my ($enc, $key) = @_;

    require Crypt::Blowfish;
    my $cb = Crypt::Blowfish->new($key);

    my $str = '';
    $str .= $cb->decrypt(pack "H16", $1) while $enc =~ /\G([A-Fa-f0-9]{16})/g;
    $str =~ y/\00//d;

    return $str
}

###----------------------------------------------------------------###

sub login_template {
    my $self = shift;
    return $self->{'login_template'} if $self->{'login_template'};

    my $text = ""
        . $self->login_header
        . $self->login_form
        . $self->login_script
        . $self->login_footer;
    return \$text;
}

sub login_header {
    return shift->{'login_header'} || q {
    [%~ TRY ; PROCESS 'login_header.tt' ; CATCH %]<!-- [% error %] -->[% END ~%]
    };
}

sub login_footer {
    return shift->{'login_footer'} || q {
    [%~ TRY ; PROCESS 'login_footer.tt' ; CATCH %]<!-- [% error %] -->[% END ~%]
    };
}

sub login_form {
    return shift->{'login_form'} || q {
    <div class="login_chunk">
    <span class="login_error">[% error %]</span>
    <form class="login_form" name="[% form_name %]" method="post" action="[% script_name %][% path_info %]">
    <input type="hidden" name="[% key_redirect %]" value="">
    <input type="hidden" name="[% key_payload %]" value="">
    <input type="hidden" name="[% key_time %]" value="">
    <input type="hidden" name="[% key_expires_min %]" value="">
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
  return q {
    [%~ IF ! use_plaintext %]
    <script src="[% md5_js_path %]"></script>
    <script>
    if (document.md5_hex) document.[% form_name %].onsubmit = function () {
      var f = document.[% form_name %];
      var u = f.[% key_user %].value;
      var p = f.[% key_pass %].value;
      var t = f.[% key_time %].value;
      var s = f.[% key_save %] && f.[% key_save %].checked ? -1 : f.[% key_expires_min %].value;
      var l = f.[% key_payload %].value;
      var r = f.[% key_redirect %].value;

      var str = u+'/'+t+'/'+s+'/'+l;
      var sum = document.md5_hex(str +'/' + document.md5_hex(p));
      var loc = f.action + '?[% key_user %]='+escape(str +'/'+ sum)+'&[% key_redirect %]='+escape(r);

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

CGI::Ex::Auth allows for auto-expiring, safe and easy web based logins.  Auth uses
javascript modules that perform MD5 hashing to cram the password on
the client side before passing them through the internet.

For the stored cookie you can choose to use cram mechanisms,
secure hash cram tokens, auto expiring logins (not cookie based),
and Crypt::Blowfish protection.  You can also choose to keep
passwords plaintext and to use perl's crypt for testing
passwords.

A downside to this module is that it does not use a session to
preserve state so get_pass_by_user has to happen on every request (any
authenticated area has to verify authentication each time).  A plus is
that you don't need to use a session if you don't want to.  It is up
to the interested reader to add caching to the get_pass_by_user
method.

=head1 METHODS

=over 4

=item C<new>

Constructor.  Takes a hashref of properties as arguments.

Many of the methods which may be overridden in a subclass,
or may be passed as properties to the new constuctor such as in the following:

    CGI::Ex::Auth->new({
        get_pass_by_user => \&my_pass_sub,
        key_user         => 'my_user',
        key_pass         => 'my_pass',
        login_template   => \"<form><input name=my_user ... </form>",
    });

The following methods will look for properties of the same name.  Each of these will be
defined separately.

    cgix
    cleanup_user
    cookies
    expires_min
    form
    form_name
    get_pass_by_user
    js_uri_path
    key_cookie
    key_expires_min
    key_logout
    key_pass
    key_payload
    key_redirect
    key_save
    key_time
    key_user
    key_verify
    login_footer
    login_form
    login_header
    login_script
    login_template
    no_cookie_verify
    path_info
    script_name
    secure_hash_keys
    template_args
    template_include_path
    use_base64
    use_blowfish
    use_crypt
    use_plaintext
    verify_payload
    verify_user

=item C<generate_token>

Takes either an auth_data object from a auth_data returned by verify_token,
or a hashref of arguments.

Possible arguments are:

    user           - the username we are generating the token for
    real_pass      - the password of the user (if use_plaintext is false
                     and use_crypt is false, the password can be an md5sum
                     of the user's password)
    use_blowfish   - indicates that we should use Crypt::Blowfish to protect
                     the generated token.  The value of this argument is used
                     as the key.  Default is false.
    use_base64     - indicates that we should use Base64 encoding to protect
                     the generated token.  Default is true.  Will not be
                     used if use_blowfish is true.
    use_plaintext  - indicates that we should keep the password in plaintext
    use_crypt      - also indicates that we should keep the password in plaintext
    expires_min    - says how many minutes until the generated token expires.
                     Values <= 0 indicate to not ever expire.  Used only on cram
                     types.
    payload        - a payload that will be passed to generate_payload and then
                     will be added to cram type tokens.  It cannot contain a /.
    prefer_cram    - If the secure_hash_keys method returns keys, and it is a non-plaintext
                     token, generate_token will create a secure_hash_cram.  Set
                     this value to true to tell it to use a normal cram.  This
                     is generally only useful in testing.

The following are types of tokens that can be generated by generate_token.  Each type includes
pseudocode and a sample of a generated that token.

    plaintext:
        user         := "paul"
        real_pass    := "123qwe"
        token        := join("/", user, real_pass);

        use_base64   := 0
        token        == "paul/123qwe"

        use_base64   := 1
        token        == "cGF1bC8xMjNxd2U="

        use_blowfish := "foobarbaz"
        token        == "6da702975190f0fe98a746f0d6514683"

        Notes: This token will be used if either use_plaintext or use_crypt is set.
        The real_pass can also be the md5_sum of the password.  If real_pass is an md5_sum
        of the password but the get_pass_by_user hook returns the crypt'ed password, the
        token will not be able to be verified.

    cram:
        user        := "paul"
        real_pass   := "123qwe"
        server_time := 1148512991         # a time in seconds since epoch
        expires_min := 6 * 60
        payload     := "something"

        md5_pass    := md5_sum(real_pass) # if it isn't already a 32 digit md5 sum
        str         := join("/", user, server_time, expires_min, payload, md5_pass)
        md5_str     := md5(sum_str)
        token       := join("/", user, server_time, expires_min, payload, md5_str)

        use_base64  := 0
        token       == "paul/1148512991/360/something/16d0ba369a4c9781b5981eb89224ce30"

        use_base64  := 1
        token       == "cGF1bC8xMTQ4NTEyOTkxLzM2MC9zb21ldGhpbmcvMTZkMGJhMzY5YTRjOTc4MWI1OTgxZWI4OTIyNGNlMzA="

        Notes: use_blowfish is available as well

    secure_hash_cram:
        user        := "paul"
        real_pass   := "123qwe"
        server_time := 1148514034         # a time in seconds since epoch
        expires_min := 6 * 60
        payload     := "something"
        secure_hash := ["aaaa", "bbbb", "cccc", "dddd"]
        rand1       := 3                  # int(rand(length(secure_hash)))
        rand2       := 39163              # int(rand(100000))

        md5_pass    := md5_sum(real_pass) # if it isn't already a 32 digit md5 sum

        sh_str1     := join(".", "sh", secure_hash[rand1], rand2)
        sh_str2     := join(".", "sh", rand1, rand2)
        str         := join("/", user, server_time, expires_min, payload, md5_pass, sh_str1)
        md5_str     := md5(sum_str)
        token       := join("/", user, server_time, expires_min, payload, md5_str, sh_str2)

        use_base64  := 0
        token       == "paul/1148514034/360/something/06db2914c9fd4e11499e0652bcf67dae/sh.3.39163"

        Notes: use_blowfish is available as well.  The secure_hash keys need to be set in the
        "secure_hash_keys" property of the CGI::Ex::Auth object.

=item C<get_valid_auth>

Performs the core logic.  Returns an auth object on successful login.
Returns false on errored login (with the details of the error stored in
$@).  If a false value is returned, execution of the CGI should be halted.
get_valid_auth WILL NOT automatically stop execution.

  $auth->get_valid_auth || exit;

Optionally, the class and a list of arguments may be passed.  This will create a
new object using the passed arguments, and then run get_valid_auth.

  CGI::Ex::Auth->get_valid_auth({key_user => 'my_user'}) || exit;

=item C<login_print>

Called if login errored.  Defaults to printing a very basic (but
adequate) page loaded from login_template..

You will want to override it with a template from your own system.
The hook that is called will be passed the step to print (currently
only "get_login_info" and "no_cookies"), and a hash containing the
form variables as well as the following:

=item C<login_hash_common>

Passed to the template swapped during login_print.

    %$form,            # any keys passed to the login script
    error              # The text "Login Failed" if a login occurred
    login_data         # A login data object if they failed authentication.
    key_user           # $self->key_user,        # the username fieldname
    key_pass           # $self->key_pass,        # the password fieldname
    key_time           # $self->key_time,        # the server time field name
    key_save           # $self->key_save,        # the save password checkbox field name
    key_payload        # $self->key_payload,     # the payload fieldname
    key_redirect       # $self->key_redirect,    # the redirect fieldname
    form_name          # $self->form_name,       # the name of the form
    script_name        # $self->script_name,     # where the server will post back to
    path_info          # $self->path_info,       # $ENV{PATH_INFO} if any
    md5_js_path        # $self->js_uri_path ."/CGI/Ex/md5.js", # script for cramming
    use_plaintext      # $self->use_plaintext,   # used to avoid cramming
    $self->key_user    # $data->{'user'},        # the username (if any)
    $self->key_pass    # '',                     # intentional blankout
    $self->key_time    # $self->server_time,     # the server's time
    $self->key_payload # $data->{'payload'}      # the payload (if any)
    $self->key_expires_min # $self->expires_min  # how many minutes crams are valid

=item C<key_logout>

If the form hash contains a true value in this field name, the current user will
be logged out.  Default is "cea_logout".

=item C<key_cookie>

The name of the auth cookie.  Default is "cea_user".

=item C<key_verify>

A field name used during a bounce to see if cookies exist.  Default is "cea_verify".

=item C<key_user>

The form field name used to pass the username.  Default is "cea_user".

=item C<key_pass>

The form field name used to pass the password.  Default is "cea_pass".

=item C<key_save>

Works in conjunction with key_expires_min.  If key_save is true, then
the cookie will be set to be saved for longer than the current session
(If it is a plaintext variety it will be given a 20 year life rather
than being a session cookie.  If it is a cram variety, the expires_min
portion of the cram will be set to -1).  If it is set to false, the cookie
will be available only for the session (If it is a plaintext variety, the cookie
will be session based and will be removed on the next loggout.  If it is
a cram variety then the cookie will only be good for expires_min minutes.

Default is "cea_save".

=item C<key_expires_min>

The name of the form field that contains how long cram type cookies will be valid
if key_save contains a false value.

Default key name is "cea_expires_min".  Default field value is 6 * 60 (six hours).

This value will have no effect when use_plaintext or use_crypt is set.

A value of -1 means no expiration.

=item C<failed_sleep>

Number of seconds to sleep if the passed tokens are invalid.  Does not apply
if validation failed because of expired tokens.  Default value is 3.
Setting to 0 disables any sleeping.

=item C<form_name>

The name of the html login form to attach the javascript to.  Default is "cea_form".

=item C<verify_token>

This method verifies the token that was passed either via the form or via cookies.
It will accept plaintext or crammed tokens (A listing of the available algorithms
for creating tokes is listed below).  It also allows for armoring the token with
base64 encoding, or using blowfish encryption.  A listing of creating these tokens
can be found under generate_token.

=item C<cleanup_user>

Called by verify_token.  Default is to do no modification.  Allows for usernames to
be lowercased, or canonized in some other way.  Should return the cleaned username.

=item C<verify_user>

Called by verify_token.  Single argument is the username.  May or may not be an
initial check to see if the username is ok.  The username will already be cleaned at
this point.  Default return is true.

=item C<get_pass_by_user>

Called by verify_token.  Given the cleaned, verified username, should return a
valid password for the user.  It can always return plaintext.  If use_crypt is
enabled, it should return the crypted password.  If use_plaintext and use_crypt
are not enabled, it may return the md5 sum of the password.

=item C<cgix>

Returns a CGI::Ex object.

=item C<form>

A hash of passed form info.  Defaults to CGI::Ex::get_form.

=item C<cookies>

The current cookies.  Defaults to CGI::Ex::get_cookies.

=item C<login_template>

Should return either a template filename to use for the login template, or it
should return a reference to a string that contains the template.  The contents
will be used in login_print and passed to the template engine.

Default login_template is the values of login_header, login_form, login_script, and
login_script concatenated together.

Values from login_hash_common will be passed to the template engine, and will
also be used to fill in the form.

The basic values are capable of handling most needs so long as appropriate
headers and css styles are used.

=item C<login_header>

Should return a header to use in the default login_template.  The default
value will try to PROCESS a file called login_header.tt that should be
located in directory specified by the template_include_path method.

It should ideally supply css styles that format the login_form as desired.

=item C<login_footer>

Same as login_header - but for the footer.  Will look for login_footer.tt by
default.

=item C<login_form>

An html chunk that contains the necessary form fields to login the user.  The
basic chunk has a username text entry, password text entry, save password checkbox,
and submit button, and any hidden fields necessary for logging in the user.

=item C<login_script>

Contains javascript that will attach to the form from login_form.  This script
is capable of taking the login_fields and creating an md5 cram which prevents
the password from being passed plaintext.

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut
