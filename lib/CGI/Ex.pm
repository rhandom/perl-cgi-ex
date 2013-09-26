package CGI::Ex;

=head1 NAME

CGI::Ex - CGI utility suite - makes powerful application writing fun and easy

=cut

###---------------------###
#  Copyright 2003-2013 - Paul Seamons
#  Distributed under the Perl Artistic License without warranty

use strict;
use warnings;

our $VERSION = '3.00';
our $PREFERRED_CGI_MODULE ||= 'CGI';
our $DEBUG_LOCATION_BOUNCE;
our $GLOBAL_CGIX;

BEGIN {
    my $v = (! $ENV{'MOD_PERL'}) ? 0 # cache mod_perl version (light if or if not mod_perl)
        # mod_perl/1.27 or mod_perl/1.99_16 or mod_perl/2.0.1
        # if MOD_PERL is set - don't die if regex fails - just assume 1.0
        : ($ENV{'MOD_PERL'} =~ m{ ^ mod_perl / (\d+\.[\d_]+) (?: \.\d+)? $ }x) ? $1 : '1.0_0';
    sub _mod_perl_version () { $v }
    sub _is_mod_perl_1    () { $v <  1.98 && $v > 0 }
    sub _is_mod_perl_2    () { $v >= 1.98 }

    if (_is_mod_perl_1) { # old mod_perl
        require Apache;
        *_apache_request = sub { Apache->request };
    } elsif (_is_mod_perl_2) {
        if (eval { require Apache2::RequestRec }) { # debian style
            require Apache2::RequestUtil;
            *_apache_request = sub { Apache2::RequestUtil->request };
        } else { # fedora and mandrake style
            require Apache::RequestUtil;
            *_apache_request = sub { Apache->request };
        }
    } else {
        *_apache_request = sub {};
    }
}

###---------------------###

sub new {
    my $class = shift;
    return $GLOBAL_CGIX if $GLOBAL_CGIX && ref($GLOBAL_CGIX) eq $class && !@_;
    return bless ref($_[0]) ? shift : {@_}, $class;
}

sub object {
    my $self = shift || die 'Usage: my $query = $cgix_obj->object';
    $self->{'object'} = shift if $#_ != -1;
    $self->{'object'} ||= do {
        my $pkg = $self->{'cgi_module'} || $PREFERRED_CGI_MODULE;
        (my $file = "$pkg.pm") =~ s|::|/|g;
        eval { require $file } or die "Could not require $pkg: $@";
        $pkg->new($self->psgi_env || ());
    };
}

sub AUTOLOAD {
    my $self = shift;
    my $meth = ($CGI::Ex::AUTOLOAD =~ /::(\w+)$/) ? $1 : die "Invalid method $CGI::Ex::AUTOLOAD";
    return $self->object->$meth(@_);
}

sub DESTROY {}

###---------------------###

sub form {
    my $self = shift;
    $self->{'form'} = (ref($_[0]) eq 'HASH') ? shift : undef if @_;
    return $self->{'form'} ||= do {
        my $obj  = shift || $self->object;
        my %hash;
        foreach my $key ($obj->param) {
            my @val = $obj->param($key);
            $hash{$key} = ($#val <= 0) ? $val[0] : \@val;
        }
        \%hash;
    };
}

sub make_form {
    my $self = shift;
    my $form = shift || $self->form();
    my $keys = ref($_[0]) ? shift : [sort keys %$form];
    my $str = '';
    foreach (@$keys) {
        my $key = $_; # make a copy
        my $val = $form->{$key};
        $key =~ s/([^\w.\-\ ])/sprintf('%%%02X', ord $1)/eg;
        $key =~ y/ /+/;
        foreach (ref($val) eq 'ARRAY' ? @$val : $val) {
            my $_val = $_; # make a copy
            $_val =~ s/([^\w.\-\ ])/sprintf('%%%02X', ord $1)/eg;
            $_val =~ y/ /+/;
            $str .= "$key=$_val&"; # intentionally not using join
        }
    }
    chop $str;
    return $str;
}

sub cookies {
    my $self = shift || __PACKAGE__->new;
    $self->{'cookies'} = (ref($_[0]) eq 'HASH') ? shift : undef  if @_;
    return $self->{'cookies'} ||= do {
        my $obj  = shift || $self->object;
        my %hash;
        foreach my $key ($obj->cookie) {
            my @val = $obj->cookie($key);
            $hash{$key} = ($#val == -1) ? "" : ($#val == 0) ? $val[0] : \@val;
        }
        \%hash;
    };
}

###---------------------###

sub apache_request { (@_ == 2) ? $_[0]->{'apache_request'} = pop : $_[0]->{'apache_request'} || _apache_request() }
sub mod_perl_version { _mod_perl_version }
sub is_mod_perl_1    { _is_mod_perl_1    }
sub is_mod_perl_2    { _is_mod_perl_2    }


# will send the Content-type header
#   $cgix->print_content_type;
#   $cgix->print_content_type('text/plain');
sub print_content_type {
    my ($self, $type, $charset) = (@_ && ref $_[0]) ? @_ : (undef, @_);
    $self = __PACKAGE__->new if ! $self;

    if ($type) {
        die "Invalid type: $type" if $type !~ m|^[\w\-\.]+/[\w\-\.\+]+$|; # image/vid.x-foo
    } else {
        $type = 'text/html';
    }
    $type .= "; charset=$charset" if $charset && $charset =~ m|^[\w\-\.\:\+]+$|;

    if ($self->psgi_env) {
        push @{ $self->{'_headers'} }, 'Content-Type', $type if !$ENV{'CONTENT_TYPED'};
        $ENV{'CONTENT_TYPED'} .= sprintf "%s, %d\n", (caller)[1,2];
    } elsif (my $r = $self->apache_request) {
        return if $r->bytes_sent;
        $r->content_type($type);
        $r->send_http_header if $self->is_mod_perl_1;
    } else {
        print "Content-Type: $type\r\n\r\n" if ! $ENV{'CONTENT_TYPED'};
        $ENV{'CONTENT_TYPED'} .= sprintf "%s, %d\n", (caller)[1,2];
    }
}
sub content_type { goto &print_content_type }

# boolean check if content has been typed
sub content_typed {
    my $self = shift || __PACKAGE__->new;

    if ($self->psgi_env) {
        return grep { $_ eq 'Content-Type' } keys %{{@{ $self->{'_headers'} || []}}};
    } elsif (my $r = $self->apache_request) {
        return $r->bytes_sent;
    } else {
        return $ENV{'CONTENT_TYPED'} ? 1 : undef;
    }
}

# location bounce nicely - even if we have already sent content
sub location_bounce {
    my ($self, $loc, $code) = @_;
    $code ||= 302; die "Invalid location_bounce code\n" if $code !~ /^30[12]$/;

    if ($self->psgi_env) {
        push @{ $self->{'_headers'} }, 'Location' => $loc;
        $self->{'_status'} = $code;
        push @{ $self->{'_body'} }, "Bounced to $loc\n";

    } elsif ($self->content_typed) {
        if ($DEBUG_LOCATION_BOUNCE) {
            print "<a class=debug href=\"$loc\">Location: $loc</a><br />\n";
        } else {
            print "<meta http-equiv=\"refresh\" content=\"0;url=$loc\" />\n";
        }

    } elsif (my $r = $self->apache_request) {
        $r->status($code);
        if ($self->is_mod_perl_1) {
            $r->header_out("Location", $loc);
            $r->content_type('text/html');
            $r->send_http_header;
            $r->print("Bounced to $loc\n");
        } else {
            $r->headers_out->add("Location", $loc);
            $r->content_type('text/html');
            $r->rflush;
        }

    } else {
        print "Location: $loc\r\n",
        "Status: $code Bounce\r\n",
        "Content-Type: text/html\r\n\r\n",
        "Bounced to $loc\r\n";
    }
}

# set a cookie nicely - even if we have already sent content
#   $cgix->set_cookie({name => $name, ...});
#   $cgix->set_cookie( name => $name, ... );
sub set_cookie {
    my $self = shift;
    my $args = ref($_[0]) ? shift : {@_};
    foreach (keys %$args) {
        next if /^-/;
        $args->{"-$_"} = delete $args->{$_};
    }

    $args->{-path} ||= '/'; # default path to / and allow for 1hour instead of 1h
    $args->{-expires} = $self->time_calc($args->{-expires}) if $args->{-expires};

    my $cookie = ''. $self->object->cookie(%$args);
    if ($self->psgi_env) {
        push @{ $self->{'_headers'} }, 'Set-Cookie' => $cookie;
    } elsif ($self->content_typed) {
        print "<meta http-equiv=\"Set-Cookie\" content=\"$cookie\" />\n";
    } else {
        if (my $r = $self->apache_request) {
            if ($self->is_mod_perl_1) {
                $r->header_out("Set-cookie", $cookie);
            } else {
                $r->headers_out->add("Set-Cookie", $cookie);
            }
        } else {
            print "Set-Cookie: $cookie\r\n";
        }
    }
}

# print the last modified time
#   $cgix->last_modified; # now
#   $cgix->last_modified((stat $file)[9]); # file's time
#   $cgix->last_modified(time, 'Expires'); # different header
sub last_modified {
    my $self = shift;
    my $time = shift || time;
    my $key  = shift || 'Last-Modified';

    # get a time string - looks like:
    # Mon Dec  9 18:03:21 2002
    # valid RFC (although not preferred)
    $time = scalar gmtime $self->time_calc($time);

    if ($self->psgi_env) {
        push @{ $self->{'_headers'} }, $key => $time;
    } elsif ($self->content_typed) {
        print "<meta http-equiv=\"$key\" content=\"$time\" />\n";
    } elsif (my $r = $self->apache_request) {
        if ($self->is_mod_perl_1) {
            $r->header_out($key, $time);
        } else {
            $r->headers_out->add($key, $time);
        }
    } else {
        print "$key: $time\r\n";
    }
}

sub expires { shift->last_modified(shift || time, 'Expires') }

# similar to expires_calc from CGI::Util
# allows for lenient calling, hour instead of just h, etc
# takes time or 0 or now or filename or types of -23minutes 
sub time_calc {
    my ($self, $time) = @_; # may only be called as a function
    if (! $time || lc($time) eq 'now') {
        return time;
    } elsif ($time =~ m/^\d+$/) {
        return $time;
    } elsif ($time =~ m/^([+-]?)\s*(\d+|\d*\.\d+)\s*([a-z])[a-z]*$/i) {
        my $m = {
            's' => 1,
            'm' => 60,
            'h' => 60 * 60,
            'd' => 60 * 60 * 24,
            'w' => 60 * 60 * 24 * 7,
            'M' => 60 * 60 * 24 * 30,
            'y' => 60 * 60 * 24 * 365,
        };
        return time + ($m->{lc($3)} || 1) * "$1$2";
    } else {
        my @stat = stat $time;
        die "Could not find file \"$time\" for time_calc.  You should pass one of \"now\", time(), \"[+-] \\d+ [smhdwMy]\" or a filename." if $#stat == -1;
        return $stat[9];
    }
}


# allow for generic status send
sub send_status {
    my ($self, $code, $msg) = @_;
    die "Missing status" if ! $code;
    $msg = "HTTP Status of $code received\n" if ! defined $msg;

    die "Cannot send a status ($code - $msg) after content has been sent" if $self->content_typed;

    if ($self->psgi_env) {
        $self->{'_status'} = $code;
        push @{ $self->{'_body'} }, $msg;
        $self->print_content_type;
    } elsif (my $r = $self->apache_request) {
        $r->status($code);
        if ($self->is_mod_perl_1) {
            $r->content_type('text/html');
            $r->send_http_header;
            $r->print($msg);
        } else {
            $r->content_type('text/html');
            $r->print($msg);
            $r->rflush;
        }
    } else {
        print "Status: $code\r\n";
        $self->print_content_type;
        print $msg;
    }
}

# allow for sending a simple header
sub send_header {
    my ($self, $key, $val) = @_;
    die "Cannot send a header ($key - $val) after content has been sent" if $self->content_typed;
    if ($self->psgi_env) {
        push @{ $self->{'_headers'} }, $key => $val;
    } elsif (my $r = $self->apache_request) {
        if ($self->is_mod_perl_1) {
            $r->header_out($key, $val);
        } else {
            $r->headers_out->add($key, $val);
        }
    } else {
        print "$key: $val\r\n";
    }
}

###----------------------------------------------------------------###

sub psgi_env { shift->{'_psgi_env'} }

sub psgi_capture {
    my ($self, $env, $sub) = @_;
    $self = $self->new if ! ref $self;
    local @$self{qw(_status _headers _body)};
    local $self->{'_psgi_env'} = $env || die "Missing PSGI env\n";
    local $self->{'cgi_module'} = 'CGI::PSGI';
    local $self->{'object'} = undef if !$self->{'object'} || !$self->{'object'}->can('psgi_header');
    my $r;
    open my $fh, '>', \ (my $body = '') or die "Failed to capture body: $!";
    my $old_fh = select $fh;
    eval { local $GLOBAL_CGIX = $self; $r = $sub->($self, $env); 1 } or do {
        my $err = $@;
        warn $err;
        @$self{qw(_status _headers _body)} = (501, undef, ["<h1>Internal Error</h1>\n$err"]) if ($self->{'_status'} || 0) != 501;
    };
    select $old_fh;
    return [$self->{'_status'} || 200, $self->{'_headers'} || ['Content-type' => 'text/html'], $self->{'_body'} || [$body]];
}

sub capture {
    my ($status, $headers, $body) = @{ shift->psgi_capture(\%ENV, shift()) };
    my $out = "Status: $status\r\n";
    for (my $i = 0; $i < @$headers; $i+=2) {
        $out .= "$headers->[$i]: $headers->[$i+1]\r\n";
    }
    $out .= "\r\n";
    $out .= $_ for @$body;
    return $out;
}

###----------------------------------------------------------------###

# allow for printing out a static javascript file
# for example $self->print_js("CGI::Ex::validate.js");
sub print_js {
    my ($self, $js_file) = @_;
    $self = $self->new if ! ref $self;

    my $stat;
    if ($js_file) {
        $js_file .= '.js' if $js_file !~ /\.js$/i;
        $js_file =~ s|::|/|g;
        if ($js_file =~ m|^/*(\w+(?:/+\w+)*\.js)$|i) {
            foreach my $path (@INC) {
                my $_file = "$path/$1";
                next if ! -f $_file;
                $js_file = $_file;
                $stat = [stat _];
                last;
            }
        }
    }

    if (! $stat) {
        if ($self->content_typed) {
            print "<h1>JS File not found for print_js</h1>\n";
        } else {
            $self->send_status(404, "JS File not found for print_js\n");
        }
        return;
    }

    if (! $self->content_typed) {
        $self->last_modified($stat->[9]);
        $self->expires('+ 1 year');
        $self->print_content_type('application/x-javascript');
    }

    return if $ENV{'REQUEST_METHOD'} && $ENV{'REQUEST_METHOD'} eq 'HEAD';

    open my $fh, '<', $js_file or die "Could not open file $js_file: $!";
    local $/ = undef;
    print <$fh>;
    close $fh;
}

sub validate {  die "Use CGI::Ex::Validate directly" }
sub conf_read { die "Use CGI::Ex::Conf directly" }
sub conf_obj {  die "Use CGI::Ex::Conf directly" }
sub fill {      die "Use CGI::Ex::Fill directly" }
sub swap_template { die "Use Template::Alloy directly" }

###----------------------------------------------------------------###

1;

__END__

=head1 CGI::Ex SYNOPSIS

    # You probably do not want to use CGI::Ex directly
    # You probably should use CGI::Ex::App instead.

    use base qw(Cea);  # shortcut to CGI::Ex::App
    __PACKAGE__->start;

    sub main_hash_swap { {what => 'world'} }
    sub main_file_print { \ q{Hello [% what.ucfirst %]} }


    # Using CGI::Ex directly:

    use CGI::Ex;
    my $cgix = CGI::Ex->new;
    $cgix->print_content_type;
    my $hash = $cgix->form;

    if ($hash->{'bounce'}) {
        $cgix->set_cookie({name => 'foo', value => 'bar'});
        $cgix->location_bounce($new_url_location);
        exit;
    }

    print "Hello World";

=head1 DESCRIPTION

The CGI::Ex suite is focused around CGI::Ex::App (aka Cea for short -
pronounced See-ah).  CGI::Ex is a suite of utilities aimed at making
writing CGI scripts more enjoyable.  Although they can all be used
separately, the main functionality of each of the modules is best
represented in the CGI::Ex::App module.

CGI::Ex::App existed before the other modern frameworks, yet it still
provides a unique and fun method for writing applications.  CGI::Ex::App
employs a hook based scaffolding system allowing for writing less
boiler plate and focusing in on the important points of your application.

CGI::Ex::App is not quite a true framework (which normally includes
pre-built html) instead CGI::Ex::App is an extended application flow
that dramatically reduces CGI build time in most cases.  It does so
using as little magic as possible.  See L<CGI::Ex::App>.

The main functionality is provided by several other modules that
may be used separately, or together through the CGI::Ex interface.

=head1 MODULES

=over 4

=item C<CGI::Ex>

This module primarily deals with the lower level abstractions.  It
provides an abstraction layer allowing for scripts to run more seamlessly
on older cgi environments, in mod_perl 1 or 2 environments, or in more recent
PSGI environments.  Originally this module had accessor methods into
the other modules listed below, but all of this functionality is replaced
by the features of L<CGI::Ex::App>.  Unless you are writing a lower level
module, you should likely just use CGI::Ex::App.

=item C<CGI::Ex::App>

Also known as L<Cea>.  Hook based application scaffolding framework.  Makes
writing applications fun.

=item C<CGI::Ex::Fill>

A regular expression based form-filler-inner.  Can be a drop in replacement for
HTML::FillInForm.  See L<CGI::Ex::Fill> for more information.  Available
by default in CGI::Ex::App.

=item C<CGI::Ex::Validate>

A form field / cgi parameter / any parameter validator.  Not quite a drop in
for most validators, although it has most of the functionality of most
of the validators but with the key additions of conditional validation.
Has a tightly integrated JavaScript portion that allows for duplicate client
side validation.  See L<CGI::Ex::Validate> for more information.  Used as the
default validator of CGI::Ex::App.

=item C<CGI::Ex::Conf>

A general use configuration, or settings, or key / value file reader.  Has
ability for providing key fallback as well as immutable key definitions.  Has
default support for yaml, storable, perl, ini, and xml and open architecture
for definition of others.  See L<CGI::Ex::Conf> for more information.

=item C<CGI::Ex::Auth>

A highly configurable web based authentication system.  See
L<CGI::Ex::Auth> for more information.

=item C<CGI::Ex::Template>

The original code base location of what is now L<Template::Alloy> (A
Template::Toolkit compatible processing engine).  Use Template::Alloy
directly.

=back

=head1 CGI::Ex METHODS

=over 4

=item form

All of the get and post variables returned by the param method of
object.  Very similar to CGI->new->Vars except that arrayrefs are
returned as arrayrefs.

    my $hash = $cgix->form;
    my $hash = $cgix->form(CGI->new); # will use CGI->new as the object

If a hashref is passed, it will cache this as the form values.

    $cgix->form(\%new_form);

=item cookies

Returns a hash of all cookies.

    my $hash = $cgix->cookies;
    my $hash = $cgix->cookies(CGI->new);

If given an additional hashref, it will cache this as the cookies.  Note
that this does not send cookie headers.  To do that, use the set_cookie
method.

    $cgix->cookies(\%new_cookies);

=item object

Returns the CGI object that is currently being used by CGI::Ex.  If none
has been set it will automatically generate an object of type
$PREFERRED_CGI_MODULE which defaults to B<CGI>.

=item make_form

Takes a hash and returns a query_string.  A second optional argument
may contain an arrayref of keys to use from the hash in building the
query_string.  First argument is undef, it will use the form stored
in itself as the hash.

=item print_content_type

Can be called multiple times during the same session.  Will only
print content-type once.  (Useful if you don't know if something
else already printed content-type).  Calling this sends the Content-type
header.

    $cgix->print_content_type;

    # OR
    $cgix->print_content_type('text/html');

    # OR
    $cgix->print_content_type('text/html', 'utf-8');

=item set_cookie

Arguments are the same as those to CGI->new->cookie({}).
Uses CGI's cookie method to create a cookie, but then, depending on
if content has already been sent to the browser will either print
a Set-cookie header, or will add a <meta http-equiv='set-cookie'>
tag (this is supported on most major browsers).  This is useful if
you don't know if something else already printed content-type.

=item location_bounce

Depending on if content has already been sent to the browser will either print
a Location header, or will add a <meta http-equiv='refresh'>
tag (this is supported on all major browsers).  This is useful if
you don't know if something else already printed content-type.  Takes
single argument of a url.  By default a status of 302 is used.  A second argument
of 301 can also be passed.

=item last_modified

Depending on if content has already been sent to the browser will either print
a Last-Modified header, or will add a <meta http-equiv='Last-Modified'>
tag (this is supported on most major browsers).  This is useful if
you don't know if something else already printed content-type.  Takes an
argument of either a time (may be a CGI -expires style time) or a filename.

=item expires

Depending on if content has already been sent to the browser will either print
a Expires header, or will add a <meta http-equiv='Expires'>
tag (this is supported on most major browsers).  This is useful if
you don't know if something else already printed content-type.  Takes an
argument of a time (may be a CGI -expires style time).

=item send_status

Send a custom status.  Works in both CGI and mod_perl.  Arguments are
a status code and the content (optional).

=item send_header

Send a http header.  Works in both CGI and mod_perl.  Arguments are
a header name and the value for that header.

=item print_js

Prints out a javascript file.  Does everything it can to make sure
that the javascript will cache.  Takes either a full filename,
or a shortened name which will be looked for in @INC. (ie /full/path/to/my.js
or CGI/Ex/validate.js or CGI::Ex::validate)

    #!/usr/bin/env perl
    use CGI::Ex;
    CGI::Ex->print_js($ENV{'PATH_INFO'});

=item psgi_capture

Takes an env hashref and coderef to run.  Will return the status, headers, and body
portions of a normal psgi request.  Stdout is temporarily redirected during this
method.  Also, CGI::Ex is turned into a singleton during this request to aid in
the capturing process of loosely coupled systems.

=item capture

Uses psgi_capture to capture output, but then prints it out as normal text.

=back

CGI::Ex used to have validate, conf_read, conf_obj, fill, and swap_template
methods but these were removed in 3.0 as all of the functionality is better
served by CGI::Ex::App.

=head1 MODULES

See also L<CGI::Ex::App>.

See also L<CGI::Ex::Auth>.

See also L<CGI::Ex::Conf>.

See also L<CGI::Ex::Die>.

See also L<CGI::Ex::Dump>.

See also L<CGI::Ex::Fill>.

See also L<CGI::Ex::Template>.

See also L<CGI::Ex::Validate>.

=head1 LICENSE

This module is distributed under the Artistic License.

=head1 AUTHOR

Paul Seamons <paul@seamons.com>

=cut
