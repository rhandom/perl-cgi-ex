package CGI::Ex;

### CGI Extended

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom

use strict;
use vars qw($VERSION
            $PREFERRED_FILL_MODULE
            $PREFERRED_CGI_MODULE
            $PREFERRED_CGI_REQUIRED
            $PREFERRED_VAL_MODULE
            $OBJECT_METHOD
            $TEMPLATE_OPEN
            $TEMPLATE_CLOSE
            $AUTOLOAD
            $DEBUG_LOCATION_BOUNCE
            @EXPORT @EXPORT_OK
            );
use base qw(Exporter);

$VERSION               = '1.12';
$PREFERRED_FILL_MODULE ||= '';
$PREFERRED_CGI_MODULE  ||= 'CGI';
$PREFERRED_VAL_MODULE  ||= '';
$OBJECT_METHOD         ||= 'param';
$TEMPLATE_OPEN         ||= qr/\[%\s*/;
$TEMPLATE_CLOSE        ||= qr/\s*%\]/;
@EXPORT = ();
@EXPORT_OK = qw(get_form get_cookies
                content_type content_typed
                );

###----------------------------------------------------------------###

sub new {
  my $class = shift || __PACKAGE__;
  my $self  = ref($_[0]) ? shift : {@_};
  return bless $self, $class;
}

### allow for holding another classed CGI style object
sub object {
  my $self = ref($_[0]) ? shift : die "Sub \"object\" must be called as a method";
  return $self->{object} ||= do {
    $PREFERRED_CGI_REQUIRED ||= do {
      my $file = $PREFERRED_CGI_MODULE;
      $file .= ".pm" if $file !~ /\.\w+$/;
      $file =~ s|::|/|g;
      eval {require $file};
      if ($@) {
        die "Couldn't require $PREFERRED_CGI_MODULE: $@";
      }
      1; # return of inner do
    };
    $PREFERRED_CGI_MODULE->new(); # return of the do
  };
}

### allow for calling their methods
sub AUTOLOAD {
  my $self   = shift;
  my $method = ($AUTOLOAD =~ /(\w+)$/) ? $1 : die "Invalid method $AUTOLOAD";
  return wantarray # does wantarray propogate up ?
    ? ($self->object->$method(@_))
    :  $self->object->$method(@_);
}

###----------------------------------------------------------------###

### form getter that will act like ->Vars only it will return arrayrefs
### for values that are arrays
sub get_form {
  my $self = shift || __PACKAGE__; # may be called as a function or a method
  $self = $self->new if ! ref $self;

  ### allow for custom hash
  return $self->{form} if $self->{form};

  ### get the info out of the object
  my $obj  = shift || $self->object;
  my $meth = $obj->can($OBJECT_METHOD);
  my %hash = ();
  foreach my $key ($obj->$meth()) {
    my @val = $obj->$meth($key);
    $hash{$key} = ($#val == -1) ? die : ($#val == 0) ? $val[0] : \@val;
  }
  return \%hash;
}

### allow for a setter
sub set_form {
  my $self = ref($_[0]) ? shift : die "Sub \"set_form\" must be called as a method";
  $self->{form} = shift || {};
}

### like get_form - but a hashref of cookies
### cookies are parsed depending upon the functionality of ->cookie
sub get_cookies {
  my $self = shift || __PACKAGE__; # may be called as a function or a method
  $self = $self->new if ! ref $self;

  my $obj  = shift || $self->object;
  my %hash = ();
  foreach my $key ($obj->cookie()) {
    my @val = $obj->cookie($key);
    $hash{$key} = ($#val == -1) ? next : ($#val == 0) ? $val[0] : \@val;
  }
  return \%hash;
}

### allow for creating a query_string
sub make_form {
  my $self = ref($_[0]) ? shift : die "Sub \"make_form\" must be called as a method";
  my $form = shift || $self->get_form();
  my $keys = (ref $_[0]) ? {map {$_ => 1} @{ shift() }} : undef;
  my $str = '';
  foreach my $key (sort keys %$form) {
    next if $keys && ! $keys->{$key};
    $key =~ s/([^\w.\- ])/sprintf('%%%02X',ord($1))/eg;
    $key =~ y/ /+/;
    foreach (ref($form->{$key}) ? @{ $form->{$key} } : $form->{$key}) {
      my $val = $_; # make a copy
      $val =~ s/([^\w.\- ])/sprintf('%%%02X',ord($1))/eg;
      $val =~ y/ /+/;
      $str .= "$key=$val&";
    }
  }
  chop($str);
  return $str;
}

###----------------------------------------------------------------###

### allow for shared apache request object
sub apache_request {
  my $self = shift || die "Sub \"apache_request\" must be called as a method";
  return if ! $ENV{'MOD_PERL'};
  if (ref $self) { # may be called as a object or a class method
    return $self->{apache_request} ||= Apache->request;
  } else {
    return Apache->request;
  }
}

### allow for a setter
sub set_apache_request {
  my $self = ref($_[0]) ? shift : die "Sub \"set_apache_request\" must be called as an object method";
  $self->{apache_request} = shift;
}

###----------------------------------------------------------------###

sub content_type {
  &print_content_type;
}

### will send the Content-type header
sub print_content_type {
  my $self = ref($_[0]) ? shift : __PACKAGE__; # may be called as function or method
  my $type = shift || 'text/html';

  if (my $r = $self->apache_request) {
    return if $r->bytes_sent;
    $r->content_type($type);
    $r->send_http_header;
  } else {
    if (! $ENV{CONTENT_TYPED}) {
      print "Content-Type: $type\r\n\r\n";
      $ENV{CONTENT_TYPED} = '';
    }
    $ENV{CONTENT_TYPED} .= sprintf("%s, %d\n", (caller)[1,2]);
  }
}

sub content_typed {
  my $self = ref($_[0]) ? shift : __PACKAGE__; # may be called as function or method
  if (my $r = $self->apache_request) {
    return $r->bytes_sent;
  } else {
    return ($ENV{CONTENT_TYPED}) ? 1 : undef;
  }
}

###----------------------------------------------------------------###

### location bounce nicely - even if we have already sent content
### may be called as function or a method
sub location_bounce {
  my $self = ref($_[0]) ? shift : __PACKAGE__; # may be called as function or method
  my $loc  = shift || '';
  if ($self->content_typed) {
    if ($DEBUG_LOCATION_BOUNCE) {
      print "<a class=debug href=\"$loc\">Location: $loc</a><br />\n";
    } else {
      print "<meta http-equiv=\"refresh\" content=\"0;url=$loc\" />\n";
    }
  } else {
    if (my $r = $self->apache_request) {
      $r->header_out("Location", $loc);
      $r->status(302);
      $r->content_type('text/html');
      $r->send_http_header;
      $r->print("Bounced to $loc\n");
    } else { 
      print "Location: $loc\r\n",
            "Status: 302 Bounce\r\n",
            "Content-Type: text/html\r\n\r\n",
            "Bounced to $loc\r\n";
    }
  }
}

### set a cookie nicely - even if we have already sent content
### may be called as function or a method - fancy algo to allow for first argument of args hash
sub set_cookie {
  my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift : __PACKAGE__->new;
  my $args = ref($_[0]) ? shift : {@_};
  foreach (keys %$args) {
    next if /^-/;
    $args->{"-$_"} = delete $args->{$_};
  }

  ### default path to / and allow for 1hour instead of 1h
  $args->{-path} ||= '/';
  $args->{-expires} = &time_calc($args->{-expires}) if $args->{-expires};

  my $cookie = "" . $self->object->cookie(%$args);

  if ($self->content_typed) {
    print "<meta http-equiv=\"Set-Cookie\" content=\"$cookie\" />\n";
  } else {
    if (my $r = $self->apache_request) {
      $r->header_out("Set-cookie", $cookie);
    } else {
      print "Set-Cookie: $cookie\r\n"
    }
  }
}

### print the last modified time
### takes a time or filename and an optional keyname
sub last_modified {
  my $self = ref($_[0]) ? shift : __PACKAGE__; # may be called as function or method
  my $time = shift || time;
  my $key  = shift || 'Last-Modified';

  ### get a time string - looks like:
  ### Mon Dec  9 18:03:21 2002
  ### valid RFC (although not prefered)
  $time = scalar(gmtime(&time_calc($time)));

  if ($self->content_typed) {
    print "<meta http-equiv=\"$key\" content=\"$time\" />\n";
  } else {
    if (my $r = $self->apache_request) {
      $r->header_out($key, $time);
    } else {
      print "$key: $time\r\n"
    }
  }

}

### add expires header
sub expires {
  my $self = ref($_[0]) ? shift : __PACKAGE__; # may be called as a function or method
  my $time = shift || time;
  return $self->last_modified($time, 'Expires');
}

### similar to expires_calc from CGI::Util
### allows for lenient calling, hour instead of just h, etc
### takes time or 0 or now or filename or types of -23minutes 
sub time_calc {
  my $time = shift; # may only be called as a function
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
    die "Couldn't not find file \"$time\" for time_calc" if $#stat == -1;
    return $stat[9];
  }
}


### allow for generic status send
sub send_status {
  my $self = ref($_[0]) ? shift : __PACKAGE__; # may be called as function or method
  my $code = shift || die "Missing status";
  my $mesg = shift;
  if (! defined $mesg) {
    $mesg = "HTTP Status of $code received\n";
  }
  if ($self->content_typed) {
    die "Cannot send a status ($code - $mesg) after content has been sent";
  }
  if (my $r = $self->apache_request) {
    $r->status($code);
    $r->content_type('text/html');
    $r->send_http_header;
    $r->print($mesg);
  } else {
    print "Status: $code\r\n";
    $self->print_content_type;
    print $mesg;
  }
}

### allow for sending a simple header
sub send_header {
  my $self = ref($_[0]) ? shift : __PACKAGE__; # may be called as function or method
  my $key  = shift;
  my $value = shift;
  if ($self->content_typed) {
    die "Cannot send a header ($key - $value) after content has been sent";
  }
  if (my $r = $self->apache_request) {
    $r->header_out($key, $value);
  } else {
    print "$key: $value\r\n";
  }
}

###----------------------------------------------------------------###

### allow for printing out a static javascript file
### for example $self->print_js("CGI::Ex::validate.js");
sub print_js {
  my $self    = shift || die "Sub \"print_js\" must be called as a method";
  my $js_file = shift || '';

  ### fix up the file - force .js on the end
  $js_file .= '.js' if $js_file !~ /\.js$/i;
  $js_file =~ s|::|/|g;

  ### get file info
  my $stat;
  if ($js_file !~ m|^\.{0,2}/|) {
    foreach my $path (@INC) {
      my $_file = "$path/$js_file";
      next if ! -f $_file;
      $js_file = $_file;
      $stat = [stat _];
      last;
    }
  } else {
    if (-f $js_file) {
      $stat = [stat _];
    }
  }

  ### no - file - 404
  if (! $stat) {
    if (! $self->content_typed) {
      $self->send_status(404, "JS File not found for print_js\n");
    } else {
      print "<h1>JS File not found for print_js</h1>\n";
    }

    return;
  }

  ### do headers
  if (! $self->content_typed) {
    $self->last_modified($stat->[9]);
    $self->expires('+ 1 year');
    $self->print_content_type('application/x-javascript');
  }

  return if $ENV{REQUEST_METHOD} && $ENV{REQUEST_METHOD} eq 'HEAD';

  ### send the contents
  if (open IN, $js_file) {
    local $/ = undef;
    print <IN>;
    close IN;
  } else {
    die "Couldn't open file  $js_file: $!";
  }
}

###----------------------------------------------------------------###

### form filler that will use either HTML::FillInForm, CGI::Ex::Fill
### or another specified filler.  Argument style is similar to
### HTML::FillInForm.  May be called as a method or a function.
sub fill {
  my $self = shift;
  my $args = shift;
  if (ref($args)) {
    if (! UNIVERSAL::isa($args, 'HASH')) {
      $args = {text => $args};
      @$args{'form','target','fill_password','ignore_fields'} = @_;
    }
  } else {
    $args = {$args, @_};
  }

  my $module = $self->{fill_module} || $PREFERRED_FILL_MODULE;

  ### allow for using the standard HTML::FillInForm
  ### too bad it won't modify our file in place for us
  if ($module eq 'HTML::FillInForm') {
    eval { require HTML::FillInForm };
    if ($@) {
      die "Couldn't require HTML::FillInForm: $@";
    }
    $args->{scalarref} = $args->{text} if $args->{text};
    $args->{fdat}      = $args->{form} if $args->{form};
    my $filled = HTML::FillInForm->new->fill(%$args);
    if ($args->{text}) {
      my $ref = $args->{text};
      $$ref = $filled;
      return 1;
    }
    return $filled;

  ### allow for some other type - for whatever reason
  } elsif ($module) {
    my $file = $module;
    $file .= '.pm' if $file !~ /\.\w+$/;
    $file =~ s|::|/|g;
    eval { require $file };
    if ($@) {
      die "Couldn't require $module: $@";
    }
    return $module->new->fill(%$args);

  ### well - we will use our own then
  } else {
    require CGI::Ex::Fill;

    ### get the text to work on
    my $ref;
    if ($args->{text}) {           # preferred method - gets modified in place
      $ref = $args->{text};
    } elsif ($args->{scalarref}) { # copy to mimic HTML::FillInForm
      my $str = ${ $args->{scalarref} };
      $ref = \$str;
    } elsif ($args->{arrayref}) {  # joined together (copy)
      my $str = join "", @{ $args->{arrayref} };
      $ref = \$str;
    } elsif ($args->{file}) {      # read it in
      open (IN, $args->{file}) || die "Couldn't open $args->{file}: $!";
      my $str = '';
      read(IN, $str, -s _) || die "Couldn't read $args->{file}: $!";
      close IN;
      $ref = \$str;
    } else {
      die "No suitable text found for fill.";
    }

    ### allow for data to be passed many ways
    my $form = $args->{form} || $args->{fobject}
      || $args->{fdat} || $self->object;
    
    &CGI::Ex::Fill::form_fill($ref,
                              $form,
                              $args->{target},
                              $args->{fill_password},
                              $args->{ignore_fields},
                              );
    return ! $args->{text} ? $$ref : 1;
  }

}

###----------------------------------------------------------------###

sub validate {
  my $self = shift || die "Sub \"validate\" must be called as a method";
  my ($form, $file) = (@_ == 2) ? (shift, shift) : ($self->object, shift);

  require CGI::Ex::Validate;

  my $args = {};
  $args->{raise_error} = 1 if $self->{raise_error};
  return CGI::Ex::Validate->new($args)->validate($form, $file);
}

###----------------------------------------------------------------###

sub conf_obj {
  my $self = shift || die "Sub \"conf_obj\" must be called as a method";
  return $self->{conf_obj} ||= do {
    require CGI::Ex::Conf;
    CGI::Ex::Conf->new(@_);
  };
}

sub conf_read {
  my $self = shift || die "Sub \"conf_read\" must be called as a method";
  return $self->conf_obj->read(@_);
}

###----------------------------------------------------------------###

### This is intended as a simple yet strong subroutine to swap
### in tags to a document.  It is intended to be very basic
### for those who may not want the full features of a Templating
### system such as Template::Toolkit (even though they should
### investigate them because they are pretty nice)
sub swap_template {
  my $self = shift || die "Sub \"swap_template\" must be called as a method";
  my $str  = shift;
  return $str if ! $str;
  my $ref  = ref($str) ? $str : \$str;

  ### basic - allow for passing a hash, or object, or code ref
  my $form = shift;
  $form = $self if ! $form && ref($self);
  $form = $self->get_form() if UNIVERSAL::isa($form, __PACKAGE__);
  
  my $get_form_value;
  my $meth;
  if (UNIVERSAL::isa($form, 'HASH')) {
    $get_form_value = sub {
      my $key = shift;
      return defined($form->{$key}) ? $form->{$key} : '';
    };
  } elsif ($meth = UNIVERSAL::can($form, $OBJECT_METHOD)) {
    $get_form_value = sub {
      my $key = shift;
      my $val = $form->$meth($key);
      return defined($val) ? $val : '';
    };
  } elsif (UNIVERSAL::isa($form, 'CODE')) {
    $get_form_value = sub {
      my $key = shift;
      my $val = &{ $form }($key);
      return defined($val) ? $val : '';
    };
  } else {
    die "Not sure how to use $form passed to swap_template_tags";
  }

  ### now do the swap
  $$ref =~ s{$TEMPLATE_OPEN \b (\w+) ((?:\.\w+)*) \b $TEMPLATE_CLOSE}{
    if (! $2) {
      &$get_form_value($1);
    } else {
      my @extra = split(/\./, substr($2,1));
      my $ref   = &$get_form_value($1);
      my $val;
      while (defined(my $key = shift(@extra))) {
        if (UNIVERSAL::isa($ref, 'HASH')) {
          if (! exists($ref->{$key}) || ! defined($ref->{$key})) {
            $val = '';
            last;
          }
          $ref = $ref->{$key};
        } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
          if (! exists($ref->[$key]) || ! defined($ref->[$key])) {
            $val = '';
            last;
          }
          $ref = $ref->[$key];
        } else {
          $val = '';
          last;
        }
      }
      if (! defined($val)) {
        if ($#extra == -1) {
          $val = $ref;
        }
        $val = '' if ! defined($val);
      }
      $val; # return of the swap
    }
  }xeg;

  return ref($str) ? 1 : $$ref;
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex - Yet Another Form Utility

=head1 SYNOPSIS

  ### CGI Module Extensions

  my $cgix = CGI::Ex->new;
  my $hashref = $cgix->get_form; # uses CGI by default
  
  ### send the Content-type header - whether or not we are mod_perl
  $cgix->print_content_type;

  my $val_hash = $cgix->conf_read($pathtovalidation);

  my $err_obj = $cgix->validate($hashref, $val_hash);
  if ($err_obj) {
    my $errors  = $err_obj->as_hash;
    my $input   = "Some content";
    my $content = "";
    SomeTemplateObject->process($input, $errors, $content);
    $cgix->fill({text => \$content, form => $hashref});
    print $content;
    exit;
  }

  print "Success\n";

  ### Filling functionality

  $cgix->fill({text => \$text, form    => \%hash});
  $cgix->fill({text => \$text, fdat    => \%hash});
  $cgix->fill({text => \$text, fobject => $cgiobject});
  $cgix->fill({text => \$text, form    => [\%hash1, $cgiobject]});
  $cgix->fill({text => \$text); # uses $self->object as the form
  $cgix->fill({text          => \$text,
                 form          => \%hash,
                 target        => 'formname',
                 fill_password => 0,
                 ignore_fields => ['one','two']});
  $cgix->fill(\$text); # uses $self->object as the form
  $cgix->fill(\$text, \%hash, 'formname', 0, ['one','two']);
  my $copy = $cgix->fill({scalarref => \$text,    fdat => \%hash});
  my $copy = $cgix->fill({arrayref  => \@lines,   fdat => \%hash});
  my $copy = $cgix->fill({file      => $filename, fdat => \%hash});

  ### Validation functionality

  my $err_obj = $cgix->validate($form, $val_hash);
  my $err_obj = $cgix->validate($form, $path_to_validation);
  my $err_obj = $cgix->validate($form, $yaml_string);

  ### get errors separated by key name
  ### useful for inline errors
  my $hash = $err_obj->as_hash;
  my %hash = $err_obj->as_hash;

  ### get aggregate list of errors
  ### useful for central error description
  my $array = $err_obj->as_array;
  my @array = $err_obj->as_array;

  ### get a string
  ### useful for central error description
  my $string = $err_obj->as_string;
  my $string = "$err_obj";

  $cgix->{raise_error} = 1;
  $cgix->validate($form, $val_hash);
    # SAME AS #
  my $err_obj = $cgix->validate($form, $val_hash);
  die $err_obj if $err_obj;

  ### Settings functionality

  ### read file via yaml
  my $ref = $cgix->conf_read('/full/path/to/conf.yaml');

  ### merge all found settings.pl files together
  @CGI::Ex::Conf::DEFAULT_PATHS = qw(/tmp /my/data/dir /home/foo);
  @CGI::Ex::Conf::DIRECTIVE     = 'MERGE';
  @CGI::Ex::Conf::DEFAULT_EXT   = 'pl';
  my $ref = $cgix->conf_read('settings');

=head1 DESCRIPTION

CGI::Ex is another form filler / validator / conf reader / template
interface.  Its goal is to take the wide scope of validators and other
useful CGI application modules out there and merge them into one
utility that has all of the necessary features of them all, as well
as several extended methods that I have found useful in working on the web.

The main functionality is provided by several other modules that
may be used separately, or together through the CGI::Ex interface.

=over 4

=item C<CGI::Ex::Fill>

A regular expression based form filler inner (accessed through B<-E<gt>fill>
or directly via its own functions).  Can be a drop in replacement for
HTML::FillInForm.  See L<CGI::Ex::Fill> for more information.

=item C<CGI::Ex::Validate>

A form field / cgi parameter / any parameter validator (accessed through
B<-E<gt>validate> or directly via its own methods).  Not quite a drop in
for most validators, although it has most of the functionality of most
of the validators but with the key additions of conditional validation.
Has a tightly integrated JavaScript portion that allows for duplicate client
side validation.  See L<CGI::Ex::Validate> for more information.

=item C<CGI::Ex::Conf>

A general use configuration, or settings, or key / value file reader.  Has
ability for providing key fallback as well as immutable key definitions.  Has
default support for yaml, storable, perl, ini, and xml and open architecture
for definition of others.  See L<CGI::Ex::Conf> for more information.

=item C<CGI::Ex::App>

A general purpose application framework.  Somewhat like CGI::Application - but
it does more of the common tasks for you.

=back

=head1 METHODS

=over 4

=item C<-E<gt>fill>

fill is used for filling hash or cgi object values into an existing
html document (it doesn't deal at all with how you got the document).
Arguments may be given as a hash, or a hashref or positional.  Some
of the following arguments will only work using CGI::Ex::Fill - most
will work with either CGI::Ex::Fill or HTML::FillInForm (assume they
are available unless specified otherwise).  (See L<CGI::Ex::Fill> for
a full explanation of functionality).  The arguments to fill are as
follows (and in order of position):

=over 4

=item C<text>

Text should be a reference to a scalar string containing the html to
be modified (actually it could be any reference or object reference
that can be modfied as a string).  It will be modified in place.
Another named argument B<scalarref> is available if you would like to
copy rather than modify.

=item C<form>

Form may be a hashref, a cgi style object, a coderef, or an array of
multiple hashrefs, cgi objects, and coderefs.  Hashes should be key
value pairs.  CGI objects should be able
to call the method B<param> (This can be overrided).  Coderefs should
expect expect the field name as an argument and should return a value.
Values returned by form may be undef, scalar, arrayref, or coderef 
(coderef values should expect an argument of field name and should
return a value).  The code ref options are available to delay or add
options to the bringing in of form informatin - without having to
tie the hash.  Coderefs are not available in HTML::FillInForm.  Also
HTML::FillInForm only allows CGI objects if an arrayref is used.

NOTE: Only one of the form, fdat, and fobject arguments are allowed at
a time.

=item C<target>

The name of the form that the fields should be filled to.  The default
value of undef, means to fill in all forms in the html.

=item C<fill_passwords>

Boolean value defaults to 1.  If set to zero - password fields will
not be filled.

=item C<ignore_fields>

Specify which fields to not fill in.  It takes either array ref of
names, or a hashref with the names as keys.  The hashref option is
not available in CGI::Ex::Fill.

=back

Other named arguments are available for compatiblity with HTML::FillInForm.
They may only be used as named arguments.

=over 4

=item C<scalarref>

Almost the same as the argument text.  If scalarref is used, the filled
html will be returned.  If text is used the html passed is filled in place.

=item C<arrayref>

An array ref of lines of the document.  Forces a returned filled html
document.

=item C<file>

An filename that will be opened, filled, and returned.

=item C<fdat>

A hashref of key value pairs.

=item C<fobject>

A cgi style object or arrayref of cgi style objects used for getting
the key value pairs.

=back

See L<CGI::Ex::Fill> for more information about the filling process.

=item C<-E<gt>object>

Returns the CGI object that is currently being used by CGI::Ex.  If none
has been set it will automatically generate an object of type
$PREFERRED_CGI_MODULE which defaults to B<CGI>.

=item C<-E<gt>validate>

Validate has a wide range of options available. (See L<CGI::Ex::Validate>
for a full explanation of functionality).  Validate has two arguments:

=over 4

=item C<form>

Can be either a hashref to be validated, or a CGI style object (which
has the param method).

=item C<val_hash>

The val_hash can be one of three items.  First, it can be a straight
perl hashref containing the validation to be done.  Second, it can
be a YAML document string.  Third, it can be the path to a file
containing the validation.  The validation in a validation file will
be read in depending upon file extension.

=back

=item C<-E<gt>get_form>

Very similar to CGI->new->Vars except that arrays are returned as
arrays.  Not sure why CGI::Val didn't do this anyway (well - yes -
legacy Perl 4 - but at some point things need to be updated).

=item C<-E<gt>set_form>

Allow for setting a custom form hash.  Useful for testing, or other
purposes.

=item C<-E<gt>get_cookies>

Returns a hash of all cookies.

=item C<-E<gt>make_form>

Takes a hash and returns a query_string.  A second optional argument
may contain an arrayref of keys to use from the hash in building the
query_string.  First argument is undef, it will use the form stored
in itself as the hash.

=item C<-E<gt>content_type>

Can be called multiple times during the same session.  Will only
print content-type once.  (Useful if you don't know if something
else already printed content-type).  Calling this sends the Content-type
header.  Trying to print -E<gt>content_type is an error.  For clarity,
the method -E<gt>print_content_type is available.

=item C<-E<gt>set_cookie>

Arguments are the same as those to CGI->new->cookie({}).
Uses CGI's cookie method to create a cookie, but then, depending on
if content has already been sent to the browser will either print
a Set-cookie header, or will add a <meta http-equiv='set-cookie'>
tag (this is supported on most major browsers).  This is useful if
you don't know if something else already printed content-type.

=item C<-E<gt>location_bounce>

Depending on if content has already been sent to the browser will either print
a Location header, or will add a <meta http-equiv='refresh'>
tag (this is supported on all major browsers).  This is useful if
you don't know if something else already printed content-type.  Takes
single argument of a url.

=item C<-E<gt>last_modified>

Depending on if content has already been sent to the browser will either print
a Last-Modified header, or will add a <meta http-equiv='Last-Modified'>
tag (this is supported on most major browsers).  This is useful if
you don't know if something else already printed content-type.  Takes an
argument of either a time (may be a CGI -expires style time) or a filename.

=item C<-E<gt>expires>

Depending on if content has already been sent to the browser will either print
a Expires header, or will add a <meta http-equiv='Expires'>
tag (this is supported on most major browsers).  This is useful if
you don't know if something else already printed content-type.  Takes an
argument of a time (may be a CGI -expires style time).

=item C<-E<gt>send_status>

Send a custom status.  Works in both CGI and mod_perl.  Arguments are
a status code and the content (optional).

=item C<-E<gt>send_header>

Send a http header.  Works in both CGI and mod_perl.  Arguments are
a header name and the value for that header.

=item C<-E<gt>print_js>

Prints out a javascript file.  Does everything it can to make sure
that the javascript will cache.  Takes either a full filename,
or a shortened name which will be looked for in @INC. (ie /full/path/to/my.js
or CGI/Ex/validate.js or CGI::Ex::validate)

=item C<-E<gt>swap_template>

This is intended as a simple yet strong subroutine to swap
in tags to a document.  It is intended to be very basic
for those who may not want the full features of a Templating
system such as Template::Toolkit (even though they should
investigate them because they are pretty nice).  The default allows
for basic template toolkit variable swapping.  There are two arguments.
First is a string or a reference to a string.  If a string is passed,
a copy of that string is swapped and returned.  If a reference to a
string is passed, it is modified in place.  The second argument is
a form, or a CGI object, or a cgiex object, or a coderef (if the second
argument is missing, the cgiex object which called the method will be
used).  If it is a coderef, it should accept key as its only argument and
return the proper value.

  my $cgix = CGI::Ex->new;
  my $form = {foo  => 'bar',
              this => {is => {nested => ['wow', 'wee']}}
             };

  my $str =  $cgix->swap_template("<html>[% foo %]<br>[% foo %]</html>", $form));
  # $str eq '<html>bar<br>bar</html>'

  $str = $cgix->swap_template("[% this.is.nested.1 %]", $form));
  # $str eq 'wee'

  $str = "[% this.is.nested.0 %]";
  $cgix->swap_template(\$str, $form);
  # $str eq 'wow'

  # may also be called with only one argument as follows:
  # assuming $cgix had a query string of ?foo=bar&baz=wow&this=wee
  $str = "<html>([% foo %]) <br>
          ([% baz %]) <br>
          ([% this %]) </html>";
  $cgix->swap_template(\$str);
  #$str eq "<html>(bar) <br>
  #        (wow) <br>
  #        (wee) </html>";
  
For further examples, please see the code contained in t/samples/cgi_ex_*
of this distribution.

If at a later date, the developer upgrades to Template::Toolkit, the
templates that were being swapped by CGI::Ex::swap_template should
be compatible with Template::Toolkit.

=back

=head1 EXISTING MODULES

The following is a list of existing validator and formfiller modules
at the time of this writing (I'm sure this probably isn't exaustive).

=over 4

=item C<Email::Valid> - Validator

=item C<SSN::Validate> - Validator

=item C<Embperl::Form::Validate> - Validator

=item C<Data::CGIForm> - Validator

=item C<HTML::FillInForm> - Form filler-iner

=item C<CGI> - CGI Getter.  Form filler-iner

=head1 TODO

Allow for mod_perl 2 header support.

=head1 MODULES

See also L<CGI::Ex::Fill>.

See also L<CGI::Ex::Validate>.

See also L<CGI::Ex::Conf>.

See also L<CGI::Ex::Dump>.

See also L<CGI::Ex::Die>.

See also L<CGI::Ex::App>.

See also L<CGI::Ex::Auth>.

=head1 AUTHOR

Paul Seamons

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut

1;
