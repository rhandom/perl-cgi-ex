package CGI::Ex;

### CGI Extended

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the GNU General Public License without warranty #
###----------------------------------------------------------------###

### See perldoc at bottom

use strict;
use vars qw($VERSION
            $PREFERRED_FILL_MODULE
            $PREFERRED_CGI_MODULE
            $PREFERRED_CGI_REQUIRED
            $PREFERRED_VAL_MODULE
            $OBJECT_METHOD
            $AUTOLOAD
            $DEBUG_LOCATION_BOUNCE
            @EXPORT @EXPORT_OK
            );
use base qw(Exporter);
use Data::DumpEx;

$VERSION               = '1.0';
$PREFERRED_FILL_MODULE ||= '';
$PREFERRED_CGI_MODULE  ||= 'CGI';
$PREFERRED_VAL_MODULE  ||= '';
@EXPORT = ();
@EXPORT_OK = qw(get_form get_cookies
                content_type content_typed
                );

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $self  = ref($_[0]) ? shift : {@_};
  return bless $self, $class;
}

### allow for holding another classed CGI style object
sub object {
  return shift()->{object} ||= do {
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

### form getter that will act like ->Vars only it will be intelligent
### thus infering that portions of CGI.pm are unintelligent
sub get_form {
  my $self = shift || __PACKAGE__;
  $self = $self->new(@_) if ! ref $self;

  my $obj  = $self->object;
  my %hash = ();
  foreach my $key ($obj->param()) {
    my @val = $obj->param($key);
    $hash{$key} = ($#val == -1) ? die : ($#val == 0) ? $val[0] : \@val;
  }
  return \%hash;
}

### like get_form - but a hashref of cookies
### cookies are parsed depending upon the functionality of ->cookie
sub get_cookies {
  my $self = shift || __PACKAGE__;
  $self = $self->new(@_) if ! ref $self;

  my $obj  = $self->object;
  my %hash = ();
  foreach my $key ($obj->cookie()) {
    my @val = $obj->cookie($key);
    $hash{$key} = ($#val == -1) ? die : ($#val == 0) ? $val[0] : \@val;
  }
  return \%hash;
}

###----------------------------------------------------------------###

sub content_type {
  my $self = ref($_[0]) ? shift : undef;
  my $type = shift || 'text/html';

  if ($ENV{MOD_PERL} && (my $r = Apache->request)) {
    return if $r->bytes_sent;
    $r->content_type($type);
    $r->send_http_header;
  } else {
    if (! $ENV{CONTENT_TYPED}) {
      print "Content-type: $type\r\n\r\n";
      $ENV{CONTENT_TYPED} = '';
    }
    $ENV{CONTENT_TYPED} .= sprintf("%s, %d\n", (caller)[1,2]);
  }
}

sub content_typed {
  my $self = ref($_[0]) ? shift : undef;
  if ($ENV{MOD_PERL} && (my $r = Apache->request)) {
    return $r->bytes_sent;
  } else {
    return ($ENV{CONTENT_TYPED}) ? 1 : undef;
  }
}

###----------------------------------------------------------------###

### location bounce nicely - even if we have already sent content
sub location_bounce {
  my $self = ref($_[0]) ? shift : undef;
  my $loc  = shift || '';
  if (&content_typed()) {
    if ($DEBUG_LOCATION_BOUNCE) {
      print "<a class=debug href=\"$loc\">Location: $loc</a><br />\n";
    } else {
      print "<meta http-equiv=\"refresh\" content=\"0;url=$loc\" />\n";
    }
  } else {
    if ($ENV{MOD_PERL} && (my $r = Apache->request)) {
      $r->header_out("Location", $loc);
      $r->status(302);
      $r->content_type('text/html');
      $r->send_http_header;
      $r->print("Bounced to $loc\n");
    } else { 
      print "Location: $loc\r\n",
            "Status: 302 Bounce\r\n",
            "Content-type: text/html\r\n\r\n",
            "Bounced to $loc\r\n";
    }
  }
}

### set a cookie nicely - even if we have already sent content
sub set_cookie {
  my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift : undef;
  my $args = ref($_[0]) ? shift : {@_};
  foreach (keys %$args) {
    next if /^-/;
    $args->{"-$_"} = delete $args->{$_};
  }

  ### default path to / and allow for 1hour instead of 1h
  ### (if your gonna make expires useful - make it useful)
  $args->{-path} ||= '/';
  if ($args->{-expires} && $args->{-expires} =~ /[a-z]/) {
    $args->{-expires} =~ s/(?<=\d[a-z])[a-z]+$//; # trim hour to h
    $args->{-expires} =~ s/(?<=\d) +(?=[a=z])//; # optional space
    $args->{-expires} =~ s/^(?=\d)/+/; # required leading +
  }

  my $cookie = "" . $self->object->cookie(%$args);

  if (&content_typed()) {
    print "<meta http-equiv=\"Set-cookie\" content=\"$cookie\" />\n";
  } else {
    if ($ENV{MOD_PERL} && (my $r = Apache->request)) {
      $r->header_out("Set-cookie", $cookie);
    } else {
      print "Set-cookie: $cookie\r\n"
    }
  }
}

###----------------------------------------------------------------###

### form filler that will use either HTML::FillInForm, CGI::Ex::Fill
### or another specified filler.  Argument style is similar to
### HTML::FillInForm.
# $object->fill({text => \$text, form    => \%hash});
# $object->fill({text => \$text, fdat    => \%hash});
# $object->fill({text => \$text, fobject => $cgiobject});
# $object->fill({text => \$text, form    => [\%hash1, $cgiobject]});
# $object->fill({text => \$text); # uses $self->object as the form
# $object->fill({text          => \$text,
#                form          => \%hash,
#                target        => 'formname',
#                fill_password => 0,
#                ignore_fields => ['one','two']});
# $object->fill(\$text); # uses $self->object as the form
# $object->fill(\$text, \%hash, 'formname', 0, ['one','two']);
# my $copy = $object->fill({scalarref => \$text,    fdat => \%hash});
# my $copy = $object->fill({arrayref  => \@lines,   fdat => \%hash});
# my $copy = $object->fill({file      => $filename, fdat => \%hash});
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
    eval { require CGI::Ex::Fill };
    if ($@) {
      die "Couldn't require CGI::Ex::Fill: $@";
    }

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
  my $self = shift;
  my ($form, $file) = (@_ == 2) ? (shift, shift) : ($self->object, shift);

  eval { require CGI::Ex::Validate };
  if ($@) {
    die "Couldn't require CGI::Ex::Validate: $@";
  }
  return CGI::Ex::Validate->new({raise_error => 1})->validate($form, $file);
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex - Yet Another Form Utility

=head1 SYNOPSIS

  ### CGI Module Extensions

  ### Filling functionality

  $object->fill({text => \$text, form    => \%hash});
  $object->fill({text => \$text, fdat    => \%hash});
  $object->fill({text => \$text, fobject => $cgiobject});
  $object->fill({text => \$text, form    => [\%hash1, $cgiobject]});
  $object->fill({text => \$text); # uses $self->object as the form
  $object->fill({text          => \$text,
                 form          => \%hash,
                 target        => 'formname',
                 fill_password => 0,
                 ignore_fields => ['one','two']});
  $object->fill(\$text); # uses $self->object as the form
  $object->fill(\$text, \%hash, 'formname', 0, ['one','two']);
  my $copy = $object->fill({scalarref => \$text,    fdat => \%hash});
  my $copy = $object->fill({arrayref  => \@lines,   fdat => \%hash});
  my $copy = $object->fill({file      => $filename, fdat => \%hash});

  ### Validation functionality
  

=head1 DESCRIPTION

CGI::Ex is another form filler/ validator.  Its goal is to take
the wide scope of validators out there and merge them into one
utility that has all of the necessary features of them all, as well
as several that I have found useful in working on the web.

=head1 METHODS

=over 4

=item C<-E<gt>fill>

fill is used for filling hash or cgi object values into an existing
html document (it doesn't deal at all with how you got the document).
Arguments may be given as a hash, or a hashref or positional.  Some
of the following arguments will only work using CGI::Ex::Fill - most
will work with either CGI::Ex::Fill or HTML::FillInForm (assume they
are available unless specified otherwise).  The arguments are as
follows (and in order of posistion):

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

=back

=head1 EXISTING MODULES FOR VALIDATION

The following is a list of existing validator and formfiller modules
at the time of this writing (I'm sure this probably isn't exaustive).
Inheritable
Separate direcotry
Global hash
function
add new directory

=over 4

=item C<Email::Valid> - Validator

Pro - Good email checker. Con - doesn't do much else.  What we
will inherit - well - we will have a type that uses it natively.

=item C<SSN::Validate> - Validator

Pro - Good SSN checker.  Con - doesn't do much else. What we
will inherit - we will use it natively.

=item C<Embperl::Form::Validate> - Validator

Pro - Add multiple rules.  Rules are array based (in order).  Has
multilanguage support.  Returns array individual hashes for errors.
Can also return array of messages.  Ability to generate JavaScript
code.  Extensible for other types (requires inheritance.

Con - Part of the Embperl distribution (not that Embperl is wrong,
just that this is a general function utility in a specialized package
- anybody wanting to use it has to install all of Embperl).
JavaScript requires form name passed to it.

=item C<Data::CGIForm> - Validator

=item C<HTML::FillInForm> - Form filler-iner

Pro - HTML::Parser based.  Very simple script.  Supports most
things you'd want to do.

Con - HTML::Parser based.  Being based on HTML::Parser is good for
standards and poor for performance.  L<CGI::Ex::Fill>.

=item C<CGI> - CGI Getter.  Form filler-iner

Pro - It's with every distribution.  It is the king of CGI modules (that is
why we are based off of it). 

Con - Your html is in your cgi - not good at all.  Even for one-off's it is better to use a form filler.

=head1 AUTHOR

Paul Seamons

=cut

1;
