package HTML::Form;

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the GNU General Public License without warranty #
###----------------------------------------------------------------###

### See perldoc at bottom

use strict;
use vars qw($PREFERRED_FILL_MODULE
            $PREFERRED_CGI_MODULE
            $OBJECT_METHOD
            $AUTOLOAD
            );

use Data::DumpEx;

$PREFERRED_FILL_MODULE = '';
$PREFERRED_CGI_MODULE  = 'CGI';
$OBJECT_METHOD = 'param';

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $self  = ref($_[0]) ? shift : {@_};
  return bless $self, $class;
}

### allow for holding another classed CGI style object
sub object {
  return shift()->{object} ||= do {
    my $file = $PREFERRED_CGI_MODULE;
    $file .= ".pm" if $file !~ /\.\w+$/;
    $file =~ s|::|/|g;
    eval {require $file};
    if ($@) {
      die "Couldn't require $PREFERRED_CGI_MODULE: $@";
    }
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

### form filler that will use either HTML::FillInForm, HTML::Form::Fill
### or another specified filler.  Argument style is similar to
### HTML::FillInForm.
sub fill {
  my $self = shift;
  my $args = ref $_[0] ? shift : {@_};

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
    eval { require HTML::Form::Fill };
    if ($@) {
      die "Couldn't require HTML::Form::Fill: $@";
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
    
    &HTML::Form::Fill::form_fill($ref,
                                 $form,
                                 $args->{target},
                                 $args->{fill_password},
                                 $args->{ignore_fields},
                                 );
    return ! $args->{text} ? $$ref : 1;
  }

}


###----------------------------------------------------------------###

1;

__END__

=head1 NAME

HTML::Form - Yet Another Form Utility

=head1 DESCRIPTION

HTML::Form is another form filler/ validator.  Its goal is to take
the wide scope of validators out there and merge them into one
utility that has all of the necessary features of them all, as well
as several that I have found useful in working on the web.

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
standards and poor for performance.  L<HTML::Form::Fill>

=item C<CGI> - CGI Getter.  Form filler-iner

Pro - It's with every distribution.  It is the king of CGI modules (that is
why we are based off of it). 

Con - Your html is in your cgi - not good at all.  Even for one-off's it is better to use a form filler.

=head1 AUTHOR

Paul Seamons

=cut

1;
