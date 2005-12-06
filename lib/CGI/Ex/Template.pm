package CGI::Ex::Template;

use strict;
use vars qw(@INCLUDE_PATH
            $CONTENT_SUBDIR
            $TEMPLATE_OPEN
            $TEMPLATE_CLOSE
            );

BEGIN {
    $CONTENT_SUBDIR ||= 'content';
    $TEMPLATE_OPEN  ||= qr/\[%\s*/;
    $TEMPLATE_CLOSE ||= qr/\s*%\]/;
};

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $args  = ref($_[0]) ? shift : {@_};

  $args->{INCLUDE_PATH} ||= \@INCLUDE_PATH;

  return bless $args, $class;
}

###----------------------------------------------------------------###

sub stash {
    my $self = shift;
    if (@_) {
        die "Stash must be a hashref" if ! UNIVERSAL::isa($_[0], 'HASH');
        $self->{'STASH'} = shift;
    }
    return $self->{'STASH'} ||= {};
}

### This is intended as a simple yet strong subroutine to swap
### in tags to a document.  It is intended to be very basic
### for those who may not want the full features of a Templating
### system such as Template::Toolkit (even though they should
### investigate them because they are pretty nice)
sub swap {
  my $self = shift;
  my $str  = shift;
  my $form = shift;
  $self->stash($form) if $form;

  return $str if ! $str;
  my $ref  = ref($str) ? $str : \$str;

  $self->swap_variables($ref);

  return ref($str) ? 1 : $$ref;
}

sub swap_variables {
    my $self = shift;
    my $ref  = shift;
    my $stash = $self->stash;

  ### now do the swap
  $$ref =~ s{$TEMPLATE_OPEN \b (\w+) ((?:\.\w+)*) \b $TEMPLATE_CLOSE}{
    my ($name, $extra) = ($1, $2);
    my $val;
    if (! $extra) {
      $val = defined($stash->{$name}) ? $stash->{$name} : '';
    } else {
      my @extra = split(/\./, substr($extra,1));
      my $ref   = defined($stash->{$name}) ? $stash->{$name} : '';
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
    }
    $val; # return of the swap
  }xeg;

}

###----------------------------------------------------------------###


sub process {
  my $self = ref($_[0]) ? shift : shift->new;
  my $in   = shift;

  ### force the content to have a .html prefix
  if (! ref $in) {
    $in .= '.html' if $in !~ /\.\w+$/;
  }

  ### prepend "content" dir as needed
  if (! ref($in)                                # not a scalar ref or a file glob
      && $in =~ m|^\w+(\.\w+)?(/\w+(\.\w+)?)*$| # not an absolute filename
      && index($in, $CONTENT_SUBDIR) == -1) {
    $in = $CONTENT_SUBDIR .'/'. $in;
  }

  return $self->SUPER::process($in, @_);
}

###----------------------------------------------------------------###

sub out {
  my $self = ref($_[0]) ? shift : shift->new;
#  dex $self;
  my $in   = shift;
  my $form = shift;
  my $fill = shift;
  my $out  = '';

  ### run the template
  my $status = $self->process($in, $form, \$out) || die $Template::ERROR;

  ### fill in any forms

  return $out;
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex::Template - Beginning interface to Templating systems - for they are many

=head1 SYNOPSIS

  None yet.

=head1 DESCRIPTION

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut

