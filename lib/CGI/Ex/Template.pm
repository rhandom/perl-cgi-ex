package CGI::Ex::Template;

use strict;
use vars qw(@INCLUDE_PATH $CONTENT_SUBDIR);
use base qw(Template);

use CGI::Ex;
use CGI::Ex::Fill;

$CONTENT_SUBDIR ||= 'content';

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $args  = ref($_[0]) ? shift : {@_};

  $args->{INCLUDE_PATH} ||= \@INCLUDE_PATH;

  return $class->SUPER::new($args);
}

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
  &CGI::Ex::Fill::form_fill(\$out, $fill) if $fill && ! $self->{no_fill};

  return $out;
}

sub print {
  my $self   = ref($_[0]) ? shift : shift->new;
  my $in     = shift;
  my $form   = shift;
  my $fill   = shift || $form;

  &CGI::Ex::content_type();
  print $self->out($in, $form, $fill);
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

