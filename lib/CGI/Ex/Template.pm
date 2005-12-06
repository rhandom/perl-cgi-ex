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
  return bless $args, $class;
}

###----------------------------------------------------------------###

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

sub stash {
    my $self = shift;
    if (@_) {
        die "Stash must be a hashref" if ! UNIVERSAL::isa($_[0], 'HASH');
        $self->{'STASH'} = shift;
    }
    return $self->{'STASH'} ||= {};
}

sub include_path {
    my $self = shift;
    return $self->{'INCLUDE_PATH'} ||= [@INCLUDE_PATH];
}

sub include_filename {
    my ($self, $file) = @_;
    if ($file =~ m|^/|) {
        # check if absolute is allowed
        return $file if -e $file;
    } else {
        my $paths = $self->include_path;
        $paths = [$paths] if ! ref $paths;
        foreach my $path (@$paths) {
            return "$path/$file" if -e "$path/$file";
        }
    }
    die "Couldn't find \"$file\" in INCLUDE_PATH";
}

sub include_file {
    my ($self, $file) = @_;
    my $full = $self->include_filename($file);
    open(my $fh, "<$full") || die "Couldn't open $file for reading: $!";
    read $fh, my $txt, -s $full;
    return $txt;
}

sub process {
    my ($self, $in, $swap, $out) = @_;

    ### get the content
    my $content;
    if (ref $in) {
        if (UNIVERSAL::isa($in, 'SCALAR')) { # reference to a string
            $content = $$in;
        } else { # should be a file handle
            local $/ = undef;
            $content = <$in>;
        }
    } else {
        $content = $self->include_file($in);
    }

    ### do swap
    $self->swap(\$content, $swap);

    ### put it back out
    if (ref $out) {
        if (UNIVERSAL::isa($out, 'SCALAR')) { # reference to a string
            $$out = $content;
        } else { # should be a file handle
            print $out $content;
        }
    } elsif ($out) {
        my $file = $self->include_filename($out);
        open(my $fh, ">$file") || die "Couldn't open \"$out\" for writing: $!";
        print $fh $content;
    } else {
        print $content;
    }

    return 1;
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

