package CGI::Ex::Conf;

### CGI Extended Conf Reader

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom

use strict;
use vars qw($VERSION 
            @DEFAULT_PATHS
            $DEFAULT_EXT
            %EXT_HANDLERS
            $DIRECTIVE
            $IMMUTABLE_QR
            $IMMUTABLE_KEY
            );

$VERSION = '0.1';

$DEFAULT_EXT = 'conf';

%EXT_HANDLERS = (''         => \&read_handler_yaml,
                 'conf'     => \&read_handler_yaml,
                 'ini'      => \&read_handler_ini,
                 'pl'       => \&read_handler_pl,
                 'sto'      => \&read_handler_storable,
                 'storable' => \&read_handler_storable,
                 'val'      => \&read_handler_yaml,
                 'xml'      => \&read_handler_xml,
                 'yaml'     => \&read_handler_yaml,
                 );

### $DIRECTIVE controls how files are looked for.
### If directories 1, 2 and 3 are passed and each has a config file
### LAST would return 3, FIRST would return 1, and MERGE will
### try to put them all together.  Merge behavior of hashes
### is determined by $IMMUTABLE_\w+ variables.
$DIRECTIVE = 'LAST'; # LAST, MERGE, FIRST

$IMMUTABLE_QR = qr/_immu(?:table)?$/i;

$IMMUTABLE_KEY = 'immutable';

###----------------------------------------------------------------###

sub new {
  my $class = shift || __PACKAGE__;
  my $self  = (@_ && ref($_[0])) ? shift : {@_}; 

  return bless $self, $class;
}

sub paths {
  my $self = shift;
  return $self->{paths} ||= \@DEFAULT_PATHS;
}

sub read_ref {
  my $self = shift;
  my $file  = shift;
  my $ext;

  ### they passed the right stuff already
  if (ref $file) {
    return $file;

  ### if contains a newline - treat it as a YAML string
  } elsif (index($file,"\n") != -1) {
    return &yaml_load($file);

  ### otherwise base it off of the file extension
  } elsif ($file =~ /\.(\w+)$/) {
    $ext = $1;
  } else {
    $ext = defined($self->{default_ext}) ? $self->{default_ext}
      : defined($DEFAULT_EXT) ? $DEFAULT_EXT : '';
    $file = length($ext) ? "$file.$ext" : $file;
  }

  ### determine the handler
  my $handler;
  if ($self->{handler}) {
    if (UNIVERSAL::isa($self->{handler}, 'CODE')) {
      $handler = $self->{handler};
    } else {
      $handler = $self->{handler}->{$ext};
    }
  }
  if (! $handler) {
    $handler = $EXT_HANDLERS{$ext} || die "Unknown file extension: $ext";
  }

  return scalar eval { &$handler($file) };
}

### allow for different kinds of merging of arguments
### allow for key fallback on hashes
### allow for immutable values on hashes
sub read {
  my $self      = shift;
  my $namespace = shift;
  my $REF       = shift;       # can pass in existing set of options
  my $IMMUTABLE = shift || {}; # can pass existing immutable types

  $self = $self->new() if ! ref $self;

  ### allow for fast short ciruit on path lookup for several cases
  my $directive;
  my @paths = ();
  if (ref($namespace)                 # already a ref
      || index($namespace,"\n") != -1 # yaml string to read in
      || $namespace =~ m|^\.{0,2}/.+$|  # absolute or relative file
      ) {
    push @paths, $namespace;
    $directive = 'FIRST';

  ### use the default directories
  } else {
    $directive = uc($self->{directive} || $DIRECTIVE);
    $namespace =~ s|::|/|g;  # allow perlish style namespace
    my $paths = $self->paths || die "No paths found during read on $namespace";
    $paths = [$paths] if ! ref $paths;
    if ($directive eq 'LAST') { # LAST shall be FIRST
      $directive = 'FIRST';
      $paths = [reverse @$paths] if $#$paths != 0;
    }
    @paths = map {"$_/$namespace"} @$paths;
  }


  ### now loop looking for a ref
  foreach my $path (@paths) {
    my $ref = $self->read_ref($path) || next;
    if (! $REF) {
      if (UNIVERSAL::isa($ref, 'ARRAY')) {
        $REF = [];
      } elsif (UNIVERSAL::isa($ref, 'HASH')) {
        $REF = {};
      } else {
        die "Unknown config type of \"".ref($ref)."\" for namespace $namespace";
      }
    } elsif (! UNIVERSAL::isa($ref, ref($REF))) {
      die "Found different reference types for namespace $namespace"
        . " - wanted a type ".ref($REF);
    }
    if (ref($REF) eq 'ARRAY') {
      if ($directive eq 'MERGE') {
        push @$REF, @$ref;
        next;
      }
      splice @$REF, 0, $#$REF + 1, @$ref;
      last;
    } else {
      my $immutable = delete $ref->{$IMMUTABLE_KEY};
      my ($key,$val);
      if ($directive eq 'MERGE') {
        while (($key,$val) = each %$ref) {
          next if $IMMUTABLE->{$key};
          my $immute = $key =~ s/$IMMUTABLE_QR//o;
          $IMMUTABLE->{$key} = 1 if $immute || $immutable;
          $REF->{$key} = $val;
        }
        next;
      }
      delete $REF->{$key} while $key = each %$REF;
      while (($key,$val) = each %$ref) {
        my $immute = $key =~ s/$IMMUTABLE_QR//o;
        $IMMUTABLE->{$key} = 1 if $immute || $immutable;
        $REF->{$key} = $val;
      }
      last;
    }
  }
  $REF->{"Immutable Keys"} = $IMMUTABLE if scalar keys %$IMMUTABLE;
  return $REF;
}

###----------------------------------------------------------------###

sub read_handler_ini {
  my $file = shift;
  require Config::IniHash;
  return &Config::IniHash::ReadINI($file);
}

sub read_handler_pl {
  my $file = shift;
  ### do has odd behavior in that it turns a simple hashref
  ### into hash - help it out a little bit
  my @ref = do $file;
  return ($#ref != 0) ? {@ref} : $ref[0];
}

sub read_handler_storable {
  my $file = shift;
  require Storable;
  return &Storable::retrieve($file);
}

sub read_handler_yaml {
  my $file = shift;
  local $/ = undef;
  local *IN;
  open (IN,$file) || die "Couldn't open $file: $!";
  my $text = <IN>;
  close IN;
  return &yaml_load($text);
}

sub yaml_load {
  my $text = shift;
  require YAML;
  my @ret = eval { &YAML::Load($text) };
  if ($@) {
    die "$@";
  }
  return ($#ret == 0) ? $ret[0] : \@ret;
}

sub read_handler_xml {
  my $file = shift;
  require XML::Simple;
  return XML::Simple::XMLin($file);
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex::Conf - CGI Extended Conf Reader

=head1 DESCRIPTION

There are half a million Conf readers out there.  Why not add one more.
Actually, this module provides a wrapper around the many file formats
and the config modules that can handle them.

=cut
