package CGI::Ex::Config;

### CGI Extended Conf Reader

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom

use strict;
use vars qw($VERSION 
            @DEFAULT_DIRS
            $DEFAULT_EXT
            %EXT_HANDLERS
            $ARRAY_DIRECTIVE
            $HASH_DIRECTIVE
            $HASH_IMMUTABLE_QR
            );

$VERSION = '0.1';

$DEFAULT_EXT = 'conf';

%EXT_HANDLERS = (''         => \&conf_handler_yaml,
                 'conf'     => \&conf_handler_yaml,
                 'ini'      => \&conf_handler_ini,
                 'pl'       => \&conf_handler_pl,
                 'sto'      => \&conf_handler_storable,
                 'storable' => \&conf_handler_storable,
                 'val'      => \&conf_handler_yaml,
                 'xml'      => \&conf_handler_xml,
                 'yaml'     => \&conf_handler_yaml,
                 );

$ARRAY_DIRECTIVE = 'FIRST'; # FIRST, LAST, JOIN

$HASH_DIRECTIVE = 'MERGE'; # FIRST, LAST, MERGE

$HASH_IMMUTABLE_QR = qr/_immu(?:ne|table|)$/i;

###----------------------------------------------------------------###

sub new {
  my $class = shift || __PACKAGE__;
  my $self  = (@_ && ref($_[0])) ? shift : {@_}; 

  return bless $self, $class;
}

sub dirs {
  my $self = shift;
  return $self->{dirs} ||= \@DEFAULT_DIRS;
}

sub get_ref {
  my $self = shift;
  my $file  = shift;
  my $ext;

  ### they passed the right stuff already
  if (ref $file) {
    return $file;

  ### if contains a newline - treat it as a YAML string
  } elsif ($file =~ /\n/) {
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
sub load {
  my $self      = shift;
  my $namespace = shift;
  my $dirs      = $self->dirs;
  my $REF       = shift; # can pass in existing set of options
  my $found;

  $namespace =~ s|::|/|g; # allow perlish style

  foreach my $dir (ref($dirs) ? @$dirs : $dirs) {
    my $ref = $self->get_ref("$dir/$namespace") || next;
    if (! $REF) {
      if (UNIVERSAL::isa($ref, 'ARRAY')) {
        $REF = [];
      } elsif (UNIVERSAL::isa($ref, 'HASH')) {
        $REF = {};
      } else {
        die "Unknown config type on $ref for namespace $namespace";
      }
    } elsif (! UNIVERSAL::isa($ref, ref($REF))) {
      die "Found different reference types for namespace $namespace";
    }
    if (ref($REF) eq 'ARRAY') {
      my $direct = $self->{array_directive} || $ARRAY_DIRECTIVE;
      if ($direct eq 'JOIN') {
        push @$REF, @$ref;
      } else {
        $REF = [@$ref];
        return $REF if $direct eq 'FIRST';
      }
    } else {
      my $direct = $self->{hash_directive} || $HASH_DIRECTIVE;
      my ($key,$val);
      if ($direct eq 'MERGE') {
        while (($key,$val) = each %$ref) {
          my $immute = $key =~ s/$HASH_IMMUTABLE_QR//o;
          next if $immute && exists $REF->{$key};
          $REF->{$key} = $val;
        }
      } else {
        $REF = {};
        while (($key,$val) = each %$ref) {
          my $immute = $key =~ s/$HASH_IMMUTABLE_QR//o;
          $REF->{$key} = $val;
        }
        return $REF if $direct eq 'FIRST';
      }
    }
  }
  return $REF;
}

###----------------------------------------------------------------###

sub conf_handler_ini {
  my $file = shift;
  require Config::IniHash;
  return &Config::IniHash::ReadINI($file);
}

sub conf_handler_pl {
  my $file = shift;
  return do $file;
}

sub conf_handler_storable {
  my $file = shift;
  require Storable;
  return &Storable::retrieve($file);
}

sub conf_handler_yaml {
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

sub conf_handler_xml {
  my $file = shift;
  require XML::Simple;
  return XML::Simple::XMLin($file);
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex::Config - CGI Extended Conf Reader

=head1 DESCRIPTION

There are half a million Conf readers out there.  Why not add one more.
Actually, this module provides a wrapper around the many file formats
and the config modules that can handle them.

=cut
