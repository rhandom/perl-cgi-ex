package CGI::Ex::Wrap;

### CGI Extended Templating

###----------------------------------------------------------------###
#  Copyright 2004 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom

use strict;
use vars qw($OPEN_TAG $CLOSE_TAG);
use CGI::Ex::Dump qw(debug);

BEGIN {
  $OPEN_TAG  = '[(';
  $CLOSE_TAG = ')]';
}

sub new {
  my $class = shift || __PACKAGE__;
  my $self  = ref($_[0]) ? shift : {@_};
  bless $self, $class;
  $self->init();
  return $self;
}

sub init {}

sub fatal {
  my $self = shift;
  die $self->error_object(@_);
}

sub error_object {
  my $self = shift;
  my $pkg  = $self->error_package;
  return $pkg->new(@_);
}

sub error_package {
  "CGI::Ex::Wrap::Error";
}

###----------------------------------------------------------------###

sub wrap {
  my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift() : new();

  ### allow pass by ref or value, ref is modified
  my $str  = defined($_[0]) ? shift : return;
  my $ref  = ref($str) ? $str : \$str;

  my $swap = \@_;

  ### what tags will we be using today?
  my $open_tag  = $self->{open_tag}  ||= $OPEN_TAG;
  my $close_tag = $self->{close_tag} ||= $CLOSE_TAG;
  my $open_len  = length($open_tag);
  my $close_len = length($close_tag);

  ### find opening tags
  my $pos_o = 0;
  while (1) {
    $pos_o = index($$ref, $open_tag, $pos_o);
    last if $pos_o == -1;

    ### find equally nested closing tags (hopefully)
    my $pos_c = $pos_o + $open_len;
    my $match;
    while (1) {
      $pos_c = index($$ref, $close_tag, $pos_c);
      $self->fatal("Missing closing tag", $ref, $pos_o)
        if $pos_c == -1;
      debug $pos_c;

      $match = substr($$ref, $pos_o + $open_len, $pos_c - ($pos_o + $open_len));
      ### need to count occurances in match
      last;
    }

    ### get the replacement text - make sure its OK
    my $replace = eval { $self->wrap_interpret(\$match) };
    if ($@) {
      my $err = UNIVERSAL::isa($@, $self->error_package) ? $@ : $self->error_object($@);
      $err->set_ref($ref);
      $err->set_pos($pos_o);
      die $err; # propagate error up

    ### if the chunk couldn't be found - restore it verbatim (well - skip it)
    } elsif (! defined $replace) {
      $pos_o += length($match) + $open_len + $close_len;

    ### we should have a valid chunk now - swap it and continue
    } else {
      substr($$ref,
             $pos_o,
             length($match) + $open_len + $close_len,
             $replace
             );
      $pos_o += length($replace);
    }
  }

  ### return copy or success
  return ref($str) ? 1 : $str;
}

sub wrap_interpret {
  my $self = shift;
  my $ref  = shift;
  "foo";
}

###----------------------------------------------------------------###

package CGI::Ex::Wrap::Error;

use strict;
use overload '""' => \&as_string;
use CGI::Ex::Dump qw(debug);

sub new {
  my $class = shift;
  my $self  = bless {}, $class;
  $self->{msg}    = shift || "An error has occured";
  $self->{ref}    = shift;
  $self->{pos}    = shift || 0;
  $self->{offset} = shift || 0;
  return $self;
}

sub set_ref {
  my $self = shift;
  $self->{ref} = shift;
}

sub set_pos {
  my $self = shift;
  $self->{pos} = shift;
}

sub set_offset {
  my $self = shift;
  $self->{offset} = shift;
}

sub add_offset {
  my $self = shift;
  $self->{offset} += shift || 0;
}

sub as_string {
  my $self = shift;
  my $msg  = $self->{msg};
  if (substr($msg, -1, 1) ne "\n") {
    my $char = $self->{pos} + $self->{offset};
    $msg .= " at char $char\n";
  }
  return $msg;
}

###----------------------------------------------------------------###
  
1;

__END__

[(set foo "what is all this")] # sets it
[(foo)]                 # prints it out
[( foo )]               # prints it out
[( "[(foo)]" )]         # prints it out
[(set foo)]             # goes to undef
[( "[(foo)]" )]         # prints [(foo)]
[(set baz "hmm")]       # sets it
[( || foo bar baz )]    # prints "hmm"
[( || foo "[(bar)]" baz # prints [(bar)]
[( set foo "one" "two" "three" )]


[( set foo "Some odd strin)g&#@#" )]
[( henc foo )]          # html encode
[( uenc foo )]          # url encode
[( set foo {Some extended string} )]
[(if foo {
    A long string
  } else {
    The other case
  }
)] 



[( set grr (match 'm//' dog) )]

[(if (eq foo "bar") ()
  elsif (eq foo "baz") ()
  else ())] 
