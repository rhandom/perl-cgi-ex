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

sub wrap {
  my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift() : new();

  ### allow pass by ref or value, ref is modified
  my $str  = defined($_[0]) ? shift : return;
  my $ref  = ref($str) ? $str : \$str;

  my $swap = \@_;

  ### what tags will we be using today?
  my $open_tag  = $self->{open_tag}  || $OPEN_TAG;
  my $close_tag = $self->{close_tag} || $CLOSE_TAG;
  my $open_len  = length($open_tag);
  my $close_len = length($close_tag);

  my $pos_o = 0;
  OUTER: while (1) {
    $pos_o = index($$ref, $open_tag, $pos_o);
    last OUTER if $pos_o == -1;
    debug $pos_o;

    my $pos_c = $pos_o + $open_len;
    my $match;
    while (1) {
      $pos_c = index($$ref, $close_tag, $pos_c);
      last OUTER if $pos_c == -1;
      debug $pos_c;

      $match = substr($$ref, $pos_o + $open_len, $pos_c - ($pos_o + $open_len));
      ### need to count occurances in match
      last;
    }

    my $replace = $self->wrap_buddy($match);
    debug $match, $replace;
    substr($$ref,
           $pos_o,
           length($match) + $open_len + $close_len,
           $replace
           );

    ### iterate from the end of the last found location
    $pos_o = $pos_c + $close_len - length($match) + length($replace);
  }


  return ref($str) ? 1 : $str;
}

sub wrap_buddy {
  my $self = shift;

  "foo";
}

1;
