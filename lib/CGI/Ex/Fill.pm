package CGI::Ex::Fill;

### CGI Extended Form Filler

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom

use strict;
use vars qw($VERSION
            @ISA @EXPORT @EXPORT_OK
            $REMOVE_SCRIPT
            $REMOVE_COMMENT
            $MARKER_SCRIPT
            $MARKER_COMMENT
            $OBJECT_METHOD
            $TEMP_TARGET
            );
use Exporter;

$VERSION   = '1.0';
@ISA       = qw(Exporter);
@EXPORT    = qw(form_fill);
@EXPORT_OK = qw(form_fill html_escape get_tagval_by_key swap_tagval_by_key);

### These directives are used to determine whether or not to
### remove html comments and script sections while filling in
### a form.  Default is on.  This may give some trouble if you
### have a javascript section with form elements that you would
### like filled in.
$REMOVE_SCRIPT  = 1;
$REMOVE_COMMENT = 1;
$MARKER_SCRIPT  = "\0SCRIPT\0";
$MARKER_COMMENT = "\0COMMENT\0";
$OBJECT_METHOD  = "param";

###----------------------------------------------------------------###

### Regex based filler - as opposed to HTML::Parser based HTML::FillInForm
### arguments are positional
### pos1 - text or textref - if textref it is modified in place
### pos2 - hash or cgi obj ref, or array ref of hash and cgi obj refs
### pos3 - target - to be used for choosing a specific form - default undef
### pos4 - boolean fill in password fields - default is true
### pos5 - hashref or arrayref of fields to ignore
sub form_fill {
  my $text          = shift;
  my $ref           = ref($text) ? $text : \$text;
  my $form          = shift;
  my $forms         = UNIVERSAL::isa($form, 'ARRAY') ? $form : [$form];
  my $target        = shift;
  my $fill_password = shift;
  my $ignore        = shift || {};
  $ignore = {map {$_ => 1} @$ignore} if UNIVERSAL::isa($ignore, 'ARRAY');
  $fill_password = 1 if ! defined $fill_password;


  ### allow for optionally removing comments and script
  my @comment;
  my @script;
  if ($REMOVE_SCRIPT) {
    $$ref =~ s|(<script\b.+?</script>)|push(@script, $1);$MARKER_SCRIPT|egi;
  }
  if ($REMOVE_COMMENT) {
    $$ref =~ s|(<!--.*?-->)|push(@comment, $1);$MARKER_COMMENT|eg;
  }

  ### if there is a target - focus in on it
  ### possible bug here - name won't be found if
  ### there is nested html inside the form tag that comes before
  ### the name field - if no close form tag - don't swap in anything
  if ($target) {
    local $TEMP_TARGET = $target;
    $$ref =~ s{(<form            # open form
                [^>]+            # some space
                \bname=([\"\']?) # the name tag
                $target          # with the correct name (allows for regex)
                \2               # closing quote
                .+?              # as much as there is
                (?=</form>))     # then end
              }{
                local $REMOVE_SCRIPT  = undef;
                local $REMOVE_COMMENT = undef;
                &form_fill($1, $form, undef, $fill_password, $ignore);
              }sigex;

    ### put scripts and comments back and return
    $$ref =~ s/$MARKER_COMMENT/shift(@comment)/eg if $#comment != -1;
    $$ref =~ s/$MARKER_SCRIPT/ shift(@script) /eg if $#script  != -1;
    return ref($text) ? 1 : $$ref;
  }

  ### build a sub to get a value
  my %indexes = (); # store indexes for multivalued elements
  my $get_form_value = sub {
    my $key = shift;
    my $all = $_[0] && $_[0] eq 'all';
    if (! defined $key || ! length $key) {
      return $all ? [] : undef;
    }

    my $val;
    foreach my $form (@$forms) {
      next if ! ref $form;
      if (UNIVERSAL::isa($form, 'HASH') && defined $form->{$key}) {
        $val = $form->{$key};
        last;
      } elsif (UNIVERSAL::can($form, $OBJECT_METHOD)) {
        $val = $form->$OBJECT_METHOD($key);
        last if defined $val;
      } elsif (UNIVERSAL::isa($form, 'CODE')) {
        $val = &{ $form }($key, $TEMP_TARGET);
        last if defined $val;
      }
    }
    if (! defined $val) {
      return $all ? [] : undef;
    }

    ### fix up the value some
    if (UNIVERSAL::isa($val, 'CODE')) {
      $val = &{ $val }($key, $TEMP_TARGET);
    }
    if (UNIVERSAL::isa($val, 'ARRAY')) {
      $val = [@$val]; # copy the values
    } elsif (ref $val) {
      # die "Value for $key is not an array or a scalar";
      $val = "$val";  # stringify anything else
    }

    ### html escape them all
    &html_escape(\$_) foreach (ref($val) ? @$val : $val);

    ### allow for returning all elements
    ### or one at a time
    if ($all) {
      return ref($val) ? $val : [$val];
    } elsif (ref($val)) {
      $indexes{$key} ||= 0;
      my $ret = $val->[$indexes{$key}] || '';
      $indexes{$key} ++; # don't wrap - if we run out of values - we're done
      return $ret;
    } else {
      return $val;
    }
  };


  ###--------------------------------------------------------------###

  ### First pass
  ### swap <input > form elements if they have a name
  $$ref =~ s{
    (<input \s (?: ([\"\'])(?:|.*?[^\\])\2 | [^>] )* >) # nested html ok
    }{
      ### get the type and name - intentionally exlude names with nested "'
      my $tag   = $1;
      my $type  = uc(&get_tagval_by_key(\$tag, 'type') || '');
      my $name  = &get_tagval_by_key(\$tag, 'name');

      if ($name && ! $ignore->{$name}) {
        if (! $type
            || $type eq 'HIDDEN'
            || $type eq 'TEXT'
            || $type eq 'FILE'
            || ($type eq 'PASSWORD' && $fill_password)) {
          
          my $value = &$get_form_value($name, 'next');
          if (defined $value) {
            &swap_tagval_by_key(\$tag, 'value', $value);
          } elsif (! defined &get_tagval_by_key(\$tag, 'value')) {
            &swap_tagval_by_key(\$tag, 'value', '');
          }

        } elsif ($type eq 'CHECKBOX'
                 || $type eq 'RADIO') {
          my $values = &$get_form_value($name, 'all');          
          if (@$values) {
            $tag =~ s{\s+\bCHECKED\b(?:=([\"\']?)checked\1)?(?=\s|>|/>)}{}ig;
            
            if ($type eq 'CHECKBOX' && @$values == 1 && $values->[0] eq 'on') {
              $tag =~ s|(/?>\s*)$| checked="checked"$1|;
            } else {
              my $fvalue = &get_tagval_by_key(\$tag, 'value');
              if (defined $fvalue) {
                foreach (@$values) {
                  next if $_ ne $fvalue;
                  $tag =~ s|(\s*/?>\s*)$| checked="checked"$1|;
                  last;
                }
              }
            }
          }
        }
      }
      $tag; # return of swap
    }sigex;


  ### Second pass
  ### swap select boxes (must be done in such a way as to allow no closing tag)
  my @start = ();
  my @close = ();
  push @start, pos($$ref) - length($1) while $$ref =~ m|(<\s*select\b)|ig;
  push @close, pos($$ref) - length($1) while $$ref =~ m|(</\s*select\b)|ig;
  for (my $i = 0; $i <= $#start; $i ++) {
    while (defined($close[$i]) && $close[$i] < $start[$i]) {
      splice (@close,$i,1,());
    }
    if ($i == $#start) {
      $close[$i] = length($$ref) if ! defined $close[$i]; # set to end of string if no closing
    } elsif (! defined($close[$i]) || $close[$i] > $start[$i + 1]) {
      $close[$i] = $start[$i + 1]; # set to start of next select if no closing or > next select
    }
  }
  for (my $i = $#start; $i >= 0; $i --) {
    my $opts = substr($$ref, $start[$i], $close[$i] - $start[$i]);
    $opts =~ s{
      (<select \s                                 # opening
       (?: "" | '' | ([\"\']).*?[^\\]\2 | [^>] )* # nested html ok
       >)                                         # end of tag
      }{}sxi || next;
    next if ! $opts;
    my $tag    = $1;
    my $name   = &get_tagval_by_key(\$tag, 'name');
    my $values = $ignore->{$name} ? [] : &$get_form_value($name, 'all');
    if ($#$values != -1) {
      my $n = $opts =~ s{
        (<option[^>]*>)           # opening tag - no embedded > allowed
          (.*?)                   # the text value
          (?=<option|$|</option>) # the next tag
        }{
          my ($tag2, $opt) = ($1, $2);
          $tag2 =~ s%\s+\bSELECTED\b(?:=([\"\']?)selected\1)?(?=\s|>|/>)%%ig;
          
          my $fvalues = &get_tagval_by_key(\$tag2, 'value', 'all');
          my $fvalue  = @$fvalues ? $fvalues->[0]
            : $opt =~ /^\s*(.*?)\s*$/ ? $1 : "";
          foreach (@$values) {
            next if $_ ne $fvalue;
            $tag2 =~ s|(\s*/?>\s*)$| selected="selected"$1|;
            last;
          }
          "$tag2$opt"; # return of the swap
        }sigex;
      if ($n) {
        substr($$ref, $start[$i], $close[$i] - $start[$i], "$tag$opts");
      }
    }
  }


  ### Third pass
  ### swap textareas (must be done in such a way as to allow no closing tag)
  @start = ();
  @close = ();
  push @start, pos($$ref) - length($1) while $$ref =~ m|(<\s*textarea\b)|ig;
  push @close, pos($$ref) - length($1) while $$ref =~ m|(</\s*textarea\b)|ig;
  for (my $i = 0; $i <= $#start; $i ++) {
    while (defined($close[$i]) && $close[$i] < $start[$i]) {
      splice (@close,$i,1,());
    }
    if ($i == $#start) {
      $close[$i] = length($$ref) if ! defined $close[$i]; # set to end of string if no closing
    } elsif (! defined($close[$i]) || $close[$i] > $start[$i + 1]) {
      $close[$i] = $start[$i + 1]; # set to start of next select if no closing or > next select
    }
  }
  for (my $i = $#start; $i >= 0; $i --) {
    my $oldval = substr($$ref, $start[$i], $close[$i] - $start[$i]);
    $oldval =~ s{
      (<textarea \s                               # opening
       (?: "" | '' | ([\"\']).*?[^\\]\2 | [^>] )* # nested html ok
       >)                                         # end of tag
      }{}sxi || next;
    my $tag   = $1;
    my $name  = &get_tagval_by_key(\$tag, 'name');
    my $value = $ignore->{$name} ? [] : &$get_form_value($name, 'next');
    next if ! defined $value;
    substr($$ref, $start[$i], $close[$i] - $start[$i], "$tag$value");
  }

  ### put scripts and comments back and return
  $$ref =~ s/$MARKER_COMMENT/shift(@comment)/eg if $#comment != -1;
  $$ref =~ s/$MARKER_SCRIPT/ shift(@script) /eg if $#script  != -1;
  return ref($text) ? 1 : $$ref;
}


### yet another html escaper
### allow pass by value or by reference (reference is modified inplace)
sub html_escape {
  my $str = shift;
  return $str if ! $str;
  my $ref = ref($str) ? $str : \$str;

  $$ref =~ s/&/&amp;/g;
  $$ref =~ s/</&lt;/g;
  $$ref =~ s/>/&gt;/g;
  $$ref =~ s/\"/&quot;/g;

  return ref($str) ? 1 : $$ref;
}

### get a named value for key="value" pairs
### usage: my $val     = &get_tagval_by_key(\$tag, $key);
### usage: my $valsref = &get_tagval_by_key(\$tag, $key, 'all');
sub get_tagval_by_key {
  my $tag = shift;
  my $ref = ref($tag) ? $tag : \$tag;
  my $key = shift;
  my $all = $_[0] && $_[0] eq 'all';
  my @all = ();
  ### this is the old way and is actually slower
  #while ($$ref =~ m{(?<!\w|\.)    # isn't preceded by a word or dot
  #                    \Q$key\E    # the key
  #                    \s*=\s*     # equals
  #                    ([\"\']?)   # possible opening quote
  #                    (|.*?[^\\]) # nothing or anything not ending in \
  #                    \1          # close quote
  #                    (?=\s|>|/>) # a space or closing >
  #                  }sigx) {
  #  my ($quot, $val) = ($1, $2);
  #  $val =~ s/\\$quot/$quot/g if $quot; # unescape escaped quotes
  #  return $val if ! $all;
  #  push @all, $val;
  #}
  $key = lc($key);
  while ($$ref =~ m{
    (?<!\w|\.)
      (\S+)                     # 1 - the key
      \s*=\s*                   # equals
      (?: ([\"\'])(|.*?[^\\])\2 # 2 - a quote, 3 - the quoted
       |  (.*?(?=\s|>|/>))      # 4 - a non-quoted string
       )
    }sigx) {
    next if lc($1) ne $key;
    my ($val,$quot) = ($2) ? ($3,$2) : ($4,undef);
    $val =~ s/\\$quot/$quot/ if $quot;
    return $val if ! $all;
    push @all, $val;
  }
  return undef if ! $all;
  return \@all;
}

### swap out values for key="value" pairs
### usage: my $count  = &swap_tagval_by_key(\$tag, $key, $val);
### usage: my $newtag = &swap_tagval_by_key($tag, $key, $val);
sub swap_tagval_by_key {
  my $tag = shift;
  my $ref = ref($tag) ? $tag : \$tag;
  my $key = shift;
  my $val = shift;
  my $n   = 0;

  ### this commented method is faster but doesn't handle nested
  ### html or javascript at all
  #  $$ref =~ s{(?<!\w|\.)    # isn't preceded by a word or dot
  #               (\Q$key\E   # the key
  #               \s*=\s*)    # equals
  #               ([\"\']?)   # possible opening quote
  #               (|.*?[^\\]) # nothing or anything not ending in \
  #               \2          # close quote
  #               (?=\s|>|/>) # a space or closing >
  #             }{
  #               ($n++) ? "" : "$1$2$val$2";
  #             }sigex;
  $$ref =~ s{(^\s*<\s*\w+\s+|\G\s+)         # 1 - open tag or previous position
               ( (\w+)                      # 2 - group, 3 - the key
                 (\s*=\s*)                  # 4 - equals
                  (?: ([\"\'])(|.*?[^\\])\5 # 5 - a quote, 6 - the quoted
                   |  (.*?(?=\s|>|/>))      # 7 - a non-quoted string
                  )
                | (?: .+?(?=\s|>|/>))       # a non keyvalue chunk (CHECKED)
               )
             }{
               if (defined($3) && $3 eq $key) { # has matching key
                 if (! $n ++) {  # only put value back on first match
                   if ($5) {     # quoted
                     "$1$3$4$5$val$5";
                   } else {      # non-quoted
                     "$1$3$4$val";
                   }
                 } else {
                   $1; # second match
                 }
               } else {
                 "$1$2"; # non-keyval
               }
             }sigex;

  ### append value on if none were swapped
  if (! $n) {
    $$ref =~ s|(\s*/?>\s*)$| value="$val"$1|;
    $n = -1;
  }

  return ref($tag) ? $n : $$ref;
}

1;

__END__

###----------------------------------------------------------------###

=head1 NAME

=head1 SYNOPSIS

  use CGI::Ex::Fill qw(form_fill);

  my $text = my_own_template_from_somewhere();

  my $form = CGI->new;
  # OR
  # my $form = {key => 'value'}
  # OR 
  # my $form = [CGI->new, CGI->new, {key1 => 'val1'}, CGI->new];


  form_fill(\$text, $form); # modifies $text
  # OR
  # my $copy = form_fill($text, $form); # copies $text


  ALSO

  my $formname = 'formname';     # table to parse (undef = anytable)
  my $fp = 0;                    # fill_passwords ? default is true
  my $ignore = ['key1', 'key2']; # OR {key1 => 1, key2 => 1};

  form_fill(\$text, $form, $formname, $fp, $ignore);

  ALSO

  ### delay getting the value until we find an element that needs it
  my $form = {key => sub {my $key = shift; # get and return value}};


=head1 DESCRIPTION

form_fill is directly comparable to HTML::FillInForm.  It will pass the
same suite of tests (actually - it is a little bit kinder on the parse as
it won't change case, reorder your attributes, or miscellaneous spaces).

HTML::FillInForm both benefits and suffers from being based on
HTML::Parser. It is good for standards and poor for performance.  Testing
the form_fill module against HTML::FillInForm gave some surprising
results.  On tiny forms (< 1 k) form_fill was ~ 17% faster than FillInForm.
If the html document incorporated very many entities at all, the
performace of FillInForm goes down (and down).  However, if you are only
filling in one form every so often, then it shouldn't matter - but form_fill
will be nicer on the tags and won't balk at ugly html.
See the benchmarks in the t/samples directory for more information (ALL
BENCHMARKS SHOULD BE TAKEN WITH A GRAIN OF SALT).

=head1 HTML COMMENT / JAVASCRIPT

Because there are too many problems that could occur with html
comments and javascript, form_fill temporarily removes them during the
fill.  You may disable this behavior by setting $REMOVE_COMMENT and
$REMOVE_SCRIPT to 0 before calling form_fill.  The main reason for
doing this would be if you wanted to have form elments inside the
javascript and comments get filled.  Disabling the removal only
results in a speed increase of 5%. The function uses \0COMMENT\0 and
\0SCRIPT\0 as placeholders so i'd avoid these in your text (Actually
they may be reset to whatever you'd like via $MARKER_COMMENT and
$MARKER_SCRIPT).

=head1 AUTHOR

Paul Seamons

=head1 LICENSE

This module may distributed under the same terms as Perl itself.

=cut
