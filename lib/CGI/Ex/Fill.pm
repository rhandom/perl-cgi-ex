package HTML::Form::Fill;

use strict;
use vars qw($VERSION
            @ISA @EXPORT @EXPORT_OK
            $REMOVE_SCRIPT
            $REMOVE_COMMENTS
            );
use Exporter;

$VERSION   = '1.0';
@ISA       = qw(Exporter);
@EXPORT    = qw(form_fill);
@EXPORT_OK = qw(form_fill get_tagval_by_key swap_tagval_by_key);

### These directives are used to determine whether or not to
### remove html comments and script sections while filling in
### a form.  Default is on.  This may give some trouble if you
### have a javascript section with form elements that you would
### like filled in.
$REMOVE_COMMENTS = 1;
$REMOVE_SCRIPT   = 1;

###----------------------------------------------------------------###

### Regex based filler - as opposed to HTML::Parser based HTML::FillInForm
### arguments are positional
### pos1 - text or textref - if textref it is modified in place
### pos2 - hash or cgi obj ref, or array ref of hash and cgi obj refs
### pos3 - target - to be used for specifying a specific form - default undef
### pos4 - don't fill in password fields
### pos5 - hashref of fields to ignore
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
    $$ref =~ s|(<script\b.+?</script>)|push(@script, $1);"\0SCRIPT\0"|egi;
  }
  if ($REMOVE_COMMENTS) {
    $$ref =~ s/(<!--.*?-->)/push(@comment, $1);"\0COMMENT\0"/eg;
  }

  ### if there is a target - focus in on it
  if ($target) {
    $$ref =~ s{(<form            # open form
                [^>]+            # some space
                \bname=([\"\']?) # the name tag
                $target          # with the correct name (allows for regex)
                \2               # closing quote
                .+?              # as much as there is
                </form>)         # then end
              }{
                local $REMOVE_SCRIPT   = undef;
                local $REMOVE_COMMENTS = undef;
                form_fill($1, $form, undef, $fill_password, $ignore);
              }sigex;

    ### put scripts and comments back and return
    $$ref =~ s/\0SCRIPT\0/ shift(@script) /eg if $#script  != -1;
    $$ref =~ s/\0COMMENT\0/shift(@comment)/eg if $#comment != -1;
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
      } elsif (UNIVERSAL::can($form, 'param')) {
        $val = $form->param($key);
        last if defined $val;
      } elsif (UNIVERSAL::isa($form, 'CODE')) {
        $val = &{ $form }($key);
        last if defined $val;
      }
    }
    if (! defined $val) {
      return $all ? [] : undef;
    }

    ### fix up the value some
    if (UNIVERSAL::isa($val, 'CODE')) {
      $val = &{ $val }($key);
    }
    if (UNIVERSAL::isa($val, 'ARRAY')) {
      $val = [@$val]; # copy the values
    } elsif (ref $val) {
      # die "Value for $key is not an array or a scalar";
      $val = "$val";  # stringify anything else
    }
    
    ### html escape them all
    $_ = &CGI::escapeHTML($_) foreach (ref($val) ? @$val : $val);

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
    (<input\s.+?>)
    }{
      ### get the type and name - intentionally exlude names with nested "'
      my $tag   = $1;
      my $type  = uc(get_tagval_by_key(\$tag, 'type') || '');
      my $name  = get_tagval_by_key(\$tag, 'name');

      if ($name && ! $ignore->{$name}) {
        if (! $type
            || $type eq 'HIDDEN'
            || $type eq 'TEXT'
            || $type eq 'FILE'
            || ($type eq 'PASSWORD' && $fill_password)) {
          
          my $value = &$get_form_value($name, 'next') || '';
          if (defined $value) {
            swap_tagval_by_key(\$tag, 'value', $value);
          }          

        } elsif ($type eq 'CHECKBOX'
                 || $type eq 'RADIO') {
          my $values = &$get_form_value($name, 'all');          
          if (@$values) {
            $tag =~ s{\s+\bCHECKED\b(?=\s|>|/>)}{}ig;
            
            if ($type eq 'CHECKBOX' && @$values == 1 && $values->[0] eq 'on') {
              $tag =~ s|(/?>)| checked$1|;
            } else {
              my $fvalue = get_tagval_by_key(\$tag, 'value');
              foreach (@$values) {
                next if $_ ne $fvalue;
                $tag =~ s|(\s*/?>)| checked$1|;
                last;
              }
            }
          }
        }
      }
      $tag; # return of swap
    }sigex;

  ### Second pass
  ### swap select boxes
  $$ref =~ s{
    (<select\s[^>]+>)  # opening tag - doesn't allow for > embedded in tag
      (.*?)            # the options
      (</select>)      # closing
    }{
      my ($tag, $opts, $close) = ($1, $2, $3);
      my $name   = get_tagval_by_key(\$tag, 'name');
      my $values = $ignore->{$name} ? [] : &$get_form_value($name, 'all');
      if (@$values) {
        $opts =~ s{
          (<option[^>]*>)            # opening tag - no embedded > allowed
            (.*?)                    # the text value
            (?=<option|$|</option>) # the next tag
          }{
            my ($tag2, $opt) = ($1, $2);
            $tag2 =~ s%\s+\bSELECTED\b(?=\s|>|/>)%%ig;
            
            my $fvalues = get_tagval_by_key(\$tag2, 'value', 'all');
            my $fvalue  = @$fvalues ? $fvalues->[0]
              : $opt =~ /^\s*(.*?)\s*$/ ? $1 : "";
            foreach (@$values) {
              next if $_ ne $fvalue;
              $tag2 =~ s|(\s*/?>)| selected$1|;
              last;
            }
            "$tag2$opt"; # return of inner swap
          }sigex;
      }
      "$tag$opts$close"; # return of swap
    }sigex;

  ### Third pass
  ### swap text areas
  $$ref =~ s{
    (<textarea\s[^>]+>)  # opening tag - doesn't allow for > embedded in tag
      (.*?)              # the options
      (</textarea>)      # closing
    }{
      my ($tag, $opts, $close) = ($1, $2, $3);
      my $name  = get_tagval_by_key(\$tag, 'name');
      my $value = $ignore->{$name} ? "" : &$get_form_value($name, 'next') || '';
      "$tag$value$close"; # return of swap
    }sigex;


  ### put scripts and comments back and return
  $$ref =~ s/\0SCRIPT\0/ shift(@script) /eg if $#script  != -1;
  $$ref =~ s/\0COMMENT\0/shift(@comment)/eg if $#comment != -1;
  return ref($text) ? 1 : $$ref;
}


### get a named value for key="value" pairs
### usage: my $val     = get_tagval_by_key(\$tag, $key);
### usage: my $valsref = get_tagval_by_key(\$tag, $key, 'all');
sub get_tagval_by_key {
  my $tag = shift;
  my $ref = ref($tag) ? $tag : \$tag;
  my $key = shift;
  my $all = $_[0] && $_[0] eq 'all';
  my @all = ();
  while ($$ref =~ m{(?<!\w|\.)    # isn't preceded by a word or dot
                      \Q$key\E    # the key
                      \s*=\s*     # equals
                      ([\"\']?)   # possible opening quote
                      (|.*?[^\\]) # nothing or anything not ending in \
                      \1          # close quote
                      (?=\s|>|/>) # a space or closing >
                    }sigx) {
    my ($quot, $val) = ($1, $2);
    $val =~ s/\\$quot/$quot/g if $quot; # unescape escaped quotes
    return $val if ! $all;
    push @all, $val;
  }
  return undef if ! $all;
  return \@all;
}

### swap out values for key="value" pairs
### usage: my $count  = swap_tagval_by_key(\$tag, $key, $val);
### usage: my $newtag = swap_tagval_by_key($tag, $key, $val);
sub swap_tagval_by_key {
  my $tag = shift;
  my $ref = ref($tag) ? $tag : \$tag;
  my $key = shift;
  my $val = shift;
  my $n   = 0;
  $$ref =~ s{(?<!\w|\.)    # isn't preceded by a word or dot
               (\Q$key\E   # the key
               \s*=\s*)    # equals
               ([\"\']?)   # possible opening quote
               (|.*?[^\\]) # nothing or anything not ending in \
               \2          # close quote
               (?=\s|>|/>) # a space or closing >
             }{
               ($n++) ? "" : "$1$2$val$2";
             }sigex;

  ### append value on if none were swapped
  if (! $n) {
    $$ref =~ s|(\s*/?>)| value="$val"$1|;
    $n = -1;
  }

  return ref($tag) ? $n : $$ref;
}

1;

__END__

###----------------------------------------------------------------###

=head1 NAME

=head1 SYNOPSIS

  use HTML::Form::Fill qw(form_fill);

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
results.  On tiny forms (< 1 k) FillInForm was 30% faster (avg).  As
soon as the html document incorporated very many entities at all, the
performace kept going down (and down).  On one simple form, FillInForm
was 30% faster.  I added 180 <BR> tags.  FillInForm lagged behind.
form_fill kept on par and ended up 420% faster.  I added another
180 <BR> and the difference jumped to 740%. Another 180 and it was
1070% faster (ALL BENCHMARKS SHOULD BE TAKEN WITH A GRAIN OF SALT).
The problem is that HTML::Parser has to fire events for
every tag it finds.  I would be interested to test a Recursive Descent
form filler against these two.

=head1 HTML COMMENTS / JAVASCRIPT

Because there are too many problems that could occur with html comments
and javascript, form_fill temporarily removes them during the fill.  You
may disable this behavior by setting $REMOVE_COMMENTS and $REMOVE_SCRIPT
to 0 before calling form_fill.  The main reason for doing this would be if
you wanted to have form elments inside the javascript and comments get filled.
Disabling the removal only results in a speed increase of 5%. The function
uses \0COMMENT\0 and \0SCRIPT\0 as placeholders so i'd avoid these in your
text.

=head1 BUGS / LIMITATIONS

The only known limitations is that if you have a <select>, <textarea>,
or <option> tag that have nested HTML attributes that occur before
the name, form_fill won't be able to determine the name.

  <!-- WORKS -->
  <select name=foo onchange=alert('<b>Wow</b>')>
  </select>

  <!-- WILL NOT WORK -->
  <select onchange=alert('<b>Wow</b>') name=foo>
  </select>

This limitation could be overcome with a more complex regex - but why.
You won't have this problem (or shouldn't) with HTML::FillInForm because
of the lengthy process used for descending the structure.

=head1 AUTHOR

Paul Seamons

=head1 LICENSE

This module may distributed under the same terms as Perl itself.

=cut
