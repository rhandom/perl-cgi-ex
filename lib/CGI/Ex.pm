package HTML::Form;

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the GNU General Public License without warranty #
###----------------------------------------------------------------###

use strict;
use vars qw($PREFERRED_FILL_MODULE);

use Data::DumpEx;

use CGI;
#use base qw(CGI);

$PREFERRED_FILL_MODULE = '';

sub new {
  my $class = shift;
  my $self  = ref($_[0]) ? shift : {@_};
  return bless $self, $class;
}


###----------------------------------------------------------------###

### HTML::FillInForm compatible form filler (in fact it can use
### HTML::FillInForm.  Also can use internal parser with extended
### functionality.
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

    my $form = $args->{form} || $args->{fobject} || $args->{fdat} || $self;
    
    $self->fill_plain($ref,
                      $form,
                      $args->{target},
                      $args->{fill_password},
                      $args->{ignore_fields},
                      );
    return ! $args->{text} ? $$ref : 1;
  }

}

### simple regex based filler
### uses positional paramters
### pos1 - text or textref - if textref it is modified in place
### pos2 - hash or cgi obj ref, or array ref of hash and cgi obj refs
### pos3 - target - to be used for specifying a specific form - default undef
### pos4 - don't fill in password fields
### pos5 - hashref of fields to ignore
sub fill_plain {
  my $self          = shift;
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
  if ($self->{remove_script}) {
    $$ref =~ s|(<script\b.+?</script>)|push(@script, $1);"\0SCRIPT\0"|egi;
  }
  if ($self->{remove_comments}) {
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
                local $self->{remove_script}   = undef;
                local $self->{remove_comments} = undef;
                $self->fill_plain($1, $form, undef, $fill_password, $ignore);
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
        $val = &{ $form }($key, $self);
        last if defined $val;
      }
    }
    if (! defined $val) {
      return $all ? [] : undef;
    }

    ### fix up the value some
    if (UNIVERSAL::isa($val, 'CODE')) {
      $val = &{ $val }($key, $self);
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

=head1 EXISTING MODULES

The following is a list of existing validator and formfiller modules
at the time of this writing (I'm sure this probably isn't exaustive).
Inheritable
Separate direcotry
Global hash
function
add new directory

=over 4

=item C<Email::Valid>

Pro - Good email checker. Con - doesn't do much else.  What we
will inherit - well - we will have a type that uses it natively.

=item C<SSN::Validate>

Pro - Good SSN checker.  Con - doesn't do much else. What we
will inherit - we will use it natively.

=item C<Embperl::Form::Validate>

Pro - Add multiple rules.  Rules are array based (in order).  Has multilanguage support.  Returns array individual hashes for errors.  Can also return array of messages.  Ability to generate JavaScript code.  Extensible for other types (requires inheritance.

Con - Part of the Embperl distribution (not that Embperl is wrong, just that this is a general function utility in a specialized package - anybody wanting to use it has to install all of Embperl).  JavaScript requires form name passed to it.

=head1 AUTHOR

Paul Seamons

=cut

1;
