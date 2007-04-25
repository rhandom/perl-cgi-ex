package CGI::Ex::Validate;

=head1 NAME

CGI::Ex::Validate - another form validator - but it does javascript in parallel

=cut

###----------------------------------------------------------------###
#  Copyright 2007 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

use strict;
use vars qw($VERSION
            $DEFAULT_EXT
            %DEFAULT_OPTIONS
            $JS_URI_PATH
            $JS_URI_PATH_YAML
            $JS_URI_PATH_VALIDATE
            $QR_EXTRA
            @UNSUPPORTED_BROWSERS
            );

$VERSION = '2.09';

$DEFAULT_EXT   = 'val';
$QR_EXTRA      = qr/^(\w+_error|as_(array|string|hash)_\w+|no_\w+)/;
@UNSUPPORTED_BROWSERS = (qr/MSIE\s+5.0\d/i);

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $self  = ref($_[0]) ? shift : {@_};

  $self = {%DEFAULT_OPTIONS, %$self} if scalar keys %DEFAULT_OPTIONS;

  return bless $self, $class;
}

###----------------------------------------------------------------###

sub cgix {
    my $self = shift;
    return $self->{'cgix'} ||= do {
        require CGI::Ex;
        CGI::Ex->new;
    };
}

### the main validation routine
sub validate {
    my $self = (! ref($_[0])) ? shift->new                    # $class->validate
                : UNIVERSAL::isa($_[0], __PACKAGE__) ? shift  # $self->validate
                : __PACKAGE__->new;                           # &validate
    my $form     = shift || die "Missing form hash";
    my $val_hash = shift || die "Missing validation hash";
    my $what_was_validated = shift; # allow for extra arrayref that stores what was validated

    ### turn the form into a form hash if doesn't look like one already
    die "Invalid form hash or cgi object" if ! ref $form;
    if (ref $form ne 'HASH') {
        local $self->{cgi_object} = $form;
        $form = $self->cgix->get_form($form);
    }

    ### make sure the validation is a hashref
    ### get_validation handle odd types
    if (ref $val_hash ne 'HASH') {
        $val_hash = $self->get_validation($val_hash) if ref $val_hash ne 'SCALAR' || ! ref $val_hash;
        die "Validation groups must be a hashref"    if ref $val_hash ne 'HASH';
    }

    ### parse keys that are group arguments - and those that are keys to validate
    my %ARGS;
    my @field_keys = grep { /^(?:group|general)\s+(\w+)/
                              ? do {$ARGS{$1} = $val_hash->{$_} ; 0}
                              : 1 }
                     sort keys %$val_hash;

    ### only validate this group if it is supposed to be checked
    return if $ARGS{'validate_if'} && ! $self->check_conditional($form, $ARGS{'validate_if'});

    ### Look first for items in 'group fields' or 'group order'
    my $fields;
    if ($fields = $ARGS{'fields'} || $ARGS{'order'}) {
        my $type = $ARGS{'fields'} ? 'group fields' : 'group order';
        die "Validation '$type' must be an arrayref when passed"
            if ! UNIVERSAL::isa($fields, 'ARRAY');
        my @temp;
        foreach my $field (@$fields) {
            die "Non-defined value in '$type'" if ! defined $field;
            if (ref $field) {
                die "Found nonhashref value in '$type'" if ref($field) ne 'HASH';
                die "Element missing \"field\" key/value in '$type'" if ! defined $field->{'field'};
                push @temp, $field;
            } elsif ($field eq 'OR') {
                push @temp, 'OR';
            } else {
                die "No element found in '$type' for $field" if ! exists $val_hash->{$field};
                die "Found nonhashref value in '$type'" if ref($val_hash->{$field}) ne 'HASH';
                push @temp, { %{ $val_hash->{$field} }, field => $field }; # copy the values to add the key
            }
        }
        $fields = \@temp;

        ### limit the keys that need to be searched to those not in fields or order
        my %found = map { $_->{'field'} => 1 } @temp;
        @field_keys = grep { ! $found{$_} } @field_keys;
    }

    ### add any remaining field_vals from our original hash
    ### this is necessary for items that weren't in group fields or group order
    foreach my $field (@field_keys) {
        die "Found nonhashref value for field $field" if ref($val_hash->{$field}) ne 'HASH';
        if (defined $val_hash->{$field}->{'field'}) {
            push @$fields, $val_hash->{$field}->{'field'};
        } else {
            push @$fields, { %{$val_hash->{$field}}, field => $field };
        }
    }

    ### Finally we have our arrayref of hashrefs that each have their 'field' key
    ### now lets do the validation
    my $found  = 1;
    my @errors;
    my $hold_error; # hold the error for a moment - to allow for an "OR" operation
    foreach (my $i = 0; $i < @$fields; $i++) {
        my $ref = $fields->[$i];
        if (! ref($ref) && $ref eq 'OR') {
            $i++ if $found; # if found skip the OR altogether
            $found = 1; # reset
            next;
        }
        $found = 1;
        die "Missing field key during normal validation" if ! $ref->{'field'};
        local $ref->{'was_validated'} = 1;
        my $err = $self->validate_buddy($form, $ref->{'field'}, $ref);
        if ($ref->{'was_validated'} && $what_was_validated) {
            push @$what_was_validated, $ref;
        }

        ### test the error - if errors occur allow for OR - if OR fails use errors from first fail
        if ($err) {
            if ($i < $#$fields && ! ref($fields->[$i + 1]) && $fields->[$i + 1] eq 'OR') {
                $hold_error = $err;
            } else {
                push @errors, $hold_error ? @$hold_error : @$err;
                $hold_error = undef;
            }
        } else {
            $hold_error = undef;
        }
    }
    push(@errors, @$hold_error) if $hold_error; # allow for final OR to work


    ### optionally check for unused keys in the form
    if ($ARGS{no_extra_fields} || $self->{no_extra_fields}) {
        my %keys = map { ($_->{'field'} => 1) } @$fields; # %{ $self->get_validation_keys($val_hash) };
        foreach my $key (sort keys %$form) {
            next if $keys{$key};
            push @errors, [$key, 'no_extra_fields', {}, undef];
        }
    }

    ### return what they want
    if (@errors) {
        my @copy = grep {/$QR_EXTRA/o} keys %$self;
        @ARGS{@copy} = @{ $self }{@copy};
        unshift @errors, $ARGS{'title'} if $ARGS{'title'};
        my $err_obj = $self->new_error(\@errors, \%ARGS);
        die    $err_obj if $ARGS{'raise_error'};
        return $err_obj;
    } else {
        return;
    }
}

sub new_error {
    my $self = shift;
    return CGI::Ex::Validate::Error->new(@_);
}

### allow for optional validation on groups and on individual items
sub check_conditional {
  my ($self, $form, $ifs, $ifs_match) = @_;

  ### can pass a single hash - or an array ref of hashes
  if (! $ifs) {
    die "Need reference passed to check_conditional";
  } elsif (! ref($ifs)) {
    $ifs = [$ifs];
  } elsif (UNIVERSAL::isa($ifs,'HASH')) {
    $ifs = [$ifs];
  }

  local $self->{'_check_conditional'} = 1;

  ### run the if options here
  ### multiple items can be passed - all are required unless OR is used to separate
  my $found = 1;
  foreach (my $i = 0; $i <= $#$ifs; $i ++) {
    my $ref = $ifs->[$i];
    if (! ref $ref) {
      if ($ref eq 'OR') {
        $i ++ if $found; # if found skip the OR altogether
        $found = 1; # reset
        next;
      } else {
        if ($ref =~ s/^\s*!\s*//) {
          $ref = {field => $ref, max_in_set => "0 of $ref"};
        } else {
          $ref = {field => $ref, required => 1};
        }
      }
    }
    last if ! $found;

    ### get the field - allow for custom variables based upon a match
    my $field = $ref->{'field'} || die "Missing field key during validate_if (possibly used a reference to a main hash *foo -> &foo)";
    $field =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;

    my $errs = $self->validate_buddy($form, $field, $ref);
    $found = 0 if $errs;
  }
  return $found;
}


### this is where the main checking goes on
sub validate_buddy {
  my $self = shift;
  my ($form, $field, $field_val, $ifs_match) = @_;

  local $self->{'_recurse'} = ($self->{'_recurse'} || 0) + 1;
  die "Max dependency level reached 10" if $self->{'_recurse'} > 10;

  my @errors = ();
  my $types  = [sort keys %$field_val];

  ### allow for not running some tests in the cgi
  if ($field_val->{'exclude_cgi'}) {
    delete $field_val->{'was_validated'};
    return 0;
  }

  ### allow for field names that contain regular expressions
  if ($field =~ m/^(!\s*|)m([^\s\w])(.*)\2([eigsmx]*)$/s) {
    my ($not,$pat,$opt) = ($1,$3,$4);
    $opt =~ tr/g//d;
    die "The e option cannot be used on validation keys on field $field" if $opt =~ /e/;
    foreach my $_field (sort keys %$form) {
      next if ($not && $_field =~ m/(?$opt:$pat)/) || (! $not && $_field !~ m/(?$opt:$pat)/);
      my @match = (undef, $1, $2, $3, $4, $5); # limit to the matches
      my $errs = $self->validate_buddy($form, $_field, $field_val, \@match);
      push @errors, @$errs if $errs;
    }
    return @errors ? \@errors : 0;
  }

  my $values   = UNIVERSAL::isa($form->{$field},'ARRAY') ? $form->{$field} : [$form->{$field}];
  my $n_values = $#$values + 1;

  ### allow for default value
  if (exists $field_val->{'default'}) {
    if ($n_values == 0 || ($n_values == 1 && (! defined($values->[0]) || ! length($values->[0])))) {
      $form->{$field} = $values->[0] = $field_val->{'default'};
    }
  }

  ### allow for a few form modifiers
  my $modified = 0;
  foreach my $value (@$values) {
    next if ! defined $value;
    if (! $field_val->{'do_not_trim'}) { # whitespace
      $value =~ s/^\s+//;
      $value =~ s/\s+$//;
      $modified = 1;
    }
    if ($field_val->{'trim_control_chars'}) {
      $value =~ y/\t/ /;
      $value =~ y/\x00-\x1F//d;
      $modified = 1;
    }
    if ($field_val->{'to_upper_case'}) { # uppercase
      $value = uc($value);
      $modified = 1;
    } elsif ($field_val->{'to_lower_case'}) { # lowercase
      $value = lc($value);
      $modified = 1;
    }
  }
  # allow for inline specified modifications (ie s/foo/bar/)
  foreach my $type (grep {/^replace_?\d*$/} @$types) {
    my $ref = UNIVERSAL::isa($field_val->{$type},'ARRAY') ? $field_val->{$type}
      : [split(/\s*\|\|\s*/,$field_val->{$type})];
    foreach my $rx (@$ref) {
      if ($rx !~ m/^\s*s([^\s\w])(.+)\1(.*)\1([eigsmx]*)$/s) {
        die "Not sure how to parse that replace ($rx)";
      }
      my ($pat, $swap, $opt) = ($2, $3, $4);
      die "The e option cannot be used in swap on field $field" if $opt =~ /e/;
      my $global = $opt =~ s/g//g;
      $swap =~ s/\\n/\n/g;
      if ($global) {
        foreach my $value (@$values) {
          $value =~ s{(?$opt:$pat)}{
            my @match = (undef, $1, $2, $3, $4, $5, $6); # limit on the number of matches
            my $copy = $swap;
            $copy =~ s/\$(\d+)/defined($match[$1]) ? $match[$1] : ""/ge;
            $modified = 1;
            $copy; # return of the swap
          }eg;
        }
      }else{
        foreach my $value (@$values) {
          next if ! defined $value;
          $value =~ s{(?$opt:$pat)}{
            my @match = (undef, $1, $2, $3, $4, $5, $6); # limit on the number of matches
            my $copy = $swap;
            $copy =~ s/\$(\d+)/defined($match[$1]) ? $match[$1] : ""/ge;
            $modified = 1;
            $copy; # return of the swap
          }e;
        }
      }
    }
  }
  ### put them back into the form if we have modified it
  if ($modified) {
    if ($n_values == 1) {
      $form->{$field} = $values->[0];
      $self->{cgi_object}->param(-name => $field, -value => $values->[0])
        if $self->{cgi_object};
    } else {
      ### values in @{ $form->{$field} } were modified directly
      $self->{cgi_object}->param(-name => $field, -value => $values)
        if $self->{cgi_object};
    }
  }

  ### only continue if a validate_if is not present or passes test
  my $needs_val = 0;
  my $n_vif = 0;
  foreach my $type (grep {/^validate_if_?\d*$/} @$types) {
    $n_vif ++;
    my $ifs = $field_val->{$type};
    my $ret = $self->check_conditional($form, $ifs, $ifs_match);
    $needs_val ++ if $ret;
  }
  if (! $needs_val && $n_vif) {
    delete $field_val->{'was_validated'};
    return 0;
  }

  ### check for simple existence
  ### optionally check only if another condition is met
  my $is_required = $field_val->{'required'} ? 'required' : '';
  if (! $is_required) {
    foreach my $type (grep {/^required_if_?\d*$/} @$types) {
      my $ifs = $field_val->{$type};
      next if ! $self->check_conditional($form, $ifs, $ifs_match);
      $is_required = $type;
      last;
    }
  }
  if ($is_required
      && ($n_values == 0 || ($n_values == 1 && (! defined($values->[0]) || ! length $values->[0])))) {
    return [] if $self->{'_check_conditional'};
    return [[$field, $is_required, $field_val, $ifs_match]];
  }

  ### min values check
  my $n = exists($field_val->{'min_values'}) ? $field_val->{'min_values'} || 0 : 0;
  if ($n_values < $n) {
    return [] if $self->{'_check_conditional'};
    return [[$field, 'min_values', $field_val, $ifs_match]];
  }

  ### max values check
  $field_val->{'max_values'} = 1 if ! exists $field_val->{'max_values'};
  $n = $field_val->{'max_values'} || 0;
  if ($n_values > $n) {
    return [] if $self->{'_check_conditional'};
    return [[$field, 'max_values', $field_val, $ifs_match]];
  }

  ### max_in_set and min_in_set checks
  my @min = grep {/^min_in_set_?\d*$/} @$types;
  my @max = grep {/^max_in_set_?\d*$/} @$types;
  foreach ([min => \@min],
           [max => \@max]) {
    my ($minmax, $keys) = @$_;
    foreach my $type (@$keys) {
      $field_val->{$type} =~ m/^\s*(\d+)(?i:\s*of)?\s+(.+)\s*$/
        || die "Invalid in_set check $field_val->{$type}";
      my $n = $1;
      foreach my $_field (split /[\s,]+/, $2) {
        my $ref = UNIVERSAL::isa($form->{$_field},'ARRAY') ? $form->{$_field} : [$form->{$_field}];
        foreach my $_value (@$ref) {
          $n -- if defined($_value) && length($_value);
        }
      }
      if (   ($minmax eq 'min' && $n > 0)
          || ($minmax eq 'max' && $n < 0)) {
        return [] if $self->{'_check_conditional'};
        return [[$field, $type, $field_val, $ifs_match]];
      }
    }
  }

  ### at this point @errors should still be empty
  my $content_checked; # allow later for possible untainting (only happens if content was checked)

  ### loop on values of field
  foreach my $value (@$values) {

    ### allow for enum types
    if (exists $field_val->{'enum'}) {
      my $ref = ref($field_val->{'enum'}) ? $field_val->{'enum'} : [split(/\s*\|\|\s*/,$field_val->{'enum'})];
      my $found = 0;
      foreach (@$ref) {
        $found = 1 if defined($value) && $_ eq $value;
      }
      if (! $found) {
        return [] if $self->{'_check_conditional'};
        push @errors, [$field, 'enum', $field_val, $ifs_match];
      }
      $content_checked = 1;
    }

    ### field equality test
    foreach my $type (grep {/^equals_?\d*$/} @$types) {
      my $field2  = $field_val->{$type};
      my $not     = ($field2 =~ s/^!\s*//) ? 1 : 0;
      my $success = 0;
      if ($field2 =~ m/^([\"\'])(.*)\1$/) {
        my $test = $2;
        $success = (defined($value) && $value eq $test);
      } elsif (exists($form->{$field2}) && defined($form->{$field2})) {
        $success = (defined($value) && $value eq $form->{$field2});
      } elsif (! defined($value)) {
        $success = 1; # occurs if they are both undefined
      }
      if ($not ? $success : ! $success) {
        return [] if $self->{'_check_conditional'};
        push @errors, [$field, $type, $field_val, $ifs_match];
      }
      $content_checked = 1;
    }

    ### length min check
    if (exists $field_val->{'min_len'}) {
      my $n = $field_val->{'min_len'};
      if (! defined($value) || length($value) < $n) {
        return [] if $self->{'_check_conditional'};
        push @errors, [$field, 'min_len', $field_val, $ifs_match];
      }
    }

    ### length max check
    if (exists $field_val->{'max_len'}) {
      my $n = $field_val->{'max_len'};
      if (defined($value) && length($value) > $n) {
        return [] if $self->{'_check_conditional'};
        push @errors, [$field, 'max_len', $field_val, $ifs_match];
      }
    }

    ### now do match types
    foreach my $type (grep {/^match_?\d*$/} @$types) {
      my $ref = UNIVERSAL::isa($field_val->{$type},'ARRAY') ? $field_val->{$type}
         : UNIVERSAL::isa($field_val->{$type}, 'Regexp') ? [$field_val->{$type}]
         : [split(/\s*\|\|\s*/,$field_val->{$type})];
      foreach my $rx (@$ref) {
        if (UNIVERSAL::isa($rx,'Regexp')) {
          if (! defined($value) || $value !~ $rx) {
              push @errors, [$field, $type, $field_val, $ifs_match];
          }
        } else {
          if ($rx !~ m/^(!\s*|)m([^\s\w])(.*)\2([eigsmx]*)$/s) {
            die "Not sure how to parse that match ($rx)";
          }
          my ($not,$pat,$opt) = ($1,$3,$4);
          $opt =~ tr/g//d;
          die "The e option cannot be used on validation keys on field $field" if $opt =~ /e/;
          if ( (     $not && (  defined($value) && $value =~ m/(?$opt:$pat)/))
               || (! $not && (! defined($value) || $value !~ m/(?$opt:$pat)/))
               ) {
            return [] if $self->{'_check_conditional'};
            push @errors, [$field, $type, $field_val, $ifs_match];
          }
        }
      }
      $content_checked = 1;
    }

    ### allow for comparison checks
    foreach my $type (grep {/^compare_?\d*$/} @$types) {
      my $ref = UNIVERSAL::isa($field_val->{$type},'ARRAY') ? $field_val->{$type}
        : [split(/\s*\|\|\s*/,$field_val->{$type})];
      foreach my $comp (@$ref) {
        next if ! $comp;
        my $test  = 0;
        if ($comp =~ /^\s*(>|<|[><!=]=)\s*([\d\.\-]+)\s*$/) {
          my $val = $value || 0;
          $val *= 1;
          if    ($1 eq '>' ) { $test = ($val >  $2) }
          elsif ($1 eq '<' ) { $test = ($val <  $2) }
          elsif ($1 eq '>=') { $test = ($val >= $2) }
          elsif ($1 eq '<=') { $test = ($val <= $2) }
          elsif ($1 eq '!=') { $test = ($val != $2) }
          elsif ($1 eq '==') { $test = ($val == $2) }

        } elsif ($comp =~ /^\s*(eq|ne|gt|ge|lt|le)\s+(.+?)\s*$/) {
          my $val = defined($value) ? $value : '';
          my ($op, $value2) = ($1, $2);
          $value2 =~ s/^([\"\'])(.*)\1$/$2/;
          if    ($op eq 'gt') { $test = ($val gt $value2) }
          elsif ($op eq 'lt') { $test = ($val lt $value2) }
          elsif ($op eq 'ge') { $test = ($val ge $value2) }
          elsif ($op eq 'le') { $test = ($val le $value2) }
          elsif ($op eq 'ne') { $test = ($val ne $value2) }
          elsif ($op eq 'eq') { $test = ($val eq $value2) }

        } else {
          die "Not sure how to compare \"$comp\"";
        }
        if (! $test) {
          return [] if $self->{'_check_conditional'};
          push @errors, [$field, $type, $field_val, $ifs_match];
        }
      }
      $content_checked = 1;
    }

    ### server side sql type
    foreach my $type (grep {/^sql_?\d*$/} @$types) {
      my $db_type = $field_val->{"${type}_db_type"};
      my $dbh = ($db_type) ? $self->{dbhs}->{$db_type} : $self->{dbh};
      if (! $dbh) {
        die "Missing dbh for $type type on field $field" . ($db_type ? " and db_type $db_type" : "");
      } elsif (UNIVERSAL::isa($dbh,'CODE')) {
        $dbh = &$dbh($field, $self) || die "SQL Coderef did not return a dbh";
      }
      my $sql  = $field_val->{$type};
      my @args = ($value) x $sql =~ tr/?//;
      my $return = $dbh->selectrow_array($sql, {}, @args); # is this right - copied from O::FORMS
      $field_val->{"${type}_error_if"} = 1 if ! defined $field_val->{"${type}_error_if"};
      if ( (! $return && $field_val->{"${type}_error_if"})
           || ($return && ! $field_val->{"${type}_error_if"}) ) {
        return [] if $self->{'_check_conditional'};
        push @errors, [$field, $type, $field_val, $ifs_match];
      }
      $content_checked = 1;
    }

    ### server side custom type
    foreach my $type (grep {/^custom_?\d*$/} @$types) {
      my $check = $field_val->{$type};
      next if UNIVERSAL::isa($check, 'CODE') ? &$check($field, $value, $field_val, $type) : $check;
      return [] if $self->{'_check_conditional'};
      push @errors, [$field, $type, $field_val, $ifs_match];
      $content_checked = 1;
    }

    ### do specific type checks
    foreach my $type (grep {/^type_?\d*$/} @$types) {
      if (! $self->check_type($value,$field_val->{'type'},$field,$form)){
        return [] if $self->{'_check_conditional'};
        push @errors, [$field, $type, $field_val, $ifs_match];
      }
      $content_checked = 1;
    }
  }

  ### allow for the data to be "untainted"
  ### this is only allowable if the user ran some other check for the datatype
  if ($field_val->{'untaint'} && $#errors == -1) {
    if (! $content_checked) {
        push @errors, [$field, 'untaint', $field_val, $ifs_match];
    } else {
      ### generic untainter - assuming the other required content_checks did good validation
      $_ = /(.*)/ ? $1 : die "Couldn't match?" foreach @$values;
      if ($n_values == 1) {
        $form->{$field} = $values->[0];
        $self->{cgi_object}->param(-name => $field, -value => $values->[0])
          if $self->{cgi_object};
      } else {
        ### values in @{ $form->{$field} } were modified directly
        $self->{cgi_object}->param(-name => $field, -value => $values)
          if $self->{cgi_object};
      }
    }
  }

  ### all done - time to return
  return @errors ? \@errors : 0;
}

###----------------------------------------------------------------###

### used to validate specific types
sub check_type {
  my $self  = shift;
  my $value = shift;
  my $type  = uc(shift);

  ### do valid email address for our system
  if ($type eq 'EMAIL') {
    return 0 if ! $value;
    my($local_p,$dom) = ($value =~ /^(.+)\@(.+?)$/) ? ($1,$2) : return 0;

    return 0 if length($local_p) > 60;
    return 0 if length($dom) > 100;
    return 0 if ! $self->check_type($dom,'DOMAIN') && ! $self->check_type($dom,'IP');
    return 0 if ! $self->check_type($local_p,'LOCAL_PART');

  ### the "username" portion of an email address
  } elsif ($type eq 'LOCAL_PART') {
    return 0 if ! defined($value) || ! length($value);
    return 0 if $value =~ m/[^a-z0-9.\-!&+]/;
    return 0 if $value =~ m/^[\.\-]/;
    return 0 if $value =~ m/[\.\-\&]$/;
    return 0 if $value =~ m/(\.\-|\-\.|\.\.)/;

  ### standard IP address
  } elsif ($type eq 'IP') {
    return 0 if ! $value;
    return (4 == grep {!/\D/ && $_ < 256} split /\./, $value, 4);

  ### domain name - including tld and subdomains (which are all domains)    
  } elsif ($type eq 'DOMAIN') {
    return 0 if ! $value;
    return 0 if $value =~ m/[^a-z0-9.\-]/;
    return 0 if $value =~ m/^[\.\-]/;
    return 0 if $value =~ m/(\.\-|\-\.|\.\.)/;
    return 0 if length($value) > 255;
    return 0 if $value !~ s/\.([a-z]+)$//;

    my $ext = $1;
    if ($ext eq 'name') { # .name domains
      return 0 if $value !~ /^[a-z0-9][a-z0-9\-]{0,62} \. [a-z0-9][a-z0-9\-]{0,62}$/x;
    } else {              # any other domains
      return 0 if $value !~ /^([a-z0-9][a-z0-9\-]{0,62} \.)* [a-z0-9][a-z0-9\-]{0,62}$/x;
    }

  ### validate a url
  } elsif ($type eq 'URL') {
    return 0 if ! $value;
    $value =~ s|^https?://([^/]+)||i || return 0;
    my $dom = $1;
    return 0 if ! $self->check_type($dom,'DOMAIN') && ! $self->check_type($dom,'IP');
    return 0 if $value && ! $self->check_type($value,'URI');

  ### validate a uri - the path portion of a request
  } elsif ($type eq 'URI') {
    return 0 if ! $value;
    return 0 if $value =~ m/\s+/;

  } elsif ($type eq 'CC') {
    return 0 if ! $value;
    ### validate the number
    return 0 if $value =~ /[^\d\-\ ]/
      || length($value) > 16
      || length($value) < 13;

    ### simple mod10 check
    $value =~ s/\D//g;
    my $sum    = 0;
    my $switch = 0;
    foreach my $digit ( reverse split //, $value ){
      $switch = 1 if ++ $switch > 2;
      my $y = $digit * $switch;
      $y -= 9 if $y > 9;
      $sum += $y;
    }
    return 0 if $sum % 10;

  }

  return 1;
}

###----------------------------------------------------------------###

sub get_validation {
    my $self = shift;
    my $val  = shift;
    require CGI::Ex::Conf;
    return CGI::Ex::Conf::conf_read($val, {html_key => 'validation', default_ext => $DEFAULT_EXT});
}

### returns all keys from all groups - even if group has validate_if
sub get_validation_keys {
  my $self     = shift;
  my $val_hash = shift;
  my $form     = shift; # with optional form - will only return keys in validated groups

  ### turn the form into a form hash if doesn't look like one already
  if ($form) {
      die "Invalid form hash or cgi object" if ! ref $form;
      if (ref $form ne 'HASH') {
          local $self->{cgi_object} = $form;
          $form = $self->cgix->get_form($form);
      }
  }

  ### make sure the validation is a hashref
  ### get_validation handle odd types
  if (ref $val_hash ne 'HASH') {
    $val_hash = $self->get_validation($val_hash) if ref $val_hash ne 'SCALAR' || ! ref $val_hash;
    die "Validation groups must be a hashref"    if ref $val_hash ne 'HASH';
  }

  ### parse keys that are group arguments - and those that are keys to validate
  my %ARGS;
  my @field_keys = grep { /^(?:group|general)\s+(\w+)/
                            ? do {$ARGS{$1} = $val_hash->{$_} ; 0}
                            : 1 }
                   sort keys %$val_hash;

  ### only validate this group if it is supposed to be checked
  return if $form && $ARGS{'validate_if'} && ! $self->check_conditional($form, $ARGS{'validate_if'});

  ### Look first for items in 'group fields' or 'group order'
  my %keys;
  if (my $fields = $ARGS{'fields'} || $ARGS{'order'}) {
    my $type = $ARGS{'fields'} ? 'group fields' : 'group order';
    die "Validation '$type' must be an arrayref when passed"
      if ! UNIVERSAL::isa($fields, 'ARRAY');
    foreach my $field (@$fields) {
        die "Non-defined value in '$type'" if ! defined $field;
        if (ref $field) {
            die "Found nonhashref value in '$type'" if ref($field) ne 'HASH';
            die "Element missing \"field\" key/value in '$type'" if ! defined $field->{'field'};
            $keys{$field->{'field'}} = $field->{'name'} || 1;
        } elsif ($field eq 'OR') {
        } else {
            die "No element found in '$type' for $field" if ! exists $val_hash->{$field};
            die "Found nonhashref value in '$type'" if ref($val_hash->{$field}) ne 'HASH';
            $keys{$field} = $val_hash->{$field}->{'name'} || 1;
        }
    }
  }

  ### add any remaining field_vals from our original hash
  ### this is necessary for items that weren't in group fields or group order
  foreach my $field (@field_keys) {
      next if $keys{$field};
      die "Found nonhashref value for field $field" if ref($val_hash->{$field}) ne 'HASH';
      if (defined $val_hash->{$field}->{'field'}) {
          $keys{$val_hash->{$field}->{'field'}} = $val_hash->{$field}->{'name'} || 1;
      } else {
          $keys{$field} = $val_hash->{$field}->{'name'} || 1;
      }
  }

  return \%keys;
}

###----------------------------------------------------------------###

### spit out a chunk that will do the validation
sub generate_js {
    ### allow for some browsers to not receive the validation js
    return "<!-- JS validation not supported in this browser $_ -->"
        if $ENV{'HTTP_USER_AGENT'} && grep {$ENV{'HTTP_USER_AGENT'} =~ $_} @UNSUPPORTED_BROWSERS;

    my $self        = shift;
    my $val_hash    = shift || die "Missing validation";
    my $form_name   = shift || die "Missing form name";
    my $js_uri_path = shift || $JS_URI_PATH;
    $val_hash = $self->get_validation($val_hash);

    ### store any extra items from self
    my %EXTRA = ();
    $EXTRA{"general $_"} = $self->{$_} for grep {/$QR_EXTRA/o} keys %$self; # add 'general' to be used in javascript

    my $js_uri_path_validate = $JS_URI_PATH_VALIDATE || do {
        die "Missing \$js_uri_path" if ! $js_uri_path;
        "$js_uri_path/CGI/Ex/validate.js";
    };

    if (! $self->{'no_jsondump'} && eval { require CGI::Ex::JSONDump }) {
        my $json = CGI::Ex::JSONDump->new({pretty => 1})->dump($val_hash);
        return qq{<script src="$js_uri_path_validate"></script>
<script>
document.validation = $json;
if (document.check_form) document.check_form("$form_name");
</script>
};

    } elsif (! $self->{'no_json'} && eval { require JSON }) {
        my $json = JSON->new(pretty => 1)->objToJson($val_hash);

        return qq{<script src="$js_uri_path_validate"></script>
<script>
document.validation = $json;
if (document.check_form) document.check_form("$form_name");
</script>
};

    } elsif (eval { require YAML }) {

        my $str = YAML::Dump((scalar keys %EXTRA) ? (\%EXTRA) : () , $val_hash);
        $str =~ s/(?<!\\)\\(?=[sSdDwWbB0-9?.*+|\-\^\${}()\[\]])/\\\\/g; # fix some issues with YAML
        $str =~ s/\n/\\n\\\n/g; # allow for one big string that flows on multiple lines
        $str =~ s/\"/\\\"/g; # quotify it

        ### get the paths
        my $js_uri_path_yaml = $JS_URI_PATH_YAML || do {
            die "Missing \$js_uri_path" if ! $js_uri_path;
            "$js_uri_path/CGI/Ex/yaml_load.js";
        };

        ### return the string
        return qq{<script src="$js_uri_path_yaml"></script>
<script src="$js_uri_path_validate"></script>
<script>
document.validation = "$str";
if (document.check_form) document.check_form("$form_name");
</script>
};
    } else {
        return '<!-- no JSON or YAML support found for JS validation -->';
    }
}

###----------------------------------------------------------------###
### How to handle errors

package CGI::Ex::Validate::Error;

use strict;
use overload '""' => \&as_string;

sub new {
    my ($class, $errors, $extra) = @_;
    die "Missing or invalid errors arrayref" if ref $errors ne 'ARRAY';
    die "Missing or invalid extra  hashref"  if ref $extra  ne 'HASH';
    return bless {errors => $errors, extra => $extra}, $class;
}

sub as_string {
  my $self = shift;
  my $extra  = $self->{extra} || {};
  my $extra2 = shift || {};

  ### allow for formatting
  my $join = defined($extra2->{as_string_join}) ? $extra2->{as_string_join}
    : defined($extra->{as_string_join}) ? $extra->{as_string_join}
    : "\n";
  my $header = defined($extra2->{as_string_header}) ? $extra2->{as_string_header}
    : defined($extra->{as_string_header}) ? $extra->{as_string_header} : "";
  my $footer = defined($extra2->{as_string_footer}) ? $extra2->{as_string_footer}
    : defined($extra->{as_string_footer}) ? $extra->{as_string_footer} : "";

  return $header . join($join, @{ $self->as_array($extra2) }) . $footer;
}

### return an array of applicable errors
sub as_array {
  my $self = shift;
  my $errors = $self->{errors} || die "Missing errors";
  my $extra  = $self->{extra}  || {};
  my $extra2 = shift || {};

  my $title = defined($extra2->{as_array_title}) ? $extra2->{as_array_title}
    : defined($extra->{as_array_title}) ? $extra->{as_array_title}
    : "Please correct the following items:";

  ### if there are heading items then we may end up needing a prefix
  my $has_headings;
  if ($title) {
    $has_headings = 1;
  } else {
    foreach (@$errors) {
      next if ref;
      $has_headings = 1;
      last;
    }
  }

  my $prefix = defined($extra2->{as_array_prefix}) ? $extra2->{as_array_prefix}
    : defined($extra->{as_array_prefix}) ? $extra->{as_array_prefix}
    : $has_headings ? '  ' : '';

  ### get the array ready
  my @array = ();
  push @array, $title if length $title;

  ### add the errors
  my %found = ();
  foreach my $err (@$errors) {
    if (! ref $err) {
      push @array, $err;
      %found = ();
    } else {
      my $text = $self->get_error_text($err);
      next if $found{$text};
      $found{$text} = 1;
      push @array, "$prefix$text";
    }
  }

  return \@array;
}

### return a hash of applicable errors
sub as_hash {
  my $self = shift;
  my $errors = $self->{errors} || die "Missing errors";
  my $extra  = $self->{extra}  || {};
  my $extra2 = shift || {};

  my $suffix = defined($extra2->{as_hash_suffix}) ? $extra2->{as_hash_suffix}
    : defined($extra->{as_hash_suffix}) ? $extra->{as_hash_suffix} : '_error';
  my $join   = defined($extra2->{as_hash_join}) ? $extra2->{as_hash_join}
    : defined($extra->{as_hash_join}) ? $extra->{as_hash_join} : '<br />';

  ### now add to the hash
  my %found  = ();
  my %return = ();
  foreach my $err (@$errors) {
    next if ! ref $err;

    my ($field, $type, $field_val, $ifs_match) = @$err;
    die "Missing field name" if ! $field;
    if ($field_val->{delegate_error}) {
      $field = $field_val->{delegate_error};
      $field =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
    }

    my $text = $self->get_error_text($err);
    next if $found{$field}->{$text};
    $found{$field}->{$text} = 1;

    $field .= $suffix;
    $return{$field} ||= [];
    $return{$field} = [$return{$field}] if ! ref($return{$field});
    push @{ $return{$field} }, $text;
  }

  ### allow for elements returned as
  if ($join) {
    my $header = defined($extra2->{as_hash_header}) ? $extra2->{as_hash_header}
      : defined($extra->{as_hash_header}) ? $extra->{as_hash_header} : "";
    my $footer = defined($extra2->{as_hash_footer}) ? $extra2->{as_hash_footer}
      : defined($extra->{as_hash_footer}) ? $extra->{as_hash_footer} : "";
    foreach my $key (keys %return) {
      $return{$key} = $header . join($join,@{ $return{$key} }) . $footer;
    }
  }

  return \%return;
}

### return a user friendly error message
sub get_error_text {
  my $self  = shift;
  my $err   = shift;
  my $extra = $self->{extra} || {};
  my ($field, $type, $field_val, $ifs_match) = @$err;
  my $dig     = ($type =~ s/(_?\d+)$//) ? $1 : '';
  my $type_lc = lc($type);

  ### allow for delegated field names - only used for defaults
  if ($field_val->{delegate_error}) {
    $field = $field_val->{delegate_error};
    $field =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
  }

  ### the the name of this thing
  my $name = $field_val->{'name'} || "The field $field";
  $name =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;

  ### type can look like "required" or "required2" or "required100023"
  ### allow for fallback from required100023_error through required_error
  my @possible_error_keys = ("${type}_error");
  unshift @possible_error_keys, "${type}${dig}_error" if length($dig);

  ### look in the passed hash or self first
  my $return;
  foreach my $key (@possible_error_keys){
    $return = $field_val->{$key} || $extra->{$key} || next;
    $return =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
    $return =~ s/\$field/$field/g;
    $return =~ s/\$name/$name/g;
    if (my $value = $field_val->{"$type$dig"}) {
      $return =~ s/\$value/$value/g if ! ref $value;
    }
    last;
  }

  ### set default messages
  if (! $return) {
    if ($type eq 'required' || $type eq 'required_if') {
      $return = "$name is required.";

    } elsif ($type eq 'min_values') {
      my $n = $field_val->{"min_values${dig}"};
      my $values = ($n == 1) ? 'value' : 'values';
      $return = "$name had less than $n $values.";

    } elsif ($type eq 'max_values') {
      my $n = $field_val->{"max_values${dig}"};
      my $values = ($n == 1) ? 'value' : 'values';
      $return = "$name had more than $n $values.";

    } elsif ($type eq 'enum') {
      $return = "$name is not in the given list.";

    } elsif ($type eq 'equals') {
      my $field2 = $field_val->{"equals${dig}"};
      my $name2  = $field_val->{"equals${dig}_name"} || "the field $field2";
      $name2 =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
      $return = "$name did not equal $name2.";

    } elsif ($type eq 'min_len') {
      my $n = $field_val->{"min_len${dig}"};
      my $char = ($n == 1) ? 'character' : 'characters';
      $return = "$name was less than $n $char.";

    } elsif ($type eq 'max_len') {
      my $n = $field_val->{"max_len${dig}"};
      my $char = ($n == 1) ? 'character' : 'characters';
      $return = "$name was more than $n $char.";

    } elsif ($type eq 'max_in_set') {
      my $set = $field_val->{"max_in_set${dig}"};
      $return = "Too many fields were chosen from the set ($set)";

    } elsif ($type eq 'min_in_set') {
      my $set = $field_val->{"min_in_set${dig}"};
      $return = "Not enough fields were chosen from the set ($set)";

    } elsif ($type eq 'match') {
      $return = "$name contains invalid characters.";

    } elsif ($type eq 'compare') {
      $return = "$name did not fit comparison.";

    } elsif ($type eq 'sql') {
      $return = "$name did not match sql test.";

    } elsif ($type eq 'custom') {
      $return = "$name did not match custom test.";

    } elsif ($type eq 'type') {
      my $_type = $field_val->{"type${dig}"};
      $return = "$name did not match type $_type.";

    } elsif ($type eq 'untaint') {
      $return = "$name cannot be untainted without one of the following checks: enum, equals, match, compare, sql, type, custom";

    } elsif ($type eq 'no_extra_fields') {
      $return = "$name should not be passed to validate.";
    }
  }

  die "Missing error on field $field for type $type$dig" if ! $return;
  return $return;

}

###----------------------------------------------------------------###

1;


__END__

=head1 SYNOPSIS

    use CGI::Ex::Validate;

    ### THE SHORT

    my $errobj = CGI::Ex::Validate->new->validate($form, $val_hash);

    ### THE LONG

    my $form = CGI->new;
     # OR #
    my $form = CGI::Ex->new; # OR CGI::Ex->get_form;
     # OR #
    my $form = {key1 => 'val1', key2 => 'val2'};


    ### simplest
    my $val_hash = {
        username => {
            required => 1,
            max_len  => 30,
            field    => 'username',
            # field is optional in this case - will use key name
        },
        email    => {
            required => 1,
            max_len  => 100,
        },
        email2   => {
            validate_if => 'email',
            equals      => 'email',
        },
    };

    ### ordered
    my $val_hash = {
        'group order' => [qw(username email email2)],
        username => {required => 1, max_len => 30},
        email    => ...,
        email2   => ...,
    };

    ### ordered again
    my $val_hash = {
        'group fields' => [{
            field    => 'username', # field is not optional in this case
            required => 1,
            max_len  => 30,
        }, {
            field    => 'email',
            required => 1,
            max_len  => 100,
        }, {
            field       => 'email2',
            validate_if => 'email',
            equals      => 'email',
        }],
    };


    my $vob    = CGI::Ex::Validate->new;
    my $errobj = $vob->validate($form, $val_hash);
    # OR #
    my $errobj = $vob->validate($form, "/somefile/somewhere.val"); # import config using yaml file
    # OR #
    my $errobj = $vob->validate($form, "/somefile/somewhere.pl");  # import config using perl file
    # OR #
    my $errobj = $vob->validate($form, "--- # a yaml document\n"); # import config using yaml str


    if ($errobj) {
        my $error_heading = $errobj->as_string; # OR "$errobj";
        my $error_list    = $errobj->as_array;  # ordered list of what when wrong
        my $error_hash    = $errobj->as_hash;   # hash of arrayrefs of errors
    } else {
        # form passed validation
    }

    ### will add an error for any form key not found in $val_hash
    my $vob = CGI::Ex::Validate->new({no_extra_keys => 1});
    my $errobj = $vob->validate($form, $val_hash);

=head1 DESCRIPTION

CGI::Ex::Validate is one of many validation modules.  It aims to have
all of the basic data validation functions, avoid adding all of the
millions of possible types, while still giving the capability for the
developer to add their own types.

CGI::Ex::Validate can work in a simple way like all of the other
validators do.  However, it also allows for grouping of validation
items and conditional validation of groups or individual items.  This
is more in line with the normal validation procedures for a website.

It also has full support for providing the same validation in javascript.
It provides methods for attaching the javascript to existing forms.

=head1 METHODS

=over 4

=item C<new>

Used to instantiate the object.  Arguments are either a hash, or hashref,
or nothing at all.  Keys of the hash become the keys of the object.

=item C<get_validation>

Given a filename or YAML string will return perl hash.  If more than one
group is contained in the file, it will return an arrayref of hashrefs.

    my $ref = $self->get_validation($file);

=item C<get_validation_keys>

Given a filename or YAML string or a validation hashref, will return all
of the possible keys found in the validation hash.  This can be used to
check to see if extra items have been passed to validate.  If a second
argument contains a form hash is passed, get_validation_keys will only
return the keys of groups that were validated.

    my $key_hashref = $self->get_validation_keys($val_hash);

The values of the hash are the names of the fields.

=item C<validate>

Arguments are a form hashref or cgi object, a validation hashref or
filename, and an optional what_was_validated arrayref (discussed
further later on).  If a CGI object is passed, CGI::Ex::get_form will
be called on that object to turn it into a hashref.  If a filename is
given for the validation, get_validation will be called on that
filename.  If the what_was_validated_arrayref is passed - it will be
populated (pushed) with the field hashes that were actually validated
(anything that was skipped because of validate_if will not be in the
array).

If the form passes validation, validate will return undef.  If it
fails validation, it will return a CGI::Ex::Validate::Error object.
If the 'raise_error' general option has been set, validate will die
with a CGI::Ex::validate::Error object as the value.

    my $err_obj = $self->validate($form, $val_hash);

    # OR #

    $self->{raise_error} = 1; # can also be listed in the val_hash
    eval { $self->validate($form, $val_hash) };
    if ($@) { my $err_obj = $@; }

=item C<generate_js>

Requires TODO JSON or YAML to work properly (see L<JSON> or L<YAML>).

Takes a validation hash, a form name, and an optional javascript uri
path and returns Javascript that can be embedded on a page and will
perform identical validations as the server side.  The validation can
be any validation hash (or arrayref of hashes.  The form name must be
the name of the form that the validation will act upon - the name is
used to register an onsubmit function.  The javascript uri path is
used to embed the locations two external javascript source files.


The javascript uri path is highly dependent upon the server
configuration and therefore must be configured manually.  It may be
passed to generate_js, or it may be specified in $JS_URI_PATH.  There
are two files included with this module that are needed -
CGI/Ex/yaml_load.js and CGI/Ex/validate.js.  When generating the js
code, generate_js will look in $JS_URI_PATH_YAML and
$JS_URI_PATH_VALIDATE.  If either of these are not set, generate_js
will default to "$JS_URI_PATH/CGI/Ex/yaml_load.js" and
"$JS_URI_PATH/CGI/Ex/validate.js".

    $self->generate_js($val_hash, 'my_form', "/cgi-bin/js")

    # would generate something like the following...

    <script src="/cgi-bin/js/CGI/Ex/yaml_load.js"></script>
    <script src="/cgi-bin/js/CGI/Ex/validate.js"></script>
    ... more js follows ...

    $CGI::Ex::Validate::JS_URI_PATH      = "/stock/js";
    $CGI::Ex::Validate::JS_URI_PATH_YAML = "/js/yaml_load.js";
    $self->generate_js($val_hash, 'my_form')

    # would generate something like the following...

    <script src="/js/yaml_load.js"></script>
    <script src="/stock/js/CGI/Ex/validate.js"></script>
    ... more js follows ...

Referencing yaml_load.js and validate.js can be done in any of
several ways.  They can be copied to or symlinked to a fixed location
in the servers html directory.  They can also be printed out by a cgi.
The method C<-E<gt>print_js> has been provided in CGI::Ex for printing
js files found in the perl hierarchy.  See L<CGI::Ex> for more details.
The $JS_URI_PATH of "/cgi-bin/js" could contain the following:

    #!/usr/bin/perl -w

    use strict;
    use CGI::Ex;

    ### path_info should contain something like /CGI/Ex/yaml_load.js
    my $info = $ENV{PATH_INFO} || '';
    die "Invalid path" if $info !~ m|^(/\w+)+.js$|;
    $info =~ s|^/+||;

    CGI::Ex->new->print_js($info);
    exit;

The print_js method in CGI::Ex is designed to cache the javascript in
the browser (caching is suggested as they are medium sized files).

=item C<-E<gt>cgix>

Returns a CGI::Ex object.  Used internally.

=back

=head1 VALIDATION HASH

The validation hash may be passed as a perl a hashref or as a
filename, or as a YAML document string.  If it is a filename, it will
be translated into a hash using the %EXT_HANDLER for the extension on
the file.  If there is no extension, it will use $DEFAULT_EXT as a
default.

The validation "hash" may also be an arrayref of hashrefs.  In this
case, each arrayref is treated as a group and is validated separately.
A group can have a validate_if function that allows for that
particular group to apply only if certain conditions are met.

=head1 GROUPS

Each hashref that is passed as a validation hash is treated as a
group.  Keys matching the regex m/^group\s+(\w+)$/ are reserved and
are counted as GROUP OPTIONS.  Keys matching the regex m/^general\s+(\w+)$/
are reserved and are counted as GENERAL OPTIONS.  Other keys (if
any, should be keys that need validation).

If the GROUP OPTION 'group validate_if' is set, the group will only
be validated if the conditions are met.  Any group with out a validate_if
fill be automatically validated.

Each of the items listed in the group will be validated.  The
validation order is determined in one of three ways:

=over 4

=item Specify 'group fields' arrayref.

    # order will be (username, password, 'm/\w+_foo/', somethingelse)
    {
      'group title' => "User Information",
      'group fields' => [
        {field => 'username',   required => 1},
        {field => 'password',   required => 1},
        {field => 'm/\w+_foo/', required => 1},
      ],
      somethingelse => {required => 1},
    }

=item Specify 'group order' arrayref.

    # order will be (username, password, 'm/\w+_foo/', somethingelse)
    {
      'group title' => "User Information",
      'group order' => [qw(username password), 'm/\w+_foo/'],
      username      => {required => 1},
      password      => {required => 1},
      'm/\w+_foo/'  => {required => 1},
      somethingelse => {required => 1},
    }

=item Do nothing - use sorted order.

    # order will be ('m/\w+_foo/', password, somethingelse, username)
    {
      'group title' => "User Information",
      username      => {required => 1},
      password      => {required => 1},
      'm/\w+_foo/'  => {required => 1},
      somethingelse => {required => 1},
    }

=back

Each of the individual field validation hashrefs should contain the
types listed in VALIDATION TYPES.

Optionally the 'group fields' or the 'group order' may contain the
word 'OR' as a special keyword.  If the item preceding 'OR' fails
validation the item after 'OR' will be tested instead.  If the item
preceding 'OR' passes validation the item after 'OR' will not be
tested.

    'group order' => [qw(zip OR postalcode state OR region)],

Each individual validation hashref will operate on the field contained
in the 'field' key.  This key may also be a regular expression in the
form of 'm/somepattern/'.  If a regular expression is used, all keys
matching that pattern will be validated.

=head1 VALIDATION TYPES

This section lists the available validation types.  Multiple instances
of the same type may be used for some validation types by adding a
number to the type (ie match, match2, match232, match_94).  Multiple
instances are validated in sorted order.  Types that allow multiple
values are:

    compare
    custom
    equals
    match
    max_in_set
    min_in_set
    replace
    required_if
    sql
    type
    validate_if

=over 4

=item C<validate_if>

If validate_if is specified, the field will only be validated
if the conditions are met.  Works in JS.

    validate_if => {field => 'name', required => 1, max_len => 30}
    # Will only validate if the field "name" is present and is less than 30 chars.

    validate_if => 'name',
    # SAME as
    validate_if => {field => 'name', required => 1},

    validate_if => '! name',
    # SAME as
    validate_if => {field => 'name', max_in_set => '0 of name'},

    validate_if => {field => 'country', compare => "eq US"},
    # only if country's value is equal to US

    validate_if => {field => 'country', compare => "ne US"},
    # if country doesn't equal US

    validate_if => {field => 'password', match => 'm/^md5\([a-z0-9]{20}\)$/'},
    # if password looks like md5(12345678901234567890)

    {
      field       => 'm/^(\w+)_pass/',
      validate_if => '$1_user',
      required    => 1,
    }
    # will validate foo_pass only if foo_user was present.

The validate_if may also contain an arrayref of validation items.  So that
multiple checks can be run.  They will be run in order.  validate_if will
return true only if all options returned true.

    validate_if => ['email', 'phone', 'fax']

Optionally, if validate_if is an arrayref, it may contain the word
'OR' as a special keyword.  If the item preceding 'OR' fails validation
the item after 'OR' will be tested instead.  If the item preceding 'OR'
passes validation the item after 'OR' will not be tested.

    validate_if => [qw(zip OR postalcode)],

=item C<required_if>

Requires the form field if the condition is satisfied.  The conditions
available are the same as for validate_if.  This is somewhat the same
as saying:

    validate_if => 'some_condition',
    required    => 1

    required_if => 'some_condition',

If a regex is used for the field name, the required_if
field will have any match patterns swapped in.

    {
      field       => 'm/^(\w+)_pass/',
      required_if => '$1_user',
    }

This example would require the "foobar_pass" field to be set
if the "foobar_user" field was passed.

=item C<required>

Requires the form field to have some value.  If the field is not present,
no other checks will be run.

=item C<min_values> and C<max_values>

Allows for specifying the maximum number of form elements passed.
max_values defaults to 1 (You must explicitly set it higher
to allow more than one item by any given name).

=item C<min_in_set> and C<max_in_set>

Somewhat like min_values and max_values except that you specify the
fields that participate in the count.  Also - entries that are not
defined or do not have length are not counted.  An optional "of" can
be placed after the number for human readability.

    min_in_set => "2 of foo bar baz",
      # two of the fields foo, bar or baz must be set
      # same as
    min_in_set => "2 foo bar baz",
      # same as
    min_in_set => "2 OF foo bar baz",

    validate_if => {field => 'whatever', max_in_set => '0 of whatever'},
      # only run validation if there were zero occurrences of whatever

=item C<enum>

Allows for checking whether an item matches a set of options.  In perl
the value may be passed as an arrayref.  In the conf or in perl the
value may be passed of the options joined with ||.

    {
      field => 'password_type',
      enum  => 'plaintext||crypt||md5', # OR enum => [qw(plaintext crypt md5)],
    }

=item C<equals>

Allows for comparison of two form elements.  Can have an optional !.

    {
      field  => 'password',
      equals => 'password_verify',
    },
    {
      field  => 'domain1',
      equals => '!domain2', # make sure the fields are not the same
    }

=item C<min_len and max_len>

Allows for check on the length of fields

    {
      field   => 'site',
      min_len => 4,
      max_len => 100,
    }

=item C<match>

Allows for regular expression comparison.  Multiple matches may
be concatenated with ||.  Available in JS.

    {
      field   => 'my_ip',
      match   => 'm/^\d{1,3}(\.\d{1,3})3$/',
      match_2 => '!/^0\./ || !/^192\./',
    }

=item C<compare>

Allows for custom comparisons.  Available types are
>, <, >=, <=, !=, ==, gt, lt, ge, le, ne, and eq.  Comparisons
also work in the JS.

    {
      field    => 'my_number',
      match    => 'm/^\d+$/',
      compare1 => '> 100',
      compare2 => '< 255',
      compare3 => '!= 150',
    }

=item C<sql>

SQL query based - not available in JS.  The database handle will be looked
for in the value $self->{dbhs}->{foo} if sql_db_type is set to 'foo',
otherwise it will default to $self->{dbh}.  If $self->{dbhs}->{foo} or
$self->{dbh} is a coderef - they will be called and should return a dbh.

    {
      field => 'username',
      sql   => 'SELECT COUNT(*) FROM users WHERE username = ?',
      sql_error_if => 1, # default is 1 - set to 0 to negate result
      # sql_db_type  => 'foo', # will look for a dbh under $self->{dbhs}->{foo}
    }

=item C<custom>

Custom value - not available in JS.  Allows for extra programming types.
May be either a boolean value predetermined before calling validate, or may be
a coderef that will be called during validation.  If coderef is called, it will
be passed the field name, the form value for that name, and a reference to the
field validation hash.  If the custom type returns false the element fails
validation and an error is added.

    {
      field => 'username',
      custom => sub {
        my ($key, $val, $type, $field_val_hash) = @_;
        # do something here
        return 0;
      },
    }

=item C<custom_js>

Custom value - only available in JS.  Allows for extra programming types.
May be either a boolean value pre-determined before calling validate, or may be
section of javascript that will be eval'ed.  The last value (return value) of
the eval'ed javascript will determine if validation passed.  A false value indicates
the value did not pass validation.  A true value indicates that it did.  See
the t/samples/js_validate_3.html page for a sample of usage.

    {
      field => 'date',
      required => 1,
      match    => 'm|^\d\d\d\d/\d\d/\d\d$|',
      match_error => 'Please enter date in YYYY/MM/DD format',
      custom_js => "
        var t=new Date();
        var y=t.getYear()+1900;
        var m=t.getMonth() + 1;
        var d=t.getDate();
        if (m<10) m = '0'+m;
        if (d<10) d = '0'+d;
        (value > ''+y+'/'+m+'/'+d) ? 1 : 0;
      ",
      custom_js_error => 'The date was not greater than today.',
    }

=item C<type>

Allows for more strict type checking.  Currently supported types
include CC (credit card).  Other types will be added upon request provided
we can add a perl and a javascript version.

    {
      field => 'credit_card',
      type  => 'CC',
    }

=back

=head1 SPECIAL VALIDATION TYPES

=over 4

=item C<field>

Specify which field to work on.  Key may be a regex in the form 'm/\w+_user/'.
This key is required if 'group fields' is used or if validate_if or required_if
are used.  It can optionally be used with other types to specify a different form
element to operate on.  On errors, if a non-default error is found, $field
will be swapped with the value found in field.

The field name may also be a regular expression in the
form of 'm/somepattern/'.  If a regular expression is used, all keys
matching that pattern will be validated.

=item C<name>

Name to use for errors.  If a name is not specified, default errors will use
"The field $field" as the name.  If a non-default error is found, $name
will be swapped with this name.

=item C<delegate_error>

This option allows for any errors generated on a field to delegate to
a different field.  If the field name was a regex, any patterns will
be swapped into the delegate_error value. This option is generally only
useful with the as_hash method of the error object (for inline errors).

    {
      field => 'zip',
      match => 'm/^\d{5}/',
    },
    {
      field => 'zip_plus4',
      match => 'm/^\d{4}/',
      delegate_error => 'zip',
    },
    {
      field => 'm/^(id_[\d+])_user$/',
      delegate_error => '$1',
    },

=item C<exclude_js>

This allows the cgi to do checking while keeping the checks from
being run in JavaScript

    {
      field      => 'cgi_var',
      required   => 1,
      exclude_js => 1,
    }

=item C<exclude_cgi>

This allows the js to do checking while keeping the checks from
being run in the cgi

    {
      field       => 'js_var',
      required    => 1,
      exclude_cgi => 1,
    }

=back

=head1 MODIFYING VALIDATION TYPES

The following types will modify the form value before it is processed.
They work in both the perl and in javascript as well.  The javascript
version changes the actual value in the form on appropriate form types.

=over 4

=item C<do_not_trim>

By default, validate will trim leading and trailing whitespace
from submitted values.  Set do_not_trim to 1 to allow it to
not trim.

    {field => 'foo', do_not_trim => 1}

=item C<trim_control_chars>

Off by default.  If set to true, removes characters in the
\x00 to \x31 range (Tabs are translated to a single space).

    {field => 'foo', trim_control_chars => 1}

=item C<replace>

Pass a swap pattern to change the actual value of the form.
Any perl regex can be passed but it is suggested that javascript
compatible regexes are used to make generate_js possible.

    {field => 'foo', replace => 's/(\d{3})(\d{3})(\d{3})/($1) $2-$3/'}

=item C<default>

Set item to default value if there is no existing value (undefined
or zero length string).

    {field => 'country', default => 'EN'}

=item C<to_upper_case> and C<to_lower_case>

Do what they say they do.

=item C<untaint>

Requires that the validated field has been also checked with
an enum, equals, match, compare, custom, or type check.  If the
field has been checked and there are no errors - the field is "untainted."

This is for use in conjunction with perl's -T switch.

=back

=head1 ERROR OBJECT

Failed validation results in an error an error object created via the
new_error method.  The default error class is CGI::Ex::Validate::Error.

The error object has several methods for determining what the errors were.

=over 4

=item C<as_array>

Returns an array or arrayref (depending on scalar context) of errors that
occurred in the order that they occurred.  Individual groups may have a heading
and the entire validation will have a heading (the default heading can be changed
via the 'as_array_title' general option).  Each error that occurred is a separate
item and are pre-pended with 'as_array_prefix' (which is a general option - default
is '  ').  The as_array_ options may also be set via a hashref passed to as_array.
as_array_title defaults to 'Please correct the following items:'.

  ### if this returns the following
  my $array = $err_obj->as_array;
  # $array looks like
  # ['Please correct the following items:', '  error1', '  error2']

  ### then this would return the following
  my $array = $err_obj->as_array({
    as_array_prefix => '  - ',
    as_array_title  => 'Something went wrong:',
  });
  # $array looks like
  # ['Something went wrong:', '  - error1', '  - error2']

=item C<as_string>

Returns values of as_array joined with a newline.  This method is used as
the stringification for the error object.  Values of as_array are joined with
'as_string_join' which defaults to "\n".  If 'as_string_header' is set, it will
be pre-pended onto the error string.  If 'as_string_footer' is set, it will be
appended onto the error string.

  ### if this returns the following
  my $string = $err_obj->as_string;
  # $string looks like
  # "Please correct the following items:\n  error1\n  error2"

  ### then this would return the following
  my $string = $err_obj->as_string({
    as_array_prefix  => '  - ',
    as_array_title   => 'Something went wrong:',
    as_string_join   => '<br />',
    as_string_header => '<span class="error">',
    as_string_footer => '</span>',
  });
  # $string looks like
  # '<span class="error">Something went wrong:<br />  - error1<br />  - error2</span>'

=item C<as_hash>

Returns a hash or hashref (depending on scalar context) of errors that
occurred.   Each key is the field name of the form that failed validation with
'as_hash_suffix' added on as a suffix.  as_hash_suffix is available as a general option
and may also be passed in via a hashref as the only argument to as_hash.
The default value is '_error'.  The values of the hash are arrayrefs of errors
that occurred to that form element.

By default as_hash will return the values of the hash as arrayrefs (a list of the errors
that occurred to that key).  It is possible to also return the values as strings.
Three options are available for formatting: 'as_hash_header' which will be pre-pended
onto the error string, 'as_hash_footer' which will be appended, and 'as_hash_join' which
will be used to join the arrayref.  The only argument required to force the
stringification is 'as_hash_join'.

  ### if this returns the following
  my $hash = $err_obj->as_hash;
  # $hash looks like
  # {key1_error => ['error1', 'error2']}

  ### then this would return the following
  my $hash = $err_obj->as_hash({
    as_hash_suffix => '_foo',
    as_hash_join   => '<br />',
    as_hash_header => '<span class="error">'
    as_hash_footer => '</span>'
  });
  # $hash looks like
  # {key1_foo => '<span class="error">error1<br />error2</span>'}

=back

=head1 GROUP OPTIONS

Any key in a validation hash matching the pattern m/^group\s+(\w+)$/
is considered a group option.  The current know options are:

=over 4

=item C<'group title'>

Used as a group section heading when as_array or as_string is called
by the error object.

=item C<'group order'>

Order in which to validate key/value pairs of group.

=item C<'group fields'>

Arrayref of validation items to validate.

=item C<'group validate_if'>

Conditions that will be checked to see if the group should be validated.
If no validate_if option is found, the group will be validated.

=back

=head1 GENERAL OPTIONS

Any key in a validation hash matching the pattern m/^general\s+(\w+)$/
is considered a general option.  General options will also be looked
for in the Validate object ($self) and can be set when instantiating
the object ($self->{raise_error} is equivalent to
$valhash->{'general raise_error'}).  The current know options are:

General options may be set in any group using the syntax:

  'general general_option_name' => 'general_option_value'

They will only be set if the group's validate_if is successful or
if the group does not have a validate_if.  It is also possible to set
a "group general" option using the following syntax:

  'group general_option_name' => 'general_option_value'

These items will only be set if the group fails validation.
If a group has a validate_if block and passes validation, the group
items will not be used.  This is so that a failed section can have
its own settings.  Note though that the last option found will be
used and that items set in $self override those set in the validation
hash.

Options may also be set globally before calling validate by
populating the %DEFAULT_OPTIONS global hash.

=over 4

=item C<'general raise_error'>

If raise_error is true, any call to validate that fails validation
will die with an error object as the value.

=item C<'general no_extra_fields'>

If no_extra_fields is true, validate will add errors for any field found
in form that does not have a field_val hashref in the validation hash.
Default is false.  If no_extra_fields is set to 'used', it will check for
any keys that were not in a group that was validated.

An important exception to this is that field_val hashrefs or field names listed
in a validate_if or required_if statement will not be included.  You must
have an explicit entry for each key.

=item C<'general \w+_error'>

These items allow for an override of the default errors.

  'general required_error' => '$name is really required',
  'general max_len_error'  => '$name must be shorter than $value characters',
    # OR #
  my $self = CGI::Ex::Validate->new({
    max_len_error => '$name must be shorter than $value characters',
  });

=item C<'general as_array_title'>

Used as the section title for all errors that occur, when as_array
or as_string is called by the error object.

=item C<'general as_array_prefix'>

Used as prefix to individual errors that occur, when as_array
or as_string is called by the error object.  Each individual error
will be prefixed with this string.  Headings will not be prefixed.
Default is '  '.

=item C<'general as_string_join'>

When as_string is called, the values from as_array will be joined with
as_string_join.  Default value is "\n".

=item C<'general as_string_header'>

If set, will be pre-pended onto the string when as_string is called.

=item C<'general as_string_footer'>

If set, will be pre-pended onto the string when as_string is called.

=item C<'general as_hash_suffix'>

Added on to key names during the call to as_hash.  Default is '_error'.

=item C<'general as_hash_join'>

By default, as_hash will return hashref values that are errors joined with
the default as_hash_join value of <br />.  It can also return values that are
arrayrefs of the errors.  This can be done by setting as_hash_join to a non-true value
(for example '')

=item C<'general as_hash_header'>

If as_hash_join has been set to a true value, as_hash_header may be set to
a string that will be pre-pended on to the error string.

=item C<'general as_hash_footer'>

If as_hash_join has been set to a true value, as_hash_footer may be set to
a string that will be postpended on to the error string.

=item C<'general no_inline'>

If set to true, the javascript validation will not attempt to generate inline
errors.  Default is true.  Inline errors are independent of confirm and alert
errors.

=item C<'general no_confirm'>

If set to true, the javascript validation will try to use an alert instead
of a confirm to inform the user of errors.  Alert and confirm are independent
or inline errors.  Default is false.

=item C<'general no_alert'>

If set to true, the javascript validation will not show an alert box
when errors occur.  Default is false.  This option only comes into
play if no_confirm is also set.  This option is independent of inline
errors.  Although it is possible to turn off all errors by setting
no_inline, no_confirm, and no_alert all to 1, it is suggested that at
least one of the error reporting facilities is left on.

=back

It is possible to have a group that contains nothing but general options.

  my $val_hash = [
    {'general error_title'    => 'The following things went wrong',
     'general error_prefix'   => '  - ',
     'general raise_error'    => 1,
     'general name_suffix'    => '_foo_error',
     'general required_error' => '$name is required',
    },
    {'group title' => 'User Information',
     username => {required => 1},
     email    => {required => 1},
     password => {required => 1},
    },
  ];

=head1 JAVASCRIPT

CGI::Ex::Validate provides for having duplicate validation on the
client side as on the server side.  Errors can be shown in any
combination of inline and confirm, inline and alert, inline only,
confirm only, alert only, and none.  These combinations are controlled
by the general options no_inline, no_confirm, and no_alert.
Javascript validation can be generated for a page using the
C<-E<gt>generate_js> Method of CGI::Ex::Validate.  It is also possible
to store the validation inline with the html.  This can be done by
giving each of the elements to be validated an attribute called
"validation", or by setting a global javascript variable called
"document.validation" or "var validation".  An html file containing this
validation will be read in using CGI::Ex::Conf::read_handler_html.

All inline html validation must be written in yaml.

It is anticipated that the html will contain something like either of the
following examples:

  <script src="/cgi-bin/js/CGI/Ex/yaml_load.js"></script>
  <script src="/cgi-bin/js/CGI/Ex/validate.js"></script>
  <script>
  // \n\ allows all browsers to view this as a single string
  document.validation = "\n\
  general no_confirm: 1\n\
  general no_alert: 1\n\
  group order: [username, password]\n\
  username:\n\
    required: 1\n\
    max_len: 20\n\
  password:\n\
    required: 1\n\
    max_len: 30\n\
  ";
  if (document.check_form) document.check_form('my_form_name');
  </script>

Alternately we can use element attributes:

  <form name="my_form_name">

  Username: <input type=text size=20 name=username validation="
    required: 1
    max_len: 20
  "><br>
  <span class=error id=username_error>[% username_error %]</span><br>

  Password: <input type=text size=20 name=password validation="
    required: 1
    max_len: 30
  "><br>
  <span class=error id=password_error>[% password_error %]</span><br>

  <input type=submit>

  </form>

  <script src="/cgi-bin/js/CGI/Ex/yaml_load.js"></script>
  <script src="/cgi-bin/js/CGI/Ex/validate.js"></script>
  <script>
  if (document.check_form) document.check_form('my_form_name');
  </script>

The read_handler_html from CGI::Ex::Conf will find either of these
types of validation.

If inline errors are asked for, each error that occurs will attempt
to find an html element with its name as the id.  For example, if
the field "username" failed validation and created a "username_error",
the javascript would set the html of <span id="username_error"></span>
to the error message.

It is suggested to use something like the following so that you can
have inline javascript validation as well as report validation errors
from the server side as well.

   <span class=error id=password_error>[% password_error %]</span><br>

If the javascript fails for some reason, the form should still be able
to submit as normal (fail gracefully).

If the confirm option is used, the errors will be displayed to the user.
If they choose OK they will be able to try and fix the errors.  If they
choose cancel, the form will submit anyway and will rely on the server
to do the validation.  This is for fail safety to make sure that if the
javascript didn't validate correctly, the user can still submit the data.

=head1 THANKS

Thanks to Eamon Daly for providing bug fixes for bugs in validate.js
caused by HTML::Prototype.

=head1 AUTHOR

Paul Seamons

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut


