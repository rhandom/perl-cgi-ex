package CGI::Ex::Validate;

### CGI Extended Validator

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom

use strict;
use vars qw($VERSION
            $ERROR_PACKAGE
            @DEFAULT_EXT %EXT_HANDLERS
            %DEFAULT_OPTIONS);

$VERSION = '0.93';

$ERROR_PACKAGE = 'CGI::Ex::Validate::Error';

@DEFAULT_EXT = ('val');

%EXT_HANDLERS = ('val'      => \&conf_handler_yaml,
                 'yaml'     => \&conf_handler_yaml,
                 'pl'       => \&conf_handler_pl,
                 'storable' => \&conf_handler_storable,
                 );

###----------------------------------------------------------------###

sub new {
  my $class = shift || __PACKAGE__;
  my $self  = (@_ && ref($_[0])) ? shift : {@_}; 

  ### allow for global defaults
  foreach (keys %DEFAULT_OPTIONS) {
    $self->{$_} = $DEFAULT_OPTIONS{$_} if ! exists $self->{$_};
  }

  return bless $self, $class;
}

###----------------------------------------------------------------###

### the main validation routine
sub validate {
  my $self = (! ref($_[0])) ? shift->new                    # $class->validate
              : UNIVERSAL::isa($_[0], __PACKAGE__) ? shift  # $self->validate
              : __PACKAGE__->new;                           # &validate
  my $form     = shift || die "Missing form hash";
  my $val_hash = shift || die "Missing validation hash";

  ### turn the form into a form if it is really a CGI object
  if (! ref($form)) {
    die "Invalid form hash or cgi object";
  } elsif(! UNIVERSAL::isa($form,'HASH')) {
    require CGI::Ex;
    $form = CGI::Ex->new->get_form($form);
  }

  ### get the validation - let get_validation deal with types
  ### if a ref is not passed - assume it is a filename
  $val_hash = $self->get_validation($val_hash);

  ### allow for validation passed as single group hash, single group array,
  ### or array of group hashes or group arrays
  my @ERRORS = ();
  my %EXTRA  = ();
  my @USED_GROUPS = ();
  my $group_order = (UNIVERSAL::isa($val_hash,'HASH')) ? [$val_hash] : $val_hash;
  foreach my $group_val (@$group_order) {
    die "Validation groups must be a hashref" if ! UNIVERSAL::isa($group_val,'HASH');
    my $title       = $group_val->{'group title'};
    my $validate_if = $group_val->{'group validate_if'};

    ### only validate this group if it is supposed to be checked
    next if $validate_if && ! $self->check_conditional($form, $validate_if);
    push @USED_GROUPS, $group_val;

    ### if the validation items were not passed as an arrayref
    ### look for a group order and then fail back to the keys of the group
    my @order  = sort keys %$group_val;
    my $fields = $group_val->{'group fields'};
    if ($fields) {
      die "'group fields' must be an arrayref" if ! UNIVERSAL::isa($fields,'ARRAY');
    } else {
      my @fields = ();
      if (my $order = $group_val->{'group order'} || \@order) {
        die "Validation 'group order' must be an arrayref" if ! UNIVERSAL::isa($order,'ARRAY');
        foreach my $field (@$order) {
          next if $field =~ /^(group|general)\s/; 
          my $field_val = exists($group_val->{$field}) ? $group_val->{$field}
            : ($field eq 'OR') ? 'OR' : die "No element found in group for $field";
          if (ref $field_val && ! $field_val->{'field'}) {
            $field_val = { %$field_val, 'field' => $field }; # copy the values to add the key
          }
          push @fields, $field_val;
        }
      }
      $fields = \@fields;
    }

    ### check which fields have been used
    my %found = ();
    foreach my $field_val (@$fields) {
      my $field = $field_val->{'field'} || die "Missing field key in validation";
      die "Duplicate order found for $field in group order or fields" if $found{$field};
      $found{$field} = 1;
    }

    ### add any remaining fields from the order
    foreach my $field (@order) {
      next if $found{$field};
      next if $field =~ /^(group|general)\s/; 
      my $field_val = $group_val->{$field};
      die "Found a nonhashref value on field $field" if ! UNIVERSAL::isa($field_val, 'HASH');
      $field_val = { %$field_val, 'field' => $field } if ! $field_val->{'field'}; # copy the values
      push @$fields, $field_val;
    }

    ### now lets do the validation
    my $found  = 1;
    my @errors = ();
    my $hold_error;
    foreach (my $i = 0; $i <= $#$fields; $i ++) {
      my $ref = $fields->[$i];
      if (! ref($ref) && $ref eq 'OR') {
        $i += ($found) ? 2 : 1; # if found skip the OR altogether
        $found = 1; # reset
        next;
      }
      $found = 1;
      die "Missing field key during normal validation" if ! $ref->{'field'};
      my @err = $self->validate_buddy($form, $ref->{'field'}, $ref);

      ### test the error - if errors occur allow for OR - if OR fails use errors from first fail
      if (scalar @err) {
        if ($i < $#$fields && ! ref($fields->[$i + 1]) && $fields->[$i + 1] eq 'OR') {
          $hold_error = \@err;
        } else {
          push @errors, $hold_error ? @$hold_error : @err;
          $hold_error = undef;
        }
      } else {
        $hold_error = undef;
      }
    }

    ### add on errors as requested
    if ($#errors != -1) {
      push @ERRORS, $title if $title;
      push @ERRORS, @errors;
    }

    ### add on general options, and group options if errors in group occurred
    foreach my $field (@order) {
      next if $field !~ /^(general|group)\s+(\w+)$/;
      my $key = $2;
      next if $1 eq 'group' && ($#errors == -1 || $key =~ /^(field|order|title)$/);
      $EXTRA{$key} = $group_val->{$field};
    }
  }

  ### store any extra items from self
  foreach my $key (keys %$self) {
    next if $key !~ /_error$/
      && $key !~ /^(raise_error|as_hash_\w+|as_array_\w+|as_string_\w+)$/;
    $EXTRA{$key} = $self->{$key};
  }

  ### allow for checking for unused keys
  if ($EXTRA{no_extra_fields}) {
    my $which = ($EXTRA{no_extra_fields} =~ /used/i) ? 'used' : 'all';
    my $ref   = ($which eq 'all') ? $val_hash : \@USED_GROUPS;
    my $keys  = $self->get_validation_keys($ref);
    foreach my $key (sort keys %$form) {
      next if $keys->{$key};
      $self->add_error(\@ERRORS, $key, 'no_extra_fields', {}, undef);
    }
  }

  ### return what they want
  if ($#ERRORS != -1) {
    my $err_obj = $ERROR_PACKAGE->new(\@ERRORS, \%EXTRA);
    die    $err_obj if $EXTRA{raise_error};
    return $err_obj;
  } else {
    return wantarray ? () : undef;
  }
}


### allow for optional validation on groups and on individual items
sub check_conditional {
  my ($self, $form, $ifs, $N_level, $ifs_match) = @_;

  $N_level ||= 0;
  $N_level ++; # prevent too many recursive checks

  ### can pass a single hash - or an array ref of hashes
  if (! $ifs) {
    die "Need reference passed to check_conditional";
  } elsif (! ref($ifs)) {
    $ifs = [$ifs];
  } elsif (UNIVERSAL::isa($ifs,'HASH')) {
    $ifs = [$ifs];
  }

  ### run the if options here
  ### multiple items can be passed - all are required unless OR is used to separate
  my $found = 1;
  foreach (my $i = 0; $i <= $#$ifs; $i ++) {
    my $ref = $ifs->[$i];
    if (! ref $ref) {
      if ($ref eq 'OR') {
        $i += ($found) ? 2 : 1; # if found skip the OR altogether
        $found = 1; # reset
        next;
      } else {
        $ref = {field => $ref, required => 1};
      }
    }
    last if ! $found;

    ### get the field - allow for custom variables based upon a match
    my $field = $ref->{'field'} || die "Missing field key during validate_if";
    $field =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;

    my @err = $self->validate_buddy($form, $field, $ref, $N_level);
    $found = 0 if scalar @err;
  }
  return $found;
}


### this is where the main checking goes on
sub validate_buddy {
  my $self = shift;
  my ($form, $field, $field_val, $N_level, $ifs_match) = @_;
  $N_level ||= 0;
  $N_level ++; # prevent too many recursive checks
  die "Max dependency level reached $N_level" if $N_level > 10;

  my @errors = ();
  my $types  = [sort keys %$field_val];

  ### allow for not running some tests in the cgi
  if (scalar $self->filter_type('exclude_cgi',$types)) {
    return wantarray ? @errors : scalar @errors;
  }

  ### allow for field names that contain regular expressions
  if ($field =~ m/^(!\s*|)m?([^\s\w])(.*)\2([eigsmx]*)$/s) {
    my ($not,$pat,$opt) = ($1,$3,$4);
    $opt =~ tr/g//d;
    die "The e option cannot be used on validation keys on field $field" if $opt =~ /e/;
    foreach my $_field (sort keys %$form) {
      next if ($not && $_field =~ m/(?$opt:$pat)/) || (! $not && $_field !~ m/(?$opt:$pat)/);
      my @match = (undef,$1,$2,$3,$4,$5); # limit to the matches
      push @errors, $self->validate_buddy($form, $_field, $field_val, $N_level, \@match);
    }
    return wantarray ? @errors : scalar @errors;
  }

  ### allow for a few form modifiers
  if (defined $form->{$field}) {
    if (! scalar $self->filter_type('do_not_trim',$types)) { # whitespace
      $form->{$field} =~ s/^\s+//;
      $form->{$field} =~ s/\s+$//;
    }
    foreach my $pat ($self->filter_type('strip_characters',$types)) { # strip characters
      $form->{$field} =~ s/$pat//;
    }
    if (scalar $self->filter_type('to_upper_case',$types)) { # uppercase
      $form->{$field} = uc($form->{$field});
    } elsif (scalar $self->filter_type('to_lower_case',$types)) { # lowercase
      $form->{$field} = lc($form->{$field});
    }
  }

  ### only continue if a validate_if is not present or passes test
  my $needs_val = 0;
  my $n_vif = 0;
  foreach my $type ($self->filter_type('validate_if',$types)) {
    $n_vif ++;
    my $ifs = $field_val->{$type};
    my $ret = $self->check_conditional($form, $ifs, $N_level, $ifs_match);
    $needs_val ++ if $ret;
  }
  if (! $needs_val && $n_vif) {
    return wantarray ? @errors : scalar @errors;
  }
  
  ### check for simple existence
  ### optionally check only if another condition is met
  my $is_required = '';
  foreach my $type ($self->filter_type('required',$types)) {
    next if ! $field_val->{$type};
    $is_required = $type;
    last;
  }
  if (! $is_required) {
    foreach my $type ($self->filter_type('required_if',$types)) {
      my $ifs = $field_val->{$type};
      next if ! $self->check_conditional($form, $ifs, $N_level, $ifs_match);
      $is_required = $type;
      last;
    }
  }
  if ($is_required && (! defined($form->{$field}) || ! length($form->{$field}))) {
    return 1 if ! wantarray;
    $self->add_error(\@errors, $field, $is_required, $field_val, $ifs_match);
    return @errors;
  }

  ### min values check
  foreach my $type ($self->filter_type('min_values',$types)) {
    my $n   = $field_val->{$type};
    my $val = exists($form->{$field}) ? $form->{$field} : [];
    my $m   = UNIVERSAL::isa($val, 'ARRAY') ? $#$val + 1 : 1;
    if ($m < $n) {
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
      return @errors;
    }
  }

  ### max values check
  my @keys = $self->filter_type('max_values',$types);
  if ($#keys == -1) {
    push @keys, 'max_values';
    $field_val->{'max_values'} = 1;
  }
  foreach my $type (@keys) {
    my $n   = $field_val->{$type};
    my $val = exists($form->{$field}) ? $form->{$field} : [];
    my $m   = (UNIVERSAL::isa($val, 'ARRAY')) ? $#$val + 1 : 1;
    if ($m > $n) {
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
      return @errors;
    }
  }
  
  ### allow for enum types
  foreach my $type ($self->filter_type('enum',$types)) {
    my $ref = ref($field_val->{$type}) ? $field_val->{$type} : [split(/\s*\|\|\s*/,$field_val->{$type})];
    my $value = $form->{$field};
    $value = '' if ! defined $value;
    if (! grep {$_ eq $value} @$ref) {
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### field equality test
  foreach my $type ($self->filter_type('equals',$types)) {
    my $field2  = $field_val->{$type};
    my $success = 0;
    if ($field2 =~ m/^([\"\'])(.*)\1$/) {
      my $test = $2;
      if (exists($form->{$field}) && defined($form->{$field})) {
        $success = ($form->{$field} eq $test);
      }
    } elsif (exists($form->{$field2}) && defined($form->{$field2})) {
      if (exists($form->{$field}) && defined($form->{$field})) {
        $success = ($form->{$field} eq $form->{$field2});
      }
    } elsif (! exists($form->{$field}) || ! defined($form->{$field})) {
      $success = 1; # occurs if they are both undefined
    }
    if (! $success) {
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### length min check
  foreach my $type ($self->filter_type('min_len',$types)) {
    my $n = $field_val->{$type};
    if (! exists($form->{$field}) || ! defined($form->{$field}) || length($form->{$field}) < $n) {
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### length max check
  foreach my $type ($self->filter_type('max_len',$types)) {
    my $n = $field_val->{$type};
    if (exists($form->{$field}) && defined($form->{$field}) && length($form->{$field}) > $n) {
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### now do match types
  foreach my $type ($self->filter_type('match',$types)) {
    my $ref = UNIVERSAL::isa($field_val->{$type},'ARRAY') ? $field_val->{$type}
       : UNIVERSAL::isa($field_val->{$type}, 'Regexp') ? [$field_val->{$type}]
       : [split(/\s*\|\|\s*/,$field_val->{$type})];
    foreach my $rx (@$ref) {
      if (UNIVERSAL::isa($rx,'Regexp')) {
        if (! defined($form->{$field}) || $form->{$field} !~ $rx) {
          $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
        }
      } else {
        if ($rx !~ m/^(!\s*|)m?([^\s\w])(.*)\2([eigsmx]*)$/s) {
          die "Not sure how to parse that match ($rx)";
        }
        my ($not,$pat,$opt) = ($1,$3,$4);
        $opt =~ tr/g//d;
        die "The e option cannot be used on validation keys on field $field" if $opt =~ /e/;
        if ( (     $not && (  defined($form->{$field}) && $form->{$field} =~ m/(?$opt:$pat)/))
             || (! $not && (! defined($form->{$field}) || $form->{$field} !~ m/(?$opt:$pat)/))
             ) {
          return 1 if ! wantarray;
          $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
        }
      }
    }
  }

  ### allow for comparison checks
  foreach my $type ($self->filter_type('compare',$types)) {
    my $comp  = $field_val->{$type} || next;
    my $value = $form->{$field};
    my $test  = 0;
    if ($comp =~ /^\s*(>|<|[><!=]=)\s*([\d\.\-]+)\s*$/) {
      $value = 0 if ! $value;
      $value *= 1;
      if    ($1 eq '>' ) { $test = ($value >  $2) }
      elsif ($1 eq '<' ) { $test = ($value <  $2) }
      elsif ($1 eq '>=') { $test = ($value >= $2) }
      elsif ($1 eq '<=') { $test = ($value <= $2) }
      elsif ($1 eq '!=') { $test = ($value != $2) }
      elsif ($1 eq '==') { $test = ($value == $2) }

    } elsif ($comp =~ /^\s*(eq|ne|gt|ge|lt|le)\s+(.+?)\s*$/) {
      $value = '' if ! defined($value);
      my ($op, $value2) = ($1, $2);
      $value2 =~ s/^([\"\'])(.*)\1$/$2/;
      if    ($op eq 'gt') { $test = ($value gt $value2) }
      elsif ($op eq 'lt') { $test = ($value lt $value2) }
      elsif ($op eq 'ge') { $test = ($value ge $value2) }
      elsif ($op eq 'le') { $test = ($value le $value2) }
      elsif ($op eq 'ne') { $test = ($value ne $value2) }
      elsif ($op eq 'eq') { $test = ($value eq $value2) }

    } else {
      die "Not sure how to compare \"$comp\"";
    }
    if (! $test) {
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### server side sql type
  foreach my $type ($self->filter_type('sql',$types)) {
    my $db_type = $field_val->{"${type}_db_type"};
    my $dbh = ($db_type) ? $self->{dbhs}->{$db_type} : $self->{dbh};
    if (! $dbh) {
      die "Missing dbh for $type type on field $field" . ($db_type ? " and db_type $db_type" : "");
    } elsif (UNIVERSAL::isa($dbh,'CODE')) {
      $dbh = &$dbh($field, $self) || die "SQL Coderef did not return a dbh";
    }
    my $sql  = $field_val->{$type};
    my @args = ($field_val) x $sql =~ tr/?//;
    my $return = $dbh->selectrow_array($sql, {}, @args); # is this right - copied from O::FORMS
    $field_val->{"${type}_error_if"} = 1 if ! defined $field_val->{"${type}_error_if"};
    if ( (! $return && $field_val->{"${type}_error_if"})
         || ($return && ! $field_val->{"${type}_error_if"}) ) {
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### server side custom type
  foreach my $type ($self->filter_type('custom',$types)) {
    my $value = $field_val->{$type};
    $value = &$value($field, $form->{$field}, $field_val, $type) if UNIVERSAL::isa($value, 'CODE');
    next if $value;
    return 1 if ! wantarray;
    $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
  }

  ### do specific type checks
  foreach my $type ($self->filter_type('type',$types)) {
    if (! $self->check_type($form->{$field},$field_val->{'type'},$field,$form)){
      return 1 if ! wantarray;
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }            

  ### all done - time to return
  return wantarray ? @errors : scalar @errors;
}

### simple error adder abstraction
sub add_error {
  my $self = shift;
  my $errors = shift;
  push @$errors, \@_;
}

### allow for multiple validations in the same hash
### ie Match, Match1, Match2, Match234
sub filter_type {
  my $self  = shift;
  my $type  = shift;
  my $order = shift || die "Missing order array";
  my @array = ();
  foreach (@$order) {
    push @array, $_ if /^\Q$type\E_?\d*$/;
  }
  return wantarray ? @array : scalar @array;
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
    return 0 if $value =~ m/[^a-z0-9.\-\!\&]/;
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
  my $ext;

  ### they passed the right stuff already
  if (ref $val) {
    return $val;

  ### if contains a newline - treat it as a YAML string
  } elsif ($val =~ /\n/) {
    return &yaml_load($val);

  ### otherwise base it off of the file extension
  } elsif ($val =~ /\.(\w+)$/) {
    $ext = $1;
  } else {
    foreach my $_ext (@DEFAULT_EXT) {
      next if ! -e "$val.$_ext";
      $ext = $_ext;
      $val = "$val.$_ext";
    }
  }

  ### now get the file
  die "Missing validation file for $val (no extension found)" if ! $ext;
  my $handler = $EXT_HANDLERS{$ext} || die "Unknown file extension: $ext";

  return &$handler($val);
}

### returns all keys from all groups - even if group has validate_if
sub get_validation_keys {
  my $self     = shift;
  my $val_hash = shift;
  my $form     = shift; # with optional form - will only return keys in validated groups
  my %keys     = ();

  ### if a form was passed - make sure it is a hashref
  if ($form) {
    if (! ref($form)) {
      die "Invalid form hash or cgi object";
    } elsif(! UNIVERSAL::isa($form,'HASH')) {
      require CGI::Ex;
      $form = CGI::Ex->new->get_form($form);
    }
  }

  my $refs     = $self->get_validation($val_hash);
  $refs = [$refs] if ! UNIVERSAL::isa($refs,'ARRAY');
  foreach my $group_val (@$refs) {
    die "Group found that was not a hashref" if ! UNIVERSAL::isa($group_val, 'HASH');

    ### if form is passed, check to see if the group passed validation
    if ($form) {
      my $validate_if = $group_val->{'group validate_if'};
      next if $validate_if && ! $self->check_conditional($form, $validate_if);
    }

    if ($group_val->{"group fields"}) {
      die "Group fields must be an arrayref" if ! UNIVERSAL::isa($group_val->{"group fields"}, 'ARRAY');
      foreach my $field_val (@{ $group_val->{"group fields"} }) {
        next if ! ref($field_val) && $field_val eq 'OR';
        die "Field_val must be a hashref" if ! UNIVERSAL::isa($field_val, 'HASH');
        my $key = $field_val->{'field'} || die "Missing field key in field_val hashref";
        $keys{$key} = 1;
      }
    } elsif ($group_val->{"group order"}) {
      die "Group order must be an arrayref" if ! UNIVERSAL::isa($group_val->{"group order"}, 'ARRAY');
      foreach my $key (@{ $group_val->{"group order"} }) {
        my $field_val = $group_val->{$key};
        next if ! $field_val && $key eq 'OR';
        die "Field_val for $key must be a hashref" if ! UNIVERSAL::isa($field_val, 'HASH');        
        $key = $field_val->{'field'} if $field_val->{'field'};
        $keys{$key} = 1;
      }
    }

    ### get all others
    foreach my $key (keys %$group_val) {
      next if $key =~ /^(general|group)\s/;
      $keys{$key} = 1;
    }
  }

  return \%keys;
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

sub conf_handler_pl {
  my $file = shift;
  return do $file;
}

sub conf_handler_storable {
  my $file = shift;
  require Storable;
  return &Storable::retrieve($file);
}

###----------------------------------------------------------------###
### How to handle errors

package CGI::Ex::Validate::Error;

use strict;
use overload '""' => \&as_string;

sub new {
  my $class  = shift || __PACKAGE__;
  my $errors = shift;
  my $extra  = shift || {};
  die "Missing or invalid arrayref" if ! UNIVERSAL::isa($errors, 'ARRAY');
  die "Missing or invalid hashref"  if ! UNIVERSAL::isa($extra,  'HASH');
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
  $name =~ s/$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;

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
      $name2 =~ s/$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
      $return = "$name did not equal $name2.";
  
    } elsif ($type eq 'min_len') {
      my $n = $field_val->{"min_len${dig}"};
      my $char = ($n == 1) ? 'character' : 'characters';
      $return = "$name was less than $n $char.";

    } elsif ($type eq 'max_len') {
      my $n = $field_val->{"max_len${dig}"};
      my $char = ($n == 1) ? 'character' : 'characters';
      $return = "$name was more than $n $char.";

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

=head1 NAME

CGI::Ex::Validate - Yet another form validator - does good javascript too

$Id: Validate.pm,v 1.36 2003-11-21 19:38:02 pauls Exp $

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
    username => {required => 1,
                 max_len  => 30
                 field    => 'username',
                 # field is optional in this case - will use key name
                },
    email    => {required => 1,
                 max_len  => 100
                },
    email2   => {validate_if => 'email'
                 equals      => 'email'
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
    'group fields' => [
      {field    => 'username', # field is not optional in this case
       required => 1,
       max_len  => 30,
      },
      {field    => 'email',
       required => 1,
       max_len  => 100,
      }
      {field       => 'email2',
       validate_if => 'email',
       equals      => 'email',
      }
    ],
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

CGI::Ex::Validate is yet another module used for validating input.  It
aims to have all of the power of former modules, while advancing them
with more flexibility, external validation files, and identical
javascript validation.  CGI::Ex::Validate can work in a simple way
like all of the other validators do.  However, it also allows for
grouping of validation items and conditional validaion of groups or
individual items.  This is more in line with the normal validation
procedures for a website.

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
argument containt a form hash is passed, get_validation_keys will only
return the keys of groups that were validated.

  my $key_hashref = $self->get_validation_keys($val_hash);

=item C<validate>

Arguments are a form hashref or cgi object, and a validation hashref or filename.
If a CGI object is passed, CGI::Ex::get_form will be called on that object
to turn it into a hashref.  If a filename is given for the validation, get_validation
will be called on that filename.

If the form passes validation, validate will return undef.  If it fails validation, it
will return a CGI::Ex::Validate::Error object.  If the 'raise_error' general option
has been set, validate will die with a CGI::Ex::validate::Error object as the value.

  my $err_obj = $self->validate($form, $val_hash);

    # OR #

  $self->{raise_error} = 1; # raise error can also be listed in the val_hash
  eval { $self->validate($form, $val_hash) };
  if ($@) {
    my $err_obj = $@;
  }

=back

=head1 VALIDATION HASH

The validation hash may be passed as a perl a hashref or 
as a filename, or as a YAML document string.  If it is a filename,
it will be translated into a hash using the %EXT_HANDLER for the
extension on the file.  If there is no extension, it will look through the
extensions in @DEFAULT_EXT until it finds an existing file.

The validation hash may also be an arrayref of hashrefs.  In this
case, each arrayref is treated as a group and is validated separately.

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

Each of the individual field validation hashrefs should contain
the types listed in VALIDATION TYPES.

Optionally the 'group fields' or the 'group order' may contain the word
'OR' as a special keyword.  If the item preceding 'OR' fails validation
the item after 'OR' will be tested instead.  If the item preceding 'OR'
passes validation the item after 'OR' will not be tested.

  'group order' => [qw(zip OR postalcode state OR region)],

Each individual validation hashref will operate on the field contained
in the 'field' key.  This key may also be a regular expression in the
form of 'm/somepattern/'.  If a regular expression is used, all keys
matching that pattern will be validated.

=head1 VALIDATION TYPES

The following are the available validation types.  Multiple instances of
the same type may be used by adding a number to the type (ie match, match2,
match232, match_94).  Multiple instances are validated in sorted order.

=over 4

=item C<validate_if>

If validate_if is specified, the field will only be validated
if the conditions are met.  Works in JS.

  validate_if => {field => 'name', required => 1, max_len => 30}
  # Will only validate if the field "name" is present and is less than 30 chars.

  validate_if => 'name',
  # SAME as
  validate_if => {field => 'name', required => 1},

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

  {
    field       => 'm/^(\w+)_pass/',
    required_if => '$1_user',
  }
  
=item C<required>

Requires the form field to have some value.  If the field is not present,
no other checks will be run.

=item C<min_values> and C<max_values>

Allows for specifying the maximum number of form elements passed.
max_values defaults to 1 (You must explicitly set it higher
to allow more than one item by any given name).

=item C<enum>

Allows for checking whether an item matches a set of options.  In perl
the value may be passed as an arrayref.  In the conf or in perl the
value may be passed of the options joined with ||.

  {
    field => 'password_type',
    enum  => 'plaintext||crypt||md5', # OR enum => [qw(plaintext crypt md5)],
  }

=item C<equals>

Allows for comparison of two form elements.

  {
    field  => 'password',
    equals => 'password_verify',
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
May be either a boolean value predermined before calling validate, or may be
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

=item C<type>
 
Allows for more strict type checking.  Many types will be added and
will be available from javascript as well.  Currently support types
are CC.

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
a different field.  This doesn't really apply to javascript.  The errors
that are displayed in js are not shown to be linked to a specific error
and will accumulate according to the validation order.  If the key name
is a regex, any patterns will be swapped into the delegate_error value.
This option is generally only useful with the as_hash method of the
error object.

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

=over 4

=item C<do_not_trim>

By default, validate will trim leading and trailing whitespace
from submitted values.  Set do_not_trim to 1 to allow it to
not trim.

  {field => 'foo', do_not_trim => 1}

=item C<strip_characters>

Pass a pattern of characters that will be removed from the field.
Any perl regex can be passed.

  {field => 'foo', strip_characters => '\D'}

=item C<to_upper_case> and C<to_lower_case>

Do what they say they do.

=back

=head1 ERROR OBJECT

Failed validation results in an error object blessed into the class found in
$ERROR_PACKAGE - which defaults to CGI::Ex::Validate::Error.

The error object has several methods for determining what the errors were.

=over 4

=item C<as_array>

Returns an array or arrayref (depending on scalar context) of errors that
occurred in the order that they occured.  Individual groups may have a heading
and the entire validation will have a heading (the default heading can be changed
via the 'as_array_title' general option).  Each error that occured is a separate
item and are prepended with 'as_array_prefix' (which is a general option - default
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
be prepended onto the error string.  If 'as_string_footer' is set, it will be
postpended onto the error string.

  ### if this returns the following
  my $string = $err_obj->as_string;
  # $string looks like
  # "Please correct the following items:\n  error1\n  error2"

  ### then this would return the following
  my $string = $err_obj->as_string({
    as_array_prefix  => '  - ',
    as_array_title   => 'Something went wrong:',
    as_string_join   => '<br />',
    as_string_header => '<span class="error">'
    as_string_footer => '</span>'
  });
  # $string looks like
  # '<span class="error">Something went wrong:<br />  - error1<br />  - error2</span>'

=item C<as_hash>

Returns a hash or hashref (depending on scalar context) of errors that
occurred.   Each key is the field name of the form that failed validation with
'as_hash_suffix' added on as a suffix.  as_hash_suffix is available as a general option
and may also be passed in via a hashref as the only argument to as_hash.
The default value is '_error'.  The values of the hash are arrayrefs of errors
that occured to that form element.

By default as_hash will return the values of the hash as arrayrefs (a list of the errors
that occured to that key).  It is possible to also return the values as strings.
Three options are available for formatting: 'as_hash_header' which will be prepended
onto the error string, 'as_hash_footer' which will be postpended, and 'as_hash_join' which
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

If set, will be prepended onto the string when as_string is called.

=item C<'general as_string_footer'>

If set, will be prepended onto the string when as_string is called.

=item C<'general as_hash_suffix'>

Added on to key names during the call to as_hash.  Default is '_error'.

=item C<'general as_hash_join'>

By default, as_hash will return hashref values that are errors joined with
the default as_hash_join value of <br />.  It can also return values that are
arrayrefs of the errors.  This can be done by setting as_hash_join to a non-true value
(for example '')

=item C<'general as_hash_header'>

If as_hash_join has been set to a true value, as_hash_header may be set to
a string that will be prepended on to the error string.

=item C<'general as_hash_footer'>

If as_hash_join has been set to a true value, as_hash_footer may be set to
a string that will be postpended on to the error string.

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

=head1 TODO

Finish javascript.

=head1 AUTHOR

Paul Seamons

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut


