package CGI::Ex::Validate;

use strict;
use overload '""' => \&stringify;
use vars qw($VERSION
            $QR_FIELD_NAME
            $DEFAULT_RAISE_ERROR
            @DEFAULT_EXT %EXT_HANDLERS);

use Data::DumpEx;
use YAML ();

$VERSION = (qw$Revision: 1.3 $ )[1];

### what is allowed in a field name
#$QR_FIELD_NAME = qr/[\w!\@\#\$%\^&*()\-+=:;\'\",.?]+/;

@DEFAULT_EXT = ('val');

%EXT_HANDLERS = ('val' => \&val_conf_handler,
                 );

###----------------------------------------------------------------###

sub new {
  my $class = shift() || __PACKAGE__;
  my $self  = (@_ && ref($_[0])) ? shift() : {@_}; 

  ### set the default to allow for override
  $self->{raise_error} = $DEFAULT_RAISE_ERROR if ! exists $self->{raise_error};

  return bless $self, $class;
}

### what are we validating
sub form {
  return shift()->{form} ||= do {
    require CGI::Ex;
    CGI::Ex->get_form; # return of the do
  };
}

###----------------------------------------------------------------###
### How to handle errors

sub fatal {
  my $self = shift;
  return undef if ! $self->{raise_error};

  ### allow for more grandios fatals
  my ($i,$pkg,$file,$line,$sub) = (1);
  ($pkg,$file,$line,$sub) = caller($i++) while ! $pkg || $pkg eq __PACKAGE__;
  $self->{caller_info} = [$pkg,$file,$line,$sub];
  die $self;
}

sub stringify {
  my $self = shift;
  return '' if ! $self->{errors} || ! @{ $self->{errors} };

  my $ref = $self->{caller_info} || [];
  return join "\n", "Some sort of validation error has occured (@$ref)", @{ $self->{errors} };
}

###----------------------------------------------------------------###

sub val_conf_handler {
  my $file = shift;
  local $/ = undef;
  open (_IN,$file) || die "Couldn't open $file: $!";
  my $text = <_IN>;
  close _IN;
  my @ret = eval { &YAML::Load($text) };
  if ($@) {
    die "Error loading info for $file: $@";
  }
  return ($#ret == 0) ? $ret[0] : \@ret;
}

sub get_validation {
  my $self = shift;
  my $file = shift;
  my $ext;

  if ($file =~ /\.(\w+)$/) {
    $ext = $1;
  } else {
    foreach my $_ext (@DEFAULT_EXT) {
      next if ! -e "$file.$_ext";
      $ext = $_ext;
    }
  }

  ### now get the file
  die "Missing validation file for $file" if ! $ext;
  my $handler = $EXT_HANDLERS{$ext} || die "Unknown file extension: $ext";

  return &$handler($file);
}

###----------------------------------------------------------------###

sub are_errors {
  my $self = shift;
  my $errs = shift || {};
  return scalar keys %$errs;
}

###----------------------------------------------------------------###

### allow for optional validation on groups and on individual items
sub check_conditional {
  my ($self, $form, $ifs, $group, $N_level) = @_;

  $N_level ||= 0;
  $N_level ++; # prevent too many recursive checks

  ### can pass a single hash - or an array ref of hashes
  if ($ifs || ! ref($ifs)) {
    die "Need reference passed to check_conditional";
  } elsif (UNIVERSAL::isa($ifs,'HASH')) {
    $ifs = [$ifs];
  }

  ### run the if options here
  ### multiple items can be passed - all are required unless OR is used to separate
  my $found = 1;
  foreach (my $i = 0; $i <= $#$ifs; $i ++) {
    my $ref = $ifs->[$i];
    if (! ref($ref) && $ref eq 'OR') {
      $i += ($found) ? 2 : 1; # if found skip the OR altogether
      $found = 1; # reset
      next;
    }
    last if ! $found;
    die "Missing field key during validate_if" if ! $ref->{'field'};
    my @err = $self->validate_buddy($form, $ref->{'field'}, $ref, $group, $N_level);
    $found = 0 if scalar @err;
  }
  return $found;
}


### the main validation routine
sub validate {
  my $self = shift;
  my $form      = shift || die "Missing form hash";
  my $val       = shift || die "Missing validation hash";
  my ($errors,$return) = (@_ && ref($_[0])) ? (shift(),0) : ({},1); # allow for passing an error hash to populate
  
  ### if a ref is not passed - assume it is a filename
  if (! ref $val) {
    $val = $self->get_validation($val);
    die "Trouble getting validation" if ! ref $val;
  }
  dex $val;

#  my $val1 = {
#    'group title' => 'User Information',
#    'group order' => [qw(username password)],
#    'group validate_if' => [{'email', 'match', 'm//'}]
#    username => {required => 1},
#    password => {required => 1},
#  };
#  my $val2 = {
#    'group title' => 'User Information',
#    'group validate_if' => [{'email', 'match', 'm//'}]
#    'group fields' => [
#      {field    => 'username',
#       required => 1},
#      {field    => 'password',
#       required => 1},
#     ],
#  };
#  my $val3 = [$val1];
#  my $val4 = [$val2];
#  my $val5 = [$val1, $val2];

  ### allow for validation passed as single group hash, single group array,
  ### or array of group hashes or group arrays
  my %DONE = ();
  my %OPTIONAL = ();
  my $group_order = (UNIVERSAL::isa($val,'HASH')) ? [$val] : $val;
  GROUP: foreach my $group (@$group_order) {
    die "Validation groups must be a hashref" if ! UNIVERSAL::isa($group,'HASH');
    my $title       = $group->{'group title'} || '';
    my $validate_if = $group->{'group validate_if'} || {};
    my $fields      = $group->{'group fields'};
    my $defaults    = $group->{'group defaults'} || {};
    my $optional    = $group->{'group optional'};

    
    ### only validate this group if it is supposed to be checked
    next if $validate_if && ! $self->check_conditional($form, $validate_if, $group);


    ### if the validation items were not passed as an arrayref
    ### look for a group order and then fail back to the keys of the group
    if (! $fields) {
      my @order  = sort grep {! /^group\s/} keys %$group;
      my @fields = ();
      my %found  = ();
      if (my $_order = $group->{'group order'}) {
        die "Validation group order must be an arrayref" if ! UNIVERSAL::isa($_order,'ARRAY');
        foreach my $field (@$_order) {
          die "Duplicate order found for $field in group order" if $found{$field};
          $found{$field} = 1;
          my $value = exists($group->{$field}) ? $group->{$field}
            : ($field eq 'OR') ? 'OR' : die "No element found in group for $field";
          push @fields, $value;
        }
      }

      ### on each hashref - make sure we have a field key - default set to group key name
      foreach my $field (@order) {
        next if $found{$field};
        $found{$field} = 1;
        my $ref = $group->{$field};
        if (! exists $ref->{'field'}) {
          my $_field = $field;
          $_field =~ s/_?\d+$//;
          $ref->{'field'} = $_field;
        }
        push @fields, $ref;
      }
      $fields = \@fields;
    } else {
      die "group fields must be passed as an arrayref" if ! UNIVERSAL::isa($fields,'ARRAY');
    }
    #dex $fields;

    ### now lets do the validation
    my $found = 1;
    my @ERROR = ();
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
      my @err = $self->validate_buddy($form, $ref->{'field'}, $ref, $group);

      ### test the error - if errors occur allow for OR - if OR fails use errors from first fail
      if (scalar @err) {
        if ($i < $#$fields && ! ref($fields->[$i + 1]) && $fields->[$i + 1] eq 'OR') {
          $hold_error = {$ref->{'field'} => \@err}; 
        } else {
          push @ERROR, $hold_error || {$ref->{'field'} => \@err};
          $hold_error = undef;
        }
      } else {
        $hold_error = undef;
      }
    }

  }

  return (! $return ? undef : $errors);
}

sub validate_buddy {
  my $self = shift;
  my ($form, $field, $field_val, $group_val, $N_level) = @_;

  my @errors  = ();

  ### only do once (RequiredIf may nest calls)
  my $is_done = {};
  return @errors if $is_done->{$field};
  $is_done->{$field} = 1;

  $N_level ||= 0;
  $N_level ++; # prevent too many recursive checks
  die "Max dependency level reached $N_level" if $N_level > 5;

  my $types = [sort keys %$field_val];

  ### supported validation types are as follows
  ##  ValidateIf
  ##  RequiredIf
  ##  Required
  ##  MinValues
  ##  MaxValues (defaults to 1)
  ##  Enum      (programming side if choices > 10)
  ##  Equals
  ##  MinLength
  ##  MaxLength
  ##  RegEx
  ##  Compare
  ##  Sql       (programming side)
  ##  Boolean   (programming side)
  ##  Type

  my $ref = $self->filter_type('exclude_cgi',$types);
  return @errors if $#$ref > -1;

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
      $form->{$field} = uc($form->{$field});
    }
  }


  ### only continue if a validate_if is not present or passes test
  my $needs_val = 0;
  my $n_vif = 0;
  foreach my $type ($self->filter_type('validate_if',$types)) {
    $n_vif ++;
    my $ifs = $field_val->{$type};
    die "Conditions for $type on field $field must be a ref" if ! ref $ifs;
    my $ret = $self->check_conditional($form, $ifs, $group_val, $N_level);
    $needs_val ++ if $ret;
  }
  return 0 if ! $needs_val && $n_vif;

  
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
      die "Conditions for $type on field $field must be a ref" if ! ref $ifs;
      next if ! $self->check_conditional($form, $ifs, $group_val, $N_level);
      $is_required = $type;
      last;
    }
  }
  if ($is_required && (! defined($form->{$field}) || ! length($form->{$field}))) {
#    isun $field_val,$field, $field;
    push @errors, [$is_required];
    return @errors;
  }

  ### min values check
  foreach my $type ($self->filter_type('MinValues',$types)) {
    my $n   = $field_val->{$type};
    my $ref = exists($form->{$field}) ? $form->{$field} : [];
    my $m   = ref($ref) ? scalar(@$ref) : 1;
    if ($m > $n) {
      $self->add_error($field,$type,$field_val,\@errors);
      return;
    }
  }

  ### max values check
  my @keys = $self->filter_type('MaxValues',$types);
  if ($#keys == -1) {
    push @keys, 'MaxValues';
    $field_val->{MaxValues} = 1;
  }
  foreach my $type (@keys) {
    my $n   = $field_val->{$type};
    my $val = exists($form->{$field}) ? $form->{$field} : [];
    my $ref = ref($val);
    my $m   = ($ref && $ref eq 'ARRAY') ? scalar(@$val) : 1;
    if ($m > $n) {
      $self->add_error($field,$type,$field_val,\@errors);
      return;
    }
  }
  
  ### allow for enum types
  foreach my $type ($self->filter_type('Enum',$types)) {
    my $ref = ref($field_val->{$type}) ? $field_val->{$type} : [split(/\s*\|\|\s*/,$field_val->{$type})];
    my $value = $form->{$field};
    $value = '' if ! defined $value;
    if (! grep {$_ eq $value} @$ref) {
      $self->add_error($field,$type,$field_val,\@errors);
    }
  }

  ### field equality test
  foreach my $type ($self->filter_type('Equals',$types)) {
    my $field2  = $field_val->{$type};
    my $success = 0;
    if ($field2 =~ m/^([\"\'])(.*)\1$/) {
      my $test = $2;
      $test = '' if $test eq '|'; # copy behavior from FORMS
      if (exists($form->{$field}) || ! defined($form->{$field})) {
        $success = ($form->{$field} eq $test);
      }
    } elsif (exists($form->{$field2}) && defined($field2)) {
      if (exists($form->{$field}) || ! defined($form->{$field})) {
        $success = ($form->{$field} eq $form->{$field2});
      }
    } elsif (! exists($form->{$field}) || ! defined($form->{$field})) {
      $success = 1; # occurs if they are both undefined
    }
    if (! $success) {
      $self->add_error($field,$type,$field_val,\@errors);
    }
  }

  ### length min check
  foreach my $type ($self->filter_type('MinLength',$types)) {
    my $n = $field_val->{$type};
    if (exists($form->{$field}) && defined($form->{$field}) && length($form->{$field}) < $n) {
      $self->add_error($field,$type,$field_val,\@errors);
    }
  }

  ### length max check
  foreach my $type ($self->filter_type('MaxLength',$types)) {
    my $n = $field_val->{$type};
    if (exists($form->{$field}) && defined($form->{$field}) && length($form->{$field}) > $n) {
      $self->add_error($field,$type,$field_val,\@errors);
    }
  }

  ### now do regex types 
  foreach my $type ($self->filter_type('RegEx',$types)) {
    my $ref = ref($field_val->{$type}) ? $field_val->{$type} : [split(/\s*\|\|\s*/,$field_val->{$type})];
    foreach my $rx (@$ref) {
      $rx =~ s/^\s*=~\s*//;
      my $not = ($rx =~ s/^\s*!~?\s*//) ? 1 : 0;
      if ($rx =~ /^\s*m?([^\w\s])(.*[^\\])\1([eisgmx]*)\s*$/s || $rx =~ /^\s*m?([^\w\s])()\1([eisgmx]*)\s*$/s) {
        my ($pat,$opt) = ($2,$3);
        $opt =~ tr/g//d;
        die "Error:invalid_regex_option - The e option cannot be used on validation regex's" if $opt =~ /e/;
        if (defined($form->{$field}) && length($form->{$field})) {
          if ( ($not && $form->{$field} =~ m/(?$opt:$pat)/)
               || (! $not && $form->{$field} !~ m/(?$opt:$pat)/)
               ) {
            $self->add_error($field,$type,$field_val,\@errors);
          }
        }
      } else {
        die "Error:regex_parse_error - Not sure how to parse that regex ($rx)";
      }
    }
  }

  ### allow for comparison checks
  foreach my $type ($self->filter_type('Compare',$types)) {
    my $comp  = $field_val->{$type} || next;
    my $value = $form->{$field};
    my $test  = 0;
    if ($comp =~ /^\s*(>|<|[><!=]=)\s*([\d\.\-]+)\s*$/) {
      $value = 0 if ! $value;
      if    ($1 eq '>' ) { $test = ($value >  $2) }
      elsif ($1 eq '<' ) { $test = ($value <  $2) }
      elsif ($1 eq '>=') { $test = ($value >= $2) }
      elsif ($1 eq '<=') { $test = ($value <= $2) }
      elsif ($1 eq '!=') { $test = ($value != $2) }
      elsif ($1 eq '==') { $test = ($value == $2) }

    } elsif ($comp =~ /^\s*(eq|ne|gt|ge|lt|le)\s+(.+?)\s*$/) {
      $value = '' if ! $value && ! length $value;
      if    ($1 eq 'gt') { $test = ($value gt $2) }
      elsif ($1 eq 'lt') { $test = ($value lt $2) }
      elsif ($1 eq 'ge') { $test = ($value ge $2) }
      elsif ($1 eq 'le') { $test = ($value le $2) }
      elsif ($1 eq 'ne') { $test = ($value ne $2) }
      elsif ($1 eq 'eq') { $test = ($value eq $2) }

    } else {
      die "Error:parse_compare - Not sure how to compare \"$comp\"";
    }
    if (! $test) {
      $self->add_error($field,$type,$field_val,\@errors);
    }
  }

  ### program side sql type
  foreach my $type ($self->filter_type('Sql',$types)) {
    my $db_type = lc($field_val->{"${type}DbType"}) || 'oracle';
    die "Error:db_type - invalid type $db_type" if $db_type !~ /^(oracle|mysql)$/;
    require O::DBI;
    my $dbh = ($self->{dbh}) ? $self->{dbh} : ($db_type eq 'oracle')
      ? O::DBI->connect_common : O::DBI->connect(@O::magics::MYSQL);
    my $s = $field_val->{$type};
    $s =~ s/\$(\w+)\b/defined($form->{$1}) ? $form->{$1} : ""/eg;
    my $return = $dbh->selectrow_array($s); # is this right - copied from O::FORMS
    $field_val->{"${type}ErrorIf"} = 1 if ! defined $field_val->{"${type}ErrorIf"};
    if ( (! $return && $field_val->{"${type}ErrorIf"}) || ($return && ! $field_val->{"${type}ErrorIf"}) ) {
      $self->add_error($field,$type,$field_val,\@errors);
    }
  }

  ### program side boolean type
  foreach my $type ($self->filter_type('Boolean',$types)) {
    next if $field_val->{$type};
    $self->add_error($field,$type,$field_val,\@errors);
  }

  ### do specific type checks
  foreach my $type ($self->filter_type('Type',$types)) {
    if (! $self->check_type($form->{$field},$field_val->{Type},$field,$form,$group_val)){
      $self->add_error($field,$type,$field_val,\@errors);
    }
  }            

  ### all done - time to return
  return;
}

### allow for multiple validations in the same hash
### ie RegEx, RegEx1, RegEx2, RegEx234
sub filter_type {
  my $self  = shift;
  my $type  = shift;
  my $order = shift || die "Error:missing_order_array";
  my @array = ();
  foreach (@$order) {
    push @array, $_ if /^\Q$type\E\d*$/;
  }
  return wantarray ? @array : \@array;
}

### allow for validation on multiple form keys at once
sub filter_form_keys {
  my $self  = shift;
  my $field = shift;
  my $form  = shift;
  my $val   = shift || {};

  ### hash will contain the keys of the form that matched
  ### the value will be the validation sub hash for that form element
  my %hash = ();

  ### let the key name match a regex
  if ($field =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s) {
    my ($not,$pat,$opt) = ($1,$3,$4);
    $opt =~ tr/g//d;
    die "Error:invalid_regex_option - The e option cannot be used on validation keys" if $opt =~ /e/;

    foreach my $key (sort keys %$form) {
      next if ($not && $key =~ m/(?$opt:$pat)/) || $key !~ m/(?$opt:$pat)/;
      my @match = (undef,$1,$2,$3,$4,$5); # limit to the matches
      my $field_val = $hash{$key} = { %{ $val->{$field} || {} } };
      foreach my $_key (qw(RequiredIf RequiredIfError ValidateIf DelegateError Proxy)) {
        next if ! $field_val->{$_key};
        $field_val->{$_key} =~ s/\$(\d+)/defined($match[$1]) ? $match[$1] : ""/ge;
      }
    }
  } else {
    $hash{$field} = $val->{$field} || {};
  }

  return \%hash;
}

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

# this works but probably wont be needed
#  ### do general email address that follow rfc
#  } elsif ($type eq 'EMAIL_GENERAL') {
#    return 0 if ! $value;
#    my($local_p,$dom) = ($value =~ /^(\.+)\@(\.+?)$/) ? ($1,$2) : return 0;
#
#    return 0 if ! $self->check_type($dom,'DOMAIN') && ! $self->check_type($dom,'IP');
#
#    if ($local_p =~ /^\"(.+)\"$/) { # allow for quoted string emails
#      my $str = $1;
#      return 0 if $str =~ /[\"\n\t]/;
#    } else {
#      return 0 if $str !~ /^ [\w\!\&\-\=\*]+ ( \. [\w\!\&\-\=\*]+ )* $/x;
#    }

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

sub add_error {
  my $self      = shift;
  my $field     = shift; # field receiving the error
  my $type      = shift;
  my $field_val = shift; # the validation hash
  my $group_val = shift; # wrap prefix
  my $errors    = shift; # the errorr

  push @$errors, $self->get_error_text($field,$type,$field_val);
}

sub get_error_text {
  my $self    = shift;
  my $field   = shift;
  my $type    = shift;
  my $field_val  = shift;
  my $prefix  = shift || $self->{error_prefix} || 'text';
  my $dig     = ($type =~ s/(\d+)$//) ? $1 : '';
  my $type_lc = lc($type);

  ### type can look like "Required" or "Required2" or "Required100023"
  ### allow for fallback from Required100023 through Required

  ### setup where to look for the error message
  my @error_keys = ("${type}Error");
  unshift @error_keys, "${type}${dig}Error" if length($dig);
  my $wrap_error;
  if ($field =~ /\W/) {
    $wrap_error = '';
  } else {
    $wrap_error = "$prefix.${field}_${type_lc}_error $prefix.${field}_error";
    if (length($dig)) {
      $wrap_error = "$prefix.${field}_${type_lc}${dig}_error " . $wrap_error;
    }
  }

  ### look in the passed hash or self first
  my $return;
  foreach my $key (@error_keys){
    $return = $field_val->{$key} || $self->{$key} || next;
    last;
  }

  ### set default messages
  if (! $return) {
    if ($type eq 'Required' || $type eq 'RequiredIf') {
      $return = "[|| $wrap_error '".field($field)." is required.']";
  
    } elsif ($type eq 'MinValues') {
      my $n = $field_val->{"MinValues${dig}"};
      $return = "[var pass.minvalues $n][|| $wrap_error '".field($field)." had less than $n values.']";
  
    } elsif ($type eq 'MaxValues') {
      my $n = $field_val->{"MaxValues${dig}"};
      $return = "[var pass.maxvalues $n][|| $wrap_error '".field($field)." had more than $n values.']";
      
    } elsif ($type eq 'Enum') {
      $return = "[|| $wrap_error '".field($field)." is not in the list.']";
  
    } elsif ($type eq 'Equals') {
      my $field2 = $field_val->{"Equals${dig}"};
      $return = "[|| $wrap_error '".field($field)." did not equal ".field($field2).".']";
  
    } elsif ($type eq 'MinLength') {
      my $n = $field_val->{"MinLength${dig}"};
      $return = "[var pass.minlength $n][|| $wrap_error '".field($field)." was less than $n characters.']";

    } elsif ($type eq 'MaxLength') {
      my $n = $field_val->{"MaxLength${dig}"};
      $return = "[var pass.maxlength $n][|| $wrap_error '".field($field)." was more than $n characters.']";

    } elsif ($type eq 'RegEx') {
      $return = "[|| $wrap_error '".field($field)." did not match regex.']";

    } elsif ($type eq 'Compare') {
      $return = "[|| $wrap_error '".field($field)." did not fit comparison.']";
  
    } elsif ($type eq 'Sql') {
      $return = "[|| $wrap_error '".field($field)." did not match sql query.']";
      
    } elsif ($type eq 'Boolean') {
      $return = "[|| $wrap_error '".field($field)." did not match boolean.']";
      
    } elsif ($type eq 'Type') {
      $return = "[|| $wrap_error '".field($field)." did not match type $field_val->{Type}.']";

    }
  }

  die "Error:missing_error - Missing error on field $field for type $type$dig" if ! $return;
  return $return;

}

sub field {
  my $field = shift;
  return 'Field' if $field =~ /\W/;
  $field =~ tr/_/ /;
  $field =~ s/\b(\w)/\u$1/g;
  return $field;
}

###----------------------------------------------------------------###


sub onsubmit_js {
  return "ONSUBMIT=\"if(document.validate_form){if(document.validate_form(this,arguments)) return true; else return false;}\"";
}

sub generate_js {
  my $self = shift;
  my $val         = shift || $self->{validation} || die "Error:missing_val_hash - Missing validation hash";
  my $val_order   = shift || [];
  
  ### allow for validation to be passed multiple ways
  if (! ref $val) {
    ($val,$val_order) = $self->get_val_hash($val);
  }

  ### determin the prefix to wrap errors with
  my $prefix = $val->{error_prefix} || $self->{error_prefix} || 'text';
  my $order  = $self->filter_order($val,$val_order);

  ### loop through the keys to setup the javascript hashes
  my $info = "\nvar VT;\nvar VF;\n";
  foreach my $field (@$order) {
    if ($val->{$field}->{'ExcludeJS'}) {
      next; # allow no validation for fields that request it
    }

    ### prepare a javascript array (hash) by the same name -- allowing for regex names
    my $copy = $field;
    if ($copy =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s) {
      my ($not,$pat,$opt) = ($1,$3,$4);
      $copy = $self->touchup_regex($not,$pat,$opt);
    }
    $copy = $self->js_escape($copy);
    $info .= "VF = \"$copy\";\nVT = VALIDATE[VF] = new Array();\n";
    
    ### populate the array with key value pairs
    foreach my $check (sort keys %{ $val->{$field} }) {
      next if $check !~ /^(ValidateIf|Required|RequiredIf|MinLength|MaxLength|Equals|RegEx|Type|Enum|Compare|Proxy|DoNotTrim)\d*$/;
      my $value = $val->{$field}->{$check};
      $value = '' if ! defined $value;

      ### if an enum has more than 20 values - skip it
      if ($check =~ /^Enum\d*$/) {
        my $ref = ref($value) ? $value : [split(/\s*\|\|\s*/,$value)];
        next if @$ref > 20;
        $value = join("||",@$ref);

      ### clean up regex checks
      } elsif ($check =~ /^RegEx\d*$/) {
        my $ref = ref($value) ? $value : [split(/\s*\|\|\s*/,$value)];
        foreach my $rx (@$ref) {
          $rx =~ s/^\s*=~\s*//;
          my $not = ($rx =~ s/^\s*!~?\s*//) ? '!' : '';
          if ($rx =~ /^\s*m?([^\w\s])(.*[^\\])\1([eisgmx]*)\s*$/s || $rx =~ /^\s*m?([^\w\s])()\1([eisgmx]*)\s*$/s) {
            my ($pat,$opt) = ($2,$3);
            $rx = $self->touchup_regex($not,$pat,$opt);
            
          } else {
            die "Error:parse_regex - Not sure how to parse that regex ($rx)";
          }
        }
        $value = join("||",@$ref);

      ### clean up Compare checks
      } elsif ($check =~ /^Compare\d*$/) {
        if ($value =~ /^\s*(>|<|[><!=]=)\s*([\d\.\-]+)\s*$/) {
          $value = "$1 $2";
        } elsif ($value =~ /^\s*(eq|ne|gt|ge|lt|le)\s+(.+?)\s*$/) {
          $value = "$1 $2";
        } else {
          die "Error:parse_compare - Not sure how to compare \"$value\"";
        }

      ### clean up some values to make the javascript easier - just remove whitespace
      } elsif ($check =~ /If$/) {
        $value =~ s/^\s*(!?)\s*/$1/;
        $value =~ s/\s+$//;
        $value =~ s/(LENGTH|VALIDATED)\s*\(\s*($QR_FIELD_NAME)\s*/$1\($2/x;
        $value =~ s/(I?EQUALS|REGEX)  \s*\(\s*($QR_FIELD_NAME)\s*,\s*/$1\($2,/x;
        $value =~ s/\s*\)/\)/;
        if ($value =~ m/^(!?REGEX\($QR_FIELD_NAME,)[\"\'](.*)[\'\"]\)$/s) {
          my ($begin,$match) = ($1,$2);
          $match =~ m/^(!?)m(\W)(.*)\2([eigsmx]*)$/ || die "Error:parse_regex - Don't know how to parse regex ($match)"; 
          my ($not,$pat,$opt) = ($1,$3,$4);
          $value = "$begin'" .$self->touchup_regex($not,$pat,$opt) ."')";
        }
      }

      ### javascript escape some stuff
      $value = $self->js_escape($value);
      $info .= "VT.${check} = \"$value\"\n";

      ### add the error
      next if $check =~ /^(ValidateIf|Proxy|DoNotTrim)$/;
      my $err = $self->js_escape($self->get_error_text($field,$check,$val->{$field},$prefix) || '');
      $info .= "VT.${check}Error = \"- $err\"\n";
    }
  }

  ### add on group info
  $info .= "VT = VALIDATE_GROUPS = new Array();\n";
  my $n = 0;
  foreach my $ref (@{ $self->get_groups($val,$val_order) }) {
    $info .= "VT[$n] = new Array(\n";
    my @copy = @$ref;
    foreach my $copy (@copy) {
      if ($copy =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s) {
        my ($not,$pat,$opt) = ($1,$3,$4);
        $copy = $self->touchup_regex($not,$pat,$opt);
      }
      $copy = $self->js_escape($copy);
      $info .= "\"$copy\",\n";
    }
    $info =~ s/\s*,\s*$/\n);\n/;
    $n ++;
  }

  ### get the validation order ready
  my $order_tx = "\n";
  foreach (@$order) {
    my $copy = $_;
    if ($copy =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s) {
      my ($not,$pat,$opt) = ($1,$3,$4);
      $copy = $self->touchup_regex($not,$pat,$opt);
    }
    $copy = $self->js_escape($copy);
    $order_tx .= "\"$copy\",\n";
  }
  $order_tx =~ s/\s*,\s*$/\n/;


  ### return the js chunk
  return qq{
    <SCRIPT language=\"JavaScript\">
    <!--
    var VALIDATE_HEADING = "[|| $prefix.js_error_heading 'Please correct the following:']";
    var VALIDATE_ORDER = new Array($order_tx);
    var VALIDATE = new Array();
    $info
    //-->
    </SCRIPT>      
    <SCRIPT language=\"JavaScript\" src=/fs_img/js/validation.js></SCRIPT>
  };
}

sub touchup_regex {
  my $self = shift;
  my($not,$pat,$opt) = @_;

  $opt =~ tr/g//d;
  die "Error:invalid_regex_option - The e option cannot be used on validation keys" if $opt =~ /e/;
  $pat =~ s{   # make sure sets inside of brackets are expanded properly
    (?<!\\)\[  # unescaped [
      (.+?)      # stuff in the middle
      (?<!\\)\]  # unescaped ]
    }{
      my $set = $1;
      $set =~ s/\\w/a-zA-Z0-9_/g;
      $set =~ s/\\d/0-9/g;
      "[$set]"; # return of the swap
    }exg;
  $pat =~ s/\\w/[a-zA-Z0-9_]/g;
  $pat =~ s/\\d/[0-9]/g;
  $pat =~ s/(?<!\\)\\(\W)/\\\\$1/g;
  return "${not}m/$pat/$opt";
}

sub js_escape {
  my $self = shift;
  my $copy = shift;
  $copy =~ s/\"/\\\"/g;
  $copy =~ s{(</?scr|</?htm|<!-|->)}{$1"+"}ig;
  $copy =~ s/\n/\"\n+\"/g;
  return $copy;
}


sub AUTOLOAD {

}

###----------------------------------------------------------------###

1;


__END__

=head1 NAME

O::Form - Yet another form validator - does good javascript too

$Id: Validate.pm,v 1.3 2003-11-11 22:40:28 pauls Exp $

=head1 SYNOPSIS

  use O::Form;

  my $partner = O::Partner->new($ENV{HTTP_HOST});
  my $form = &O::CGI::GET_FORM();
  my $step = 'somestep'; # same as passed to partner->print

  # simple way ###
  
  my $fob  = O::Form->new;
  my $errors = $fob->validate($form,'somestep'); # returns hash of errors

  if ($fob->are_errors($errors)) {
    $partner->print($step,$form,$errors, {js => $fob->generate_js($step)});
  }

  ### extended way ###

  my $fob  = O::Form->new({
    partner => $partner, # reuse the same partner object
  });

  my ($val_hash,$val_order) = $fob->get_val_hash($step);
  my $filtered_order = $fob->filter_order($val_hash,$val_order); # redundant

  my $errors = $fob->validate($form,$val_hash);
  OR
  my $errors = $fob->validate($form,$val_hash,$val_order);
  OR
  my $errors = {};
  $fob->validate($form,$val_hash,$val_order,$errors); # errors added to hash

  if ($fob->are_errors($errors)) {
    $partner->print($step,$form,$errors, {
      js => $fob->generate_js($step),
      OR
      js => $fob->generate_js($step,$val_hash,$val_order),
      js_onsubmit => $fob->onsubmit_js,
    });
  }



  ### instead of passing the js items you can do the following in the html

  <html>
  <form [jsgen onsubmit]>
  </form>
  [jsgen] OR [jsgen "somecgi/somestep"]
  </html>

=head1 DESCRIPTION

O::Form is yet another module used for validating input.  It
aims to have all of the power of former modules, while advancing them
with more flexibility, external validation files, and identical
javascript validation.  Other functions from O::FORMS and 
O::Validate and O::Check may eventually make their way in here
so long as doing so carries an optimization.



=head1 VAL HASH

The validation hash may be passed as a perl a hashref or 
as a .val filename.  The .val file is intended to be parallel
to the .htm file of the content (ie signup/main.val is used by
default when signup/main.htm is printed).  The .val file is read
in using O::ConfUtil::conf_read with the option of sep_by_newlines
being passed (See L<O::ConfUtil>).  The key names represent
fields in the form to be validated.  Some keys need not exist as
long as the key is a regex or a Validation type of Proxy has been
added.  If the key name is regex, the validation will apply to any
key which matches the regex.  The regex may be negated.  If a
validation type of Proxy exists the form will validate on the field
listed in Proxy.

# SAMPLE CONF FILE

array:GROUP(update)
  update
  m/^id[12]_/

array:GROUP(new)
  add
  m/^new_/

hash:new_user
  Required       1
  RequiredError  New username is required
  MaxLength      30
  MaxLengthError New username must be less than 30 characters
  MinLength      3
  MinLengthError New username must be more than 3 characters

hash:m/^(id[12]_\d+)_user$/
  Required 1

=head1 METHODS

=over 4

=item C<new>

Constructor.  Takes a hash or hashref as arguments.  Optional keys
are form, partner, validation and validation errors.  All arguments
are optional and should probably not be used - in favor of passing
them through other methods.

  $fob = O::Form->new({
    partner => O::Partner->new($ENV{HTTP_HOST}),
    form    => &O:::CGI::GET_FORM(),
    validation => $val_hash,
    RequiredError => '[text.required_error]',
    EnumError     => '[text.enum_error]',
  });

=item C<validate>

Primary method - used to do the actual validation.  Arguments are
the form hashref (if none is passed it will default to $self->{form}),
the validation hashref or validation step (if none is passed it will default to
$self->{validation}), the validation order arrayref (the order in
which to validate the validation keys.  An option last argument is an
error hashref.  If this is passed then the errors will be added to it
rather than returned in a hashref.

  $errors = $fob->validate($form, $valhash);

  $errors = $fob->validate($form, $valhash, $valorder);

  $errors = $fob->validate($form, "cgi/step");

  my $errors = {};
  $fob->validate($form, "cgi/step", [], $errors);

=item C<are_errors>

Allows for determining if there are any error in the hashref returned 
from validate.  At the moment, only keys that have errors will
be in the hash so it is possible to do if (keys %$errors) but it is 
better to use the method anyway.

  if ($fob->are_errors($errors)) { die "Invalid" }

=item C<generate_js>

This method will return the javascript that should be placed in the 
code to do the validation.  It will take the validation hash or step,
and the validation order.
Normally this method is not necessary, as there is a wrap function that
will bring the code in by merely placing "[jsgen]" or "[jsgen 'cgi/step']" in
the html.

  $js = $fob->generate_js($valhash);
  $js = $fob->generate_js($valhash,$valorder);
  $js = $fob->generate_js('cgi/step');

=item C<onsubmit_js>

Will return the onsubmit code to activate the validation script.
Normally this method is not necessary, as there is a wrap function that
will bring the code in by merely placing "[jsgen]" or "[jsgen 'cgi/step']" in
the html.

  $onsubmit = $fob->onsubmit_js;

=item C<get_val_hash>

Takes the validation step as an arugment.  Will return the validation hash in scalar
context and the validation hash and order in an array context.  It will use the
partner object found in $self->{partner}.

  $valhash = $fob->get_val_hash('cgi/step');
  ($valhash,$val_order) = $fob->get_val_hash('cgi/step');

=item C<filter_order>

Removes keys that don't pertain to validation.

  $order = $fob->filter_order($val,$val_order);

=back

=head1 VALIDATION TYPES

The following are the available validation types.  Multiple instances of
the same type may be used by adding a number to the type (ie RegEx, RegEx2,
RegEx232).

=over 4

=item C<ValidateIf>

If ValidateIf is specified, the field will only be validated
if the conditions are met.  Works in JS.

  ValidateIf country 
  # Will only validate if country is present and has a true value

  ValidateIf LENGTH(country)
  # Will only validate if country is present and has any value

  ValidateIf EQUALS(country,"US")
  # only if country's value is equal to US

  ValidateIf !EQUALS(country,"US");
  # if country doesn't equal US

  ValidateIf REGEX(password,'m/^md5\([a-z0-9]{20}\)$/')
  # if password looks like an md5

  hash:m/^(\w+)_pass/
    ValidateIf $1_user
    Required 1
  # will validate only if user was present.


=item C<RequireIf>

Requires the form field if the condition is satisfied.  The conditions
available are the same as for ValidateIf.  This is somewhat the same
as saying:

  ValidateIf some_condition
  Required 1

  RequiredIf some_condition

  hash:m/^(\w+)_pass/
    RequiredIf $1_user
  
=item C<Required>

Requires the form field to have some value.  If the field is not present,
no other checks will be run.

=item C<MinValues> and C<MaxValues>

Allows for specifying the maximum number of form elements passed.
MaxValues defaults to 1.

=item C<Enum>

Allows for checking whether an item matches a set of options.  In perl
the value may be passed as an arrayref.  In the conf or in perl the
value may be passed of the options joined with ||.  Values with over
20 options will not be validated in javascript.
  
  hash:password_type
    Enum plaintext||crypt||md5

=item C<Equals>

Allows for comparison of two form elements.

  hash:password
    Equals password_verify

=item C<MinLength and MaxLength>

Allows for check on the length of fields

  hash:site
    MinLength 4
    MaxLength 100

=item C<RegEx>

Allows for regular expression comparison.  Multiple RegEx may
be concatenated with ||.  Available in JS.

  hash:my_ip
    RegEx /^\d{1,3}(\.\d{1,3})3$/
    RegEx2 !/^0\./+||+!/^192\./

=item C<Compare>

Allows for custom comparisons.  Available types are
>, <, >=, <=, !=, ==, gt, lt, ge, le, ne, and eq.  Comparisons
also work in the JS.

  hash:my_number
    RegEx /^\d+$/
    Compare1 >100
    Compare2 <255
    Compare3 !=+150

=item C<Sql>

SQL query based - not available in JS.

=item C<Boolean>

Boolean value - not available in JS.

=item C<Type>
 
Allows for more strict type checking.  Many types will be added and
will be available from javascript as well.  Currently support types
are CC.

  hash:credit_card
    Type: CC

=back

=head1 GROUP TYPES

=over 4

=item C<GROUP(whatever)>

Specfying a group allows for blocks of items to be validated as
long as a condition is met.  This is a shortcut for having to put
ValidateIf on each item, and also allows for different sets of data
to be validated depending on the conditions met.  If an item is in
a group that doesn't match the requested condition and is not in any
other group that does match a requested condition, the item will not be
validated.

array:GROUP(bill)
  EQUALS(pay_option,'bill')
  bill_cc
  bill_exp_year
  bill_exp_mon
  bill_name

A group item may now also be a regex.  The previouse example could
be written as follows:

array:GROUP(bill)
  EQUALS(pay_optoin,'bill')
  m/^bill_/

=back

=head1 SPECIAL TYPES

=over 4

=item C<DelegateError>

This option allows for any errors generated on a field to delegate to
a different field.  This doesn't really apply to javascript.  The errors
that are displayed in js are not shown to be linked to a specific error
and will accumulate according to the validation order.  If the key name
is a regex, any patterns will be swapped into the DelegateError value.

hash:zip
  RegEx m/^\d{5}/

hash:zip_plus4
  DelegateError zip
  RegEx m/^\d{4}/

hash:m/^(id_[\d+])_user$/
  DelegateError $1

=item C<Proxy>

This option allows for one field to use another field from the form.
Errors will delegate to the proxy field unless DelegateError
is specified to another field.  The value will be swapped using
patterns matched from a regex field name.

hash:domain
  ValidateIf EQUALS(type,'tld')
  Required 0
  RegEx m/^\w+\.\w+$/

hash:domain2
  Proxy domain
  ValidateIf EQUALS(type,'sub')
  Required 1
  RegEx m/^\w+(\.\w+)+$/

hash:m/(id_\d+)_pass/
  Proxy $1_pass2
  
=item C<ExcludeJS>

This allows the cgi to do checking while keeping the checks from
being run in JavaScript

hash:cgi_var
  Required 1
  ExcludeJS 1

=item C<ExcludeCGI>

This allows the js to do checking while keeping the checks from
being run in the cgi

hash:js_var
  Required 1
  ExcludeCGI 1

=back

=cut


