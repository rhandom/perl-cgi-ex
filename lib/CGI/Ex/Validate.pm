package CGI::Ex::Validate;

use strict;
use vars qw($VERSION $QR_FIELD_NAME $DEFAULT_RAISE_ERROR $DEFAULT_EXT $EXT_HANDLERS);
use overload '""' => \&stringify;

use Data::DumpEx;

$VERSION = (qw$Revision: 1.1 $ )[1];

### what is allowed in a field name
$QR_FIELD_NAME = qr/[\w!\@\#\$%\^&*()\-+=:,.?]+/;

$DEFAULT_EXT = 'val';

%EXT_HANDLERS = ('val' => \&val_conf_handler
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
  return "Some sort of validation error has occured (@$ref)";
}

###----------------------------------------------------------------###

sub get_val_hash {
  my $self = shift;
  my $step = shift;
  $step =~ s/\.\w+$//;
  $step .= ".val";
  my $file = ($step =~ m|^/|) ? $step : $self->partner->get_content_filename($step);
  if (! -e $file) {
    die "Error:no_validation_file - No validation file found at $file";
  }
  my ($val,$val_order) = &O::ConfUtil::conf_read($file,'sep_by_newlines');
  return wantarray ? ($val,$val_order) : $val;
}

sub val_conf_handler {
  my $self = shift;
  my $file = shift;
  require CGI::Ex::ConfUtil;
  return &CGI::Ex::ConfUtil::conf_read($file, 'values_as_array');
}

###----------------------------------------------------------------###

sub are_errors {
  my $self = shift;
  my $errs = shift || {};
  return scalar keys %$errs;
}

sub filter_order {
  my $self = shift;
  my ($val,$val_order) = @_;
  my @order = ();

  my %exists = ();

  ### look for keys that look like validation keys
  foreach (@$val_order, (sort keys %$val)) {
    next if $exists{$_};
    $exists{$_} = 1;
    next if ! $val->{$_};
    next if /^\s*GROUP\(/;
    my $ref = ref $val->{$_};
    next if ! $ref || $ref ne 'HASH';
    next if ! keys %{ $val->{$_} };
    push @order, $_;
  }

  return \@order;
}

###----------------------------------------------------------------###

sub validate {
  my $self = shift;
  my $form      = shift || $self->form;
  my $val       = shift || $self->{validation} || die "Error:missing_val_hash - Missing validation hash";
  my $val_order = shift || [];
  my $is_done   = {};
  my ($errors,$return) = (@_ && ref($_[0])) ? (shift(),0) : ({},1); # allow for passing an error hash to populate
  
  ### allow for validation to be passed multiple ways
  if (! ref $val) {
    ($val,$val_order) = $self->get_val_hash($val);
  }

  my $N_level = 0;

  ### determine if there are any group items that don't apply
  my $unused_items = $self->unused_group_items($form,$val,$val_order,$errors,$is_done,$N_level);
  my $unused_regex = $self->unused_group_regex($unused_items);

  ### loop on the errors and attempt to validate
  foreach my $key (@{ $self->filter_order($val,$val_order) }) {
    my $ok = 1;
    foreach (@$unused_regex) {
      next if $key =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s; # don't regex the regex
      my($not,$rx) = @$_;
      $ok = 0 if ($not && $key !~ $rx) || $key =~ $rx;
    }
    next if ! $ok;
    my $hash = $self->filter_form_keys($key,$form,$val);
    foreach my $field (sort keys %$hash) {
      my $val_el = $hash->{$field};
      $self->validate_buddy($field,$form,$val,$val_el,$unused_items,$errors,$is_done,$N_level);
    }
  }

  return ! $return ? undef : $errors;
}

sub validate_buddy {
  my $self = shift;
  my ($field,$form,$val,$val_el,$unused,$errors,$is_done,$N_level) = @_;
  $N_level ++; # prevent recursive required if
  die "Error:max_dependency_level - Max dependency level reached $N_level" if $N_level > 5;

  ### only do once (RequiredIf may nest calls)
  return if $is_done->{$field};
  $is_done->{$field} = 1;

  ### allow for specifying where we get our form info (Proxy)
  ### and where we send our errors (DelegateError)
  ### and which wrap area to prefix errors with
  my $target_form  = $val_el->{Proxy} || $field;
  my $target_error = $val_el->{DelegateError} || $target_form;
  my $prefix       = $val->{error_prefix};
  my $types        = [sort keys %$val_el];

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

  ### check for items that were in a group that does not apply
  return if $unused->{$field};

  my $ref = $self->filter_type('ExcludeCGI',$types);
  return if $#$ref > -1;

  ### automatically remove leading and trailing whitespace
  if (! scalar $self->filter_type('DoNotTrim',$types)) {
    $form->{$target_form} =~ s/^\s+//;
    $form->{$target_form} =~ s/\s+$//;
  }  

  ### only continue if a validateif is not present
  my $needs_val = 0;
  my $n_vi = 0;
  foreach my $type ($self->filter_type('ValidateIf',$types)) {
    $n_vi ++;
    my $ret = $self->check_conditional($val_el->{$type},$form,$val,$unused,$errors,$is_done,$N_level);
    $needs_val ++ if $ret;
  }
  return 0 if ! $needs_val && $n_vi;

  
  ### check for simple existence
  ### optionally check only if another condition is met
  my $is_required = '';
  foreach my $type ($self->filter_type('Required',$types)) {
    next if ! $val_el->{$type};
    $is_required = $type;
    last;
  }
  if (! $is_required) {
    foreach my $type ($self->filter_type('RequiredIf',$types)) {
      next if ! $self->check_conditional($val_el->{$type},$form,$val,$unused,$errors,$is_done,$N_level);
      $is_required = $type;
      last;
    }
  }
  if ($is_required && (! defined($form->{$target_form}) || ! length($form->{$target_form}))) {
#    isun $val_el,$target_form, $target_error;
    $self->add_error($target_error,$is_required,$val_el,$prefix,$errors);
    return;
  }

  ### min values check
  foreach my $type ($self->filter_type('MinValues',$types)) {
    my $n   = $val_el->{$type};
    my $ref = exists($form->{$target_form}) ? $form->{$target_form} : [];
    my $m   = ref($ref) ? scalar(@$ref) : 1;
    if ($m > $n) {
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
      return;
    }
  }

  ### max values check
  my @keys = $self->filter_type('MaxValues',$types);
  if ($#keys == -1) {
    push @keys, 'MaxValues';
    $val_el->{MaxValues} = 1;
  }
  foreach my $type (@keys) {
    my $n   = $val_el->{$type};
    my $val = exists($form->{$target_form}) ? $form->{$target_form} : [];
    my $ref = ref($val);
    my $m   = ($ref && $ref eq 'ARRAY') ? scalar(@$val) : 1;
    if ($m > $n) {
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
      return;
    }
  }
  
  ### allow for enum types
  foreach my $type ($self->filter_type('Enum',$types)) {
    my $ref = ref($val_el->{$type}) ? $val_el->{$type} : [split(/\s*\|\|\s*/,$val_el->{$type})];
    my $value = $form->{$target_form};
    $value = '' if ! defined $value;
    if (! grep {$_ eq $value} @$ref) {
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
    }
  }

  ### field equality test
  foreach my $type ($self->filter_type('Equals',$types)) {
    my $field2  = $val_el->{$type};
    my $success = 0;
    if ($field2 =~ m/^([\"\'])(.*)\1$/) {
      my $test = $2;
      $test = '' if $test eq '|'; # copy behavior from FORMS
      if (exists($form->{$target_form}) || ! defined($form->{$target_form})) {
        $success = ($form->{$target_form} eq $test);
      }
    } elsif (exists($form->{$field2}) && defined($field2)) {
      if (exists($form->{$target_form}) || ! defined($form->{$target_form})) {
        $success = ($form->{$target_form} eq $form->{$field2});
      }
    } elsif (! exists($form->{$target_form}) || ! defined($form->{$target_form})) {
      $success = 1; # occurs if they are both undefined
    }
    if (! $success) {
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
    }
  }

  ### length min check
  foreach my $type ($self->filter_type('MinLength',$types)) {
    my $n = $val_el->{$type};
    if (exists($form->{$target_form}) && defined($form->{$target_form}) && length($form->{$target_form}) < $n) {
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
    }
  }

  ### length max check
  foreach my $type ($self->filter_type('MaxLength',$types)) {
    my $n = $val_el->{$type};
    if (exists($form->{$target_form}) && defined($form->{$target_form}) && length($form->{$target_form}) > $n) {
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
    }
  }

  ### now do regex types 
  foreach my $type ($self->filter_type('RegEx',$types)) {
    my $ref = ref($val_el->{$type}) ? $val_el->{$type} : [split(/\s*\|\|\s*/,$val_el->{$type})];
    foreach my $rx (@$ref) {
      $rx =~ s/^\s*=~\s*//;
      my $not = ($rx =~ s/^\s*!~?\s*//) ? 1 : 0;
      if ($rx =~ /^\s*m?([^\w\s])(.*[^\\])\1([eisgmx]*)\s*$/s || $rx =~ /^\s*m?([^\w\s])()\1([eisgmx]*)\s*$/s) {
        my ($pat,$opt) = ($2,$3);
        $opt =~ tr/g//d;
        die "Error:invalid_regex_option - The e option cannot be used on validation regex's" if $opt =~ /e/;
        if (defined($form->{$target_form}) && length($form->{$target_form})) {
          if ( ($not && $form->{$target_form} =~ m/(?$opt:$pat)/)
               || (! $not && $form->{$target_form} !~ m/(?$opt:$pat)/)
               ) {
            $self->add_error($target_error,$type,$val_el,$prefix,$errors);
          }
        }
      } else {
        die "Error:regex_parse_error - Not sure how to parse that regex ($rx)";
      }
    }
  }

  ### allow for comparison checks
  foreach my $type ($self->filter_type('Compare',$types)) {
    my $comp  = $val_el->{$type} || next;
    my $value = $form->{$target_form};
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
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
    }
  }

  ### program side sql type
  foreach my $type ($self->filter_type('Sql',$types)) {
    my $db_type = lc($val_el->{"${type}DbType"}) || 'oracle';
    die "Error:db_type - invalid type $db_type" if $db_type !~ /^(oracle|mysql)$/;
    require O::DBI;
    my $dbh = ($self->{dbh}) ? $self->{dbh} : ($db_type eq 'oracle')
      ? O::DBI->connect_common : O::DBI->connect(@O::magics::MYSQL);
    my $s = $val_el->{$type};
    $s =~ s/\$(\w+)\b/defined($form->{$1}) ? $form->{$1} : ""/eg;
    my $return = $dbh->selectrow_array($s); # is this right - copied from O::FORMS
    $val_el->{"${type}ErrorIf"} = 1 if ! defined $val_el->{"${type}ErrorIf"};
    if ( (! $return && $val_el->{"${type}ErrorIf"}) || ($return && ! $val_el->{"${type}ErrorIf"}) ) {
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
    }
  }

  ### program side boolean type
  foreach my $type ($self->filter_type('Boolean',$types)) {
    next if $val_el->{$type};
    $self->add_error($target_error,$type,$val_el,$prefix,$errors);
  }

  ### do specific type checks
  foreach my $type ($self->filter_type('Type',$types)) {
    if (! $self->check_type($form->{$target_form},$val_el->{Type},$target_form,$form,$val)){
      $self->add_error($target_error,$type,$val_el,$prefix,$errors);
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
      my $val_el = $hash{$key} = { %{ $val->{$field} || {} } };
      foreach my $_key (qw(RequiredIf RequiredIfError ValidateIf DelegateError Proxy)) {
        next if ! $val_el->{$_key};
        $val_el->{$_key} =~ s/\$(\d+)/defined($match[$1]) ? $match[$1] : ""/ge;
      }
    }
  } else {
    $hash{$field} = $val->{$field} || {};
  }

  return \%hash;
}

### return a list of items that were not in any valid groups
sub unused_group_items {
  my $self = shift;
  my ($form,$val,$val_order,$errors,$is_done,$N_level) = @_;

  my %used   = ();
  my %unused = ();

  ### iterate on each group
  foreach my $array (@{ $self->get_groups($val,$val_order) }) {

    ### test the condition to see if the group applies
    my $cond = $array->[0] || next;
    my $val = $self->check_conditional($cond,$form,$val,{},$errors,$is_done,$N_level);

    ### put elements into the right bucket
    my $ref = ($val) ? \%used : \%unused;
    for (1 .. $#$array) {
      ### get applicable key names from the form
      foreach my $field (keys %{ $self->filter_form_keys($array->[$_],$form) }) {
        $ref->{$field} = 1;
      }
      ### save the regex if it looks like a regex
      $ref->{$array->[$_]} = 1 if $array->[$_] =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s;
    }

  }
  
  delete $unused{$_} foreach keys %used;
  return \%unused;
}

sub unused_group_regex {
  my $self   = shift;
  my $unused = shift || {};
  my @regex  = ();
  foreach my $key (keys %$unused) {
    next if $key !~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s;
    delete $unused->{$key};
    my ($not,$pat,$opt) = ($1,$3,$4);
    $opt =~ tr/g//d;
    die "Error:invalid_regex_option - The e option cannot be used on validation keys" if $opt =~ /e/;
    push @regex, [$not,qr/(?$opt:$pat)/];
  }
  return \@regex;
}

sub get_groups {
  my $self = shift;
  my ($val,$val_order) = @_;
  my @array = ();
  foreach my $field (@$val_order){
    next if $field !~ /^\s*GROUP\(/;
    my $ref = $val->{$field};
    next if ! ref $ref;
    next if ref($ref) ne 'ARRAY';
    next if ! (1 + $#$ref);
    push @array, $ref;
  }
  return \@array;
}

### used with ValidateIf and RequiredIf
### to see if a prerequisite has been met
sub check_conditional {
  my $self = shift;
  my ($field,$form,$val,$unused,$errors,$is_done,$N_level) = @_;
  my $not = ($field =~ s/^\s*!\s*//) ? 1 : 0;
  if ($field =~ /^\s*(I?EQUALS|LENGTH|VALIDATED|REGEX)\((.*)\)\s*/) {
    my ($func, $args) = (uc($1),$2);
    my $test;
    if ($func eq 'EQUALS' || $func eq 'IEQUALS' || $func eq 'REGEX') {
      if ($args =~ /^\s*($QR_FIELD_NAME)\s*,\s*([\"\'])(.*)\2\s*$/) {
        my $field2 = $1;
        my $match  = $3;
        my $val_el = $val->{$field2} || {};
        $self->validate_buddy($field2,$form,$val,$val_el,$unused,$errors,$is_done,$N_level);      
        my $_errors = $errors->{"${field2}_error"} || [];
        if ($#$_errors + 1) {
          $test = $not;
        } else {
          my $value = $form->{$field2};
          $value = "" if ! defined($value);
          if ($func eq 'EQUALS') {
            $test = ($value eq $match);
          } elsif ($func eq 'IEQUALS') {
            $test = (lc($value) eq lc($match));
          } elsif ($func eq 'REGEX') {
            if ($match =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s) {
              my ($not,$pat,$opt) = ($1,$3,$4);
              $opt =~ tr/g//d;
              die "Error: invalid_regex_option - The e option cannot be used on validation keys" if $opt =~ /e/;
              $test = ($value =~ m/(?$opt:$pat)/);
            }
          }
        }
      }
    } elsif ($func eq 'LENGTH') {
      if ($args =~ /^\s*($QR_FIELD_NAME)\s*$/) {
        my $value = $form->{$1};
        if (defined $value) {
          $test = length($value);
        }
      }
    } elsif ($func eq 'VALIDATED') {
      if ($args =~ /^\s*($QR_FIELD_NAME)\s*$/) {
        my $field2 = $1;
        if ($field2 ne $field) {
          my $val_el = $val->{$field2} || {};
          $self->validate_buddy($field2,$form,$val,$val_el,$unused,$errors,$is_done,$N_level);      
          my $_errors = $errors->{"${field2}_error"} || [];
          $test = scalar(@$_errors);
        }
      }
    }
    return $not ? ! $test : $test;

  } elsif ($form->{$field}) {
    return $not ? 0 : 1;
  }
  return $not ? 1 : 0;
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
  my $self   = shift;
  my $target = shift; # field receiving the error
  my $type   = shift;
  my $val_el = shift; # the validation hash
  my $prefix = shift; # wrap prefix
  my $errors = shift; # the errorr

  my $ref    = $errors->{"${target}_error"} ||= [];
  push @$ref, $self->get_error_text($target,$type,$val_el,$prefix);
}

sub get_error_text {
  my $self    = shift;
  my $field   = shift;
  my $type    = shift;
  my $val_el  = shift;
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
    $return = $val_el->{$key} || $self->{$key} || next;
    last;
  }

  ### set default messages
  if (! $return) {
    if ($type eq 'Required' || $type eq 'RequiredIf') {
      $return = "[|| $wrap_error '".field($field)." is required.']";
  
    } elsif ($type eq 'MinValues') {
      my $n = $val_el->{"MinValues${dig}"};
      $return = "[var pass.minvalues $n][|| $wrap_error '".field($field)." had less than $n values.']";
  
    } elsif ($type eq 'MaxValues') {
      my $n = $val_el->{"MaxValues${dig}"};
      $return = "[var pass.maxvalues $n][|| $wrap_error '".field($field)." had more than $n values.']";
      
    } elsif ($type eq 'Enum') {
      $return = "[|| $wrap_error '".field($field)." is not in the list.']";
  
    } elsif ($type eq 'Equals') {
      my $field2 = $val_el->{"Equals${dig}"};
      $return = "[|| $wrap_error '".field($field)." did not equal ".field($field2).".']";
  
    } elsif ($type eq 'MinLength') {
      my $n = $val_el->{"MinLength${dig}"};
      $return = "[var pass.minlength $n][|| $wrap_error '".field($field)." was less than $n characters.']";

    } elsif ($type eq 'MaxLength') {
      my $n = $val_el->{"MaxLength${dig}"};
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
      $return = "[|| $wrap_error '".field($field)." did not match type $val_el->{Type}.']";

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

###----------------------------------------------------------------###

1;


__END__

=head1 NAME

O::Form - Yet another form validator - does good javascript too

$Id: Validate.pm,v 1.1 2003-11-06 22:55:47 pauls Exp $

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


