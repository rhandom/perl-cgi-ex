package CGI::Ex::Validate;

use strict;
use vars qw($VERSION
            $DEFAULT_RAISE_ERROR
            @DEFAULT_EXT %EXT_HANDLERS);

use Data::DumpEx;
use YAML ();

$VERSION = (qw$Revision: 1.8 $ )[1];

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

sub val_conf_handler {
  my $file = shift;
  local $/ = undef;
  open (_IN,$file) || die "Couldn't open $file: $!";
  my $text = <_IN>;
  close _IN;
  return &yaml_load($text);
}

sub yaml_load {
  my $text = shift;
  my @ret = eval { &YAML::Load($text) };
  if ($@) {
    die "$@";
  }
  return ($#ret == 0) ? $ret[0] : \@ret;
}

sub get_validation {
  my $self = shift;
  my $file = shift;
  my $ext;

  ### if contains a newline - treat it as a YAML file
  if ($file =~ /\n/) {
    return &yaml_load($file);

  ### otherwise base it off of the file extension
  } elsif ($file =~ /\.(\w+)$/) {
    $ext = $1;
  } else {
    foreach my $_ext (@DEFAULT_EXT) {
      next if ! -e "$file.$_ext";
      $ext = $_ext;
    }
  }

  ### now get the file
  die "Missing validation file for $file (no extension found)" if ! $ext;
  my $handler = $EXT_HANDLERS{$ext} || die "Unknown file extension: $ext";

  return &$handler($file);
}

###----------------------------------------------------------------###

### the main validation routine
sub validate {
  my $self = shift;
  my $form      = shift || die "Missing form hash";
  my $val       = shift || die "Missing validation hash";
  
  ### turn the form into a form if it is really a CGI object
  if (! ref($form)) {
    die "Invalid form hash or cgi object";
  } elsif(! UNIVERSAL::isa($form,'HASH')) {
    require CGI::Ex;
    $form = CGI::Ex->new->get_form($form);
  }

  ### if a ref is not passed - assume it is a filename
  if (! ref $val) {
    $val = $self->get_validation($val);
    die "Trouble getting validation" if ! ref $val;
  }
#  dex $val;


  ### allow for validation passed as single group hash, single group array,
  ### or array of group hashes or group arrays
  my @ERRORS = ();
  my $group_order = (UNIVERSAL::isa($val,'HASH')) ? [$val] : $val;
  foreach my $group (@$group_order) {
    die "Validation groups must be a hashref" if ! UNIVERSAL::isa($group,'HASH');
    my $title       = $group->{'group title'};
    my $validate_if = $group->{'group validate_if'};
    my $fields      = $group->{'group fields'};
    my $defaults    = $group->{'group defaults'} || {};
    my $optional    = $group->{'group optional'};

    
    ### only validate this group if it is supposed to be checked
    next if $validate_if && ! $self->check_conditional($form, $validate_if);


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
  }

#  dex \@ERRORS;

  ### add on the top level title
  if ($#ERRORS != -1) {
    my $title = exists($self->{general_title}) ? $self->{general_title} : "Please correct the following items:";
    unshift(@ERRORS, $title) if $title;
  }

  ### return what they want
  if ($#ERRORS != -1) {
    push @ERRORS, $self; # save for posterity
    my $pkg = __PACKAGE__."::Error"; # not overridable
    if ($self->{raise_error}) {
      die $pkg->new(\@ERRORS); # die with error object
    } elsif (wantarray) {
      return @ERRORS; # give back the bulk info
    } else {
      return $pkg->new(\@ERRORS); # return error object
    }
  } elsif (wantarray) {
    return ();
  } else {
    return undef;
  }
}

###----------------------------------------------------------------###


### allow for optional validation on groups and on individual items
sub check_conditional {
  my ($self, $form, $ifs, $N_level, $ifs_match) = @_;

  $N_level ||= 0;
  $N_level ++; # prevent too many recursive checks

  ### can pass a single hash - or an array ref of hashes
  if (! $ifs || ! ref($ifs)) {
    dex dtrace();
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
  return @errors if scalar $self->filter_type('exclude_cgi',$types);

  ### allow for field names that contain regular expressions
  if ($field =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s) {
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
    die "Conditions for $type on field $field must be a ref" if ! ref $ifs;
    my $ret = $self->check_conditional($form, $ifs, $N_level, $ifs_match);
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
      next if ! $self->check_conditional($form, $ifs, $N_level, $ifs_match);
      $is_required = $type;
      last;
    }
  }
  if ($is_required && (! defined($form->{$field}) || ! length($form->{$field}))) {
#    dex $field_val,$field, $field;
    $self->add_error(\@errors, $field, $is_required, $field_val, $ifs_match);
    return @errors;
  }

  ### min values check
  foreach my $type ($self->filter_type('min_values',$types)) {
    my $n   = $field_val->{$type};
    my $val = exists($form->{$field}) ? $form->{$field} : [];
    my $m   = UNIVERSAL::isa($val, 'ARRAY') ? $#$val + 1 : 1;
    if ($m < $n) {
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
      return;
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
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
      return;
    }
  }
  
  ### allow for enum types
  foreach my $type ($self->filter_type('enum',$types)) {
    my $ref = ref($field_val->{$type}) ? $field_val->{$type} : [split(/\s*\|\|\s*/,$field_val->{$type})];
    my $value = $form->{$field};
    $value = '' if ! defined $value;
    if (! grep {$_ eq $value} @$ref) {
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### field equality test
  foreach my $type ($self->filter_type('equals',$types)) {
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
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### length min check
  foreach my $type ($self->filter_type('min_len',$types)) {
    my $n = $field_val->{$type};
    if (exists($form->{$field}) && defined($form->{$field}) && length($form->{$field}) < $n) {
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### length max check
  foreach my $type ($self->filter_type('max_len',$types)) {
    my $n = $field_val->{$type};
    if (exists($form->{$field}) && defined($form->{$field}) && length($form->{$field}) > $n) {
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### now do match types
  foreach my $type ($self->filter_type('match',$types)) {
    my $ref = ref($field_val->{$type}) ? $field_val->{$type} : [split(/\s*\|\|\s*/,$field_val->{$type})];
    foreach my $rx (@$ref) {
      my $not = ($rx =~ s/^\s*!~?\s*//) ? 1 : 0;
      if ($rx !~ /^\s*m?([^\w\s])(.*[^\\])\1([eisgmx]*)\s*$/s
          && $rx !~ /^\s*m?([^\w\s])()\1([eisgmx]*)\s*$/s) {
        die "Not sure how to parse that match ($rx)";
      }
      my ($pat,$opt) = ($2,$3);
      $opt =~ tr/g//d;
      die "The e option cannot be used on validation match's" if $opt =~ /e/;
      if (defined($form->{$field}) && length($form->{$field})) {
        if ( ($not && $form->{$field} =~ m/(?$opt:$pat)/)
             || (! $not && $form->{$field} !~ m/(?$opt:$pat)/)
             ) {
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
      die "Not sure how to compare \"$comp\"";
    }
    if (! $test) {
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### program side sql type
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
      $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  ### server side boolean type
  foreach my $type ($self->filter_type('boolean',$types)) {
    next if $field_val->{$type};
    $self->add_error(\@errors, $field, $type, $field_val, $ifs_match);
  }

  ### do specific type checks
  foreach my $type ($self->filter_type('type',$types)) {
    if (! $self->check_type($form->{$field},$field_val->{'type'},$field,$form)){
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
### How to handle errors

package CGI::Ex::Validate::Error;

use strict;
use overload '""' => \&as_string;
use Data::DumpEx;

sub new {
  my $class = shift || __PACKAGE__;
  my $self  = shift;
  die "Missing or invalid arrayref" if ! UNIVERSAL::isa($self, 'ARRAY');
  return bless $self, $class;
}

sub as_string {
  my $self = shift;
  return join "\n", @{ $self->as_array };
}

sub as_array {
  my $err_obj = shift;
  die "Invalid object" if ! UNIVERSAL::isa($err_obj, 'ARRAY');
  my $self = UNIVERSAL::isa($err_obj->[$#$err_obj],'CGI::Ex::Validate') ? pop(@$err_obj) : {};

  ### if there are heading items then we may end up needing a prefix
  my $has_headings = 0;
  foreach (@$err_obj) {
    next if ref;
    $has_headings = 1;
    last;
  }

  ### now build the array
  my @array = ();
  my $prefix  = defined($_[0]) ? shift()
    : defined($self->{error_prefix}) ? $self->{error_prefix}
    : $has_headings ? '  ' : '';
  my %found = ();
  foreach my $err (@$err_obj) {
    if (! ref $err) {
      push @array, $err;
      %found = ();
    } else {
      my $text = $err_obj->get_error_text($err);
      next if $found{$text};
      $found{$text} = 1;
      push @array, "$prefix$text";
    }
  }
    
  return wantarray ? @array : \@array;
}

sub as_hash {
  my $err_obj = shift;
  die "Invalid object" if ! UNIVERSAL::isa($err_obj, 'ARRAY');
  my $self = UNIVERSAL::isa($err_obj->[$#$err_obj],'CGI::Ex::Validate') ? pop(@$err_obj) : {};

  my $form = ref($_[0]) ? shift : {};
  my $suffix = defined($_[0]) ? shift()
    : defined($self->{name_suffix}) ? $self->{name_suffix}
    : '_error';

  ### now add to the hash
  my %found = ();
  foreach my $err (@$err_obj) {
    next if ! ref $err;

    my $field = $err->[0] || die "Missing field name";

    my $text = $err_obj->get_error_text($err);
    next if $found{$field}->{$text};
    $found{$field}->{$text} = 1;

    $field .= $suffix;
    $form->{$field} ||= [];
    $form->{$field} = [$form->{$field}] if ! ref($form->{$field});
    push @{ $form->{$field} }, $text;
  }
    
  return wantarray ? %$form : $form;
}

sub get_error_text {
  my $self  = shift;
  my $err   = shift;
  my ($field, $type, $field_val, $ifs_match) = @$err;
  my $dig     = ($type =~ s/(_?\d+)$//) ? $1 : '';
  my $type_lc = lc($type);
  
  ### type can look like "required" or "required2" or "required100023"
  ### allow for fallback from Required100023 through Required
    
  ### setup where to look for the error message
  my @error_keys = ("${type}_error");
  unshift @error_keys, "${type}${dig}_error" if length($dig);
  
  ### look in the passed hash or self first
  my $return;
  foreach my $key (@error_keys){
    $return = $field_val->{$key} || next;
    $return =~ s/$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
    last;
  }


  ### set default messages
  if (! $return) {
    ### the the name of this thing
    my $name = $field_val->{'name'} || "The field $field";
    $name =~ s/$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;

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
      $return = "$name did not match.";

    } elsif ($type eq 'compare') {
      $return = "$name did not fit comparison.";
  
    } elsif ($type eq 'sql') {
      $return = "$name did not match sql test.";
      
    } elsif ($type eq 'boolean') {
      $return = "$name did not match boolean test.";
      
    } elsif ($type eq 'type') {
      my $_type = $field_val->{"type${dig}"};
      $return = "$name did not match type $_type.";

    }
  }

  die "Missing error on field $field for type $type$dig" if ! $return;
  return $return;

}

###----------------------------------------------------------------###

1;


__END__

=head1 NAME

O::Form - Yet another form validator - does good javascript too

$Id: Validate.pm,v 1.8 2003-11-12 09:12:10 pauls Exp $

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


