/**----------------------------------------------------------------***
*  Copyright 2003 - Paul Seamons                                     *
*  Distributed under the Perl Artistic License without warranty      *
*  Based upon YAML.pm v0.35 from Perl
***----------------------------------------------------------------**/
// $Revision: 1.1 $

function Validate () {
  this.error    = vob_error;
  this.validate = vob_validate;
}

function vob_error () {

}

function vob_validate (form, val_hash) {
  if (typeof(val_hash) == 'string') {
    if (! document.yaml_load)
      return this.error("Cannot parse yaml string - document.yaml_load is not loaded");
    val_hash = document.yaml_load(val_hash);
  }

  var ERRORS = new Array ();
  var EXTRA  = new Array ();
  var USED_GROUPS = new Array();

  // distinguishing between associative and index based arrays is harder than in perl
  if (! val_hash[0] || typeof(val_hash[0]) != 'object') val_hash = new Array(val_hash);
  for (var i = 0; i < val_hash.length; i ++) {
    var group_val = val_hash[i];
    if (typeof(group_val) != 'object' || group_val[0]) return this.error("Validation groups must be a hash");
    var title       = group_val['group title'];
    var validate_if = group_val['group validate_if'];

    if (validate_if && ! this.check_conditional(form, validate_if)) continue;
    USED_GROUPS.push(group_val);

    /// if the validation items were not passed as an arrayref
    /// look for a group order and then fail back to the keys of the group
    var fields = group_val['group fields'];
    var order  = new Array();
    for (var key in group_val) order[order.length] = key;
    order = order.sort();
    if (fields) {
      if (typeof(fields) != 'object' || (fields.length && ! fields[0]))
        return this.error("'group fields' must be an arrayref");
    } else {
      fields = new Array();
      var _order = (group_val['group order']) ? group_val['group order'] : order;
      if (typeof(_order) != 'object' || (_order.length && ! _order[0]))
        return this.error("'group order' must be an arrayref");
      for (var j = 0; j < _order.length; j ++) {
        var field = _order[j];
        if (field.match('^(group|general)\\s')) continue;
        var field_val = (group_val[field]) ? group_val[field] : (field == 'OR') ? 'OR'
          : return this.error('No element found in group for '+field);
        if (typeof(field_val) == 'object' && ! field_val['field']) field_val['field'] = field;
        fields[fields.length] = field_val;
      }
    }

    /// check which fields have been used
    var found = new Array();
    for (var j = 0; j < fields.length; j ++) {
      var field_val = fields[j];
      var field = (field_val['field']) ? field_val['field'] : return this.error("Missing field key in validation");
      if (found[field]) return this.error('Duplicate order found for '+field+' in group order or fields');
      found[field] = 1;
    }

    /// add any remaining fields from the order
    for (var j = 0; j < order.length; j ++) {
      var field = order[j];
      if (found[field] || field.match('^(group|general)\\s')) continue;
      var field_val = group_val[field];
      if (typeof(field_val) != 'object' || field_val[0]) return this.error('Found a nonhashref value on field '+field);
      if (! field_val['field']) field_val['field'] = field;
      fields[fields.length] = field_val;
    }

    /// now lets do the validation
    var is_found  = 1;
    var errors = new Array();
    var hold_error;
    for (var j = 0; j < fields.length; j ++) {
      var ref = fields[j];
      if (typeof(ref) != object && ref == 'OR') {
        j += (is_found) ? 2 : 1;
        is_found = 1;
        continue;
      }
      found = 1;
      if (! ref['field']) return this.error("Missing field key during normal validation");
      var err = this.validate_buddy(form, ref['field'], ref);

      /// test the error - if errors occur allow for OR - if OR fails use errors from first fail
      if (err.length) {
        if (i <= fields.length && typeof(fields[i + 1] != 'object') && fields[i + 1] == 'OR') {
          hold_error = err;
        } else {
          errors[errors.length] = (hold_error) ? hold_error : err;
          hold_error = '';
        }
      } else {
        hold_error = '';
      }
    }

    /// add on errors as requested
    if (errors.length) {
      if (title) ERRORS[ERRORS.length] = title;
      for (var j = 0; j < errors.length; j ++) ERRORS[ERRORS.length] = errors[j];
    }

    /// add on general options, and group options if errors in group occurred
    var m;
    for (var j = 0; j < order.length; j ++) {
      var field = order[j];
      if (! (m = field.match('^(general|group)\\s+(\\w+)$'))) continue;
      if (m[1] == 'group' && (errors.length == 0 || m[2].match('^(field|order|title)$'))) continue;
      EXTRA[m[2]] = group_val[field];
    }
  }

  /// store any extra items from self
  for (var key in this) {
    if (! key.match('_error$')
        && ! key.match('^(raise_error|as_hash_\\w+|as_array_\\w+|as_string_\\w+)$')) continue;
    EXTRA[key] = this[key];
  }

  /// allow for checking for unused keys
  // if (EXTRA['no_extra_fields'])
  // won't do anything about this for now - let the server handle it

  /// return what they want
  if (errors.length) return new ValidateError(ERRORS, EXTRA);
  return;
}


/// allow for optional validation on groups and on individual items
function check_conditional () {
  my ($self, $form, $ifs, $N_level, $ifs_match) = @_;

  $N_level ||= 0;
  $N_level ++; # prevent too many recursive checks

  /// can pass a single hash - or an array ref of hashes
  if (! $ifs) {
    return this.error("Need reference passed to check_conditional";
  } elsif (! ref($ifs)) {
    $ifs = [$ifs];
  } elsif (UNIVERSAL::isa($ifs,'HASH')) {
    $ifs = [$ifs];
  }

  /// run the if options here
  /// multiple items can be passed - all are required unless OR is used to separate
  var found = 1;
  foreach (var i = 0; $i <= $#$ifs; $i ++) {
    var ref = $ifs->[$i];
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

    /// get the field - allow for custom variables based upon a match
    var field = $ref->{'field'} || return this.error("Missing field key during validate_if";
    $field =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;

    my @err = this.validate_buddy($form, $field, $ref, $N_level);
    $found = 0 if scalar @err;
  }
  return $found;
}


/// this is where the main checking goes on
function validate_buddy () {
  var self = shift;
  my ($form, $field, $field_val, $N_level, $ifs_match) = @_;
  $N_level ||= 0;
  $N_level ++; # prevent too many recursive checks
  return this.error("Max dependency level reached $N_level" if $N_level > 10;

  my @errors = ();
  var types  = [sort keys %$field_val];

  /// allow for not running some tests in the cgi
  if (scalar this.filter_type('exclude_cgi',$types)) {
    return wantarray ? @errors : scalar @errors;
  }

  /// allow for field names that contain regular expressions
  if ($field =~ m|^(!?)m(\W)(.*)\2([eigsmx]*)$|s) {
    my ($not,$pat,$opt) = ($1,$3,$4);
    $opt =~ tr/g//d;
    return this.error("The e option cannot be used on validation keys on field $field" if $opt =~ /e/;
    foreach var _field (sort keys %$form) {
      next if ($not && $_field =~ m/(?$opt:$pat)/) || (! $not && $_field !~ m/(?$opt:$pat)/);
      my @match = (undef,$1,$2,$3,$4,$5); # limit to the matches
      push @errors, this.validate_buddy($form, $_field, $field_val, $N_level, \@match);
    }
    return wantarray ? @errors : scalar @errors;
  }

  /// allow for a few form modifiers
  if (defined $form->{$field}) {
    if (! scalar this.filter_type('do_not_trim',$types)) { # whitespace
      $form->{$field} =~ s/^\s+//;
      $form->{$field} =~ s/\s+$//;
    }
    foreach var pat (this.filter_type('strip_characters',$types)) { # strip characters
      $form->{$field} =~ s/$pat//;
    }
    if (scalar this.filter_type('to_upper_case',$types)) { # uppercase
      $form->{$field} = uc($form->{$field});
    } elsif (scalar this.filter_type('to_lower_case',$types)) { # lowercase
      $form->{$field} = lc($form->{$field});
    }
  }

  /// only continue if a validate_if is not present or passes test
  var needs_val = 0;
  var n_vif = 0;
  foreach var type (this.filter_type('validate_if',$types)) {
    $n_vif ++;
    var ifs = $field_val->{$type};
    var ret = this.check_conditional($form, $ifs, $N_level, $ifs_match);
    $needs_val ++ if $ret;
  }
  if (! $needs_val && $n_vif) {
    return wantarray ? @errors : scalar @errors;
  }
  
  /// check for simple existence
  /// optionally check only if another condition is met
  var is_required = '';
  foreach var type (this.filter_type('required',$types)) {
    next if ! $field_val->{$type};
    $is_required = $type;
    last;
  }
  if (! $is_required) {
    foreach var type (this.filter_type('required_if',$types)) {
      var ifs = $field_val->{$type};
      next if ! this.check_conditional($form, $ifs, $N_level, $ifs_match);
      $is_required = $type;
      last;
    }
  }
  if ($is_required && (! defined($form->{$field}) || ! length($form->{$field}))) {
    return 1 if ! wantarray;
    this.add_error(\@errors, $field, $is_required, $field_val, $ifs_match);
    return @errors;
  }

  /// min values check
  foreach var type (this.filter_type('min_values',$types)) {
    var n   = $field_val->{$type};
    var val = exists($form->{$field}) ? $form->{$field} : [];
    var m   = UNIVERSAL::isa($val, 'ARRAY') ? $#$val + 1 : 1;
    if ($m < $n) {
      return 1 if ! wantarray;
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
      return @errors;
    }
  }

  /// max values check
  my @keys = this.filter_type('max_values',$types);
  if ($#keys == -1) {
    push @keys, 'max_values';
    field_val['max_values'] = 1;
  }
  foreach var type (@keys) {
    var n   = $field_val->{$type};
    var val = exists($form->{$field}) ? $form->{$field} : [];
    var m   = (UNIVERSAL::isa($val, 'ARRAY')) ? $#$val + 1 : 1;
    if ($m > $n) {
      return 1 if ! wantarray;
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
      return @errors;
    }
  }
  
  /// allow for enum types
  foreach var type (this.filter_type('enum',$types)) {
    var ref = ref($field_val->{$type}) ? $field_val->{$type} : [split(/\s*\|\|\s*/,$field_val->{$type})];
    var value = $form->{$field};
    $value = '' if ! defined $value;
    if (! grep {$_ eq $value} @$ref) {
      return 1 if ! wantarray;
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  /// field equality test
  foreach var type (this.filter_type('equals',$types)) {
    var field2  = $field_val->{$type};
    var success = 0;
    if ($field2 =~ m/^([\"\'])(.*)\1$/) {
      var test = $2;
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
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  /// length min check
  foreach var type (this.filter_type('min_len',$types)) {
    var n = $field_val->{$type};
    if (! exists($form->{$field}) || ! defined($form->{$field}) || length($form->{$field}) < $n) {
      return 1 if ! wantarray;
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  /// length max check
  foreach var type (this.filter_type('max_len',$types)) {
    var n = $field_val->{$type};
    if (exists($form->{$field}) && defined($form->{$field}) && length($form->{$field}) > $n) {
      return 1 if ! wantarray;
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  /// now do match types
  foreach var type (this.filter_type('match',$types)) {
    var ref = UNIVERSAL::isa($field_val->{$type},'ARRAY') ? $field_val->{$type}
       : UNIVERSAL::isa($field_val->{$type}, 'Regexp') ? [$field_val->{$type}]
       : [split(/\s*\|\|\s*/,$field_val->{$type})];
    foreach var rx (@$ref) {
      if (UNIVERSAL::isa($rx,'Regexp')) {
        if (! defined($form->{$field}) || $form->{$field} !~ $rx) {
          this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
        }
      } else {
        var not = ($rx =~ s/^\s*!~?\s*//) ? 1 : 0;
        if ($rx !~ /^\s*m?([^\w\s])(.*[^\\])\1([eisgmx]*)\s*$/s
            && $rx !~ /^\s*m?([^\w\s])()\1([eisgmx]*)\s*$/s) {
          return this.error("Not sure how to parse that match ($rx)";
        }
        my ($pat,$opt) = ($2,$3);
        $opt =~ tr/g//d;
        return this.error("The e option cannot be used on validation match's" if $opt =~ /e/;
        if ( (     $not && (  defined($form->{$field}) && $form->{$field} =~ m/(?$opt:$pat)/))
             || (! $not && (! defined($form->{$field}) || $form->{$field} !~ m/(?$opt:$pat)/))
             ) {
          return 1 if ! wantarray;
          this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
        }
      }
    }
  }

  /// allow for comparison checks
  foreach var type (this.filter_type('compare',$types)) {
    var comp  = $field_val->{$type} || next;
    var value = $form->{$field};
    var test  = 0;
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
      return this.error("Not sure how to compare \"$comp\"";
    }
    if (! $test) {
      return 1 if ! wantarray;
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  /// server side sql type
  foreach var type (this.filter_type('sql',$types)) {
    var db_type = $field_val->{"${type}_db_type"};
    var dbh = ($db_type) ? this.{dbhs}->{$db_type} : this.{dbh};
    if (! $dbh) {
      return this.error("Missing dbh for $type type on field $field" . ($db_type ? " and db_type $db_type" : "");
    } elsif (UNIVERSAL::isa($dbh,'CODE')) {
      $dbh = &$dbh($field, $self) || return this.error("SQL Coderef did not return a dbh";
    }
    var sql  = $field_val->{$type};
    my @args = ($field_val) x $sql =~ tr/?//;
    var return = $dbh->selectrow_array($sql, {}, @args); # is this right - copied from O::FORMS
    $field_val->{"${type}_error_if"} = 1 if ! defined $field_val->{"${type}_error_if"};
    if ( (! $return && $field_val->{"${type}_error_if"})
         || ($return && ! $field_val->{"${type}_error_if"}) ) {
      return 1 if ! wantarray;
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }

  /// server side custom type
  foreach var type (this.filter_type('custom',$types)) {
    var value = $field_val->{$type};
    $value = &$value($field, $form->{$field}, $field_val, $type) if UNIVERSAL::isa($value, 'CODE');
    next if $value;
    return 1 if ! wantarray;
    this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
  }

  /// do specific type checks
  foreach var type (this.filter_type('type',$types)) {
    if (! this.check_type($form->{$field},field_val['type'],$field,$form)){
      return 1 if ! wantarray;
      this.add_error(\@errors, $field, $type, $field_val, $ifs_match);
    }
  }            

  /// all done - time to return
  return wantarray ? @errors : scalar @errors;
}

/// simple error adder abstraction
function add_error () {
  var self = shift;
  var errors = shift;
  push @$errors, \@_;
}

/// allow for multiple validations in the same hash
/// ie Match, Match1, Match2, Match234
function filter_type () {
  var self  = shift;
  var type  = shift;
  var order = shift || return this.error("Missing order array";
  my @array = ();
  foreach (@$order) {
    push @array, $_ if /^\Q$type\E_?\d*$/;
  }
  return wantarray ? @array : scalar @array;
}



/// used to validate specific types
function check_type () {
  var self  = shift;
  var value = shift;
  var type  = uc(shift);

  /// do valid email address for our system
  if ($type eq 'EMAIL') {
    return 0 if ! $value;
    my($local_p,$dom) = ($value =~ /^(.+)\@(.+?)$/) ? ($1,$2) : return 0;

    return 0 if length($local_p) > 60;
    return 0 if length($dom) > 100;
    return 0 if ! this.check_type($dom,'DOMAIN') && ! this.check_type($dom,'IP');
    return 0 if ! this.check_type($local_p,'LOCAL_PART');

  /// the "username" portion of an email address
  } elsif ($type eq 'LOCAL_PART') {
    return 0 if ! defined($value) || ! length($value);
    return 0 if $value =~ m/[^a-z0-9.\-\!\&]/;
    return 0 if $value =~ m/^[\.\-]/;
    return 0 if $value =~ m/[\.\-\&]$/;
    return 0 if $value =~ m/(\.\-|\-\.|\.\.)/;

  /// standard IP address
  } elsif ($type eq 'IP') {
    return 0 if ! $value;
    return (4 == grep {!/\D/ && $_ < 256} split /\./, $value, 4);

  /// domain name - including tld and subdomains (which are all domains)    
  } elsif ($type eq 'DOMAIN') {
    return 0 if ! $value;
    return 0 if $value =~ m/[^a-z0-9.\-]/;
    return 0 if $value =~ m/^[\.\-]/;
    return 0 if $value =~ m/(\.\-|\-\.|\.\.)/;
    return 0 if length($value) > 255;
    return 0 if $value !~ s/\.([a-z]+)$//;

    var ext = $1;
    if ($ext eq 'name') { # .name domains
      return 0 if $value !~ /^[a-z0-9][a-z0-9\-]{0,62} \. [a-z0-9][a-z0-9\-]{0,62}$/x;
    } else {              # any other domains
      return 0 if $value !~ /^([a-z0-9][a-z0-9\-]{0,62} \.)* [a-z0-9][a-z0-9\-]{0,62}$/x;
    }
    
  /// validate a url
  } elsif ($type eq 'URL') {
    return 0 if ! $value;
    $value =~ s|^https?://([^/]+)||i || return 0;
    var dom = $1;
    return 0 if ! this.check_type($dom,'DOMAIN') && ! this.check_type($dom,'IP');
    return 0 if $value && ! this.check_type($value,'URI');
    
  /// validate a uri - the path portion of a request
  } elsif ($type eq 'URI') {
    return 0 if ! $value;
    return 0 if $value =~ m/\s+/;

  } elsif ($type eq 'CC') {
    return 0 if ! $value;
    /// validate the number
    return 0 if $value =~ /[^\d\-\ ]/
      || length($value) > 16
      || length($value) < 13;

    /// simple mod10 check
    $value =~ s/\D//g;
    var sum    = 0;
    var switch = 0;
    foreach var digit ( reverse split //, $value ){
      $switch = 1 if ++ $switch > 2;
      var y = $digit * $switch;
      $y -= 9 if $y > 9;
      $sum += $y;
    }
    return 0 if $sum % 10;

    
  }

  return 1;
}


package CGI::Ex::Validate::Error;

use strict;
use overload '""' => \&as_string;

function new () {
  var class  = shift || __PACKAGE__;
  var errors = shift;
  var extra  = shift || {};
  return this.error("Missing or invalid arrayref" if ! UNIVERSAL::isa($errors, 'ARRAY');
  return this.error("Missing or invalid hashref"  if ! UNIVERSAL::isa($extra,  'HASH');
  return bless {errors => $errors, extra => $extra}, $class;
}

function as_string () {
  var self = shift;
  var extra  = this.{extra} || {};
  var extra2 = shift || {};

  /// allow for formatting
  var join = defined($extra2->{as_string_join}) ? $extra2->{as_string_join}
    : defined($extra->{as_string_join}) ? $extra->{as_string_join}
    : "\n";
  var header = defined($extra2->{as_string_header}) ? $extra2->{as_string_header}
    : defined($extra->{as_string_header}) ? $extra->{as_string_header} : "";
  var footer = defined($extra2->{as_string_footer}) ? $extra2->{as_string_footer}
    : defined($extra->{as_string_footer}) ? $extra->{as_string_footer} : "";

  return $header . join($join, @{ this.as_array($extra2) }) . $footer;
}

/// return an array of applicable errors
function as_array () {
  var self = shift;
  var errors = this.{errors} || return this.error("Missing errors";
  var extra  = this.{extra}  || {};
  var extra2 = shift || {};

  var title = defined($extra2->{as_array_title}) ? $extra2->{as_array_title}
    : defined($extra->{as_array_title}) ? $extra->{as_array_title}
    : "Please correct the following items:";

  /// if there are heading items then we may end up needing a prefix
  var has_headings;
  if ($title) {
    $has_headings = 1;
  } else {
    foreach (@$errors) {
      next if ref;
      $has_headings = 1;
      last;
    }
  }

  var prefix = defined($extra2->{as_array_prefix}) ? $extra2->{as_array_prefix}
    : defined($extra->{as_array_prefix}) ? $extra->{as_array_prefix}
    : $has_headings ? '  ' : '';

  /// get the array ready
  my @array = ();
  push @array, $title if length $title;

  /// add the errors
  my %found = ();
  foreach var err (@$errors) {
    if (! ref $err) {
      push @array, $err;
      %found = ();
    } else {
      var text = this.get_error_text($err);
      next if $found{$text};
      $found{$text} = 1;
      push @array, "$prefix$text";
    }
  }
    
  return \@array;
}

/// return a hash of applicable errors
function as_hash () {
  var self = shift;
  var errors = this.{errors} || return this.error("Missing errors";
  var extra  = this.{extra}  || {};
  var extra2 = shift || {};

  var suffix = defined($extra2->{as_hash_suffix}) ? $extra2->{as_hash_suffix}
    : defined($extra->{as_hash_suffix}) ? $extra->{as_hash_suffix} : '_error';
  var join   = exists($extra2->{as_hash_join}) ? $extra2->{as_hash_join}
    : exists($extra->{as_hash_join}) ? $extra->{as_hash_join} : undef;

  /// now add to the hash
  my %found  = ();
  my %return = ();
  foreach var err (@$errors) {
    next if ! ref $err;

    my ($field, $type, $field_val, $ifs_match) = @$err;
    return this.error("Missing field name" if ! $field;
    if ($field_val->{delegate_error}) {
      $field = $field_val->{delegate_error};
      $field =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
    }

    var text = this.get_error_text($err);
    next if $found{$field}->{$text};
    $found{$field}->{$text} = 1;

    $field .= $suffix;
    $return{$field} ||= [];
    $return{$field} = [$return{$field}] if ! ref($return{$field});
    push @{ $return{$field} }, $text;
  }

  /// allow for elements returned as 
  if ($join) {
    var header = defined($extra2->{as_hash_header}) ? $extra2->{as_hash_header}
      : defined($extra->{as_hash_header}) ? $extra->{as_hash_header} : "";
    var footer = defined($extra2->{as_hash_footer}) ? $extra2->{as_hash_footer}
      : defined($extra->{as_hash_footer}) ? $extra->{as_hash_footer} : "";
    foreach var key (keys %return) {
      $return{$key} = $header . join($join,@{ $return{$key} }) . $footer;
    }
  }

  return \%return;
}

/// return a user friendly error message
function get_error_text () {
  var self  = shift;
  var err   = shift;
  var extra = this.{extra} || {};
  my ($field, $type, $field_val, $ifs_match) = @$err;
  var dig     = ($type =~ s/(_?\d+)$//) ? $1 : '';
  var type_lc = lc($type);

  /// allow for delegated field names - only used for defaults
  if ($field_val->{delegate_error}) {
    $field = $field_val->{delegate_error};
    $field =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
  }

  /// the the name of this thing
  var name = field_val['name'] || "The field $field";
  $name =~ s/$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;

  /// type can look like "required" or "required2" or "required100023"
  /// allow for fallback from required100023_error through required_error
  my @possible_error_keys = ("${type}_error");
  unshift @possible_error_keys, "${type}${dig}_error" if length($dig);
  
  /// look in the passed hash or self first
  var return;
  foreach var key (@possible_error_keys){
    $return = $field_val->{$key} || $extra->{$key} || next;
    $return =~ s/\$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
    $return =~ s/\$field/$field/g;
    $return =~ s/\$name/$name/g;
    if (var value = $field_val->{"$type$dig"}) {
      $return =~ s/\$value/$value/g if ! ref $value;
    }
    last;
  }

  /// set default messages
  if (! $return) {
    if ($type eq 'required' || $type eq 'required_if') {
      $return = "$name is required.";
  
    } elsif ($type eq 'min_values') {
      var n = $field_val->{"min_values${dig}"};
      var values = ($n == 1) ? 'value' : 'values';
      $return = "$name had less than $n $values.";
  
    } elsif ($type eq 'max_values') {
      var n = $field_val->{"max_values${dig}"};
      var values = ($n == 1) ? 'value' : 'values';
      $return = "$name had more than $n $values.";
      
    } elsif ($type eq 'enum') {
      $return = "$name is not in the given list.";
  
    } elsif ($type eq 'equals') {
      var field2 = $field_val->{"equals${dig}"};
      var name2  = $field_val->{"equals${dig}_name"} || "the field $field2";
      $name2 =~ s/$(\d+)/defined($ifs_match->[$1]) ? $ifs_match->[$1] : ''/eg if $ifs_match;
      $return = "$name did not equal $name2.";
  
    } elsif ($type eq 'min_len') {
      var n = $field_val->{"min_len${dig}"};
      var char = ($n == 1) ? 'character' : 'characters';
      $return = "$name was less than $n $char.";

    } elsif ($type eq 'max_len') {
      var n = $field_val->{"max_len${dig}"};
      var char = ($n == 1) ? 'character' : 'characters';
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
      var _type = $field_val->{"type${dig}"};
      $return = "$name did not match type $_type.";

    } elsif ($type eq 'no_extra_fields') {
      $return = "$name should not be passed to validate.";
    }
  }

  return this.error("Missing error on field $field for type $type$dig" if ! $return;
  return $return;

}
