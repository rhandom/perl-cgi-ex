/**----------------------------------------------------------------***
*  Copyright 2004 - Paul Seamons                                     *
*  Distributed under the Perl Artistic License without warranty      *
*  Based upon CGI/Ex/Validate.pm v1.14 from Perl                     *
*  For instructions on usage, see perldoc of CGI::Ex::Validate       *
***----------------------------------------------------------------**/
// $Revision: 1.27 $

function Validate () {
 this.error             = vob_error;
 this.validate          = vob_validate;
 this.check_conditional = vob_check_conditional;
 this.filter_types      = vob_filter_types;
 this.add_error         = vob_add_error;
 this.validate_buddy    = vob_validate_buddy;
 this.check_type        = vob_check_type;
 this.get_form_value    = vob_get_form_value;
}

function ValidateError (errors, extra) {
 this.errors = errors;
 this.extra  = extra;

 this.as_string = eob_as_string;
 this.as_array  = eob_as_array;
 this.as_hash   = eob_as_hash;
 this.get_error_text = eob_get_error_text;
 this.first_field    = eob_first_field;
}

///----------------------------------------------------------------///

function vob_error (err) {
 alert (err);
}

function vob_validate (form, val_hash) {
 if (typeof(val_hash) == 'string') {
   if (! document.yaml_load)
     return this.error("Cannot parse yaml string - document.yaml_load is not loaded");
   val_hash = document.yaml_load(val_hash);
 }

 var ERRORS = new Array ();
 var EXTRA  = new Array ();
 //  var USED_GROUPS = new Array();

 // distinguishing between associative and index based arrays is harder than in perl
 if (! val_hash.length) val_hash = new Array(val_hash);
 for (var i = 0; i < val_hash.length; i ++) {
   var group_val = val_hash[i];
   if (typeof(group_val) != 'object' || group_val.length) return this.error("Validation groups must be a hash");
   var title       = group_val['group title'];
   var validate_if = group_val['group validate_if'];

   if (validate_if && ! this.check_conditional(form, validate_if)) continue;
   //    USED_GROUPS.push(group_val);

   /// if the validation items were not passed as an arrayref
   /// look for a group order and then fail back to the keys of the group
   var fields = group_val['group fields'];
   var order  = new Array();
   for (var key in group_val) order[order.length] = key;
   order = order.sort();
   if (fields) {
     if (typeof(fields) != 'object' || ! fields.length)
       return this.error("'group fields' must be a non-empty array");
   } else {
     fields = new Array();
     var _order = (group_val['group order']) ? group_val['group order'] : order;
     if (typeof(_order) != 'object' || ! _order.length)
       return this.error("'group order' must be a non-empty array");
     for (var j = 0; j < _order.length; j ++) {
       var field = _order[j];
       if (field.match('^(group|general)\\s')) continue;
       var field_val = group_val[field];
       if (! field_val) {
         if (field == 'OR') field_val = 'OR';
         else return this.error('No element found in group for '+field);
       }
       if (typeof(field_val) == 'object' && ! field_val['field']) field_val['field'] = field;
       fields[fields.length] = field_val;
     }
   }

   /// check which fields have been used
   var found = new Array();
   for (var j = 0; j < fields.length; j ++) {
     var field_val = fields[j];
     var field = field_val['field'];
     if (! field) return this.error("Missing field key in validation");
     // if (found[field]) return this.error('Duplicate order found for '+field+' in group order or fields');
     found[field] = 1;
   }

   /// add any remaining fields from the order
   for (var j = 0; j < order.length; j ++) {
     var field = order[j];
     if (found[field] || field.match('^(group|general)\\s')) continue;
     var field_val = group_val[field];
     if (typeof(field_val) != 'object' || field_val.length) return this.error('Found a non-hash value on field '+field);
     if (! field_val['field']) field_val['field'] = field;
     fields[fields.length] = field_val;
   }

   /// now lets do the validation
   var is_found  = 1;
   var errors = new Array();
   var hold_error;

   for (var j = 0; j < fields.length; j ++) {
     var ref = fields[j];
     if (typeof(ref) != 'object' && ref == 'OR') {
       if (is_found) j ++;
       is_found = 1;
       continue;
     }
     is_found = 1;
     if (! ref['field']) return this.error("Missing field key during normal validation");
     var err = this.validate_buddy(form, ref['field'], ref);

     /// test the error - if errors occur allow for OR - if OR fails use errors from first fail
     if (err.length) {
       if (j <= fields.length && typeof(fields[j + 1] != 'object') && fields[j + 1] == 'OR') {
         hold_error = err;
       } else {
         if (hold_error) err = hold_error;
         for (var k = 0; k < err.length; k ++) errors[errors.length] = err[k];
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
 if (ERRORS.length) return new ValidateError(ERRORS, EXTRA);
 return;
}


/// allow for optional validation on groups and on individual items
function vob_check_conditional (form, ifs, N_level, ifs_match) {

 if (! N_level) N_level = 0;
 N_level ++;

 /// can pass a single hash - or an array ref of hashes
 if (! ifs) {
   return this.error("Need reference passed to check_conditional");
 } else if (typeof(ifs) != 'object') {
   ifs = new Array(ifs);
 } else if (! ifs.length) { // turn hash into array of hash
   ifs = new Array(ifs);
 }

 /// run the if options here
 /// multiple items can be passed - all are required unless OR is used to separate
 var is_found = 1;
 var m;
 for (var i = 0; i < ifs.length; i ++) {
   var ref = ifs[i];
   if (typeof(ref) != 'object') {
     if (ref == 'OR') {
       if (is_found) i++;
       is_found = 1;
       continue;
     } else {
       var field = ref;
       ref = new Array();
       if (m = field.match('^(\\s*!\\s*)')) {
         field = field.substring(m[1].length);
         ref['max_in_set'] = '0 of ' + field;
       } else {
         ref['required'] = 1;
       }
       ref['field'] = field;
     }
   }
   if (! is_found) break;

   /// get the field - allow for custom variables based upon a match
   var field = ref['field'];
   if (! field) return this.error("Missing field key during validate_if");
   field = field.replace(new RegExp('\\$(\\d+)','g'), function (all, N) {
     if (typeof(ifs_match) != 'object'
         || typeof(ifs_match[N]) == 'undefined') return ''
     return ifs_match[N];
   });

   var err = this.validate_buddy(form, field, ref, N_level);
   if (err.length) is_found = 0;
 }
 return is_found;
}

function vob_filter_types (type, types) {
 var values = new Array();
 var regexp = new RegExp('^'+type+'_?\\d*$');
 for (var i = 0; i < types.length; i++)
   if (types[i].match(regexp)) values[values.length] = types[i];
 return values;
}

function vob_add_error (errors,field,type,field_val,ifs_match) {
 errors[errors.length] = new Array(field, type, field_val, ifs_match);
}

/// this is where the main checking goes on
function vob_validate_buddy (form, field, field_val, N_level, ifs_match) {
 if (! N_level) N_level = 0;
 if (++ N_level > 10) return this.error("Max dependency level reached " + N_level);
 if (! form.elements) return;

 var errors = new Array();
 var types  = new Array();
 for (var key in field_val) types[types.length] = key;
 types = types.sort();

 /// allow for not running some tests in the cgi
 if (this.filter_types('exclude_js', types).length) return errors;

 /// allow for field names that contain regular expressions
 var m;
 if (m = field.match('^(!\\s*|)m([^\\s\\w])(.*)\\2([eigsmx]*)$')) {
   var not = m[1];
   var pat = m[3];
   var opt = m[4];
   if (opt.indexOf('e') != -1) return this.error("The e option cannot be used on field "+field);
   opt = opt.replace(new RegExp('[sg]','g'),'');
   var reg = new RegExp(pat, opt);

   var keys = new Array();
   for (var i = 0; i < form.elements.length; i ++) {
     var _field = form.elements[i].name;
     if (! _field) continue;
     if ( (not && ! (m = _field.match(reg))) || (m = _field.match(reg))) {
       var err = this.validate_buddy(form, _field, field_val, N_level, m);
       for (var j = 0; j < err.length; j ++) errors[errors.length] = err[j];
     }
   }
   return errors;
 }

 var _value = this.get_form_value(form[field]);
 var values;
 if (typeof(_value) == 'object') {
   values = _value;
 } else {
   values = new Array();
   values[values.length] = _value;
 }
 var n_values = (typeof(_value) == 'undefined') ? 0 : values.length;

 /// allow for default value
 var tests = this.filter_types('default', types);
 if (n_values == 0 || (n_values == 1 && values[0].length == 0)) {
   for (var i = 0; i < tests.length; i ++) {
     var el = form[field];
     var type = el.type;
     if (type && (type == 'hidden' || type == 'password' || type == 'text' || type == 'textarea' || type == 'submit')) el.value = values[0] = field_val[tests[i]];
   }
 }

 /// allow for a few form modifiers
 var modified = 0;
 for (var i = 0; i < values.length; i ++) {
   if (typeof(values[i]) == 'undefined') continue;
   if (! this.filter_types('do_not_trim',types).length)
     values[i] = values[i].replace('^\\s+','').replace(new RegExp('\\s+$',''),'');
   if (this.filter_types('to_upper_case',types).length) {
     values[i] = values[i].toUpperCase();
   } else if (this.filter_types('to_lower_case',types).length) {
     values[i] = values[i].toLowerCase();
   }
 }
 var tests = this.filter_types('replace', types);
 for (var i = 0; i < tests.length; i ++) {
   var ref = field_val[tests[i]];
   ref = (typeof(ref) == 'object') ? ref : ref.split(new RegExp('\\s*\\|\\|\\s*'));
   for (var j = 0; j < ref.length; j ++) {
     if (! (m = ref[j].match('^\\s*s([^\\s\\w])(.+)\\1(.*)\\1([eigmx]*)$')))
       return this.error("Not sure how to parse that replace "+ref[j]);
     var pat  = m[2];
     var swap = m[3];
     var opt  = m[4];
     if (opt.indexOf('e') != -1)
       return this.error("The e option cannot be used on field "+field+", replace "+tests[i]);
     var regexp = new RegExp(pat, opt);
     for (var k = 0; k < values.length; k ++) {
       if (values[k].match(regexp)) modified = 1;
       values[k] = values[k].replace(regexp,swap);
     }
   }
 }
 if (modified && n_values == 1) {
   var el = form[field];
   var type = el.type;
   if (! type) return '';
   if (type == 'hidden' || type == 'password' || type == 'text' || type == 'textarea' || type == 'submit')
     el.value = values[0];
 }

 /// only continue if a validate_if is not present or passes test
 var needs_val = 0;
 var n_vif = 0;
 var tests = this.filter_types('validate_if', types);
 for (var i = 0; i < tests.length; i ++) {
   n_vif ++;
   var ifs = field_val[tests[i]];
   var ret = this.check_conditional(form, ifs, N_level, ifs_match);
   if (ret) needs_val ++;
 }
 if (! needs_val && n_vif) return errors;

 
 /// check for simple existence
 /// optionally check only if another condition is met
 var is_required = '';
 var tests = this.filter_types('required', types);
 for (var i = 0; i < tests.length; i ++) {
   if (! field_val[tests[i]]) continue;
   is_required = tests[i];
   break;
 }
 if (! is_required) {
   var tests = this.filter_types('required_if', types);
   for (var i = 0; i < tests.length; i ++) {
     var ifs = field_val[tests[i]];
     if (! this.check_conditional(form, ifs, N_level, ifs_match)) continue;
     is_required = tests[i];
     break;
   }
 }
 if (is_required && (typeof(_value) == 'undefined'
                     || ((typeof(_value) == 'object' && _value.length == 0)
                         || ! _value.length))) {
   this.add_error(errors, field, is_required, field_val, ifs_match);
   return errors;
 }

 /// min values check
 var tests = this.filter_types('min_values', types);
 for (var i = 0; i < tests.length; i ++) {
   var n = field_val[tests[i]];
   if (n_values < n) {
     this.add_error(errors, field, tests[i], field_val, ifs_match);
     return errors;
   }
 }

 /// max values check
 var tests = this.filter_types('max_values', types);
 if (! tests.length) {
   tests[tests.length] = 'max_values';
   field_val['max_values'] = 1;
 }
 for (var i = 0; i < tests.length; i ++) {
   var n = field_val[tests[i]];
   if (n_values > n) {
     this.add_error(errors, field, tests[i], field_val, ifs_match);
     return errors;
   }
 }

 /// min_in_set and max_in_set check
 for (var h = 0; h < 2 ; h++) {
   var minmax = (h == 0) ? 'min' : 'max';
   var tests = this.filter_types(minmax+'_in_set', types);
   for (var i = 0; i < tests.length; i ++) {
     if (! (m = field_val[tests[i]].match('^\\s*(\\d+)(?:\\s*[oO][fF])?\\s+(.+)\\s*$')))
       return this.error("Invalid in_set check "+field_val[tests[i]]);
     var n       = m[1];
     var _fields = m[2].split(new RegExp('[\\s,]+'));
     for (var k = 0; k < _fields.length; k ++) {
       var _value = this.get_form_value(form[_fields[k]]);
       var _values;
       if (typeof(_value) == 'undefined') continue;
       if (typeof(_value) == 'object') {
         _values = _value;
       } else {
         _values = new Array();
         _values[_values.length] = _value;
       }
       for (var l = 0; l < _values.length; l ++) {
         var _value = _values[l];
         if (typeof(_value) != 'undefined' && _value.length) n --;
       }
     }
     if (   (minmax == 'min' && n > 0)
         || (minmax == 'max' && n < 0)) {
       this.add_error(errors, field, tests[i], field_val, ifs_match);
       return errors;
     }
   }
 }

 // the remaining tests operate on each value of a field
 for (var n = 0; n < values.length; n ++) {
   var value = values[n];

   /// allow for enum types
   var tests = this.filter_types('enum', types);
   for (var i = 0; i < tests.length; i ++) {
     var hold  = field_val[tests[i]];
     var _enum = (typeof(hold) == 'object') ? hold : hold.split(new RegExp('\\s*\\|\\|\\s*'));
     var is_found = 0;
     for (var j = 0; j < _enum.length; j ++) {
       if (value != _enum[j]) continue;
       is_found = 1;
       break;
     }
     if (! is_found) this.add_error(errors, field, tests[i], field_val, ifs_match);
   }

   /// field equality test
   var tests = this.filter_types('equals', types);
   for (var i = 0; i < tests.length; i ++) {
     var field2  = field_val[tests[i]];
     var success = 0;
     if (m = field2.match('^(["\'])(.*)\\1$')) {
       if (value == m[2]) success = 1;
     } else {
       var value2 = this.get_form_value(form[field2]);
       if (typeof(value2) == 'undefined') value2 = '';
       if (value == value2) success = 1;
     }
     if (! success) this.add_error(errors, field, tests[i], field_val, ifs_match);
   }

   /// length min check
   var tests = this.filter_types('min_len', types);
   for (var i = 0; i < tests.length; i ++) {
     var n = field_val[tests[i]];
     if (value.length < n) this.add_error(errors, field, tests[i], field_val, ifs_match);
   }

   /// length max check
   var tests = this.filter_types('max_len', types);
   for (var i = 0; i < tests.length; i ++) {
     var n = field_val[tests[i]];
     if (value.length > n) this.add_error(errors, field, tests[i], field_val, ifs_match);
   }

   /// now do match types
   var tests = this.filter_types('match', types);
   for (var i = 0; i < tests.length; i ++) {
     var ref = field_val[tests[i]];
     ref = (typeof(ref) == 'object') ? ref
       : (typeof(ref) == 'function') ? new Array(ref)
       : ref.split(new RegExp('\\s*\\|\\|\\s*'));
     for (var j = 0; j < ref.length; j ++) {
       if (typeof(ref[j]) == 'function') {
         if (! value.match(ref[j])) this.add_error(errors, field, tests[i], field_val, ifs_match);
       } else {
         if (! (m = ref[j].match('^\\s*(!\\s*|)m([^\\s\\w])(.*)\\2([eigsmx]*)\\s*$')))
           return this.error("Not sure how to parse that match ("+ref[j]+")");
         var not = m[1];
         var pat = m[3];
         var opt = m[4];
         if (opt.indexOf('e') != -1)
           return this.error("The e option cannot be used on field "+field+", test "+tests[i]);
         opt = opt.replace(new RegExp('[sg]','g'),'');
         var regexp = new RegExp(pat, opt);
         if (   (  not &&   value.match(regexp))
             || (! not && ! value.match(regexp))) {
           this.add_error(errors, field, tests[i], field_val, ifs_match);
         }
       }
     }
   }

   /// allow for comparison checks
   var tests = this.filter_types('compare', types);
   for (var i = 0; i < tests.length; i ++) {
     var ref = field_val[tests[i]];
     ref = (typeof(ref) == 'object') ? ref : ref.split(new RegExp('\\s*\\|\\|\\s*'));
     for (var j = 0; j < ref.length; j ++) {
       var comp = ref[j];
       if (! comp) continue;
       var hold = false;
       var copy = value;
       if (m = comp.match('^\\s*(>|<|[><!=]=)\\s*([\\d\.\-]+)\\s*$')) {
         if (! copy) copy = 0;
         copy *= 1;
         if      (m[1] == '>' ) hold = (copy >  m[2])
         else if (m[1] == '<' ) hold = (copy <  m[2])
         else if (m[1] == '>=') hold = (copy >= m[2])
         else if (m[1] == '<=') hold = (copy <= m[2])
         else if (m[1] == '!=') hold = (copy != m[2])
         else if (m[1] == '==') hold = (copy == m[2])
       } else if (m = comp.match('^\\s*(eq|ne|gt|ge|lt|le)\\s+(.+?)\\s*$')) {
         m[2] = m[2].replace('^(["\'])(.*)\\1$','$1');
         if      (m[1] == 'gt') hold = (copy >  m[2])
         else if (m[1] == 'lt') hold = (copy <  m[2])
         else if (m[1] == 'ge') hold = (copy >= m[2])
         else if (m[1] == 'le') hold = (copy <= m[2])
         else if (m[1] == 'ne') hold = (copy != m[2])
         else if (m[1] == 'eq') hold = (copy == m[2])
       } else {
         return this.error("Not sure how to compare \""+comp+"\"");
       }
       if (! hold) this.add_error(errors, field, tests[i], field_val, ifs_match);
     }
   }

   /// do specific type checks
   var tests = this.filter_types('type',types);
   for (var i = 0; i < tests.length; i ++)
     if (! this.check_type(value, field_val[tests[i]], field, form))
       this.add_error(errors, field, tests[i], field_val, ifs_match);

   /// do custom_js type checks
   // this will allow for a custom piece of javascript
   // the js is evaluated and should return 1 for success
   // or 0 for failure - the variables field, value, and field_val (the hash) are available
   var tests = this.filter_types('custom_js',types);
   for (var i = 0; i < tests.length; i ++)
     if (! eval(field_val[tests[i]]))
       this.add_error(errors, field, tests[i], field_val, ifs_match);
 }

 /// all done - time to return
 return errors;
}

/// used to validate specific types
function vob_check_type (value, type, field, form) {
 var m;

 /// do valid email address for our system
 if (type == 'EMAIL') {
   if (! value) return 0;
   if (! (m = value.match('^(.+)\@(.+?)$'))) return 0;
   if (m[1].length > 60)  return 0;
   if (m[2].length > 100) return 0;
   if (! this.check_type(m[2],'DOMAIN') && ! this.check_type(m[2],'IP')) return 0;
   if (! this.check_type(m[1],'LOCAL_PART')) return 0;

 /// the "username" portion of an email address
 } else if (type == 'LOCAL_PART') {
   if (typeof(value) == 'undefined' || ! value.length) return 0;
   if (! value.match('[^a-z0-9.\\-!&]')) return 0;
   if (! value.match('^[.\\-]'))         return 0;
   if (! value.match('[.\\-&]$'))        return 0;
   if (! value.match('(\\.-|-\\.|\\.\\.)')) return 0;

 /// standard IP address
 } else if (type == 'IP') {
   if (! value) return 0;
   var dig = value.split(new RegExp('\\.'));
   if (dig.length != 4) return 0;
   for (var i = 0; i < 4; i ++)
     if (typeof(dig[i]) == 'undefined' || dig[i].match('\\D') || dig[i] > 255) return 0;

 /// domain name - including tld and subdomains (which are all domains)
 } else if (type == 'DOMAIN') {
   if (! value) return 0;
   if (value.match('^[a-z0-9.-]{4,255}$')) return 0;
   if (value.match('^[.\\-]'))             return 0;
   if (value.match('(\\.-|-\\.|\\.\\.)'))  return 0;
   if (! (m = value.match('\.([a-z]+)$'))) return 0;
   value = value.substring(0,value.lastIndexOf('.'));

   if (m[1] == 'name') {
     if (! value.match('^[a-z0-9][a-z0-9\\-]{0,62}\\.[a-z0-9][a-z0-9\\-]{0,62}$')) return 0;
   } else
     if (! value.match('^([a-z0-9][a-z0-9\\-]{0,62}\\.)*[a-z0-9][a-z0-9\\-]{0,62}$')) return 0;

 /// validate a url
 } else if (type == 'URL') {
   if (! value) return 0;
   if (! (m = value.match(new RegExp('^https?://([^/]+)','i'),''))) return 0;
   value = value.substring(m[0].length);
   if (! this.check_type(m[1],'DOMAIN') && ! this.check_type(m[1],'IP')) return 0;
   if (value && ! this.check_type(value,'URI')) return 0;

 /// validate a uri - the path portion of a request
 } else if (type == 'URI') {
   if (! value) return 0;
   if (value.match('\\s')) return 0;

 } else if (type == 'CC') {
   if (! value) return 0;
   if (value.match('[^\\d\\- ]') || value.length > 16 || value.length < 13) return;
   /// simple mod10 check
   value = value.replace(new RegExp('[\\- ]','g'), '');
   var sum = 0;
   var swc = 0;

   for (var i = value.length - 1; i >= 0; i --) {
     if (++ swc > 2) swc = 1;
     var y = value.charAt(i) * swc;
     if (y > 9) y -= 9;
     sum += y;
   }
   if (sum % 10) return 0;

 }

 return 1;
}

// little routine that will get the values from the form
// it will return multiple values as an array
function vob_get_form_value (el) {
 if (! el) return '';
 if (el.disabled) return '';
 var type = el.type ? el.type.toLowerCase() : '';
 if (el.length && type != 'select-one') {
   var a = new Array();
   for (var j=0;j<el.length;j++) {
     if (type.indexOf('multiple')) {
       if (el[j].selected) a[a.length] = el[j].value;
     } else {
       if (el[j].checked)  a[a.length] = vob_get_form_value(el[j]);
     }
   }
   if (a.length == 0) return '';
   if (a.length == 1) return a[0];
   return a;
 }
 if (! type) return '';
 if (type == 'hidden' || type == 'password' || type == 'text' || type == 'textarea' || type == 'submit')
   return el.value;
 if (type.indexOf('select') != -1) {
   if (! el.length) return '';
   return el[el.selectedIndex].value;
 }
 if (type == 'checkbox' || type == 'radio') {
   return el.checked ? el.value : '';
 }
 if (type == 'file') {
   return el.value; // hope this works
 }
 alert('Unknown form type for '+el.name+': '+type);
 return '';
}

///----------------------------------------------------------------///

function eob_get_val (key, extra2, extra1, _default) {
 if (typeof(extra2[key]) != 'undefined') return extra2[key];
 if (typeof(extra1[key]) != 'undefined') return extra1[key];
 return _default;
}

function eob_as_string (extra2) {
 var extra1 = this.extra;
 if (! extra2) extra2 = new Array();

 var joiner = eob_get_val('as_string_join',   extra2, extra1, '\n');
 var header = eob_get_val('as_string_header', extra2, extra1, '');
 var footer = eob_get_val('as_string_footer', extra2, extra1, '');

 return header + this.as_array(extra2).join(joiner) + footer;
}

/// return an array of applicable errors
function eob_as_array (extra2) {
 var errors = this.errors;
 var extra1 = this.extra;
 if (! extra2) extra2 = new Array();

 var title = eob_get_val('as_array_title', extra2, extra1, 'Please correct the following items:');

 /// if there are heading items then we may end up needing a prefix
 var has_headings;
 if (title) has_headings = 1;
 else {
   for (var i = 0; i < errors.length; i ++) {
     if (typeof(errors[i]) != 'string') continue;
     has_headings = 1;
     break;
   }
 }

 var prefix = eob_get_val('as_array_prefix', extra2, extra1, has_headings ? '  ' : '');

 /// get the array ready
 var arr = new Array();
 if (title && title.length) arr[arr.length] = title;
 /// add the errors
 var found = new Array();
 for (var i = 0; i < errors.length; i ++) {
   if (typeof(errors[i]) == 'string') {
     arr[arr.length] = errors[i];
     found = new Array();
   } else {
     var text = this.get_error_text(errors[i]);
     if (found[text]) continue;
     found[text] = 1;
     arr[arr.length] = prefix + text;
   }
 }

 return arr;
}

/// return a hash of applicable errors
function eob_as_hash (extra2) {
 var errors = this.errors;
 var extra1 = this.extra;
 if (! extra2) extra2 = new Array();
 var suffix = eob_get_val('as_hash_suffix', extra2, extra1, '_error');
 var joiner = eob_get_val('as_hash_join',   extra2, extra1, '<br />');

 /// now add to the hash
 var found = new Array();
 var ret   = new Array();
 for (var i = 0; i < errors.length; i ++) {
   if (typeof(errors[i]) == 'string') continue;
   if (! errors[i].length) continue;

   var field     = errors[i][0];
   var type      = errors[i][1];
   var field_val = errors[i][2];
   var ifs_match = errors[i][3];

   if (! field) return alert("Missing field name");
   if (field_val['delegate_error']) {
     field = field_val['delegate_error'];
     field = field.replace(new RegExp('\\$(\\d+)','g'), function (all, N) {
       if (typeof(ifs_match) != 'object'
           || typeof(ifs_match[N]) == 'undefined') return ''
       return ifs_match[N];
     });
   }

   var text = this.get_error_text(errors[i]);
   if (! found[field]) found[field] = new Array();
   if (found[field][text]) continue;
   found[field][text] = 1;

   field += suffix;
   if (! ret[field]) ret[field] = new Array();
   ret[field].push(text);
 }

 /// allow for elements returned as
 if (joiner) {
   var header = eob_get_val('as_hash_header', extra2, extra1, '');
   var footer = eob_get_val('as_hash_footer', extra2, extra1, '');
   for (var key in ret) ret[key] = header + ret[key].join(joiner) + footer;
 }

 return ret;
}

/// return a user friendly error message
function eob_get_error_text (err) {
 var extra     = this.extra;
 var field     = err[0];
 var type      = err[1];
 var field_val = err[2];
 var ifs_match = err[3];
 var m;

 var dig = (m = type.match('(_?\\d+)$')) ? m[1] : '';
 var type_lc = type.toLowerCase();

 /// allow for delegated field names - only used for defaults
 if (field_val['delegate_error']) {
   field = field_val['delegate_error'];
   field = field.replace(new RegExp('\\$(\\d+)','g'), function (all, N) {
     if (typeof(ifs_match) != 'object'
         || typeof(ifs_match[N]) == 'undefined') return ''
     return ifs_match[N];
   });
 }

 /// the the name of this thing
 var name = (field_val['name']) ? field_val['name'] : "The field " +field;
 name = name.replace(new RegExp('\\$(\\d+)','g'), function (all, N) {
   if (typeof(ifs_match) != 'object'
       || typeof(ifs_match[N]) == 'undefined') return ''
   return ifs_match[N];
 });


 /// type can look like "required" or "required2" or "required100023"
 /// allow for fallback from required100023_error through required_error
 var possible_keys = new Array(type + '_error');
 if (dig.length) possible_keys.unshift(type + dig + '_error');

 /// look in the passed hash or self first
 for (var i = 0; i < possible_keys.length; i ++) {
   var key = possible_keys[i];
   var ret = field_val[key];
   if (! ret) {
     if (extra[key]) ret = extra[key];
     else continue;
   }
   ret = ret.replace(new RegExp('\\$(\\d+)','g'), function (all, N) {
     if (typeof(ifs_match) != 'object'
         || typeof(ifs_match[N]) == 'undefined') return ''
     return ifs_match[N];
   });
   ret = ret.replace(new RegExp('\\$field','g'), field);
   ret = ret.replace(new RegExp('\\$name' ,'g'), name);
   if (field_val[type + dig] && typeof(field_val[type + dig]) == 'string')
     ret = ret.replace(new RegExp('\\$value' ,'g'), field_val[type + dig]);
   return ret;
 }

 /// set default messages
 if (type == 'required' || type == 'required_if') {
   return name + " is required.";

 } else if (type == 'min_values') {
   var n = field_val["min_values" + dig];
   var values = (n == 1) ? 'value' : 'values';
   return name + " had less than "+n+" "+values+".";

 } else if (type == 'max_values') {
   var n = field_val["max_values" + dig];
   var values = (n == 1) ? 'value' : 'values';
   return name + " had more than "+n+" "+values+".";

 } else if (type == 'min_in_set') {
   var set = field_val["min_in_set" + dig];
   return "Not enough fields were chosen from the set ("+set+")";
   return "Too many fields were chosen from the set ("+set+")";

 } else if (type == 'max_in_set') {
   var set = field_val["max_in_set" + dig];
   return "Too many fields were chosen from the set ("+set+")";

 } else if (type == 'enum') {
   return name + " is not in the given list.";

 } else if (type == 'equals') {
   var field2 = field_val["equals" + dig];
   var name2  = field_val["equals" +dig+ "_name"];
   if (! name2) name2 = "the field " +field2;
   name2 = name2.replace(new RegExp('\\$(\\d+)','g'), function (all, N) {
     if (typeof(ifs_match) != 'object'
         || typeof(ifs_match[N]) == 'undefined') return ''
     return ifs_match[N];
   });
   return name + " did not equal " + name2 +".";

 } else if (type == 'min_len') {
   var n = field_val["min_len"+dig];
   var chars = (n == 1) ? 'character' : 'characters';
   return name + " was less than "+n+" "+chars+".";

 } else if (type == 'max_len') {
   var n = field_val["max_len"+dig];
   var chars = (n == 1) ? 'character' : 'characters';
   return name + " was more than "+n+" "+chars+".";

 } else if (type == 'match') {
   return name + " contains invalid characters.";

 } else if (type == 'compare') {
   return name + " did not fit comparison.";

 } else if (type == 'type') {
   var _type = field_val["type"+dig];
   return name + " did not match type "+_type+".";

 } else if (type == 'custom_js') {
   return name + " did not match custom_js"+dig+" check.";

 }

 return alert("Missing error on field "+field+" for type "+type+dig);
}

function eob_first_field () {
 for (var i = 0; i < this.errors.length; i++) {
   if (typeof(this.errors[i]) != 'object') continue;
   if (! this.errors[i][0]) continue;
   return this.errors[i][0];
 }
 return;
}

///----------------------------------------------------------------///

document.validate = function (form, val_hash) {
 // undo previous inline
 if (document.did_inline) {
   for (var key in document.did_inline) {
     var el = document.getElementById(key);
     if (el) el.innerHTML = '';
   }
   document.did_inline = undefined;
 }

 // do the validate
 val_hash = document.load_val_hash(form, val_hash);
 if (typeof(val_hash) == 'undefined') return true;
 if (! document.val_obj) document.val_obj = new Validate();
 var err_obj = document.val_obj.validate(form, val_hash);

 // return success
 if (! err_obj) return true;

 // focus
 var field = err_obj.first_field();
 if (field && form[field] && form[field].focus) form[field].focus();

 // inline
 if (! err_obj.extra.no_inline) {
   var d = document.did_inline = new Array();
   var hash = err_obj.as_hash();
   for (var key in hash) {
     var el = document.getElementById(key);
     if (el) el.innerHTML = hash[key];
     d[key] = 1;
   }
 }

 // alert
 if (! err_obj.extra.no_confirm) {
   return confirm(err_obj.as_string()) ? false : true;
 } else if (! err_obj.extra.no_alert) {
   alert(err_obj.as_string());
   return false;
 } else if (! err_obj.extra.no_inline) {
   return false;
 } else {
   return true;
 }
}

document.load_val_hash = function (form, val_hash) {
 // check the form we are using
 if (! form) return alert('Missing form or form name');
 if (typeof(form) == 'string') {
   if (! document[form]) return alert('No form by name '+form);
   form = document[form];
 }

 // if we already have validation - use it
 if (form.val_hash) return form.val_hash;

 // load in the validation and save it for future use
 if (typeof(val_hash) != 'object') {
   // get the hash from a javascript function
   if (typeof(val_hash) == 'function') {
     val_hash = val_hash(formname);
   } else if (typeof(val_hash) == 'undefined') {
     var el;
     // get hash from a global js variable
     if (typeof(document.validation) != 'undefined') {
       val_hash = document.validation;
     // get hash from a element by if of validation
     } else if (el = document.getElementById('validation')) {
       val_hash = el.innerHTML;
       val_hash = val_hash.replace(new RegExp('&lt;', 'ig'),'<');
       val_hash = val_hash.replace(new RegExp('&gt;', 'ig'),'>');
       val_hash = val_hash.replace(new RegExp('&amp;','ig'),'&');
     // read hash from <input name=foo validation="">
     } else {
       var order = new Array();
       var str   = '';
       var yaml  = form.getAttribute('validation');
       if (yaml) {
         if (m = yaml.match('^( +)')) yaml = yaml.replace(new RegExp('^'+m[1], 'g'), ''); //unindent
         yaml = yaml.replace(new RegExp('\\s*$',''),'\n'); // add trailing
         str += yaml;
       }
       var m;
       for (var i = 0; i < form.elements.length; i ++) {
         var name = form.elements[i].name;
         var yaml = form.elements[i].getAttribute('validation');
         if (! name || ! yaml) continue;
         yaml = yaml.replace(new RegExp('\\s*$',''),'\n'); // add trailing
         yaml = yaml.replace(new RegExp('^(.)','mg'),' $1');    // indent all
         yaml = yaml.replace(new RegExp('^( *[^\\s&*\\[\\{])',''),'\n$1'); // add newline
         str += name +':' + yaml;
         order[order.length] = name;
       }
       if (str) val_hash = str + "group order: [" + order.join(', ') + "]\n";
     }
   }
   if (typeof(val_hash) == 'string') {
     if (! document.yaml_load) return;
     document.hide_yaml_errors = (! document.show_yaml_errors);
     if (location.search && location.search.indexOf('show_yaml_errors') != -1)
       document.hide_yaml_errors = 0;
     val_hash = document.yaml_load(val_hash);
     if (document.yaml_error_occured) return;
   }
 }

 // attach to the form
 form.val_hash = val_hash;
 return form.val_hash;
}


document.check_form = function (form, val_hash) {
 // check the form we are using
 if (! form) return alert('Missing form or form name');
 if (typeof(form) == 'string') {
   if (! document[form]) return alert('No form by name '+form);
   form = document[form];
 }

 // void call - allow for getting it at run time rather than later
 document.load_val_hash(form, val_hash);

 // attach handler
 form.onsubmit = function () {return document.validate(this)};
}

// the end //
