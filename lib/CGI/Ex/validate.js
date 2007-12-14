// Copyright 2007 - Paul Seamons - $Revision: 1.58 $
// Distributed under the Perl Artistic License without warranty
// See perldoc CGI::Ex::Validate for usage

var v_did_inline  = {};
var v_has_changed = {};

function ValidateError (errors, extra) {
 this.errors = errors;
 this.extra  = extra;
 this.as_string = eob_as_string;
 this.as_array  = eob_as_array;
 this.as_hash   = eob_as_hash;
 this.first_field = eob_first_field;
}

//

function v_error (err) { alert (err) }

function v_clean_val_hash (val_hash) {
 if (typeof(val_hash) != 'object') return {error: v_error("Validation must be an associative array (hash)")};

 var order  = [];
 for (var key in val_hash) {
  if (key == 'extend') continue; // Protoype Array()
  if (key.match(/^general\s/)) {
    var new_key = key.replace(/^general\s+/, 'group ');
    val_hash.new_key = val_hash.key;
    delete val_hash.key;
    key = new_key;
  }
  order.push(key);
 }
 order = order.sort();

 var f = val_hash['group set_hook'];
 if (f && typeof(f) == 'string') val_hash['group set_hook'] = eval(f);
 f = val_hash['group clear_hook'];
 if (f && typeof(f) == 'string') val_hash['group clear_hook'] = eval(f);

 var fields = val_hash['group fields'];
 if (fields) {
  if (typeof(fields) != 'object' || ! fields.length)
   return {error:v_error("'group fields' must be a non-empty array")};
 } else {
  fields = [];
  var _order = (val_hash['group order']) ? val_hash['group order'] : order;
  if (typeof(_order) != 'object' || ! _order.length)
   return {error:v_error("'group order' must be a non-empty array")};
  for (var i = 0; i < _order.length; i++) {
   var field = _order[i];
   if (field.match(/^group\s/)) continue;
   var field_val = val_hash[field];
   if (! field_val) {
    if (field == 'OR') field_val = 'OR';
    else return {error:v_error('No element found in group for '+field)};
   }
   if (typeof(field_val) == 'object' && ! field_val['field']) field_val['field'] = field;
   fields.push(field_val);
  }
 }

 var found = {};
 for (var i = 0; i < fields.length; i++) {
  var field_val = fields[i];
  var field = field_val.field;
  if (! field) return {error:v_error("Missing field key in validation")};
  found[field] = 1;
 }

 for (var i = 0; i < order.length; i++) {
  var field = order[i];
  if (found[field] || field.match(/^group\s/)) continue;
  var field_val = val_hash[field];
  if (typeof(field_val) != 'object' || field_val.length) return {error:v_error('Found a non-hash value on field '+field)};
  if (! field_val.field) field_val.field = field;
  fields.push(field_val);
 }

 for (var i = 0; i < fields.length; i++) v_clean_field_val(fields[i]);

 return {'fields':fields, 'order':order};
}

function v_clean_field_val (field_val) {
 if (! field_val.order) field_val.order = v_field_order(field_val);
 if (! field_val.deps) field_val.deps = {};
 for (var i = 0; i < field_val.order.length; i++) {
  var k = field_val.order[i];
  var v = field_val[k];
  if (typeof(v) == 'undefined') return {error:v_error('No matching validation found on field '+field+' for type '+k)};
  if (k.match(/^(min|max)_in_set(\d*)$/)) {
   if (typeof(v) == 'string') {
    if (! (m = v.match(/^\s*(\d+)(?:\s*[oO][fF])?\s+(.+)\s*$/))) return {error:v_error("Invalid "+k+" check "+v)};
    field_val[k] = m[2].split(/[\s,]+/);
    field_val[k].unshift(m[1]);
   }
   for (var j = 1; j < field_val[k].length; j++) if (field_val[k][j] != field_val.field) field_val.deps[field_val[k][j]] = 1;
  } else if (k.match(/^(enum|compare)\d*$/)) {
   if (typeof(v) == 'string') field_val[k] = v.split(/\s*\|\|\s*/);
  } else if (k.match(/^match\d*$/)) {
   if (typeof(v) == 'string') v = field_val[k] = v.split(/\s*\|\|\s*/);
   for (var j = 0; j < v.length; j++) {
    if (typeof(v[j]) != 'string' || v[j] == '!') continue;
    var m = v[j].match(/^\s*(!\s*|)m([^\s\w])(.*)\2([eigsmx]*)\s*$/);
    if (! m) return {error:v_error("Not sure how to parse that match ("+v[j]+")")};
    var not = m[1];
    var pat = m[3];
    var opt = m[4];
    if (opt.indexOf('e') != -1) return {error:v_error("The e option cannot be used on field "+field_val.field+", test "+k)};
    opt = opt.replace(/[sg]/g,'');
    v[j] = new RegExp(pat, opt);
    if (not) v.splice(j, 0, '!');
   }
  } else if (k.match(/^(async_js|custom_js)\d*$/)) {
   if (typeof(v) == 'string' && v.match(/^\s*function\s*/)) field_val[k] = eval(v);
  }
 }
}

function v_validate (form, val_hash) {
 var clean  = v_clean_val_hash(val_hash);
 if (clean.error) return clean.error;
 var order  = clean.order;
 var fields = clean.fields;

 var ERRORS = [];
 var EXTRA  = [];
 var title       = val_hash['group title'];
 var validate_if = val_hash['group validate_if'];
 if (validate_if && ! v_check_conditional(form, validate_if)) return;

 var is_found  = 1;
 var errors = [];
 var hold_error;

 for (var j = 0; j < fields.length; j++) {
  var ref = fields[j];
  if (typeof(ref) != 'object' && ref == 'OR') {
   if (is_found) j++;
   is_found = 1;
   continue;
  }
  is_found = 1;
  if (! ref.field) return v_error("Missing field key during normal validation");
  var err = v_validate_buddy(form, ref.field, ref);

  if (err.length) {
   if (j <= fields.length && typeof(fields[j + 1] != 'object') && fields[j + 1] == 'OR') {
    hold_error = err;
   } else {
    if (hold_error) err = hold_error;
    for (var k = 0; k < err.length; k++) errors.push(err[k]);
    hold_error = '';
   }
  } else {
   hold_error = '';
  }
 }

 if (errors.length) {
  if (title) ERRORS.push(title);
  for (var j = 0; j < errors.length; j++) ERRORS.push(errors[j]);
 }

 var m;
 for (var j = 0; j < order.length; j++) {
  var field = order[j];
  if (! (m = field.match(/^group\s+(\w+)$/))) continue;
  if (errors.length == 0 || m[1].match(/^(field|order|title|validate_if)$/)) continue;
   EXTRA[m[1]] = val_hash[field];
 }

 if (ERRORS.length) return new ValidateError(ERRORS, EXTRA);
 return;
}

function v_check_conditional (form, ifs, N_level, ifs_match) {
 if (! N_level) N_level = 0;
 N_level++;

 if (! ifs) return v_error("Need reference passed to check_conditional");
 if (typeof(ifs) != 'object' || ! ifs.length) ifs = [ifs];

 var is_found = 1;
 var m;
 for (var i = 0; i < ifs.length; i++) {
  var ref = ifs[i];
  if (typeof(ref) != 'object') {
   if (ref == 'OR') {
    if (is_found) i++;
    is_found = 1;
    continue;
   } else {
    var field = ref;
    ref = {};
    if (m = field.match(/^(\s*!\s*)/)) {
     field = field.substring(m[1].length);
     ref.max_in_set = [0, field];
    } else {
     ref.required = 1;
    }
    ref.field = field;
   }
  }
  if (! is_found) break;

  var field = ref.field;
  if (! field) return v_error("Missing field key during validate_if");
  field = field.replace(/\$(\d+)/g, function (all, N) {
   return (typeof(ifs_match) != 'object' || typeof(ifs_match[N]) == 'undefined') ? '' : ifs_match[N];
  });

  v_clean_field_val(ref);
  var err = v_validate_buddy(form, field, ref, N_level);
  if (err.length) is_found = 0;
 }
 return is_found;
}

function v_filter_types (type, types) {
 var values = [];
 var regexp = new RegExp('^'+type+'_?\\d*$');
 for (var i = 0; i < types.length; i++)
  if (types[i].match(regexp)) values.push(types[i]);
 return values;
}

function v_add_error (errors,field,type,field_val,ifs_match,form) {
 errors.push([field, type, field_val, ifs_match]);
 if (field_val.clear_on_error) {
  var el = form[field];
  if (el && el.type && el.type.match(/(hidden|password|text|textarea|submit)/)) el.value = '';
 }
 return errors;
}

function v_field_order (field_val) {
 var o = [];
 for (var k in field_val) if (! k.match(/^(extend|field|name)$/) && ! k.match(/_error$/)) o.push(k);
 return o.sort();
}

function v_validate_buddy (form, field, field_val, N_level, ifs_match) {
 var errors = [];
 if (! N_level) N_level = 0;
 if (++N_level > 10) { v_error("Max dependency level reached " + N_level); return errors }
 if (! form.elements || field_val.exclude_js) return errors;
 var types = field_val.order || v_field_order(field_val);

 var m;
 if (m = field.match(/^(!\s*|)m([^\s\w])(.*)\2([eigsmx]*)$/)) {
  var not = m[1];
  var pat = m[3];
  var opt = m[4];
  if (opt.indexOf('e') != -1) { v_error("The e option cannot be used on field "+field); return errors }
  opt = opt.replace(/[sg]/g,'');
  var reg = new RegExp(pat, opt);

  for (var i = 0; i < form.elements.length; i++) {
   var _field = form.elements[i].name;
   if (! _field) continue;
   if ( (not && ! (m = _field.match(reg))) || (m = _field.match(reg))) {
    var err = v_validate_buddy(form, _field, field_val, N_level, m);
    for (var j = 0; j < err.length; j++) errors.push(err[j]);
   }
  }
  return errors;
 }

 var _value   = v_get_form_value(form[field]);
 var modified = 0;

 if (typeof(field_val['default']) != 'undefined'
     && (typeof(_value) == 'undefined'
         || (typeof(_value) == 'object' && _value.length == 0)
         || ! _value.length)) {
  _value = field_val['default'];
  modified = 1;
 }

 var values   = (typeof(_value) == 'object') ? _value : [_value];
 var n_values = (typeof(_value) == 'undefined') ? 0 : values.length;

 for (var i = 0; i < values.length; i++) {
  if (typeof(values[i]) == 'undefined') continue;
  var orig = values[i];
  if (! field_val.do_not_trim)      values[i] = values[i].replace(/^\s+/,'').replace(/\s+$/,'');
  if (field_val.trim_control_chars) values[i] = values[i].replace(/\t/g,' ').replace(/[\x00-\x1F]/g,'');
  if (field_val.to_upper_case) values[i] = values[i].toUpperCase();
  if (field_val.to_lower_case) values[i] = values[i].toLowerCase();

  var tests = v_filter_types('replace', types);
  for (var k = 0; k < tests.length; k++) {
   var ref = field_val[tests[k]];
   ref = (typeof(ref) == 'object') ? ref : ref.split(/\s*\|\|\s*/);
   for (var j = 0; j < ref.length; j++) {
    if (! (m = ref[j].match(/^\s*s([^\s\w])(.+)\1(.*)\1([eigmx]*)$/)))
     return v_error("Not sure how to parse that replace "+ref[j]);
    var pat  = m[2];
    var swap = m[3];
    var opt  = m[4];
    if (opt.indexOf('e') != -1) { v_error("The e option cannot be used on field "+field+", replace "+tests[i]); return errors }
    var regexp = new RegExp(pat, opt);
    values[i] = values[i].replace(regexp, swap);
   }
  }

  if (orig != values[i]) modified = 1;
 }
 if (modified && n_values == 1) {
  var el = form[field];
  if (el && el.type && el.type.match(/(hidden|password|text|textarea|submit)/)) el.value = values[0];
 }

 var needs_val = 0;
 var n_vif = 0;
 var tests = v_filter_types('validate_if', types);
 for (var i = 0; i < tests.length; i++) {
  n_vif++;
  var ifs = field_val[tests[i]];
  var ret = v_check_conditional(form, ifs, N_level, ifs_match);
  if (ret) needs_val++;
 }
 if (! needs_val && n_vif) return errors;

 var is_required = '';
 var tests = v_filter_types('required', types);
 for (var i = 0; i < tests.length; i++) {
  if (! field_val[tests[i]] || field_val[tests[i]] == 0) continue;
  is_required = tests[i];
  break;
 }
 if (! is_required) {
  var tests = v_filter_types('required_if', types);
  for (var i = 0; i < tests.length; i++) {
   var ifs = field_val[tests[i]];
   if (! v_check_conditional(form, ifs, N_level, ifs_match)) continue;
   is_required = tests[i];
   break;
  }
 }
 if (is_required) {
  var found;
  for (var i = 0; i < values.length; i++) {
   if (values[i].length) {
    found = 1;
    break;
   }
  }
  if (! found) return v_add_error(errors, field, is_required, field_val, ifs_match, form);
 }

 if (field_val.min_values && n_values < field_val.min_values)
  return v_add_error(errors, field, 'min_values', field_val, ifs_match, form);

 if (typeof(field_val.max_values) == 'undefined') field_val.max_values = 1;
 if (field_val.max_values && n_values > field_val.max_values)
  return v_add_error(errors, field, 'max_values', field_val, ifs_match, form);

 for (var h = 0; h < 2 ; h++) {
  var minmax = (h == 0) ? 'min' : 'max';
  var tests = v_filter_types(minmax+'_in_set', types);
  for (var i = 0; i < tests.length; i++) {
   var a = field_val[tests[i]];
   var n = a[0];
   for (var k = 1; k < a.length; k++) {
    var _value = v_get_form_value(form[a[k]]);
    var _values;
    if (typeof(_value) == 'undefined') continue;
    _values = (typeof(_value) == 'object') ? _value : [_value];
    for (var l = 0; l < _values.length; l++) {
     var _value = _values[l];
     if (typeof(_value) != 'undefined' && _value.length) n--;
    }
   }
   if (   (minmax == 'min' && n > 0)
     || (minmax == 'max' && n < 0)) {
    v_add_error(errors, field, tests[i], field_val, ifs_match, form);
    return errors;
   }
  }
 }

 for (var i = 0; i < types.length; i++) {
  var type = types[i];
  var _fv  = field_val[type];
  for (var n = 0; n < values.length; n++) {
   var value = values[n];

   if (type.match(/^enum\d*$/)) {
    var is_found = 0;
    for (var j = 0; j < _fv.length; j++) if (value == _fv[j]) { is_found = 1; break }
    if (! is_found) v_add_error(errors, field, type, field_val, ifs_match, form);
   }

   if (type.match(/^equals\d*$/)) {
    var not = _fv.match(/^!\s*/);
    if (not) _fv = _fv.substring(not[0].length);
    var success = 0;
    if (m = _fv.match(/^([\"\'])(.*)\1$/)) {
     if (value == m[2]) success = 1;
    } else {
     var value2 = v_get_form_value(form[_fv]);
     if (typeof(value2) == 'undefined') value2 = '';
     if (value == value2) success = 1;
    }
    if (not && success || ! not && ! success)
     v_add_error(errors, field, type, field_val, ifs_match, form);
   }

   if (type == 'min_len' && value.length < _fv) v_add_error(errors, field, 'min_len', field_val, ifs_match, form);
   if (type == 'max_len' && value.length > _fv) v_add_error(errors, field, 'max_len', field_val, ifs_match, form);

   if (type.match(/^match\d*$/)) {
    for (var j = 0; j < _fv.length; j++) {
     if (typeof(_fv[j]) == 'string') continue;
     var not = (j > 0 && typeof(_fv[j-1]) == 'string' && _fv[j-1] == '!') ? 1 : 0;
     if (   (  not &&   value.match(_fv[j]))
         || (! not && ! value.match(_fv[j]))) v_add_error(errors, field, type, field_val, ifs_match, form);
    }
   }

   if (type.match(/^compare\d*$/)) {
    for (var j = 0; j < _fv.length; j++) {
     var comp = _fv[j];
     if (! comp) continue;
     var hold = false;
     var copy = value;
     if (m = comp.match(/^\s*(>|<|[><!=]=)\s*([\d\.\-]+)\s*$/)) {
      if (! copy) copy = 0;
      copy *= 1;
      if      (m[1] == '>' ) hold = (copy >  m[2])
      else if (m[1] == '<' ) hold = (copy <  m[2])
      else if (m[1] == '>=') hold = (copy >= m[2])
      else if (m[1] == '<=') hold = (copy <= m[2])
      else if (m[1] == '!=') hold = (copy != m[2])
      else if (m[1] == '==') hold = (copy == m[2])
     } else if (m = comp.match(/^\s*(eq|ne|gt|ge|lt|le)\s+(.+?)\s*$/)) {
      if (     m[2].match(/^\"/)) m[2] = m[2].replace(/^"(.*)"$/,'$1');
      else if (m[2].match(/^\'/)) m[2] = m[2].replace(/^'(.*)'$/,'$1');
      if      (m[1] == 'gt') hold = (copy >  m[2])
      else if (m[1] == 'lt') hold = (copy <  m[2])
      else if (m[1] == 'ge') hold = (copy >= m[2])
      else if (m[1] == 'le') hold = (copy <= m[2])
      else if (m[1] == 'ne') hold = (copy != m[2])
      else if (m[1] == 'eq') hold = (copy == m[2])
     } else {
      v_error("Not sure how to compare \""+comp+"\"");
      return errors;
     }
     if (! hold) v_add_error(errors, field, type, field_val, ifs_match, form);
    }
   }

   if (type.match(/^type\d*$/))
    if (! v_check_type(value, _fv, field, form))
     v_add_error(errors, field, type, field_val, ifs_match, form);
  }

  // the js is evaluated and should return 1 for success
  // or 0 for failure - the variables field, value, and field_val (the hash) are available
  if (type.match(/^custom_js\d*$/)) {
   var value = values.length == 1 ? values[0] : values;
   if (typeof(_fv) == 'function'
       ? ! _fv({'value':value, 'field_val':field_val, 'form':form, 'key':field_val.field})
       : ! eval(_fv)) v_add_error(errors, field, type, field_val, ifs_match, form);
  }
 }

 return errors;
}

function v_check_type (value, type, field, form) {
 var m;
 type = type.toUpperCase();

 if (type == 'EMAIL') {
  if (! value) return 0;
  if (! (m = value.match(/^(.+)@(.+?)$/))) return 0;
  if (m[1].length > 60)  return 0;
  if (m[2].length > 100) return 0;
  if (! v_check_type(m[2],'DOMAIN') && ! v_check_type(m[2],'IP')) return 0;
  if (! v_check_type(m[1],'LOCAL_PART')) return 0;

 } else if (type == 'LOCAL_PART') {
  if (typeof(value) == 'undefined' || ! value.length) return 0;
  if (typeof(v_local_part) != 'undefined') return (value.match(v_local_part) ? 1 : 0);
  if (value.match(/[^a-z0-9.\-!&+]/)) return 0;
  if (value.match(/^[.\-]/))          return 0;
  if (value.match(/[.\-&]$/))         return 0;
  if (value.match(/(\.-|-\.|\.\.)/))  return 0;

 } else if (type == 'IP') {
  if (! value) return 0;
  var dig = value.split(/\./);
  if (dig.length != 4) return 0;
  for (var i = 0; i < 4; i++)
   if (typeof(dig[i]) == 'undefined' || dig[i].match(/\D/) || dig[i] > 255) return 0;

 } else if (type == 'DOMAIN') {
  if (! value) return 0;
  if (! value.match(/^[a-z0-9.-]{4,255}$/)) return 0;
  if (value.match(/^[.\-]/))             return 0;
  if (value.match(/(\.-|-\.|\.\.)/))  return 0;
  if (! (m = value.match(/\.([a-z]+)$/))) return 0;
  value = value.substring(0,value.lastIndexOf('.'));
  if (m[1] == 'name') {
   if (! value.match(/^[a-z0-9][a-z0-9\-]{0,62}\.[a-z0-9][a-z0-9\-]{0,62}$/)) return 0;
  } else
   if (! value.match(/^([a-z0-9][a-z0-9\-]{0,62}\.)*[a-z0-9][a-z0-9\-]{0,62}$/)) return 0;

 } else if (type == 'URL') {
  if (! value) return 0;
  if (! (m = value.match(/^https?:\/\/([^\/]+)/i))) return 0;
  value = value.substring(m[0].length);
  var dom = m[1].replace(/:\d+$/).replace(/\.$/);
  if (! v_check_type(dom,'DOMAIN') && ! v_check_type(m[1],'IP')) return 0;
  if (value && ! v_check_type(value,'URI')) return 0;

 } else if (type == 'URI') {
  if (! value) return 0;
  if (value.match(/\s/)) return 0;

 } else if (type == 'CC') {
  if (! value) return 0;
  if (value.match(/[^\d\- ]/)) return 0;
  value = value.replace(/[\- ]/g, '');
  if (value.length > 16 || value.length < 13) return 0;
  // mod10
  var sum = 0;
  var swc = 0;
  for (var i = value.length - 1; i >= 0; i--) {
   if (++swc > 2) swc = 1;
   var y = value.charAt(i) * swc;
   if (y > 9) y -= 9;
   sum += y;
  }
  if (sum % 10) return 0;
 }

 return 1;
}

function v_get_form_value (el) {
 if (! el) return '';
 if (el.disabled) return '';
 var type = el.type ? el.type.toLowerCase() : '';
 if (el.length && type != 'select-one') {
  var a = [];
  for (var j=0;j<el.length;j++) {
   if (type.indexOf('multiple') != -1) {
    if (el[j].selected) a.push(el[j].value);
   } else {
    if (el[j].checked)  a.push(v_get_form_value(el[j]));
   }
  }
  if (a.length == 0) return '';
  if (a.length == 1) return a[0];
  return a;
 }
 if (! type) return '';
 if (type.match(/(hidden|password|text|textarea|submit)/)) return el.value;
 if (type.indexOf('select') != -1) {
  if (! el.length) return '';
  return el[el.selectedIndex].value;
 }
 if (type == 'checkbox' || type == 'radio') return el.checked ? el.value : '';
 if (type == 'file') return el.value;

 alert('Unknown form type for '+el.name+': '+type);
 return '';
}

function v_find_val () {
 var key = arguments[0];
 for (var i = 1; i < arguments.length; i++) {
  if (typeof(arguments[i]) == 'string') return arguments[i];
  if (typeof(arguments[i]) == 'undefined') continue;
  if (typeof(arguments[i][key]) != 'undefined') return arguments[i][key];
 }
 return '';
}

function v_get_error_text (err, extra1, extra2) {
 var field     = err[0];
 var type      = err[1];
 var field_val = err[2];
 var ifs_match = err[3];
 var m;

 var dig = (m = type.match(/(_?\d+)$/)) ? m[1] : '';
 var type_lc = type.toLowerCase();
 var v = field_val[type + dig];

 if (field_val.delegate_error) {
  field = field_val.delegate_error;
  field = field.replace(/\$(\d+)/g, function (all, N) {
   if (typeof(ifs_match) != 'object'
     || typeof(ifs_match[N]) == 'undefined') return ''
   return ifs_match[N];
  });
 }

 var name = field_val.name || "The field " +field;
 name = name.replace(/\$(\d+)/g, function (all, N) {
  if (typeof(ifs_match) != 'object'
    || typeof(ifs_match[N]) == 'undefined') return ''
  return ifs_match[N];
 });

 var msg = v_find_val(type + '_error', extra1, extra2);
 if (! msg) {
   if (dig.length) msg = field_val[type + dig + '_error'];
   if (! msg)      msg = field_val[type +       '_error'];
 }
 if (msg) {
  msg = msg.replace(/\$(\d+)/g, function (all, N) {
   if (typeof(ifs_match) != 'object' || typeof(ifs_match[N]) == 'undefined') return '';
   return ifs_match[N];
  });
  msg = msg.replace(/\$field/g, field);
  msg = msg.replace(/\$name/g, name);
  if (v && typeof(v) == 'string') msg = msg.replace(/\$value/g, v);
  return msg;
 }

 if (type == 'equals') {
  var field2 = field_val["equals" + dig];
  var name2  = field_val["equals" +dig+ "_name"];
  if (! name2) name2 = "the field " +field2;
  name2 = name2.replace(/\$(\d+)/g, function (all, N) {
   return (typeof(ifs_match) != 'object' || typeof(ifs_match[N]) == 'undefined') ? '' : ifs_match[N];
  });
  return name + " did not equal " + name2 +".";
 }
 if (type == 'min_in_set') return "Not enough fields were chosen from the set ("+v[0]+' of '+v.join(", ").replace(/^\d+,\s*/,'')+")";
 if (type == 'max_in_set') return "Too many fields were chosen from the set ("  +v[0]+' of '+v.join(", ").replace(/^\d+,\s*/,'')+")";

 return name + (
  (type == 'required' || type == 'required_if') ? " is required."
  : type == 'match'      ? " contains invalid characters."
  : type == 'compare'    ? " did not fit comparison."
  : type == 'custom_js'  ? " did not match custom_js"+dig+" check."
  : type == 'enum'       ? " is not in the given list."
  : type == 'min_values' ? " had less than "+v+" value"+(v == 1 ? '' : 's')+"."
  : type == 'max_values' ? " had more than "+v+" value"+(v == 1 ? '' : 's')+"."
  : type == 'min_len'    ? " was less than "+v+" character"+(v == 1 ? '' : 's')+"."
  : type == 'max_len'    ? " was more than "+v+" character"+(v == 1 ? '' : 's')+"."
  : type == 'type'       ? " did not match type "+v+"."
  : alert("Missing error on field "+field+" for type "+type+dig));
}

//

function eob_as_string (extra) {
 var joiner = v_find_val('as_string_join',   extra, this.extra, '\n');
 var header = v_find_val('as_string_header', extra, this.extra, '');
 var footer = v_find_val('as_string_footer', extra, this.extra, '');
 return header + this.as_array(extra).join(joiner) + footer;
}

function eob_as_array (extra) {
 var errors = this.errors;
 var title  = v_find_val('as_array_title', extra, this.extra, 'Please correct the following items:');

 var has_headings;
 if (title) has_headings = 1;
 else for (var i = 0; i < errors.length; i++) if (typeof(errors[i]) == 'string') has_headings = 1;

 var prefix = v_find_val('as_array_prefix', extra, this.extra, (has_headings ? '  ' : ''));

 var arr = [];
 if (title && title.length) arr.push(title);

 var found = {};
 for (var i = 0; i < errors.length; i++) {
  if (typeof(errors[i]) == 'string') {
   arr.push(errors[i]);
   found = {};
  } else {
   var text = v_get_error_text(errors[i], extra, this.extra);
   if (found[text]) continue;
   found[text] = 1;
   arr.push(prefix + text);
  }
 }

 return arr;
}

function eob_as_hash (extra) {
 var errors = this.errors;
 var suffix = v_find_val('as_hash_suffix', extra, this.extra, '_error');
 var joiner = v_find_val('as_hash_join',   extra, this.extra, '<br/>');

 var found = {};
 var ret   = {};
 for (var i = 0; i < errors.length; i++) {
  if (typeof(errors[i]) == 'string') continue;
  if (! errors[i].length) continue;

  var field     = errors[i][0];
  var type      = errors[i][1];
  var field_val = errors[i][2];
  var ifs_match = errors[i][3];

  if (! field) return alert("Missing field name");
  if (field_val['delegate_error']) {
   field = field_val['delegate_error'];
   field = field.replace(/\$(\d+)/g, function (all, N) {
    if (typeof(ifs_match) != 'object'
        || typeof(ifs_match[N]) == 'undefined') return ''
    return ifs_match[N];
   });
  }

  var text = v_get_error_text(errors[i], extra, this.extra);
  if (! found[field]) found[field] = {};
  if (found[field][text]) continue;
  found[field][text] = 1;

  field += suffix;
  if (! ret[field]) ret[field] = [];
  ret[field].push(text);
 }

 if (joiner) {
  var header = v_find_val('as_hash_header', extra, this.extra, '');
  var footer = v_find_val('as_hash_footer', extra, this.extra, '');
  for (var key in ret) {
   if (key == 'extend') continue; // Protoype Array()
   ret[key] = header + ret[key].join(joiner) + footer;
  }
 }

 return ret;
}

function eob_first_field () {
 for (var i = 0; i < this.errors.length; i++) {
  if (typeof(this.errors[i]) != 'object') continue;
  if (! this.errors[i][0]) continue;
  return this.errors[i][0];
 }
 return;
}

//

document.validate = function (form, val_hash) {
 val_hash = document.load_val_hash(form, val_hash);
 if (typeof(val_hash) == 'undefined') return true;

 for (var key in v_did_inline) {
  if (key == 'extend') continue; // Protoype Array()
  v_inline_error_clear(key, val_hash, form);
 }

 var err_obj = v_validate(form, val_hash);
 if (! err_obj) return true;

 var field = err_obj.first_field();
 if (field && form[field]) {
   if (form[field].focus) form[field].focus();
   else if (form[field].length && form[field][0].focus) form[field][0].focus();
 }

 if (! val_hash['group no_inline']) {
  var hash = err_obj.as_hash({as_hash_suffix:""});
  for (var key in hash) {
   if (key == 'extend') continue; // Protoype Array()
   v_inline_error_set(key, hash[key], val_hash, form);
  }
 }

 if (! val_hash['group no_confirm']) {
  return confirm(err_obj.as_string()) ? false : true;
 } else if (! val_hash['group no_alert']) {
  alert(err_obj.as_string());
  return false;
 } else if (! val_hash['group no_inline']) {
  return false;
 } else {
  return true;
 }
}

document.load_val_hash = function (form, val_hash) {
 if (! form) return alert('Missing form or form name');
 if (typeof(form) == 'string') {
  if (! document[form]) return alert('No form by name '+form);
  form = document[form];
 }

 if (form.val_hash) return form.val_hash;

 if (typeof(val_hash) != 'object') {
  if (typeof(val_hash) == 'function') {
   val_hash = val_hash(formname);
  } else if (typeof(val_hash) == 'undefined') {
   var el;
   if (typeof(document.validation) != 'undefined') {
    val_hash = document.validation;
   } else if (el = document.getElementById('validation')) {
    val_hash = el.innerHTML.replace(/&lt;/ig,'<').replace(/&gt;/ig,'>').replace(/&amp;/ig,'&');
   } else {
    var order = [];
    var str   = '';
    var yaml  = form.getAttribute('validation');
    if (yaml) {
     if (m = yaml.match(/^( +)/)) yaml = yaml.replace(new RegExp('^'+m[1], 'g'), '');
     yaml = yaml.replace(/\s*$/,'\n');
     str += yaml;
    }
    var m;
    for (var i = 0; i < form.elements.length; i++) {
     var name = form.elements[i].name;
     var yaml = form.elements[i].getAttribute('validation');
     if (! name || ! yaml) continue;
     yaml = yaml.replace(/\s*$/,'\n').replace(/^(.)/mg,' $1').replace(/^( *[^\s&*\[\{])/,'\n$1');
     str += name +':' + yaml;
     order.push(name);
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
   val_hash = val_hash[0];
  }
 }

 form.val_hash = val_hash;
 return form.val_hash;
}

document.check_form = function (form, val_hash) {
 if (! form) return alert('Missing form or form name');
 if (typeof(form) == 'string') {
  if (! document[form]) return alert('No form by name '+form);
  form = document[form];
 }

 val_hash = document.load_val_hash(form, val_hash);
 if (! val_hash) return;

 var types = val_hash['group onevent'] || {submit:1};
 if (typeof(types) == 'string') types = types.split(/\s*,\s*/);
 if (typeof(types.length) != 'undefined') {
  var t = {};
  for (var i = 0; i < types.length; i++) t[types[i]] = 1;
  types = t;
 }
 val_hash['group onevent'] = types;

 if (types.change || types.blur) {
  var clean = v_clean_val_hash(val_hash);
  if (clean.error) return clean.error;
  var h = {};
  _add = function (k, v) { if (! h[k]) h[k] = []; h[k].push(v) };
  for (var i = 0; i < clean.fields.length; i++) {
   _add(clean.fields[i].field, clean.fields[i]);
   for (var j in clean.fields[i].deps) if (j != clean.fields[i].field) _add(j, clean.fields[i]);
  }
  for (var k in h) {
   if (k == 'extend') continue; // Protoype Array()
   var el = form[k];
   if (! el) return v_error("No form element by the name "+k);
   v_el_attach(el, h[k], form, val_hash);
  }
 }

 if (types.submit) {
  var orig_submit = form.onsubmit || function () { return true };
  form.onsubmit = function (e) { return document.validate(this) && orig_submit(e, this) };
 }
}

function v_el_attach (el, fvs, form, val_hash) {
 if (! el.type) {
  if (el.length) for (var i = 0; i < el.length; i++) v_el_attach(el[i], fvs, form, val_hash);
  return;
 }
 var types = val_hash['group onevent'];
 var func = function () {
  var e = [];
  var f = {};
  for (var i = 0; i < fvs.length; i++) {
   var field_val = fvs[i];
   var _e = v_validate_buddy(form, field_val.field, field_val);
   for (var j = 0; j < _e.length; j++) e.push(_e[j]);
   f[field_val.delegate_error || field_val.field] = 1;
  }
  if (! e.length) {
    for (var k in f) v_inline_error_clear(k, val_hash, form);
    return;
  }
  e = new ValidateError(e, {});
  e = e.as_hash({as_hash_suffix:"", first_only:(val_hash['group first_only']?1:0)});
  for (var k in e) {
   if (k == 'extend') continue; // Protoype Array()
   v_inline_error_set(k, e[k], val_hash, form);
  }
 };
 if (types.blur) el.onblur = func;
 if (types.change) {
  if (el.type.match(/(password|text|textarea)/)) el.onkeyup = func;
  else if (el.type.match(/(checkbox|radio)/)) el.onclick = func;
  else if (el.type.match(/(select)/)) el.onchange = func;
 }
}

function v_inline_error_clear (key, val_hash, form) {
   delete(v_did_inline[key]);
   var f = val_hash['group clear_hook'] || document.validate_clear_hook;
   if (typeof(f) == 'function') if (f(key, val_hash, form)) return 1;
   var el = document.getElementById(key + v_find_val('as_hash_suffix', val_hash, '_error'));
   if (el) el.innerHTML = '';
}

function v_inline_error_set (key, val, val_hash, form) {
   v_did_inline[key] = 1;
   var f = val_hash['group set_hook'] || document.validate_set_hook;
   if (typeof(f) == 'function') if(f(key, val, val_hash, form)) return 1;
   var el = document.getElementById(key + v_find_val('as_hash_suffix', val_hash, '_error'));
   if (el) el.innerHTML = val;
}
