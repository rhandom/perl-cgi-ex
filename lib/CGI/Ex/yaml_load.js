<script>
var yaml = "\n\
- general as_array_title: 'PLEASE FIX THE FOLLOWING:'\n\
  general as_array_prefix: '  - '\n\
  general raise_error: 0\n\
  general required_error: '$name IS REALLY REQUIRED'\n\
  general max_len_error: '$name MUST BE SHORTER THAN $value CHARACTERS'\n\
  general min_len_error: '$name MUST BE AT LEAST $value CHARACTERS LONG'\n\
  general as_string_header: |\n\
   -------------------------\n\
  general as_string_footer: |\n\
   \n\
   -------------------------\n\
  general as_string_join: |\n\
    <br>\n\
- group title: '-- User information --'\n\
  group validate_if:\n\
     - field: email\n\
       match: m/^\\w+$/\n\
       maxlen: 100\n\
     - OR\n\
     - field: email\n\
       equals: email2\n\
\n\
  group order: [username, email, email2, i_agree]\n\
  group fields:\n\
   -\n\
    field: m/(u..)/\n\
    match: m/^$/\n\
    match_error: '$name didn''t match ($1)'\n\
   - &user\n\
    field: username\n\
    name: Username\n\
    max_len: 30\n\
    min_len:  3\n\
    match:   m/^\\w+$/\n\
    required: 1\n\
   -\n\
    field: email\n\
    name:    Email\n\
    maxlen:  100\n\
    match:   m/^[\\w\.]+\@(\\w+\.)+\\w+$/\n\
    required: 1\n\
   -\n\
    field: email2\n\
    validate_if: email\n\
    equals: email\n\
   -\n\
    field: i_agree\n\
    required: 1\n\
    required_error: You must agree to the terms of service.\n\
- group title: '-- Other information --'\n\
  group fields:\n\
   - *user\n\
";

var yaml2 = "\n\
foo: bar\n\
baz: bee\n\
";

document.yaml_load = function (text) {
  var yaml = new YAML();
  return yaml.parse(text);
}

function YAML () {
  this.parse            = yaml_parse;
  this.error            = yaml_error;
  this.warn             = yaml_warn;
  this.parse_throwaway  = yaml_parse_throwaway;
  this.parse_node       = yaml_parse_node;
  this.parse_next_line  = yaml_parse_next_line;
  this.parse_qualifiers = yaml_parse_qualifiers;
  this.parse_explicit   = yaml_parse_explicit;
  this.parse_implicit   = yaml_parse_implicit;
  this.parse_map        = yaml_parse_map;
  this.parse_seq        = yaml_parse_seq;
  this.parse_inline     = yaml_parse_inline;
}

function yaml_error (err) {
  err += '\nDocument: '+this.document+'\n';
  err += '\nLine: '    +this.line    +'\n';
  alert(err);
  return;
}

function yaml_warn (err) {
  alert(err);
  return;
}

function yaml_parse (text) {

  // translate line endings down to \012
  text = text.replace(new RegExp('\015\012','g'), '\012');
  text = text.replace(new RegExp('\015','g'), '\012');
  if (text.match('[\\x00-\\x08\\x0B-\\x0D\\x0E-\\x1F]'))
    return this.error("Bad characters found");
  if (text.length && ! text.match('\012$'))
    text += '\012';

  // storage of yaml lines and resulting data structure
  this.line      = 1;
  this.lines     = text.split("\012");
  this.document  = 0;
  this.documents = new Array();

  // get rid of leading space and add first line as needed
  this.parse_throwaway();
  if (! this.eoy && ! this.lines[0].match('^---(\\s|$)')) {
    this.lines.unshift('--- #YAML:1.0');
    this.line --;
  }

  // loop looking for data structures
  while (! this.eoy) {
    this.anchors = new Array();
    this.offset  = new Array();
    this.options = new Array();
    this.document  ++;
    this.done      = 0;
    this.level     = 0;
    this.offset[0] = -1;
    this.preface   = '';
    this.content   = '';
    this.indent    = -1;

    // parse headers and options
    var m = this.lines[0].match('---\\s*(.*)$')
    if (! m) return this.error("Missing YAML separator\n("+this.lines[0]+")");
    var words = m[1].split("\\s+");
    while (words.length && (m = words[0].match('^#(\\w+):(\\S.*)$'))) {
      words.shift();
      if (this.options[m[1]]) {
        yaml.warn("Parse warn - multiple options " + m[1]);
        continue;
      }
      this.options[m[1]] = m[2];
    }

    if (this.options['YAML'] && this.options['YAML'] != '1.0')
      return this.error('Bad YAML version number - must be 1.0');
    if (this.options['TAB'] && ! this.options['TAB'].match('^(NONE|\\d+)(:HARD)?$'))
      return this.error('Unrecognized TAB policy');

    // add the node
    this.documents.push(this.parse_node());
  }

  // return this.error(this.documents[0]['foo']);
  return this.documents;
}

// strip leading lines with comments
function yaml_parse_throwaway () {
  while (this.lines.length && this.lines[0].match('^\\s*(#|$)')) {
    this.lines.shift();
    this.line ++;
  }
  this.eoy = this.done = ! this.lines.length;
}

// get the node
function yaml_parse_node (no_next) {
  if (! no_next) this.parse_next_line(2); // COLLECTION

  var preface   = this.preface;
  this.preface  = '';
  var node      = '';
  var type      = '';
  var indicator = '';
  var escape    = '';
  var chomp     = '';

  var info     = this.parse_qualifiers(preface);
  var anchor   = info[0];
  var alias    = info[1];
  var explicit = info[2];
  var implicit = info[3];
  var yclass   = info[4];
  preface      = info[5];


  // return aliases now
  if (alias) {
    if (! this.anchors[alias]) return this.error("Parse error - missing alias: "+alias);
    return this.anchors[alias];
  }

  // see if this is a literal or an unfold block
  this.inline = '';
  if (preface.length) {
    m = preface.match('^([>\\|])([+\\-]?)\\d*\\s*');
    if (m) {
      indicator   = m[1];
      chomp       = m[2];
      preface     = preface.substring(0,m[0].length);
    } else {
      this.inline = preface;
      preface     = '';
    }
  }

  
  if (this.inline.length) {
    node = this.parse_inline(1, implicit, explicit, yclass);
    if (this.inline.length) return this.error("Parse error - must be single line ("+this.inline+')');
  } else {
    this.level ++;
    // block items
    if (indicator) {
      node = '';
      while (! this.done && this.indent == this.offset[this.level]) {
        node += this.content + '\n';
        this.parse_next_line(1); // LEAF
      }
      if (indicator == '>') {
        //  node = node.replace(new RegExp('^(\\S.*)\n(?=\\S)', 'gm'), '$1');
        node = node.replace(new RegExp('^(\\S.*)\n(\n+\\S)','gm'), '$1$2');
      }
      if (! chomp || chomp == '-') node = node.replace('\n+$','');
      if (chomp && chomp == '+')   node += '\n';
      if (implicit) node = this.parse_implicit(node);

    } else {
      if (! this.offset[this.level]) this.offset[this.level] = 0;
      if (this.indent == this.offset[this.level]) {
        if (this.content.match('^-( |$)')) {
          node = this.parse_seq(anchor);
        } else if (this.content.match('(^\\?|:( |$))')) {
          node = this.parse_map(anchor);
        } else if (preface.match('^\\s*$')) {
          node = ''; //this.parse_implicit('');
        } else {
          return this.error('Parse error - bad node +('+this.content+')('+preface+')');
        }
      } else {
        node = '';
      }
    }
    this.level --
  }
  //  this.offset[this.offset.length - 1] = this.level;
  this.offset = this.offset.splice(0, this.level + 1);

  if (explicit) {
    if (yclass) return this.error("Parse error - classes not supported");
    else node = this.parse_explicit(node, explicit);
  }
  if (anchor) this.anchors[anchor] = node;

  return node;
}

function yaml_parse_next_line (type) {
  var m;
  var level  = this.level;
  var offset = this.offset[level];

  if (offset == undefined) return this.error("Parse error - Bad level " + level);

  // done with the current line - get the next
  // remove following commented lines
  this.lines.shift();
  this.line ++;
  this.eoy = this.done = ! this.lines.length;
  if (this.eoy) return;
  this.parse_throwaway();
  if (this.eoy) return;

  // Determine the offset for a new leaf node
  if (this.preface && (m = this.preface.match('[>\\|][+\\-]?(\\d*)\\s*$'))) {
    if (m[1].length && m[1] == '0') return this.error("Parse error zero indent");
    type = 1;
    if (m[1].length) {
      this.offset[level + 1] = offset + m[1];
    } else if (m = this.lines[0].match('^( *)\\S') && m[1].length > offset) {
      this.offset[level + 1] = m[1].length;
    } else {
      this.offset[level + 1] = offset + 1;
    }
    level ++;
    offset = this.offset[level];
  }

  // COLLECTION
  if (type == 2 && this.preface.match('^\\s*(!\\S*|&\\S+)*\\s*$')) {
    m = this.lines[0].match('^( *)\\S');
    if (! m) return this.error("Missing leading space on line "+this.lines[0]);
    this.offset[level + 1] = (m[1].length > offset) ? m[1].length : offset + 1;
    offset = this.offset[++ level];

  // LEAF
  } else if (type == 1) {
    // skip blank lines and comment lines
    while (this.lines.length && this.lines[0].match('^\\s*(#|$)')) {
      m = this.lines[0].match('^( *)');
      if (! m) return this.error("Missing leading space on comment " + this.lines[0]);
      if (m[1].length > offset) break;
      this.lines.shift();
      this.line ++;
    }
    this.eoy = this.done = ! this.lines.length;      
  } else {
    this.parse_throwaway();
  }

  if (this.eoy) return;
  if (this.lines[0].match('^---(\\s|$)')) {
    this.done = 1;
    return;
  }

  if (type == 1 && (m = this.lines[0].match('^ {'+offset+'}(.*)$'))) {
    this.indent = offset;
    this.content = m[1];
  } else if (this.lines[0].match('^\\s*$')) {
    this.indent = offset;
    this.content = '';
  } else {
    m = this.lines[0].match('^( *)(\\S.*)$');
    // # yaml.warn("   indent(${\length($1)})  offsets(@{$o->{offset}}) \n");
    var len = (m) ? m[1].length : 0;
    while (this.offset[level] > len) level --;
    if (this.offset[level] != len)
      return this.error("Parse error inconsitent indentation:\n"
                        + '(this.lines[0]: '+this.lines[0]+', len: '+len+', level: '+level+', this.offset[level]: '+this.offset[level]+')\n');
    
    this.indent  = len;
    this.content = m ? m[2] : '';
  }

  if (this.indent - offset > 1)
    return this.error("Parse error - indentation");

  return;
}

function yaml_parse_qualifiers (preface) {
  var info = new Array();
  // 0 = anchor
  // 1 = alias
  // 2 = explicit
  // 3 = implicit
  // 4 = class - not used for now
  // 5 = preface

  var m;
  while (preface.match('^[&\\*!]')) {
    // explicit, implicit
    if (m = preface.match('^\!(\\S*)\\s*')) {
      preface = preface.substring(m[0].length);
      if (m[1].length) info[2] = m[1];
      else info[3] = 1;
    // anchor, alias
    } else if (m = preface.match('^([&\\*])([^ ,:]+)\s*')) {
      preface = preface.substring(m[0].length);
      if (! m[2].match('^\\w+$')) return this.error("Bad name "+m[2]);
      if (info[0] || info[1]) return this.error("Already found anchor or alias "+m[2]);
      if (m[1] == '&') info[0] = m[2];
      if (m[1] == '*') info[1] = m[2];
    }
  }

  info[5] = preface;
  return info;
}

function yaml_parse_explicit (node, explicit) {
  var m;
  if (m = explicit.match('^(int|float|bool|date|time|datetime|binary)$')) {
    //return this.error("No handler yet for explict " + m[1]);
    // just won't check types for now
    return node;
  } else if (m = explicit.match('^perl/(glob|regexp|code|ref):(\\w(\\w|::)*)?$')) {
    return this.error("No handler yet for perltype " + m[1]);
  } else if (m = explicit.match('^perl/(\\@|\\$)?([a-zA-Z](\\w|::)+)$')) {
    return this.error("No handler yet for perl object " + m[1]);
  } else if (! (m = explicit.match('/'))) {
    return this.error("Load error - no conversion "+explicit);
  } else {
    return this.error("No YAML::Node handler made yet "+explicit);
  }
}

function yaml_parse_implicit (value) {
  value.replace('\\s*$','');
  if (value == '') return '';
  if (value.match('^-?\\d+$')) return 0 + value;
  if (value.match('^[+-]?(\\d*)(\\.\\d*|)?([Ee][+-]?\\d+)?$')) return 1 * value;
  if (value.match('^\\d{4}\-\\d{2}\-\\d{2}(T\\d{2}:\\d{2}:\\d{2}(\\.\\d*[1-9])?(Z|[-+]\\d{2}(:\\d{2})?))?$')
      || value.match('^\\w')) return "" + value;
  if (value == '~') return undefined;
  if (value == '+') return 1;
  if (value == '-') return 0;
  return this.error("Parse Error bad implicit value ("+value+")");
}

function yaml_parse_map (anchor) {
  var m;
  var node = new Array ();
  if (anchor) this.anchors[anchor] = node;
  
  while (! this.done && this.indent == this.offset[this.level]) {
    var key;
    if (this.content.match('^?\\s*')) {
      this.preface = this.content;
      key = '' + this.parse_node();
    } else if (m = this.content.match('^=\\s*')) {
      this.content = this.content.substring(m[0].length);
      key = "\x07YAML\x07VALUE\x07";
    } else if (m = this.content.match('^//\\s*')) {
      this.content = this.content.substring(m[0].length);
      key = "\x07YAML\x07COMMENT\x07";
    } else {

      this.inline = this.content;
      key = this.parse_inline();
      this.content = this.inline;
      this.inline = '';
    }

    if (! (m = this.content.match('^:\\s*'))) return this.error("Parse error - bad map element "+this.content);
    this.content = this.content.substring(m[0].length);
   
    this.preface = this.content;

    var value = this.parse_node();

    if (node[key]) this.warn('Warn - duplicate key '+key);
    else node[key] = value;

  }

  return node;
}

function yaml_parse_seq (anchor) {
  var m;
  var node = new Array ();
  if (anchor) this.anchors[anchor] = node;
  while (! this.done && this.indent == this.offset[this.level]) {
    var m;
    if ((m = this.content.match('^- (.*)$')) || (m = this.content.match('^-()$'))) {
      this.preface = m[1];
    } else return this.error("Parse error - bad seq element "+this.content);

    if (m = this.preface.match('^(\\s*)(\\w.*:( |$).*)$')) {
      this.indent = this.offset[this.level] + 2 + m[1].length;
      this.content = m[2];
      this.offset[++ this.level] = this.indent;
      this.preface = '';
      node.push(this.parse_map(''));
      this.level --;
      this.offset[this.offset.length - 1] = this.level;
    } else {
      node.push(this.parse_node());
    }
  }

  return node;
}

function yaml_parse_inline (top, top_implicit, top_explicit, top_class) {
  this.inline = this.inline.replace('^\\s+','').replace('\\s+$','');

  var info     = this.parse_qualifiers(this.inline);
  var anchor   = info[0];
  var alias    = info[1];
  var explicit = info[2];
  var implicit = info[3];
  var yclass   = info[4];
  this.inline  = info[5];
  var node;
  var m;

  // copy the reference
  if (alias) {
    if (! this.anchors[alias]) return this.error("Parse error - missing alias: "+alias);
    node = this.anchors[alias];

  // new key based array
  } else if (m = this.inline.match('^\\{')) {
    this.inline = this.inline.substring(m[0].length);
    node = new Array ();
    while (! (m = this.inline.match('^\\}'))) {
      var key = this.parse_inline();
      if (! (m = this.inline.match('^:\\s+'))) return this.error("Parse error - bad map element "+this.inline);
      this.inline = this.inline.substring(m[0].length);
      var value = this.parse_inline();
      if (node[key]) this.warn("Warn - duplicate key found: "+key);
      else node[key] = value;
      if (this.inline.match('^\\}')) break;
      if (! (m = this.inline.match('^,\\s*'))) return this.error("Parse error - missing map comma "+this.inline);
      this.inline = this.inline.substring(m[0].length);
    }
    this.inline = this.inline.substring(m[0].length);    

  // new array
  } else if (m = this.inline.match('^\\[\\s*')) {
    this.inline = this.inline.substring(m[0].length);
    node = new Array ();
    while (! (m = this.inline.match('^\\]'))) {
      node.push(this.parse_inline());
      if (m = this.inline.match('^\\]')) break;
      if (! (m = this.inline.match('^,\\s*'))) return this.error("Parse error - missing seq comma "+this.inline);
      this.inline = this.inline.substring(m[0].length);
    }
    this.inline = this.inline.substring(m[0].length);

  // double quoted
  } else if (this.inline.match('^"')) {
    if (m = this.inline.match('^"((?:"|[^"])*)"\\s*(.*)$')) {
      this.inline = m[2];
      m[1].replace(new RegExp('"','g'),'"');
      node = m[1];
    } else {
      return this.error("Bad double quote "+this.inline);
    }
    node = unescape(node); // built in
    if (implicit || top_implicit) node = this.parse_implicit(node);

  // single quoted
  } else if (this.inline.match("^'")) {
    if (m = this.inline.match("^'((?:''|[^'])*)'\\s*(.*)$")) { 
      this.inline = m[2];
      m[1].replace(new RegExp("''",'g'),"'");
      node = m[1];
    } else {
      return this.error("Bad single quote "+this.inline);
    }
    node = unescape(node);  // built in
    if (implicit || top_implicit) node = this.parse_implicit(node);

  // simple
  } else {
    if (top) {
      node = this.inline;
      this.inline = '';
    } else {
      if (m = this.inline.match('^([^!@#%^&*,\\[\\]{}\\:]*)')) {
        this.inline = this.inline.substring(m[1].length);
        node = m[1];
      } else {
        return this.error ("Bad simple match "+this.inline);
      }
      if (! explicit && ! top_explicit) node = this.parse_implicit(node);
    }
  }
  if (explicit || top_explicit) {
    if (! explicit) explicit = top_explicit;
    if (yclass) return this.error("Parse error - classes not supported");
    else node = this.parse_explicit(node, explicit);
  }

  if (anchor) this.anchors[anchor] = node;

  return node;
}

function dump (obj, name) {
  if (! name) name = '[obj]';
  if (typeof(obj) != 'object') return name+'='+obj+'\n';
  var t = '';
  for (var i in obj) {
    t += dump(obj[i], name +'.'+i);
  }
  return t;
}

var t1 = new Date();
var a = document.yaml_load(yaml);
var t2 = new Date();
var el = (t2.getTime() - t1.getTime())/1000;
alert('done '+el);
var t = 'Dump:\n' + dump(a);
alert(t);
</script>
