package O::WrapEx;

# [var a.is_disabled [if [|| a.new form.is_updateable] "" else " DISABLED"]]

use strict;
use vars qw($REVISION $VERSION);
$REVISION  = q$Revision: 1.1 $; # set the revision number into a variable
$VERSION   = ($REVISION =~ /([\d\.]+)/) ? $1 : "None";

use vars qw(@ISA @EXPORT_OK $sph $sph_qr $UNIQUE_KEY $AUTOLOAD
            %FUNC_PRE_SWAP %FUNC_POST_SWAP %AREAS
            %LANGUAGES
            );
use Exporter ();
use O::s2dr ();
use O::ConfUtil ();
use O::Is qw(isun cluck confess);
use O::My qw(isAlpha);
use O::User ();

BEGIN {
  if( $ENV{MOD_PERL} ){
    require O::ENCODE;
    require O::Form;
    require O::Graph;
    require O::Partner;
    require O::ServiceAuth;
    require O::Upgrade;
    require O::WrapISML;
    require Digest::MD5;
  }
}

@ISA       = ('Exporter');
@EXPORT_OK = qw(wrap);
$sph       = chr(186); # swap placement holder
$sph_qr    = qr/$sph\d+$sph/;
$UNIQUE_KEY = '1vunderbund'; # key that won't be anywhere else
%LANGUAGES = (
              'es' => 'Spanish',
              'rn' => 'Red Neck',
              );

###----------------------------------------------------------------###

%AREAS = (
          'browser'      => \&area_browser,
          'cart'         => \&area_cart,
          'cookie'       => \&area_cookie,
          'env'          => \&area_env,
          'form'         => \&area_form,
          'form_error'   => \&area_form_error,
          'form_hidden'  => \&area_form_hidden,
          'graph'        => \&area_graph,
          'isml'         => \&area_isml,
          'pass'         => 1,
          'partner'      => \&area_partner,
          'service'      => \&area_service,
          'site'         => \&area_site,
          'text'         => \&area_text,
          'upgrade'      => \&area_upgrade,
          'ALT_POINTERS' => 1,
          'DIRS'         => 1,
          'FILE'         => 1,
          'PROP'         => 1,
          );

# functions BEFORE [] swapped out
%FUNC_PRE_SWAP = (
                  'alt'    => sub {'[alt]'},
                  'henc'   => \&func_henc,
                  'ic'     => \&func_ic,
                  'last'   => \&func_lastnext,
                  'lc'     => \&func_lc,
                  'next'   => \&func_lastnext,
                  'nowrap' => \&func_nowrap,
                  'push'   => \&func_var,
                  'sort'   => \&func_sort,
                  'sub'    => \&func_sub,
                  'throw'  => \&func_throw,
                  'uc'     => \&func_uc,
                  'uenc'   => \&func_uenc,
                  'var'    => \&func_var,
                  );

# functions called AFTER [] swapped out
%FUNC_POST_SWAP = (
                   'clear'   => \&func_clear,
                   'content' => \&func_content,
                   'dedupe'  => \&func_dedupe,
                   'delete'  => \&func_delete,
                   'eq'      => \&func_eq,
                   'exists'  => \&func_exists,
                   'ge'      => \&func_eq,
                   'gt'      => \&func_eq,
                   'if'      => \&func_if,
                   'join'    => \&func_join,
                   'jsgen'   => \&func_jsgen,
                   'keys'    => \&func_keys,
                   'le'      => \&func_eq,
                   'length'  => \&func_length,
                   'logcsv'  => \&func_logcsv,
                   'loop'    => \&func_loop,
                   'lt'      => \&func_eq,
                   'md5'     => \&func_md5,
                   'ne'      => \&func_eq,
                   'random'  => \&func_random,
                   'regex'   => \&func_regex,
                   'split'   => \&func_split,
                   'sprintf' => \&func_sprintf,
                   'store'   => \&func_store,
                   'wordwrap'=> \&func_wordwrap, 
                   'values'  => \&func_values,
                   '&&'      => \&func_numeric,
                   '||'      => \&func_numeric,
                   '+'       => \&func_numeric,
                   '-'       => \&func_numeric,
                   '*'       => \&func_numeric,
                   '/'       => \&func_numeric,
                   '%'       => \&func_numeric,
                   '**'      => \&func_numeric,
                   '>'       => \&func_numeric,
                   '<'       => \&func_numeric,
                   '>='      => \&func_numeric,
                   '<='      => \&func_numeric,
                   '=='      => \&func_numeric,
                   '!='      => \&func_numeric,
                   '..'      => \&func_dot_dot,
                   );

###----------------------------------------------------------------###

sub new {
  my $type  = shift;
  my $class = ref($type) || $type || __PACKAGE__;
  my $self  = ref($_[0]) ? $_[0] : !(@_ % 2) ? {@_} : die 'Usage: O::WrapEx->new(partner=>$p)';
  bless $self, $class;
  $self->{partner} ||= do {
    require O::Partner;
    O::Partner->new($ENV{HTTP_HOST} || 'default.partner');
  };
  $self->{form}    ||= [];                               # array ref of form hashrefs
  $self->{step}    ||= 'NOSTEPNAME';                     # step of the cgi
  $self->{N}       ||= 0;                                # current level of recursion
  $self->{W}       ||= $self->{partner}->{wrap} ||= {};  # wrap cache location

  $ENV{SCRIPT_NAME} ||= $0;

  $self->{W}->{limit}->{recursive} ||= $self->{recursive_limit} || 15;
  $self->{W}->{PROP}->{ALL}->{step} = ($self->{step} =~ m|^[^/]+/(.+)$|) ? $1 : $self->{step};
  $self->{W}->{PROP}->{ALL}->{cgi}  = ($self->{step} =~ m|^([^/]+)/.+$|) ? $1
    : ($ENV{SCRIPT_NAME} =~ m|/([^/]+?)\d*(\.\w+)?$|) ? $1 : 'NOSCRIPTNAME';

  return $self;
}

sub DESTROY {}

sub AUTOLOAD {
  my $self = shift;
  my $meth = $AUTOLOAD =~ /(\w+)$/ ? $1 : cluck "Invalid method \"$AUTOLOAD\"";
  isun $meth;
  die "unknown method \"$meth\"";
}

###----------------------------------------------------------------###

sub wrap {
  my $self = shift;
  if( ! ref($self) ){
    $self = __PACKAGE__->new($self,@_);
  }elsif( @_ == 1 ){
    $self->{text} = shift;
  }else{
    die "not implemented";
  }

  ### allow for logging variables
  if( $ENV{QUERY_STRING} && $ENV{QUERY_STRING} =~ /wrapvars/ ){
    $self->{VAR} = {};
  }

  ### do the main swap
  my $n = $self->wrap_swap($self->{text});

  ### allow for logging variables
  if( $self->{VAR} ){
    my $qr = join("|",($ENV{QUERY_STRING} =~ /wrapvars=([a-z]\w*)/g));
    $qr = $qr ? qr/^($qr)\./i : qr/./;
    &O::Is::content_type;
    print "<b>Wrap Vars...</b><br>\n";
    foreach (sort keys %{ $self->{VAR} }){
      next if $_ !~ $qr;
      print "&nbsp;&nbsp;$_<br>\n";
    }
    delete $self->{VAR};
  }

  return $n;
}

### this is the main beast
sub wrap_swap {
  my $self = shift;
  my $ref  = shift;
  my $return_swap = (@_ && $_[0] eq 'return_swap');

  return if ! defined $$ref;
  return if index($$ref,'[') == -1;

  my $i;
  my $j = 0;
  my $n = 20000;

  my @SWAP = ();

  OUTER: while ( 1 ){
    $i = index($$ref,'[',$j);
    last if $i == -1;

    my $str;
    my $k = $i;
    my $oc = 0;
    while(1){
      $j = index($$ref,']',$k);
      last OUTER if $j == -1;
      $str = substr($$ref,$k,$j - $k + 1);
      $oc += $str =~ s/\[/\[/g;
      $oc -= $str =~ s/\]/\]/g;
      last if $oc <= 0;
      $k = $j + 1;
    }
    $str = substr($$ref,$i,$j - $i + 1) if $k != $i;

    push @SWAP, $str;
    my $len = $j - $i + 1;
    my $tag = $sph.$#SWAP.$sph;
    substr($$ref,$i,$len,$tag);
    $j += length($tag) - $len + 1;
    if( $n-- < 0 ){
      &O::Is::ISUN("Max swaps reached");
      last;
    }
    
  }
 
  if( $return_swap ){
    return \@SWAP;
  }
  
  my $num = @SWAP;
  return 0 if ! $num;

  if( $num == 1 && length($$ref) == 3 ){
    $$ref = $self->wrap_buddy($SWAP[0]);
    return 1;
  }

  $$ref =~ s{$sph_qr}{
    my $str = shift(@SWAP);

    ### remove comments /* */
    if ($str) {
      my $i = index($str,'/*');
      while ($i > -1) {
        my $j = index($str,'*/',$i);
        last if $j == -1;  # $j = $i if $j == -1;
        substr($str,$i,$j - $i + 2,"");
        $i = index($str,'/*',$i);
      }
    }

    $self->wrap_buddy($str); # return of the s///
  }oeg;
  return $num;
}

sub wrap_unswap {
  my $self = shift;
  my $ref  = shift;
  my $SWAP = shift;
  $$ref =~ s/$sph(\d+)$sph/$SWAP->[$1]/go;
}

### helper script to make Wrap a single pass
sub wrap_buddy {
  my $self = shift;
  my $tag  = shift;
  return $tag if ! defined($tag);

  ### don't let me go too far
  local $self->{N} = $self->{N} + 1;
  if( $self->{N} > $self->{W}->{limit}->{recursive} ){
    $tag =~ s/\[/&\#91;/g;
    $tag =~ s/\]/&\#93;/g;
    return "&#91;Recursive limit ($self->{W}->{limit}->{recursive}) reached \"$tag\"&#93;";
  }

  ### strip off preceding and postpending space
  my ($pre,$post) = ("","");
  if( $tag =~ s/^(\[\s*)// ){
    $pre = $1;
    $post = $tag =~ s/(\s*\])$// ? $1 : '';
  }

  ### allow for function calls
  if( $tag =~ s/^([a-z]{2,}5?|[\&\|\+\-\>\<\=\*\/\%\!\.]{1,2})($|\s+)//s ){
    my ($action,$spc) = ($1,$2);

    if( $FUNC_PRE_SWAP{$action} ){
      my $coderef = $FUNC_PRE_SWAP{$action};
      return $self->$coderef($tag, $action);
    }

    if( $FUNC_POST_SWAP{$action} ){
      my $coderef = $FUNC_POST_SWAP{$action};
      my $SWAP = index($tag,'[') == -1 ? [] : $self->wrap_swap(\$tag,'return_swap');
      return $self->$coderef($tag, $SWAP, $action);
    }

    if( $action ne '!' ){
#      isun "$pre$action$spc$tag$post";
      return "$pre$action$spc$tag$post";
    }else{
      $tag = "$action$spc$tag";
    }
  }

  ### remove level of complication
  my $SWAP = index($tag,'[') == -1 ? [] : $self->wrap_swap(\$tag,'return_swap');

  ### do standard variables
  if( $tag =~ /^([$sph\w]+)\.([$sph\w]+)(?:\.([$sph\w]+))?$/o ){
    my($area,$name,$type) = ($1,$2,$3);
    $type = '' unless defined $type;

    if( ref($SWAP) && @$SWAP ){
      if( $area =~ $sph_qr ){
        $self->wrap_unswap(\$area,$SWAP);
        $self->wrap_swap(\$area);
      }
      if( $name =~ $sph_qr ){
        $self->wrap_unswap(\$name,$SWAP);
        $self->wrap_swap(\$name);
        
      }
      if( $type =~ $sph_qr ){
        $self->wrap_unswap(\$type,$SWAP);
        $self->wrap_swap(\$type);
      }
    }
    my $_tag = "$pre$area.$name".(length($type)?".$type":"").$post;

    return $self->next_alt( $area,$name,$type,$_tag);
  }

  ### allow for text strings
  if(    $tag =~ m/^(")([^\"]*?)(")$/
      || $tag =~ m/^(')([^\']*?)(')$/
      || $tag =~ /^q(\{)(.*)\}$/s
      || $tag =~ /^q(\()(.*)\)$/s
      || $tag =~ /^q(\<)(.*)\>$/s
      || $tag =~ /^q([^\s\w])(.*)\1$/s ){
    my $txt = $2;
    $txt =~ s/%([a-f0-9]{2})/chr(hex($1))/egi;
    $self->wrap_unswap(\$txt,$SWAP);
    $self->wrap_swap(\$txt);
    return $txt;
  }

  ### allow for trinary conditions
  if( index($tag,'?') > -1 && index($tag,':') > -1 ){
    my $found = 0; # found a match
    my $qms   = 0; # question marks
    while( $tag =~ m/\G\s*([^\?\:]*)($|\s*\?|\s*\:)/g ){
      my ($cond,$oper) = ($1,$2);
      $cond =~ s/^\s+//;
      $cond =~ s/\s+$//;
      if( $oper ne '?' ){
        if( $found == $qms ){
          return '' unless $cond;
          $self->wrap_unswap(\$cond,$SWAP);
          return $self->wrap_buddy($cond);
        }
        $qms --;
      }else{
        if( $found == $qms ){
          $self->wrap_unswap(\$cond,$SWAP);
          my $not   = $cond =~ s/^\s*!\s*//;
          my $_cond = $self->wrap_buddy($cond);
          my $val   = 0;
          if( defined($_cond) ){
            if( $cond eq $_cond ){
              $val = 0;
            }else{
              $val = $_cond;
            }
          }
          $found += ($not ? !$val : $val) ? 1 : 0;
          $qms ++;
        }else{
          $qms ++;
        }
      }
    }
  }

  ### allow for complex pass in variables
  if( index($tag,";") > -1 ){
    my @a;
    if( index($tag,";;") > -1 ){
      @a = split(/\s*;;\s*/,$tag);
    } else {
      @a = split(/\s*;\s*/,$tag);
    }
    my $txt = shift(@a);
    $self->wrap_unswap(\$txt,$SWAP);
    if( $txt !~ m/^\w+\.\w+(\.\d+)?$/ ){
#      return "$pre$tag$post";
    }
    my $inline = {};
    foreach( @a ){
      next unless /^(\w+)\s*=\s*(.+)$/sg;
      my($key,$val) = ($1,$2);
      $self->wrap_unswap(\$val,$SWAP);
      $inline->{$key} = $self->wrap_buddy($val);
    }
    $self->{W}->{pass}->{$_} = $inline->{$_} foreach (keys %$inline);
    $txt = $self->wrap_buddy($txt);
    delete( $self->{W}->{pass}->{$_} )       foreach (keys %$inline);
    return $txt;
  }

  ### nothing happened - just return it
  $self->wrap_unswap(\$tag,$SWAP);
  return "$pre$tag$post";
}

### load the individual areas such as form, site, service, etc
sub load_element {
  my $self = shift;
  my ($area,$name) = @_;

  ### return if the information is already gotten
  return if $self->{W}->{$area} && exists $self->{W}->{$area}->{$name};

  ### handle special areas
  my $file = exists($self->{W}->{PROP}->{$area}) ? $self->{W}->{PROP}->{$area}->{area} || $area : $area;
  if( $AREAS{$file} ){
    my $coderef = $AREAS{$file};
    return if ! ref($coderef);
    my $retval = $self->$coderef($area,$name);
    return if $retval; # if the return value is false - continue looking through content files
  }

  ### by now the cgi and step for an element should be determined
  my $ref  = exists($self->{W}->{PROP}->{$area}) ? $self->{W}->{PROP}->{$area} : {};
  my $cgi  = $ref->{cgi}  || $self->{W}->{PROP}->{ALL}->{cgi};
  my $step = $ref->{step} || $self->{W}->{PROP}->{ALL}->{step} || '';

  ### use the default method
  $self->area_default($area,$name,$file,$cgi,$step);
}

### allow for alternating
sub next_alt {
  my $self = shift;
  my ($area,$name,$index) = @_; # optimized (short args)

  $self->load_element($area,$name) if ! defined($self->{W}->{$area}->{$name});

  if ($self->{VAR}) {
    $self->{VAR}->{"$area.$name"} ||= 1;
  }

  ### short circuit isml
  if ($area eq 'isml') {
    $self->{W}->{isml}->{$name} ||= 0;
    $self->{W}->{isml}->{$name} ++;
    my $copy = $self->{W}->{isml}->FETCH($name);
    $self->wrap_swap(\$copy);
    return $copy;
  } elsif ($area eq 'graph' && ($name eq 'html' || $name eq 'text' || $name eq 'draw')) {
    return delete $self->{W}->{$area}->{$name};
  }

  ### handle special areas
  if( $index ){
    if( $index eq 'random' ){
      return '' unless defined $self->{W}->{$area}->{$name};
      return $self->{W}->{$area}->{$name} unless ref($self->{W}->{$area}->{$name});
      return $self->{W}->{$area}->{$name}->[ rand(@{ $self->{W}->{$area}->{$name} }) ];
    }elsif( $index eq 'length' ){
      return 0 unless defined $self->{W}->{$area}->{$name};
      return ref($self->{W}->{$area}->{$name}) ? $#{$self->{W}->{$area}->{$name}}+1 : length($self->{W}->{$area}->{$name}) ? 1 : 0;
    }elsif( $index eq 'alt' ){
      $self->{W}->{ALT_POINTERS}->{$area}->{$name} ||= 0;
      return $self->{W}->{ALT_POINTERS}->{$area}->{$name} + 1;
    }
  }

  ### failed lookup - return the text
  if( ! defined $self->{W}->{$area}->{$name} ){
    return $_[3] ? $_[3] : "";
  }

  ### return normal pieces
  my $ref = ref $self->{W}->{$area}->{$name};
  if( ! $ref ){
    my $copy = $self->{W}->{$area}->{$name};
    return '' if ! defined($copy);
    $self->wrap_swap(\$copy) if index($copy,'[') != -1;
    return $copy;
  }elsif( $ref eq 'HASH' ){
    return {%{ $self->{W}->{$area}->{$name} }};
  }elsif( $ref ne 'ARRAY') {
    confess "Don't know how to handle type $ref";
  }

  ### return a specific index
  if( $index ){
    return "" if $index - 1 > $#{ $self->{W}->{$area}->{$name} }; # almost never happens
    my $copy = $self->{W}->{$area}->{$name}->[$index-1];
    return '' if ! defined($copy);
    $self->wrap_swap(\$copy) if index($copy,'[') != -1;
    return $copy;

  }elsif( length($index) ){
    my $copy;
    if( exists($self->{W}->{ALT_POINTERS}->{$area}->{$name}) ){
      my $alt_i = $self->{W}->{ALT_POINTERS}->{$area}->{$name} - 1;
      $alt_i = scalar(@{ $self->{W}->{$area}->{$name} }) - 1 if $alt_i < 0;
      $copy = $self->{W}->{$area}->{$name}->[$alt_i];
    }else{
      $copy = $self->{W}->{$area}->{$name}->[0];
    }
    return '' if ! defined($copy);
    $self->wrap_swap(\$copy) if index($copy,'[') != -1;
    return $copy;
  }

  ### alternate
  my $alt_i = exists($self->{W}->{ALT_POINTERS}->{$area}->{$name}) ? $self->{W}->{ALT_POINTERS}->{$area}->{$name} : 0;
  my $copy = $self->{W}->{$area}->{$name}->[$alt_i];
  $alt_i = 0 if ++ $alt_i >= scalar(@{ $self->{W}->{$area}->{$name} });
  $self->{W}->{ALT_POINTERS}->{$area}->{$name} = $alt_i;
  return '' if ! defined($copy);
  $self->wrap_swap(\$copy) if index($copy,'[') != -1;
  return $copy;
}

sub next_val_from_str {
  my $self = shift;
  my ($ref,$SWAP,$returnstrnotval) = @_;
  my $do_unswap = ref($SWAP) && scalar @$SWAP;
  my $not = ($$ref =~ s/^\s*!//);
  my $val;
  $$ref =~ s/^\s+//;
  if(   $$ref =~ s/^(")([^\"]*?)(")($|\s+)//
     || $$ref =~ s/^(')([^\']*?)(')($|\s+)//
     || $$ref =~ s/^q(\{)(.*?)\}($|\s+)//s
     || $$ref =~ s/^q(\()(.*?)\)($|\s+)//s
     || $$ref =~ s/^q(\<)(.*?)\>($|\s+)//s
     || $$ref =~ s/^q([^\s\w])(.*?)\1($|\s+)//s ){
    $val = $2;
    $self->wrap_unswap(\$val,$SWAP) if $do_unswap;
  }elsif( $$ref =~ s/^(\-?(\d*\.\d+|\d+))($|\s+)// ){
    $val = $1;
  }elsif( $$ref =~ s/^(els(if|e))($|\s+)// ){
    return $1;
  }elsif( $$ref =~ s/^((\w+)\.(\w+)(?:\.(\w+))?)($|\s+)// ){
    return "[$1]" if $returnstrnotval;
    my($area,$name,$num) = ($2,$3,$4);
    $num = '' unless defined $num;
    $val = $self->next_alt( $area,$name,$num,'');
  }elsif( $$ref =~ s/^(\S+)($|\s+)// ){
    $val = $1;
    $self->wrap_unswap(\$val,$SWAP) if $do_unswap;
    return $val if $returnstrnotval;
    if( index($val,'[') != -1 ){
      $self->wrap_swap(\$val);
      if( defined($val) && $val =~ s/^(([a-z]\w*)\.([a-z]\w*)(?:\.(\w+))?)($|\;)// ){
        my($area,$name,$num) = ($2,$3,$4); 
        $num = '' unless defined $num;
        $val = $self->next_alt( $area,$name,$num,'');
      }
    }
  }else{
    return $not ? 1 : "";
  }
  if( $not ){
    return $val ? 0 : 1;
  }
  return defined($val) ? $val : 0;
}

###----------------------------------------------------------------###
### non object subs

### allow for multiple values for one key (cycle through)
sub check_for_alt {
  return undef unless defined($_[0]);
  return $_[0] if ref($_[0]) =~ /ARRAY|CODE/;
  my @_A = split(/\s*\[alt\]\s*/,$_[0]);
  return @_A > 1 ? \@_A : $_[0];
}

### turn a string or whatever into a number
sub numify {
  return 0  if ! $_[0];
  return $1 if $_[0] =~ /^(-?(\d*\.\d+|\d+)?)$/;
  return 0;
}

###----------------------------------------------------------------###
### extra subs

sub lang {
  my $self = shift;
  return $self->{W}->{user}->{lang} ||= do {
    $self->load_site;
    $self->area_cookie('cookie','lang');
    my $lang = $self->get_form_val('lang');
    $lang ||= $self->{W}->{form}->{lang};
    $lang ||= $self->{W}->{cookie}->{lang};
    $lang ||= $self->{W}->{site}->{util_language};
    $lang ||= $ENV{HTTP_ACCEPT_LANGUAGE};
    $lang ||= '';
    $lang = lc($lang);
    $lang =~ tr/a-z//cd;
    $lang =~ s/^(..).+$/$1/;
    $lang = '' if ! $LANGUAGES{$lang};
    $lang; # return of the do
  };
}

sub load_site {
  my $self = shift;
  return if $self->{W}->{site} && ref($self->{W}->{site}) && keys(%{ $self->{W}->{site} }) > 1;
  
  my $u = $self->{W}->{site} = $self->{user} ||= O::User->new( $self->{W}->{site}->{site}
                                                               || $self->{partner}->{test_site_name} );
  $u->{email_username} ||= 'webmaster';
  $u->{email_domain} ||= $u->{site};
  $self->{W}->{site}->{email} = "$u->{email_username}\@$u->{email_domain}";
  delete $self->{W}->{site}->{password};

  if (&O::My::isBizHosting($self->{partner})){
    $self->{W}->{site}->{"biz_enabled"} = "1";
  }
}

sub get_wrap_directories {
  my $self     = shift;
  my $_partner = shift;
  my $type     = shift() || 'wrap';
  my $ref      = [];
  push @$ref, &O::s2dr::s2sr($_partner->{test_site_name})."/filebox/$type"
    if $type ne 'content';

  $_partner->start_lineage if ! $_partner->{lineage};
  foreach my $domain ( @{ $_partner->{lineage} } ) {
    push @$ref, &O::s2dr::s2dr("my.$domain")."/public/$type";
  }

  @$ref = grep { -d } @$ref;
  return $ref;
}

sub get_content_directories {
  my $self = shift;
  my $_partner = shift;
  return $self->get_wrap_directories($_partner,'content');
}


sub get_form_val {
  my $self = shift;
  my $name = shift;
  my $ref  = ref($self->{form});
  return if ! $ref;
  if( $ref ne 'ARRAY' ){
    isun &O::Is::caller_trace();
    if( $ref eq 'HASH' ){
      $self->{form} = [$self->{form}];
    }
  }
  foreach my $_form (@{ $self->{form} }){
    next if ! ref($_form);
    if( exists($_form->{$name}) ){
      return check_for_alt( $_form->{$name} );
    }
  }
  return '';
}

sub conf_read {
  my $self = shift;
  my $area = shift;

  my @pieces = split(m|/|,$area);
  foreach (@pieces){
    die 'Usage : $conf = $self->conf_read("area") or $conf = $self->conf_read("cgi/area")' if /\W/;
  }
  my $cgi = (@pieces >= 2) ? shift(@pieces) : $ENV{SCRIPT_NAME} || 'NOSCRIPT';
  my $step = "$cgi/none";
  $area = pop(@pieces);

  $self->func_var("$area 'wrap:$step/$area.wrap'");
  $self->load_element($area,$UNIQUE_KEY);
  delete $self->{W}->{$area}->{$UNIQUE_KEY};
  
  return $self->{W}->{$area} || {};
}

sub conf_write {
  my $self = shift;
  my $area = shift;
  my $update = shift || return 0;

  ### setup wrap info
  my @pieces = split(m|/|,$area);
  foreach (@pieces){
    die 'Usage : conf_write("area",$update,$partner) or conf_read("cgi/area",$update)' if /\W/;
  }
  my $cgi = (@pieces >= 2) ? shift(@pieces) : $ENV{SCRIPT_NAME} || 'NOSCRIPT';
  my $step = "$cgi/none";
  $area = pop(@pieces);

  $self->func_var("$area 'wrap:$step/$area.wrap'");
  local $self->{W}->{$area} = $update;
  my $ret = $self->func_store($area);
  isun $ret
    if $ret;
  return 1;
}

###----------------------------------------------------------------###
### subs for loading areas
### a false return value will allow the system to fall through the .wrap files
### a true value will allow the system to only use what is loaded

sub area_browser {
  my ($self,$area,$name) = @_;
  my $ie = 0;
  my $nn = 0;
  my $ua = $self->{W}->{$area}->{ua} = $ENV{HTTP_USER_AGENT} || '';
  if( $ua =~ m|^.+compatible;\s*MSIE\s*(\d+(?:\.\d+)?)|i ){
    $ie = $1;
  }elsif( $ua =~ m|^Mozilla/(\d+(?:\.\d+)?)|i && $ua !~ m|compatible|i ){
    $nn = $1;
  }
  $self->{W}->{$area}->{ie} = $ie;
  $self->{W}->{$area}->{nn} = $nn;
  return 1;
}

sub area_cart {
  my ($self,$area,$name) = @_;
  if( $name =~ /^(is_setup_started|is_setup|is_activated|is_deactivated|options_not_set|options_not_set_required|is_testmode|was_old_cart)$/ ){
    return 1 if exists $self->{W}->{$area}->{$name};
    $self->load_site;
    require O::Cart;
    my $obj = $self->{W}->{$area}->{obj_cache} ||= O::Cart->new({
      user    => $self->{W}->{site},
      partner => $self->{partner},
      form    => {},
    });
    $self->{W}->{$area}->{$name} = $obj->$name();
  }
  return 1;
}

sub area_cookie {
  my ($self,$area,$name) = @_;
  my $c = defined $ENV{HTTP_COOKIE} ? $ENV{HTTP_COOKIE} : defined $ENV{COOKIE} ? $ENV{COOKIE} : '';
  foreach ( split(/\s*[&;]\s*/,$c) ){
    my($key,$val) = split(/=/,$_,2);
    $key = '' if ! defined $key;
    $val = '' if ! defined $val;
    $key =~ tr/+/ /;
    $val =~ tr/+/ /;
    $key =~ s/%([a-z0-9]{2})/chr(hex($1))/egi;
    $val =~ s/%([a-z0-9]{2})/chr(hex($1))/egi;
    $self->{W}->{cookie}->{$key} = $val;
  }
  if( ! exists $self->{W}->{$area}->{$name} ){
    $self->{W}->{$area}->{$name} = '';
  }
  return 1;
}

sub area_default {
  my ($self,$area,$name,$file,$cgi,$step) = @_;

  ### allow for "content:" to fall back like wrap does
  if( exists($self->{W}->{PROP}->{$area}) && exists($self->{W}->{PROP}->{$area}->{content}) ){
    my $file = $self->{W}->{PROP}->{$area}->{content};
    my $dirs;

    ### check for user overrides on content
    my $user_c_dir = &O::s2dr::s2sr($ENV{HTTP_HOST} || '') ."/filebox/content";
    if( $self->{W}->{user}->{override} ){
      $dirs = [$user_c_dir];
    }else{
      $self->{W}->{DIRS_C}->{ALL} ||= $self->get_content_directories($self->{partner});
      $dirs = exists($self->{W}->{DIRS_C}->{$area}) ? $self->{W}->{DIRS_C}->{$area} : $self->{W}->{DIRS_C}->{ALL};
      if( $dirs->[0] && $dirs->[0] eq $user_c_dir ){
        $dirs = [@$dirs]; # copy values of ref
        shift(@$dirs);    # get rid of user directory - overrides not allowed
      }
    }

    ### allow for language
    my @files = ($file);
    if ($file =~ m/\.txt$/ && (my $lang = $self->lang)) {
      unshift @files, "$file.$lang";
    }

    foreach my $dir (@$dirs){
      foreach my $file (@files) {
        my $file_long = "$dir/$file";
        next if exists $self->{W}->{FILE}->{"$file_long - $area"};
        $self->{W}->{FILE}->{"$file_long - $area"} = 1;
        
        if( -e $file_long ){
          $self->{W}->{FILE}->{"$file_long - $area"} ++;
          my $ref = &O::ConfUtil::conf_read( $file_long );
          
          foreach my $key (keys %$ref){
            next if exists $self->{W}->{$area}->{$key};
            $self->{W}->{$area}->{$key} = check_for_alt( $ref->{$key} );
          }

          return 1 if exists $self->{W}->{$area}->{$name};
        }
      }
    }
  }


  ### now look in the normal wrap directories
  $self->{W}->{DIRS}->{ALL} ||= $self->get_wrap_directories($self->{partner});
  my $dirs = exists($self->{W}->{DIRS}->{$area}) ? $self->{W}->{DIRS}->{$area} : $self->{W}->{DIRS}->{ALL};

  ### files to look at on each partner
  my @subs = ("");
  unshift @subs, "$cgi/"       if length($cgi);
  unshift @subs, "$cgi/$step/" if length($step);

  ### loop through the partner area
  foreach my $dir (@$dirs){
    foreach my $sub ( @subs ){

      my $file_long = "$dir/$sub$file.wrap";
      next if exists $self->{W}->{FILE}->{"$file_long - $area"};
      $self->{W}->{FILE}->{"$file_long - $area"} = 1;

      if( -e $file_long ){
        $self->{W}->{FILE}->{"$file_long - $area"} ++;
        my $ref = &O::ConfUtil::conf_read( $file_long );

        foreach my $key (keys %$ref){
          next if exists $self->{W}->{$area}->{$key};
          $self->{W}->{$area}->{$key} = check_for_alt( $ref->{$key} );
        }

        return 1 if exists $self->{W}->{$area}->{$name};
      }
    }
  }

  ### this property won't be found anywhere
  $self->{W}->{$area}->{$name} = undef;
}

sub area_env {
  my ($self,$area,$name) = @_;
  my $ref = {
    path_info     => $ENV{PATH_INFO}    || '',
    remote_addr   => $ENV{REMOTE_ADDR}  || '',
    script_name   => $ENV{SCRIPT_NAME}  || '',
    http_host     => $ENV{HTTP_HOST}    || '',
    http_referer  => $ENV{HTTP_REFERER} || '',
    http_user_agent => $ENV{HTTP_USER_AGENT} || '',
    redirect_uri  => $ENV{REDIRECT_URI} || '',
    request_uri   => $ENV{REQUEST_URI}  || '',
    hostname      => $ENV{HOSTNAME} || '',
    step          => $self->{W}->{PROP}->{ALL}->{step},
    cgi           => $self->{W}->{PROP}->{ALL}->{cgi},
    title         => $self->{partner}->{html_title} || '',
    time          => time(),
    is_secure     => O::My::isSecure(),
    is_alpha      => isAlpha,
    http          => 'http://',
    q             => '?',
    plus          => '+',
    colon         => ':',
    caret         => '^',
    semi          => ';',
    quote         => '"',
    wo            => '[',
    wc            => ']',
    blank         => '',
    spc           => ' ',
    jsnl          => "\\n",
    nl            => "\n",
    nbsp          => '&nbsp;',
    uncache       => " onsubmit=\"var X=new Date();this.action+=(this.action.indexOf('?')>-1?'&':'?')+'Xtime='+X.getTime();\"",
  };
  foreach (keys %$ref){
    $self->{W}->{$area}->{$_} = $ref->{$_} if ! exists $self->{W}->{$area}->{$_};
  }
  return 1;
}

sub area_form {
  my ($self,$area,$name) = @_;
  $self->{W}->{$area}->{$name} = $self->get_form_val( $name );
  $self->{W}->{$area}->{$name} = '' if ! defined($self->{W}->{$area}->{$name});
  return 1;
}

sub area_form_error {
  my ($self,$area,$name) = @_;
  $self->load_element('form', $name ) if ! exists $self->{W}->{'form'}->{$name};
  my $er = $self->{W}->{form}->{$name};
  if( defined($er) && length($er) ){
    my $tag = "[loop errortext [values form.$name] error.inlinetext]";
    $self->wrap_buddy($tag);
    $er = $tag;
  }
  $self->{W}->{$area}->{$name} = $er;
  return 1;
}

sub area_form_hidden {
  my ($self,$area,$name) = @_;
  $self->load_element('form', $name ) if ! exists $self->{W}->{form}->{$name};
  my $ref = $self->{W}->{form}->{$name};
  $ref = [$ref] if ref($ref) ne 'ARRAY';
  $self->{W}->{$area}->{$name} = "";
  foreach my $id (@$ref){
    local $_ = $id;
    if( defined($_) && length($_) ){
      s/&/&amp;/g;
      s/</&lt;/g;
      s/\"/&quot;/g;
      s/([^\r\n\ -\~])/sprintf("&\#%03d;",ord($1))/eg;
      $_ = "<input type=hidden name=\"$name\" value=\"$_\">";
    }
    $self->{W}->{$area}->{$name} .= $_;
  }
  return 1;
}

sub area_graph {
  my ($self,$area,$name) = @_;
  if ($name eq 'html' || $name eq 'draw' || $name eq 'text') {
    my $html = eval {
      require O::Graph;
      my $g = O::Graph->new(%{$self->{W}->{$area} || {}}, %{$self->{W}->{'pass'} || {}});
      $g->{type} = 'text' if $name eq 'text';
      $g->html; # return of the eval
    };
    $html = "&#91;O::Graph Error - $@&#93;" if $@;
    $self->{W}->{$area}->{$name} = $html;
    return 1;
  }
  return 0;
}

sub area_isml {
  my ($self,$area,$name) = @_;
  return 1 if $self->{W}->{$area} && UNIVERSAL::isa($self->{W}->{isml},'O::WrapISML');
  require O::WrapISML;
  $self->{W}->{isml} = O::WrapISML->new($self->{partner});
  return 1;
}

sub area_partner {
  my ($self,$area,$name) = @_;
  $self->{W}->{$area}->{domain} ||= $self->{partner}->{domain};
  ### this is to give the url that the site was signed up on
  ### needs to return fs.earl for t87.fs.earl
  ### needs to return freeservers.com for spack.8m.com
  my $return = 0;
  if($name eq 'signup_partner') {
    $self->load_site;
    $self->{partner}->start_lineage;
    my $lineage = [@{$self->{partner}{lineage}}];
    if($self->{user}{service_level} > $O::User::DEFAULT_SERVICE_LEVEL) {
      $self->{user}->get("unupgraded_partner_domain");
      unshift @{$lineage}, $self->{user}{unupgraded_partner_domain} if($self->{user}{unupgraded_partner_domain});
    }
    foreach(@{$lineage}) {
      if(-e O::s2dr::s2dr($_)) {
        $self->{W}->{$area}->{$name} = $_;
        $return = 1;
        last;
      }
    }
  }

  return $return;
}

sub area_service {
  my ($self,$area,$name) = @_;
  return 0 if $name !~ /(^|\.|_)(auth|avail|grand|free)$/;
  require O::ServiceAuth;

  $self->load_site;

  my $service = O::ServiceAuth->new({
    user    => $self->{W}->{site},
    partner => $self->{partner},
    service => $self->{W}->{PROP}->{ALL}->{cgi},
  });
  
  $service->{service} = $1 if $name =~ /^(.+)_(avail|auth|free|grand)$/;
  if   ( $2 eq 'avail' ){ $self->{W}->{$area}->{$name} = $service->ServiceAvail; }
  elsif( $2 eq 'auth'  ){ $self->{W}->{$area}->{$name} = $service->ServiceAuth; }
  elsif( $2 eq 'free'  ){ $self->{W}->{$area}->{$name} = $service->ServiceFree; }
  elsif( $2 eq 'grand' ){ $self->{W}->{$area}->{$name} = $service->grand; }
  return 1;
}

sub area_site {
  my ($self,$area,$name) = @_;
  my @keys = qw(banners email email_type_id file_size name quota site webmaster_gender bitmask);
  $self->load_site;
  if( ! $self->{W}->{$area} ){
    $self->{W}->{$area} = {};
    $self->{W}->{$area}->{$_} = $self->{W}->{site}->{$_} foreach (@keys);
  }
  if( $name eq 'username' ){
    my $suffix = $self->wrap_buddy("[preferences.allow_short_username]") || '';
    my $username = $self->wrap_buddy("[site.site]") || '';
    $username =~ s/\Q.$suffix\E$//;
    $self->{W}->{$area}->{username} = $username;

  }elsif( $name =~ /^bitmask_(\w+)$/ ){
    my $mask = $1;
    my $val  = 0;
    if( exists($O::User::MASK->{$mask}) && $self->{W}->{$area}->{bitmask} ){
      $val = &O::User::mask($self->{W}->{$area}->{bitmask},$mask) || 0;
    }
    $self->{W}->{$area}->{$name} = $val;

  }elsif( $name eq 'epoch' ) {
    if($self->{user} && !$self->{user}{epoch}) {
      $self->{user}->setup_date2epoch;
    }
    $self->{W}->{$area}->{$name} = $self->{user}{epoch} || 0;
  }elsif( $name eq 'days_old' ) {
    my $user = $self->{W}{site} || $self->{user};
    $user->setup_date2epoch;
    $self->{W}->{$area}->{$name} = int( (time - $user->{epoch}) / 86400);

  }elsif( $name eq 'weeks_old' ) {
    my $user = $self->{W}{site} || $self->{user};
    $user->setup_date2epoch;
    require O::tima;
    $self->{W}->{$area}->{$name} = O::tima::weeks_old($user->{epoch});

  }elsif( ! exists $self->{W}->{$area}->{$name} ){
    $self->{W}->{$area}->{$name} = '';

  }
  return 1;
}

sub area_text {
  my ($self,$area,$name) = @_;
  if (! exists($self->{W}->{PROP}->{$area})) {
    my $file = $self->{W}->{PROP}->{ALL}->{cgi} .'/'. $self->{W}->{PROP}->{ALL}->{step};
    $file .= '.txt' if $file !~ s|\.\w+$|.txt|;
    $self->{W}->{PROP}->{$area}->{content} = $file;
  }
  return 0;
}

sub area_upgrade {
  my ($self,$area,$name) = @_;
  if( ! $self->{W}->{$area} || ! tied(%{ $self->{W}->{$area} }) ){
    require O::Upgrade;
    $self->{W}->{$area} = {};
    O::Upgrade->new($self->{partner}->{'test_site_name'})->get_upgrade_swaps( $self->{W}->{$area}, {} );
  }
  return 1;
}

###----------------------------------------------------------------###
### subs that provide wrap functions

sub func_clear {
  my ($self,$tag,$SWAP,$action) = @_;
  my $element = $self->next_val_from_str(\$tag,$SWAP,'returnstrnotval');
  $element =~ s/[\[\]]//g;

  my ($area,$name);
  if( $element =~ /^(\w+)$/ ){
    $area = $1;
  }else{
    return "&#91;Invalid area clear ($element)&#93;";
  }
  
  return "&#91;Protected area ($area)&#93;" if $AREAS{$area};

  delete $self->{W}->{$area};
  delete $self->{W}->{PROP}->{$area};
  delete $self->{W}->{DIRS}->{$area};
  delete $self->{W}->{DIRS_C}->{$area}; # clear content dirs also
  foreach my $key (keys %{ $self->{W}->{FILE} } ){
    delete $self->{W}->{FILE}->{$key} if $key =~ /\s+\Q$area\E$/;
  }

  return '';
}

sub func_content{
  my ($self,$tag,$SWAP,$action) = @_;
  my $file = $self->next_val_from_str(\$tag,$SWAP);
  my $one  = $self->next_val_from_str(\$tag,$SWAP) || '';
  my $two  = $self->next_val_from_str(\$tag,$SWAP) || '';
  my $no_wrap  = ($one && $one =~ /literal/i) || ($two && $two =~ /literal/i);
  my $relative = ($one && $one =~ /relative/i) || ($two && $two =~ /relative/i);
  $self->wrap_swap(\$file);
  return "&#91;Unsecure file \"$file\"&#93;" if $file=~m|\.\./| || $file=~m|^/|;
  $file = "$self->{W}->{PROP}->{ALL}->{cgi}/$file" unless $file =~ m|/|;
  my $ret = $self->{partner}->content($file,{suppress_header=>1,relative_to_s2dr=>$relative});
  $self->wrap_swap(\$ret) if ! $no_wrap;
  return $ret;
}

sub func_dedupe {
  my ($self,$tag,$SWAP,$action) = @_;
  my %hash = ();
  while($tag){
    my $element = $self->next_val_from_str(\$tag,$SWAP);
    next if ! defined $element;
    if( ref($element) ){
      map {$hash{$_}=1} @$element;
    }else{
      $hash{$element}=1;
    }
  }
  return [sort keys %hash];
}

sub func_delete {
  my ($self,$tag,$SWAP,$action) = @_;
  my $element = $self->next_val_from_str(\$tag,$SWAP,'returnstrnotval');
  $element =~ s/[\[\]]//g;
  if( $element =~ /^\w+$/ ){
    delete $self->{W}->{$element};
  }elsif( $element =~ /^(\w+)\.(\w+)$/ ){
    delete $self->{W}->{$1}->{$2};
  }elsif( $element =~ /^(\w+)\.(\w+)\.(\d+)$/ ){
    my $ref = $self->{W}->{$1}->{$2};
    if( defined($ref) && ref($ref) eq 'ARRAY' ){
      my $index = $3 - 1;
      splice @$ref, $index, 1;
    }else{
      delete $self->{W}->{$1}->{$2};
    }
  }else{
    return "&#91;Invalid delete ($element)&#93;";
  }
  return '';
}

sub func_dot_dot {
  my ($self,$tag,$SWAP) = @_;
  my $n = $self->next_val_from_str(\$tag,$SWAP);
  my $m = numify($self->next_val_from_str(\$tag,$SWAP));
  my $i = abs(numify($self->next_val_from_str(\$tag,$SWAP))) || 1;
  my @a = ();
  if   ( $n > $m ){ for (my $j = $n; $j >= $m; $j -= $i){ push(@a, $j); } }
  elsif( $n < $m ){ for (my $j = $n; $j <= $m; $j += $i){ push(@a, $j); } }
  else{ push @a, $n }
  return \@a;
}

sub func_eq {
  my ($self,$tag,$SWAP,$action) = @_;
  my $one = $self->next_val_from_str(\$tag,$SWAP);
  my $two = $self->next_val_from_str(\$tag,$SWAP);
  $self->wrap_swap(\$one);
  $self->wrap_swap(\$two);
  return $action eq 'eq' ? ($one eq $two)
    : $action eq 'ne' ? ($one ne $two)
    : $action eq 'gt' ? ($one gt $two)
    : $action eq 'lt' ? ($one lt $two)
    : $action eq 'ge' ? ($one ge $two)
    : $action eq 'le' ? ($one le $two) : "";
}

sub func_exists {
  my ($self,$tag,$SWAP,$action) = @_;
  $self->wrap_unswap(\$tag,$SWAP);
  $self->wrap_swap(\$tag);
  if( $tag =~ /^(\w+)\.(\w+)$/ ){
    my ($area,$name) = ($1,$2);
    $self->load_element($area,$name);
    return exists($self->{W}->{$area}->{$name}) && defined($self->{W}->{$area}->{$name});
  }elsif( $tag =~ /^(\w+)$/ ){
    return 1 if exists($self->{W}->{$1}) && scalar keys %{ $self->{W}->{$1} };
  }
  return 0;
}

sub func_henc {
  my $txt = shift()->wrap_buddy(shift());
  $txt = "" if ! defined $txt;
  $txt =~ s/&/&amp;/g;
  $txt =~ s/</&lt;/g;
  $txt =~ s/>/&gt;/g;
  $txt =~ s/\"/&quot;/g;
  $txt =~ s/\'/&\#39;/g;
  $txt =~ s/\[/&\#91;/g;
  $txt =~ s/\]/&\#93;/g;
  return $txt;
}

sub func_hex {
  my ($self,$tag,$SWAP,$action) = @_;
  my $hex = $self->next_val_from_str(\$tag,$SWAP);
  return 0 if $hex !~ /^[A-F0-9]+$/i;
  return hex($hex);
}

sub func_ic {
  my $txt = shift()->wrap_buddy(shift());
  $txt = "" if ! defined $txt;
  $txt =~ s/\b(\w+)/\u\L$1/g;
  return $txt;
}

sub func_if {
  my ($self,$tag,$SWAP,$action) = @_;
  my $if = $self->next_val_from_str(\$tag,$SWAP);
  $self->wrap_swap(\$if);
  my $trueblock = $self->next_val_from_str(\$tag,$SWAP,'returnstrnotval');
  if( $if ){
    $self->wrap_swap(\$trueblock);
    return $trueblock;
  }
  my $elseblock = $self->next_val_from_str(\$tag,$SWAP,'returnstrnotval');
  while ( $elseblock eq 'elsif' ){
    $if = $self->next_val_from_str(\$tag,$SWAP);
    $self->wrap_swap(\$if);
    $trueblock = $self->next_val_from_str(\$tag,$SWAP,'returnstrnotval');
    if( $if ){
      $self->wrap_swap(\$trueblock);
      return $trueblock;
    }
    $elseblock = $self->next_val_from_str(\$tag,$SWAP);
  }
  $elseblock = $self->next_val_from_str(\$tag,$SWAP,'returnstrnotval') if $elseblock eq 'else';
  $self->wrap_swap(\$elseblock);
  return $elseblock;
}

sub func_join {
  my ($self,$tag,$SWAP,$action) = @_;
  my $join = $self->next_val_from_str(\$tag,$SWAP);
  my @val = ();
  while( $tag ){
    my $val = $self->next_val_from_str(\$tag,$SWAP);
    push @val, (ref($val) ? @$val : $val);
  }
  my $ret = join($join,@val);
  $self->wrap_swap(\$ret);
  return $ret;
}

sub func_jsgen {
  my ($self,$tag,$SWAP,$action) = @_;
  require O::Form;
  my $cgi_step = $self->next_val_from_str(\$tag,$SWAP);
  $self->wrap_swap(\$cgi_step);
  if ($cgi_step =~ /onsubmit/i) {
    return &O::Form::onsubmit_js();
  }
  if (! $cgi_step || $cgi_step =~ m|^/| || $cgi_step =~ m|/\.+/|) {
    $cgi_step = $self->{W}->{PROP}->{ALL}->{cgi} .'/'. $self->{W}->{PROP}->{ALL}->{step};
  } elsif ($cgi_step !~ m|/|) {
    $cgi_step = $self->{W}->{PROP}->{ALL}->{cgi} ."/$cgi_step";
  }
  my $js_hashname = $self->next_val_from_str(\$tag,$SWAP) || '';
  $self->wrap_swap(\$js_hashname) if $js_hashname;
  my $fob = O::Form->new({partner => $self->{partner}});
  my $ret = $fob->generate_js($cgi_step,[],$js_hashname); # second argument is order - gen_js will create it for us
  $self->wrap_swap(\$ret);
  return $ret;
}

sub func_keys {
  my ($self,$tag,$SWAP,$action) = @_;
  $self->wrap_unswap(\$tag,$SWAP);
  $self->wrap_swap(\$tag);
  if( $tag =~ /^(\w+)$/ ){
    $self->load_element($tag,$UNIQUE_KEY);
    delete $self->{W}->{$tag}->{$UNIQUE_KEY};
    return [] if ! $self->{W}->{$1};
    return [keys %{$self->{W}->{$1}}];
  }elsif( $tag =~ /^(\w+)\.(\w+)$/ ){
    my ($area,$name) = ($1,$2);
    $self->load_element($area,$name);
    my $ref = $self->{W}->{$area} ? $self->{W}->{$area}->{$name} : undef;
    $ref = defined($ref) ? (ref($ref) ? $ref : [($area ne 'form' || length($ref)) ? $ref : ()]) : [];
    return [keys %$ref] if ref($ref) eq 'HASH';
    return @$ref ? [1..@$ref] : [];
  }
  return [];
}

sub func_lastnext {
  my $self = shift;
  my $tag  = shift;
  my $action = shift;
  my $ikey = ($tag =~ s/^([a-z]\w*)($|\s+)//s) ? $1 : '_';
  my $ref = $self->{W}->{PROP}->{LAST};
  return "" if ! defined($ref) || ! ref($ref);
  my $last = ($tag =~ m/^0*1/) ? $action."inc" : $action;
  if( $ikey eq '_' && (keys %$ref) == 1 ){
    $ref->{"@{[keys %$ref]}"} = $last;
  }elsif( exists $ref->{$ikey} ){ 
    $ref->{$ikey} = $last;
  }
  return "";
}

sub func_lc {
  my $txt = shift()->wrap_buddy(shift());
  $txt = "" if ! defined $txt;
  return "\L$txt";
}

sub func_length {
  my ($self,$tag,$SWAP,$action) = @_;
  return length($self->next_val_from_str(\$tag,$SWAP)) || 0;
}

sub func_logcsv {
  my ($self,$tag,$SWAP,$action) = @_;
  my @param = ();
  push @param, $self->next_val_from_str(\$tag,$SWAP) while (length($tag) && scalar(@param) < 30);

  my $file = shift(@param);
  die "Invalid file $file" if $file =~ /\W/;
  $file .= ".csv";

  my $dir = "$O::magics::ROOTDIR_TMPCOMMON/wrapex";
  return "&#91;Couldn't create dir wrapex: $!&#93;" if ! -d $dir && ! mkdir $dir, 0755;

  ### create a csv line
  my $str  = join ",", map {s/^\s+//;                     # remove pre whitespace
                            s/\s+$//;                     # remove post whitespace
                            s/(?<!\\)\"/\\\"/;            # escape unescaped quotes
                            (/[\s\,\"]/) ? "\"$_\"" : $_; # quote strings that need it
                          } @param;
  my ($sec,$min,$hour,$day,$mon,$year) = localtime;
  my $time = sprintf("%4d-%02d-%02d,%02d:%02d",$year+1900,$mon+1,$day,$hour,$min);

  ### spit it out
  open(_OUT,">>$dir/$file") || return "&#91;Couldn't open $file for writing: $!&#93;";
  print _OUT "$time,$str\n";
  close(_OUT);

  ### move old files if they are too large
  rename("$dir/$file","$dir/$file.$time") if -s "$dir/$file" > 1_000_000;

  return "";
}

sub func_loop {
  my ($self,$tag,$SWAP,$action) = @_;
  my $ikey = ($tag =~ s/^([a-z]\w*)\s+//s) ? $1 : '_';
  $self->{W}->{PROP}->{LAST}->{$ikey} = undef;
  my $array = $self->next_val_from_str(\$tag,$SWAP);
  $array = [] if ! defined($array);
  $array = [1..($array =~ /^\+?(\d+)/ ? $1 : 0)] if ! ref($array);
  my $text = "";
  my $chunk = $self->next_val_from_str(\$tag,$SWAP,'returnstrnotval');
  foreach my $iter (@$array){
    $self->{W}->{pass}->{$ikey} = defined($iter) ? $iter : "";
    my $copy = $chunk;
    $self->wrap_swap(\$copy);
    if( $self->{W}->{PROP}->{LAST}->{$ikey} ){
      if( $self->{W}->{PROP}->{LAST}->{$ikey} =~ /last(\w*)/ ){
        $text .= $copy if $1;
        last;
      }elsif( $self->{W}->{PROP}->{LAST}->{$ikey} =~ /next(\w*)/ ){
        $self->{W}->{PROP}->{LAST}->{$ikey} = undef;
        next;
      }
    }
    $text .= $copy;
  }
  delete $self->{W}->{pass}->{$ikey};
  delete $self->{W}->{PROP}->{LAST}->{$ikey};
  return $text;
}

sub func_md5 {
  my $self = shift;
  my($tag,$SWAP,$action) = @_;
  my $str = $self->next_val_from_str(\$tag,$SWAP);
  $self->wrap_swap(\$str);

  require Digest::MD5;
  return &Digest::MD5::md5_hex($str);
}

sub func_nowrap {
  my $self = shift;
  my $tag  = shift;
  if( $tag =~ /^(\w+)\.(\w+)$/ ){
    my ($area,$name) = ($1,$2);
    $self->load_element($area,$name);
    return $self->{W}->{$area}->{$name};
  }
  return $tag;
}

sub func_numeric {
  my ($self,$tag,$SWAP,$action) = @_;
  my $n = $self->next_val_from_str(\$tag,$SWAP);
  if   ( $action eq '&&' ){ $n = $n && $self->next_val_from_str(\$tag,$SWAP) while $tag && $n; $n = $n ? 1 : 0 }
  elsif( $action eq '||' ){ $n = $n || $self->next_val_from_str(\$tag,$SWAP) while $tag && ! $n; $n = $n ? $n : '' }
  else{
    $n = numify($n);
    if   ( $action eq '+'  ){ $n += numify($self->next_val_from_str(\$tag,$SWAP)) while $tag; }
    elsif( $action eq '-'  ){ $n -= numify($self->next_val_from_str(\$tag,$SWAP)) while $tag; }
    elsif( $action eq '*'  ){ $n *= numify($self->next_val_from_str(\$tag,$SWAP)) while $tag; }
    elsif( $action eq '/'  ){
      while($tag) {
        my $divisor = numify($self->next_val_from_str(\$tag,$SWAP)); 
        if($divisor != 0) {
          $n /= $divisor;
        }
      }
    }
    elsif( $action eq '%'  ){ $n %= numify($self->next_val_from_str(\$tag,$SWAP)) while $tag; }
    elsif( $action eq '**' ){ $n = $n ** numify($self->next_val_from_str(\$tag,$SWAP)); }
    elsif( $action eq '>'  ){ return ($n >  numify($self->next_val_from_str(\$tag,$SWAP)) ? 1 : 0); }
    elsif( $action eq '<'  ){ return ($n <  numify($self->next_val_from_str(\$tag,$SWAP)) ? 1 : 0); }
    elsif( $action eq '>=' ){ return ($n >= numify($self->next_val_from_str(\$tag,$SWAP)) ? 1 : 0); }
    elsif( $action eq '<=' ){ return ($n <= numify($self->next_val_from_str(\$tag,$SWAP)) ? 1 : 0); }
    elsif( $action eq '==' ){ return ($n == numify($self->next_val_from_str(\$tag,$SWAP)) ? 1 : 0); }
    elsif( $action eq '!=' ){ return ($n != numify($self->next_val_from_str(\$tag,$SWAP)) ? 1 : 0); }
  }
  return $n;
}

sub func_random {
  my ($self,$tag,$SWAP,$action) = @_;
  my $from = numify($self->next_val_from_str(\$tag,$SWAP));
  my $to = numify($self->next_val_from_str(\$tag,$SWAP));
  ($from,$to) = ($to,$from) if $from > $to;
  return int(rand($to-$from+1) + $from);
}

sub func_regex {
  my ($self,$tag,$SWAP,$action) = @_;
  my $regex = $self->next_val_from_str(\$tag,$SWAP);
  my @val = ();
  while( $tag ){
    my $val = $self->next_val_from_str(\$tag,$SWAP);
    push @val, (ref($val) ? @$val : $val);
  }
  
  my @ret = ();
  if( $regex =~ m|^m(.)(.+?)\1([smixg]*)$| ){
    my ($pat,$op) = ($2,$3);
    my $global = $op =~ s/g//g;
    foreach my $val (@val) {
      push @ret, ($global ? ($val =~ m/(?$op:$pat)/g) : ($val =~ m/(?$op:$pat)/));
    }
  }elsif( $regex =~ m|^s(.)(.+?)\1(.*)\1([smixg]*)$| ){
    my ($pat,$swap,$op) = ($2,$3,$4);
    my $global = $op =~ s/g//g;
    foreach my $val (@val) {
      if( $global ){
        $val =~ s{(?$op:$pat)}{
          my @match = (undef,$1,$2,$3,$4,$5,$6); # limit on the number of matches
          my $copy = $swap;
          $copy =~ s/\$(\d+)/defined($match[$1]) ? $match[$1] : ""/ge;
          $copy =~ s/\\n/\n/g;
          $copy =~ s/\\r/\r/g;
          $copy; # return of the swap
        }eg;
      }else{
        $val =~ s{(?$op:$pat)}{
          my @match = (undef,$1,$2,$3,$4,$5,$6); # limit on the number of matches
          my $copy = $swap;
          $copy =~ s/\$(\d+)/defined($match[$1]) ? $match[$1] : ""/ge;
          $copy =~ s/\\n/\n/g;
          $copy =~ s/\\r/\r/g;
          $copy; # return of the swap
        }e;
      }
      push @ret, $val;
    }
  }
  $self->{W}->{regex}->{_} = \@ret;
  return @ret > 1 ? \@ret : defined($ret[0]) ? $ret[0] : '';
}

sub func_split {
  my ($self,$tag,$SWAP,$action) = @_;
  my $regex = $self->next_val_from_str(\$tag,$SWAP);
  my $str   = $self->next_val_from_str(\$tag,$SWAP);
  my $n     = numify($self->next_val_from_str(\$tag,$SWAP));
  if( $regex !~ m|^m(.)(.+?)\1([smix]*)$| ){
    return ["(Invalid split regex '$regex')"];
  }
  $regex = qr/(?$3:$2)/;
  my @ret = $n ? split($regex,$str,$n) : split($regex,$str);
  return \@ret;
}


sub func_sprintf {
  my ($self,$tag,$SWAP,$action) = @_;
  my $format = $self->next_val_from_str(\$tag,$SWAP);
  $self->wrap_swap(\$format);
  my @val = ();
  push @val, $self->next_val_from_str(\$tag,$SWAP) while $tag;
  return eval{
    local $^W = 0;
    sprintf($format,@val); # return of eval
  } || "&#91; Error in sprintf - $@ &#93;";
}

sub func_sort {
  my $array = shift()->wrap_buddy(shift());
  return $array if ! defined($array) || ! ref($array);
  return [sort @$array];
}

sub func_store {
  my ($self,$tag,$SWAP,$action) = @_;
  my $element = $self->next_val_from_str(\$tag,$SWAP,'returnstrnotval');
  $element =~ s/[\[\]]//g;

  my ($area,$name);
  if( $element =~ /^(\w+)$/ ){
    ($area,$name) = ($1,"");
  }elsif( $element =~ /^(\w+)\.(\w+)/ ){
    ($area,$name) = ($1,$2);
  }else{
    return "&#91;Invalid store ($element)&#93;";
  }
  
  return "&#91;Protected area ($area)&#93;" if $AREAS{$area};

  ### where does this conf come from
  my ($cgi,$file) = (exists $self->{W}->{PROP}->{$area})
    ? ($self->{W}->{PROP}->{$area}->{cgi}, $self->{W}->{PROP}->{$area}->{area})
    : ($self->{W}->{PROP}->{ALL}->{cgi}, $area);

  $self->load_site;
  my $s2sr = $self->{user}->{s2sr} || &O::s2dr::s2sr($ENV{HTTP_HOST} || '');
  return "&#91;Cannot store a value on an uninstalled site.&#93;" if ! -d $s2sr;
  mkdir("$s2sr/filebox",0755)           if ! -d "$s2sr/filebox";
  mkdir("$s2sr/filebox/wrap",0755)      if ! -d "$s2sr/filebox/wrap";
  mkdir("$s2sr/filebox/wrap/$cgi",0755) if ! -d "$s2sr/filebox/wrap/$cgi";

  my $update = $name ? {$name => $self->{W}->{$area}->{$name}} : $self->{W}->{$area};
  eval{ &O::ConfUtil::conf_write("$s2sr/filebox/wrap/$cgi/$file.wrap",$update) };
  if( $@ ){
    return "&#91;Error saving values ($@)&#93;";
  }
  return "";

}

sub func_sub {
  my $self = shift;
  my $tag  = shift;

  $tag =~ s/^([\w]+)($|\s+)// || return "&#91;Invalid sub call &#91;$tag&#93;&#93;";
  my $sub_name = $1;

  ### load the sub if necessary
  if( $sub_name !~ /^isun$/i ){
    if( ! exists $self->{W}->{form}->{$sub_name} ){
      $self->{W}->{form}->{$sub_name} = $self->get_form_val( $sub_name );
    }
    
    if( ! defined $self->{W}->{form}->{$sub_name} ){
      return "&#91;Non existant sub \"$sub_name\"&#93;";
    }elsif( ref($self->{W}->{form}->{$sub_name}) ne 'CODE' ){
      return "&#91;Item \"$sub_name\" is not a subroutine&#93;";
    }
  }elsif( $tag eq 'form' ){
    ### flatten the form arrays
    foreach my $_form (@{ $self->{form} }){
      next if ! ref($_form);
      foreach my $key (keys %$_form){
        next if exists $self->{W}->{form}->{$key};
        $self->{W}->{form}->{$key} = $_form->{$key}
      }
    }
  }

  ### grab parameters
  my $is_area = ($tag =~ /^[a-z]\w*$/i);
  my $param = [];
  if( $is_area ){
    push @$param, $self->{W}->{$tag};
  }else{
    foreach my $val (split(/\s+/,$tag)){
      push @$param, $self->wrap_buddy($val);
    }
  }

  ### allow for debugging
  if( $sub_name =~ /^isun$/i ){
    my $txt  = &O::Is::is_dump($is_area ? $param->[0] : $param);
    &O::Is::is_cleanup(\$txt,$tag,1);
    &O::Is::content_type();
    print "<b>Wrap isun...</b><br>\n" . $txt ."<hr width=100%>\n";
    return '';
  }

  ### get the result and swap it
  my $txt = &{ $self->{W}->{form}->{$sub_name} }( @$param );
  $self->wrap_swap(\$txt) if defined($txt) && ! ref($txt);
  return $txt;
}

sub func_uc {
  my $txt = shift()->wrap_buddy(shift());
  $txt = "" if ! defined $txt;
  return "\U$txt";
}

sub func_uenc {
  my $txt = shift()->wrap_buddy(shift());
  $txt = "" if ! defined $txt;
  $txt =~ s/([^\w\.\-\/])/sprintf("%%%02X",ord($1))/eg;
  return $txt;
}

sub func_values {
  my ($self,$tag,$SWAP,$action) = @_;
  $self->wrap_unswap(\$tag,$SWAP);
  $self->wrap_swap(\$tag);
  if( $tag =~ /^(\w+)$/ ){
    $self->load_element($tag,$UNIQUE_KEY);
    delete $self->{W}->{$tag}->{$UNIQUE_KEY};
    return [] if ! $self->{W}->{$1};
    return [values %{$self->{W}->{$1}}];
  }elsif( $tag =~ /^(\w+)\.([^\.\n]+)$/ ){
    my ($area,$name) = ($1,$2);
    $self->load_element($area,$name);
    my $ref = $self->{W}->{$area} ? $self->{W}->{$area}->{$name} : undef;
    $ref = defined($ref) ? (ref($ref) ? $ref : [($area ne 'form' || length($ref)) ? $ref : ()]) : [];
    return [values %$ref] if ref($ref) eq 'HASH';
    return [@$ref];
  }
  return [];
}

sub func_var {
  my $self = shift;
  my $tag  = shift;
  
  $tag =~ s/^(\S+)//os || return ''; # optimized
  my $one = $1;
  $one =~ s/=(\w+)$/$1/ if index($one,'=') > -1; # replace [var this=sub that]
  $tag =~ s/^\s*\=//;
  $tag =~ s/^\s+//;

  ### do entire areas
  if( ! ($one =~ /\W/) ){ # optimized

    if( $AREAS{$one} && $one ne 'text' ){ # optimized
      return "&#91;Area \"$one\" may not be set as an area&#93;";
    }

    if( $tag =~ /^references\s+(\w+)\s*$/ ){
      if( defined $self->{W}->{$1} && ref $self->{W}->{$1} ){
        $self->{W}->{$one} = $self->{W}->{$1};
        return '';
      }else{
        return "&#91;Area \"$1\" is not defined&#93;";
      }
    }

    ### allow for setting an area equal to a return
    if( $tag =~ s/^sub\s+// ){
      my $ref = $self->func_sub( $tag );
      if( ! ref($ref) || ! UNIVERSAL::isa($ref,'HASH') ){
        return "&#91;Area \"$one\" could not be set using &#91;sub $tag&#93;&#93;";
      }else{
        $self->{W}->{$one} = $ref;
        return '';
      }
    }

    ### allow for complex creation of the thing that will get set in var
    my $ref = $self->wrap_buddy($tag);
    if( ref($ref) eq 'HASH' ){
      $self->{W}->{$one} = $ref;
      return '';
    }
    $tag = '"'.$ref.'"';

    if( $tag =~ m%^["']inherit[:\s]+([\w\.\-]*)['"]$% ){
      my $site = $1;
      $self->wrap_swap(\$site);
      if( ! $site ){
        delete $self->{W}->{DIRS}->{$one};
        delete $self->{W}->{DIRS_C}->{$one};
      }else{
        $self->{W}->{DIRS}->{$one}   = $self->get_wrap_directories(O::Partner->new($site));
        $self->{W}->{DIRS_C}->{$one} = $self->get_content_directories(O::Partner->new($site));
      }
      return "";

    }elsif( $tag =~ m%^["']wrap[:\s]+([\w\/\.\-]+(\.wrap)?)['"]$% ){

      ### ask for a different location or request a specific file
      my($file, $cgi, $step, $area) = ($1, '','','');
      if( $file =~ m|^(\w+)/(\w+)$| ){
        ($cgi,$step,$area)=($1,$2,$one);
      }elsif( $file =~ m|^(\w+)/(\w+)/([\w/]+)\.wrap$| ){
        ($cgi,$step,$area)=($1,$2,$3);
      }elsif( $file =~ m|^(\w+)/(\w+)\.wrap$| ){
        ($cgi,$step,$area)=($1,'',$2);
      }elsif( $file =~ m|^(\w+)\.wrap$| ){
        ($cgi,$step,$area)=('','',$1);
      }else{
        return "&#91;Area \"$one\" -- bad filename \"$file\"&#93;";
      }
      $area = 'ALL' if $one eq 'ALL';
      my $ref = $self->{W}->{PROP}->{$one} ||= {};
      $ref->{cgi}  = $cgi;
      $ref->{step} = $step;
      $ref->{area} = $area;

      $self->{W}->{$one} = {};
      foreach (keys %{ $self->{W}->{FILE} }){
        delete $self->{W}->{FILE}->{$_} if index($_,"$one.wrap - $area") > -1;
      }

    }elsif( $tag =~ m%^["']content[:\s]+([\w\/\.\-]+)['"]\s*$% ){
      my $file = $1;
      return "&#91;Unsecure file \"$file\"&#93;" if $file=~m|\.\./| || $file=~m|^/|;
      $file = "$self->{W}->{PROP}->{ALL}->{cgi}/$file" if ! ($file =~ m|/|);
      $file =~ s|(/[^/\.]+)$|$1.htm|;

      my $ref = $self->{W}->{PROP}->{$one} ||= {};
      $ref->{content} = $file;
      $self->{W}->{$one} = {};
      foreach (keys %{ $self->{W}->{FILE} }){
        delete $self->{W}->{FILE}->{$_} if index($_,"$file - $one") > -1;
      }

    }else{
      return "&#91;Area \"$one\" -- unknown option \"$tag\"&#93;";
    }

  }else{

    $self->wrap_swap(\$one);

    ### override individual elements
    if( my($area,$name,$index) = split(/\./,$one) ){ # optimized

      my $do_push = (@_ && $_[0] eq 'push');
      my $ref;
      
      ### allow for setting alts or indi
      if( defined($index) ){
        if( $index eq 'alt' ){
          my $val = numify($self->wrap_buddy($tag));
          $val --;
          $val = 0 if $val < 0;
          $self->{W}->{ALT_POINTERS}->{$area}->{$name} = $val;
          return '';
        }elsif( $index =~ /^\d+$/ ){
          $self->load_element($area,$name);        
          if( ref($self->{W}->{$area}->{$name}) ){
            if( $index > $#{ $self->{W}->{$area}->{$name} } + 1 ){
              return "&#91;Set index out of bounds ($area.$name.$index)&#93;";
            }elsif( $index == 0 ){
              $self->{W}->{ALT_POINTERS}->{$area}->{$name} ||= 0;
              $ref = \$self->{W}->{$area}->{$name}->[ $self->{W}->{ALT_POINTERS}->{$area}->{$name} ];
            }else{
              $ref = \$self->{W}->{$area}->{$name}->[ $index - 1 ];
            }
          }else{
            return "&#91;Set index out of bounds ($area.$name.$index)&#93;" if $index !~ /^[01]$/;
            $ref = \$self->{W}->{$area}->{$name};
          }
        }else{
          return "&#91;Cannot set $area,$name,$index&#93;";        
        }

        ### normal areas
      }else{
        $ref = \$self->{W}->{$area}->{$name};
      }

      if( $do_push ){
        $$ref = [] if ! defined $$ref;
        $$ref = [$$ref] if ! ref $$ref; 
        $ref = \$$ref->[@{ $$ref }];
      }

      if( $tag =~ /^references\s+(\w+)\.(\w+)\s*$/ ){ # optimized
        my ($_area,$_name) = ($1,$2);
        $self->load_element($_area,$_name);
        if( defined $self->{W}->{$_area} && defined $self->{W}->{$_area}->{$_name} ){
          $$ref = $self->{W}->{$_area}->{$_name};
        }else{
          return "&#91;&#91;$1.$2&#93; is not defined&#93;";
        }

      }elsif( $tag =~ s/^literal\b\s*// ){
        $$ref = $tag;

      }elsif( $tag =~ m%^["']content[:\s]+([\w\/\.\-]+)['"](\s+\S+)?$% ){
        my $file = $1;
        my $relative = ($2 && $2 =~ /relative/i) ? 1 : 0;
        return "&#91;Unsecure file \"$file\"&#93;" if $file=~m|\.\./| || $file=~m|^/|;
        $file = "$self->{W}->{PROP}->{ALL}->{cgi}/$file" unless $file =~ m|/|;
        my $ret = $self->{partner}->content($file,{suppress_header=>1, relative_to_s2dr=>$relative});
        if( $do_push && ref($ret) ){
          splice @{ $self->{W}->{$area}->{$name} }, -1, 1, $ret;
        }else{
          $$ref = $ret;
        }

      }elsif( $tag =~ /^(")([^\"]*)(")\s*$/ ||
              $tag =~ /^(')([^\']*)(')\s*$/ ||
              $tag =~ /^q(\{)(.*)\}\s*$/s ||
              $tag =~ /^q(\()(.*)\)\s*$/s ||
              $tag =~ /^q(\<)(.*)\>\s*$/s ||
              $tag =~ /^q([^\s\w])(.*)\1\s*$/s
              ){
        my $txt = $2;
        $txt =~ s/%([a-f0-9]{2})/chr(hex($1))/egi;
        my $ret = check_for_alt( $txt );
        if( $do_push && ref($ret) ){
          splice @{ $self->{W}->{$area}->{$name} }, -1, 1, @$ret;
        }else{
          $$ref = $ret;
        }

      }elsif( $tag =~ /^(-?(?:\d*\.\d+|\d+))\s*$/ ){
        $$ref = $1;

      }else{
        my $ret = $self->wrap_buddy($tag);
        if( $do_push && ref($ret) ){
          splice @{ $self->{W}->{$area}->{$name} }, -1, 1, @$ret;
        }else{
          $$ref = $ret;
        }

      }
      
    }else{
      return "&#91;Unknown var or push declaration \"$tag\"&#93;";
    }
  }


  ### should be good
  return '';
}

sub func_wordwrap {
  my ($self,$tag,$SWAP,$action) = @_;
  my $width = abs(numify($self->next_val_from_str(\$tag,$SWAP))) || 50;
  $width = 2 if $width < 2;
  my $in = $self->next_val_from_str(\$tag,$SWAP);
  $self->wrap_swap(\$in);
  my $out = "";
  while ( 1 ){
    if( $in =~ s/^(.{2,$width}?)$// ){
      $out .= "$1\n";
    }elsif( $in =~ s/^(.{2,$width}?)[\ \t]*\n/\n/ ){
      $out .= "$1\n";
    }elsif( $in =~ s/^(.{2,$width})[\ \t]+(.)/$2/ ){
      $out .= "$1\n";
    }elsif( $in =~ s/^(\S{$width,})\s*// ){
      $out .= "$1\n";
    }elsif( $in =~ s/^\n([\r\n]*)// ){
      $out .= $1;
    }else{
      last;
    }
  }
  chomp($out) if ! length($in);
  $out .= $in;
  return $out;
}

# function to help track which templates are actually still in use
sub func_throw {
  my ($self, $tag) = @_;
  require O::ERROR;
  O::ERROR::THROW($tag, {
    template => $self->{W}{PROP}{ALL}{step},
  });
  return "<!-- threw $tag -->";
}

###----------------------------------------------------------------###

sub contains_brackets {
  my $str = shift;
  my $txt = ref($str) ? $str : \$str;

  return $$txt =~ /\[\w+(\.\w+|\s)\.*?\]/s;
}

sub get_unresolved_brackets {
  my $str = shift;
  my $txt = ref($str) ? $str : \$str;

  my $return = [];

  while($$txt =~ /(\[.*?\])/gs) {
    push @{$return}, $1;
  }
  return $return;
}

1;
