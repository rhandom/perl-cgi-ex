package CGI::Ex::Template;

###----------------------------------------------------------------###
#  See the perldoc in CGI/Ex/Template.pod
#  Copyright 2007 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

use strict;
use warnings;
use base qw(Exporter);
use CGI::Ex::Template::Exception;

our $VERSION   = '2.14';
our @EXPORT_OK = qw(@CONFIG_COMPILETIME @CONFIG_RUNTIME
                    $QR_OP $QR_OP_ASSIGN $QR_OP_PREFIX $QR_PRIVATE
                    $OP $OP_ASSIGN $OP_PREFIX $OP_POSTFIX $OP_DISPATCH);

###----------------------------------------------------------------###

our $AUTOLOAD;
our $AUTOIMPORT = {
    TT      => [qw(parse_tree_tt3 process)],
    Compile => [qw(load_perl)],
    HTE     => [qw(parse_tree_hte param output register_function clear_param query new_file new_scalar_ref new_array_ref new_filehandle)],
    Parse   => [qw(parse_tree parse_expr apply_precedence parse_args dump_parse dump_parse_expr)],
    Play    => [qw(play_tree)],
    Tmpl    => [qw(parse_tree_tmpl set_delimiters set_strip set_value set_values parse_string set_dir parse_file loop_iteration fetch_loop_iteration)],
};
our $AUTOLOOKUP = { map { my $type = $_; map { ($_ => $type) } @{ $AUTOIMPORT->{$type} } } keys %$AUTOIMPORT };

sub DESTROY {}
sub AUTOLOAD {
    my $self = shift;
    my $meth = ($AUTOLOAD && $AUTOLOAD =~ /::(\w+)$/) ? $1 : $self->throw('autoload', "Invalid method $AUTOLOAD");
    my $type = delete($AUTOLOOKUP->{$meth}) || do { $meth = "SUPER::$meth"; $self->$meth(@_) };

    my $pkg  = __PACKAGE__."::$type";
    my $file = "$pkg.pm";
    $file =~ s|::|/|g;
    require $file;

    for my $name (@{ $AUTOIMPORT->{ $type }}) {
        no strict 'refs';
        *{__PACKAGE__."::$name"} = \&{"$pkg\::$name"};
    }

    return $self->$meth(@_);
}

###----------------------------------------------------------------###

our $QR_PRIVATE = qr/^[_.]/;

our $SYNTAX = {
    cet  => sub { shift->parse_tree_tt3(@_) },
    ht   => sub { my $self = shift; local $self->{'V2EQUALS'} = 0; local $self->{'EXPR'} = 0; $self->parse_tree_hte(@_) },
    hte  => sub { my $self = shift; local $self->{'V2EQUALS'} = 0; $self->parse_tree_hte(@_) },
    tt3  => sub { shift->parse_tree_tt3(@_) },
    tt2  => sub { my $self = shift; local $self->{'V2PIPE'} = 1; $self->parse_tree_tt3(@_) },
    tt1  => sub { my $self = shift; local $self->{'V2PIPE'} = 1; local $self->{'V1DOLLAR'} = 1; $self->parse_tree_tt3(@_) },
    tmpl => sub { shift->parse_tree_tmpl(@_) },
};

our $SCALAR_OPS = {
    '0'      => sub { $_[0] },
    abs      => sub { local $^W; abs shift },
    atan2    => sub { local $^W; atan2($_[0], $_[1]) },
    chunk    => \&vmethod_chunk,
    collapse => sub { local $_ = $_[0]; s/^\s+//; s/\s+$//; s/\s+/ /g; $_ },
    cos      => sub { local $^W; cos $_[0] },
    defined  => sub { defined $_[0] ? 1 : '' },
    exp      => sub { local $^W; exp $_[0] },
    fmt      => \&vmethod_fmt_scalar,
    'format' => \&vmethod_format,
    hash     => sub { {value => $_[0]} },
    hex      => sub { local $^W; hex $_[0] },
    html     => sub { local $_ = $_[0]; s/&/&amp;/g; s/</&lt;/g; s/>/&gt;/g; s/\"/&quot;/g; s/\'/&apos;/g; $_ },
    indent   => \&vmethod_indent,
    int      => sub { local $^W; int $_[0] },
    item     => sub { $_[0] },
    js       => sub { local $_ = $_[0]; return if ! $_; s/\n/\\n/g; s/\r/\\r/g; s/(?<!\\)([\"\'])/\\$1/g; $_ },
    lc       => sub { lc $_[0] },
    lcfirst  => sub { lcfirst $_[0] },
    length   => sub { defined($_[0]) ? length($_[0]) : 0 },
    list     => sub { [$_[0]] },
    log      => sub { local $^W; log $_[0] },
    lower    => sub { lc $_[0] },
    match    => \&vmethod_match,
    new      => sub { defined $_[0] ? $_[0] : '' },
    null     => sub { '' },
    oct      => sub { local $^W; oct $_[0] },
    rand     => sub { local $^W; rand shift },
    remove   => sub { vmethod_replace(shift, shift, '', 1) },
    repeat   => \&vmethod_repeat,
    replace  => \&vmethod_replace,
    search   => sub { my ($str, $pat) = @_; return $str if ! defined $str || ! defined $pat; return $str =~ /$pat/ },
    sin      => sub { local $^W; sin $_[0] },
    size     => sub { 1 },
    split    => \&vmethod_split,
    sprintf  => sub { local $^W; my $pat = shift; sprintf($pat, @_) },
    sqrt     => sub { local $^W; sqrt $_[0] },
    srand    => sub { local $^W; srand $_[0]; '' },
    stderr   => sub { print STDERR $_[0]; '' },
    substr   => \&vmethod_substr,
    trim     => sub { local $_ = $_[0]; s/^\s+//; s/\s+$//; $_ },
    uc       => sub { uc $_[0] },
    ucfirst  => sub { ucfirst $_[0] },
    upper    => sub { uc $_[0] },
    uri      => \&vmethod_uri,
    url      => \&vmethod_url,
};

our $FILTER_OPS = { # generally - non-dynamic filters belong in scalar ops
    eval     => [\&filter_eval, 1],
    evaltt   => [\&filter_eval, 1],
    file     => [\&filter_redirect, 1],
    redirect => [\&filter_redirect, 1],
};

our $LIST_OPS = {
    defined => sub { return 1 if @_ == 1; defined $_[0]->[ defined($_[1]) ? $_[1] : 0 ] },
    first   => sub { my ($ref, $i) = @_; return $ref->[0] if ! $i; return [@{$ref}[0 .. $i - 1]]},
    fmt     => \&vmethod_fmt_list,
    grep    => sub { local $^W; my ($ref, $pat) = @_; [grep {/$pat/} @$ref] },
    hash    => sub { local $^W; my $list = shift; return {@$list} if ! @_; my $i = shift || 0; return {map {$i++ => $_} @$list} },
    import  => sub { my $ref = shift; push @$ref, grep {defined} map {ref eq 'ARRAY' ? @$_ : undef} @_; '' },
    item    => sub { $_[0]->[ $_[1] || 0 ] },
    join    => sub { my ($ref, $join) = @_; $join = ' ' if ! defined $join; local $^W; return join $join, @$ref },
    last    => sub { my ($ref, $i) = @_; return $ref->[-1] if ! $i; return [@{$ref}[-$i .. -1]]},
    list    => sub { $_[0] },
    max     => sub { local $^W; $#{ $_[0] } },
    merge   => sub { my $ref = shift; return [ @$ref, grep {defined} map {ref eq 'ARRAY' ? @$_ : undef} @_ ] },
    new     => sub { local $^W; return [@_] },
    null    => sub { '' },
    nsort   => \&vmethod_nsort,
    pick    => \&vmethod_pick,
    pop     => sub { pop @{ $_[0] } },
    push    => sub { my $ref = shift; push @$ref, @_; return '' },
    reverse => sub { [ reverse @{ $_[0] } ] },
    shift   => sub { shift  @{ $_[0] } },
    size    => sub { local $^W; scalar @{ $_[0] } },
    slice   => sub { my ($ref, $a, $b) = @_; $a ||= 0; $b = $#$ref if ! defined $b; return [@{$ref}[$a .. $b]] },
    sort    => \&vmethod_sort,
    splice  => \&vmethod_splice,
    unique  => sub { my %u; return [ grep { ! $u{$_}++ } @{ $_[0] } ] },
    unshift => sub { my $ref = shift; unshift @$ref, @_; return '' },
};

our $HASH_OPS = {
    defined => sub { return 1 if @_ == 1; defined $_[0]->{ defined($_[1]) ? $_[1] : '' } },
    delete  => sub { my $h = shift; delete @{ $h }{map {defined($_) ? $_ : ''} @_}; '' },
    each    => sub { [%{ $_[0] }] },
    exists  => sub { exists $_[0]->{ defined($_[1]) ? $_[1] : '' } },
    fmt     => \&vmethod_fmt_hash,
    hash    => sub { $_[0] },
    import  => sub { my ($a, $b) = @_; @{$a}{keys %$b} = values %$b if ref($b) eq 'HASH'; '' },
    item    => sub { my ($h, $k) = @_; $k = '' if ! defined $k; $QR_PRIVATE && $k =~ $QR_PRIVATE ? undef : $h->{$k} },
    items   => sub { [ %{ $_[0] } ] },
    keys    => sub { [keys %{ $_[0] }] },
    list    => \&vmethod_list_hash,
    new     => sub { local $^W; return (@_ == 1 && ref $_[-1] eq 'HASH') ? $_[-1] : {@_} },
    null    => sub { '' },
    nsort   => sub { my $ref = shift; [sort {   $ref->{$a} <=>    $ref->{$b}} keys %$ref] },
    pairs   => sub { [map { {key => $_, value => $_[0]->{$_}} } sort keys %{ $_[0] } ] },
    size    => sub { scalar keys %{ $_[0] } },
    sort    => sub { my $ref = shift; [sort {lc $ref->{$a} cmp lc $ref->{$b}} keys %$ref] },
    values  => sub { [values %{ $_[0] }] },
};

our $VOBJS = {
    Text => $SCALAR_OPS,
    List => $LIST_OPS,
    Hash => $HASH_OPS,
};
foreach (values %$VOBJS) {
    $_->{'Text'} = $_->{'fmt'};
    $_->{'Hash'} = $_->{'hash'};
    $_->{'List'} = $_->{'list'};
}

### setup the operator parsing
our $OPERATORS = [
    # type      precedence symbols              action (undef means play_operator will handle)
    ['prefix',  99,        ['\\'],              undef                       ],
    ['postfix', 98,        ['++'],              undef                       ],
    ['postfix', 98,        ['--'],              undef                       ],
    ['prefix',  97,        ['++'],              undef                       ],
    ['prefix',  97,        ['--'],              undef                       ],
    ['right',   96,        ['**', 'pow'],       sub {     $_[0] ** $_[1]  } ],
    ['prefix',  93,        ['!'],               sub {   ! $_[0]           } ],
    ['prefix',  93,        ['-'],               sub { @_ == 1 ? 0 - $_[0] : $_[0] - $_[1] } ],
    ['left',    90,        ['*'],               sub {     $_[0] *  $_[1]  } ],
    ['left',    90,        ['/'],               sub {     $_[0] /  $_[1]  } ],
    ['left',    90,        ['div', 'DIV'],      sub { int($_[0] /  $_[1]) } ],
    ['left',    90,        ['%', 'mod', 'MOD'], sub {     $_[0] %  $_[1]  } ],
    ['left',    85,        ['+'],               sub {     $_[0] +  $_[1]  } ],
    ['left',    85,        ['-'],               sub { @_ == 1 ? 0 - $_[0] : $_[0] - $_[1] } ],
    ['left',    85,        ['~', '_'],          undef                       ],
    ['none',    80,        ['<'],               sub {     $_[0] <  $_[1]  } ],
    ['none',    80,        ['>'],               sub {     $_[0] >  $_[1]  } ],
    ['none',    80,        ['<='],              sub {     $_[0] <= $_[1]  } ],
    ['none',    80,        ['>='],              sub {     $_[0] >= $_[1]  } ],
    ['none',    80,        ['lt'],              sub {     $_[0] lt $_[1]  } ],
    ['none',    80,        ['gt'],              sub {     $_[0] gt $_[1]  } ],
    ['none',    80,        ['le'],              sub {     $_[0] le $_[1]  } ],
    ['none',    80,        ['ge'],              sub {     $_[0] ge $_[1]  } ],
    ['none',    75,        ['=='],              sub {     $_[0] == $_[1]  } ],
    ['none',    75,        ['eq'],              sub {     $_[0] eq $_[1]  } ],
    ['none',    75,        ['!='],              sub {     $_[0] != $_[1]  } ],
    ['none',    75,        ['ne'],              sub {     $_[0] ne $_[1]  } ],
    ['none',    75,        ['<=>'],             sub {     $_[0] <=> $_[1] } ],
    ['none',    75,        ['cmp'],             sub {     $_[0] cmp $_[1] } ],
    ['left',    70,        ['&&'],              undef                       ],
    ['right',   65,        ['||'],              undef                       ],
    ['none',    60,        ['..'],              sub {     $_[0] .. $_[1]  } ],
    ['ternary', 55,        ['?', ':'],          undef                       ],
    ['assign',  53,        ['+='],              sub {     $_[0] +  $_[1]  } ],
    ['assign',  53,        ['-='],              sub {     $_[0] -  $_[1]  } ],
    ['assign',  53,        ['*='],              sub {     $_[0] *  $_[1]  } ],
    ['assign',  53,        ['/='],              sub {     $_[0] /  $_[1]  } ],
    ['assign',  53,        ['%='],              sub {     $_[0] %  $_[1]  } ],
    ['assign',  53,        ['**='],             sub {     $_[0] ** $_[1]  } ],
    ['assign',  53,        ['~=', '_='],        sub {     $_[0] .  $_[1]  } ],
    ['assign',  52,        ['='],               undef                       ],
    ['prefix',  50,        ['not', 'NOT'],      sub {   ! $_[0]           } ],
    ['left',    45,        ['and', 'AND'],      undef                       ],
    ['right',   40,        ['or', 'OR'],        undef                       ],
];
our ($QR_OP, $QR_OP_PREFIX, $QR_OP_ASSIGN, $OP, $OP_PREFIX, $OP_DISPATCH, $OP_ASSIGN, $OP_POSTFIX, $OP_TERNARY);
sub _op_qr { # no mixed \w\W operators
    my %used;
    my $chrs = join '|', reverse sort map {quotemeta $_} grep {++$used{$_} < 2} grep {! /\{\}|\[\]/} grep {/^\W{2,}$/} @_;
    my $chr  = join '',          sort map {quotemeta $_} grep {++$used{$_} < 2} grep {/^\W$/}     @_;
    my $word = join '|', reverse sort                    grep {++$used{$_} < 2} grep {/^\w+$/}    @_;
    $chr = "[$chr]" if $chr;
    $word = "\\b(?:$word)\\b" if $word;
    return join('|', grep {length} $chrs, $chr, $word) || die "Missing operator regex";
}
sub _build_ops {
    $QR_OP        = _op_qr(map {@{ $_->[2] }} grep {$_->[0] ne 'prefix'} @$OPERATORS);
    $QR_OP_PREFIX = _op_qr(map {@{ $_->[2] }} grep {$_->[0] eq 'prefix'} @$OPERATORS);
    $QR_OP_ASSIGN = _op_qr(map {@{ $_->[2] }} grep {$_->[0] eq 'assign'} @$OPERATORS);
    $OP           = {map {my $ref = $_; map {$_ => $ref}      @{$ref->[2]}} grep {$_->[0] ne 'prefix' } @$OPERATORS}; # all non-prefix
    $OP_PREFIX    = {map {my $ref = $_; map {$_ => $ref}      @{$ref->[2]}} grep {$_->[0] eq 'prefix' } @$OPERATORS};
    $OP_DISPATCH  = {map {my $ref = $_; map {$_ => $ref->[3]} @{$ref->[2]}} grep {$_->[3]             } @$OPERATORS};
    $OP_ASSIGN    = {map {my $ref = $_; map {$_ => 1}         @{$ref->[2]}} grep {$_->[0] eq 'assign' } @$OPERATORS};
    $OP_POSTFIX   = {map {my $ref = $_; map {$_ => 1}         @{$ref->[2]}} grep {$_->[0] eq 'postfix'} @$OPERATORS}; # bool is postfix
    $OP_TERNARY   = {map {my $ref = $_; map {$_ => 1}         @{$ref->[2]}} grep {$_->[0] eq 'ternary'} @$OPERATORS}; # bool is ternary
}
_build_ops();

our $WHILE_MAX    = 1000;
our $EXTRA_COMPILE_EXT = '.sto';
our $MAX_EVAL_RECURSE  = 50;
our $MAX_MACRO_RECURSE = 50;
our $STAT_TTL          ||= 1;
our $QR_INDEX = '(?:\d*\.\d+ | \d+)';

our @CONFIG_COMPILETIME = qw(SYNTAX ANYCASE INTERPOLATE PRE_CHOMP POST_CHOMP SEMICOLONS V1DOLLAR V2PIPE V2EQUALS);
our @CONFIG_RUNTIME     = qw(DUMP VMETHOD_FUNCTIONS);

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $args  = ref($_[0]) ? { %{ shift() } } : {@_};

  ### allow for lowercase args
  if (my @keys = grep {/^[a-z][a-z_]+$/} keys %$args) {
      @{ $args }{ map { uc $_ } @keys } = delete @{ $args }{ @keys };
  }

  my $self  = bless $args, $class;

  ### "enable" debugging - we only support DEBUG_DIRS and DEBUG_UNDEF
  if ($self->{'DEBUG'}) {
      $self->{'_debug_dirs'}  = 1 if $self->{'DEBUG'} =~ /^\d+$/ ? $self->{'DEBUG'} & 8 : $self->{'DEBUG'} =~ /dirs|all/;
      $self->{'_debug_undef'} = 1 if $self->{'DEBUG'} =~ /^\d+$/ ? $self->{'DEBUG'} & 2 : $self->{'DEBUG'} =~ /undef|all/;
  }

  return $self;
}

###----------------------------------------------------------------###

sub _process {
    my $self = shift;
    my $file = shift;
    local $self->{'_vars'} = shift || {};
    my $out_ref = shift || $self->throw('undef', "Missing output ref");
    local $self->{'_top_level'} = delete $self->{'_start_top_level'};
    my $i = length $$out_ref;

    ### parse and execute
    my $doc;
    eval {
        ### handed us a precompiled document
        if (ref($file) eq 'HASH' && $file->{'_tree'}) {
            $doc = $file;

        ### load the document
        } else {
            $doc = $self->load_template($file) || $self->throw('undef', "Zero length content");;
        }

        ### prevent recursion
        $self->throw('file', "recursion into '$doc->{name}'")
            if ! $self->{'RECURSION'} && $self->{'_in'}->{$doc->{'name'}} && $doc->{'name'} ne 'input text';

        local $self->{'_in'}->{$doc->{'name'}} = 1;
        local $self->{'_component'} = $doc;
        local $self->{'_template'}  = $self->{'_top_level'} ? $doc : $self->{'_template'};
        local @{ $self }{@CONFIG_RUNTIME} = @{ $self }{@CONFIG_RUNTIME};

        #if (! $doc->{'_perl'}) {
        #    $doc->{'_perl'} = $self->load_perl($doc);
        #}
        #$doc->{'_perl'}->($self, $out_ref);

        ### execute the document
        if (! @{ $doc->{'_tree'} }) { # no tags found - just return the content
            $$out_ref .= ${ $doc->{'_content'} };
        } else {
            $self->play_tree($doc->{'_tree'}, $out_ref);
        }

        ### trim whitespace from the beginning and the end of a block or template
        if ($self->{'TRIM'}) {
            substr($$out_ref, $i, length($$out_ref) - $i) =~ s{ \s+ $ }{}x; # tail first
            substr($$out_ref, $i, length($$out_ref) - $i) =~ s{ ^ \s+ }{}x;
        }
    };

    ### handle exceptions
    if (my $err = $@) {
        $err = $self->exception('undef', $err) if ref($err) !~ /Template::Exception$/;
        $err->doc($doc) if $doc && $err->can('doc') && ! $err->doc;
        die $err if ! $self->{'_top_level'} || $err->type !~ /stop|return/;
    }

    return 1;
}

###----------------------------------------------------------------###

sub load_template {
    my $self = shift;
    my $file = shift;
    return if ! defined $file;

    my $doc = {name => $file};
    my $ref = $self->{'_documents'}->{$file};

    ### looks like a string reference
    if (ref $file) {
        $doc->{'_content'}    = $file;
        $doc->{'name'}        = 'input text';
        $doc->{'_is_str_ref'} = 1;

    ### looks like a previously cached-in-memory document
    } elsif ($ref
             && (   time - $ref->{'cache_time'} < ($self->{'STAT_TTL'} || $STAT_TTL) # don't stat more than once a second
                 || $ref->{'modtime'} == (stat $ref->{'_filename'})[9]               # otherwise see if the file was modified
                    )) {
        $doc = $self->{'_documents'}->{$file};
        return $doc;

    ### looks like a block name of some sort
    } elsif ($self->{'BLOCKS'}->{$file}) {
        my $block = $self->{'BLOCKS'}->{$file};
        $block = $block->() if UNIVERSAL::isa($block, 'CODE');
        if (! UNIVERSAL::isa($block, 'HASH')) {
            $self->throw('block', "Unsupported BLOCK type \"$block\"") if ref $block;
            $block = eval { $self->load_template(\$block) } || $self->throw('block', 'Parse error on predefined block');
        }
        $doc->{'_tree'} = $block->{'_tree'} || $self->throw('block', "Invalid block definition (missing tree)");
        return $doc;

    ### handle cached not_founds
    } elsif ($self->{'_not_found'}->{$file}
             && ((time - $self->{'_not_found'}->{$file}->{'cache_time'}
                  < ($self->{'NEGATIVE_STAT_TTL'} || $self->{'STAT_TTL'} || $STAT_TTL))  # negative cache for a second
                 || do { delete $self->{'_not_found'}->{$file}; 0 } # clear cache on failure
                 )) {
        die $self->{'_not_found'}->{$file}->{'exception'};

    ### go and look on the file system
    } else {
        $doc->{'_filename'} = eval { $self->include_filename($file) };
        if (my $err = $@) {
            ### allow for blocks in other files
            if ($self->{'EXPOSE_BLOCKS'} && ! $self->{'_looking_in_block_file'}) {
                local $self->{'_looking_in_block_file'} = 1;
                my $block_name = '';
                while ($file =~ s|/([^/.]+)$||) {
                    $block_name = length($block_name) ? "$1/$block_name" : $1;
                    my $ref = eval { $self->load_template($file) } || next;
                    my $_tree = $ref->{'_tree'};
                    foreach my $node (@$_tree) {
                        next if ! ref $node;
                        next if $node->[0] eq 'META';
                        last if $node->[0] ne 'BLOCK';
                        next if $block_name ne $node->[3];
                        $doc->{'_content'} = $ref->{'_content'};
                        $doc->{'_tree'}    = $node->[4];
                        $doc->{'modtime'} = $ref->{'modtime'};
                        $file = $ref->{'name'};
                        last;
                    }
                }
                $err = '' if ! $doc->{'_tree'};
            } elsif ($self->{'DEFAULT'}) {
                $err = '' if ($doc->{'_filename'} = eval { $self->include_filename($self->{'DEFAULT'}) });
            }
            if ($err) {
                ### cache the negative error
                if (! defined($self->{'NEGATIVE_STAT_TTL'}) || $self->{'NEGATIVE_STAT_TTL'}) {
                    $err = $self->exception('undef', $err) if ref($err) !~ /Template::Exception$/;
                    $self->{'_not_found'}->{$file} = {
                        cache_time => time,
                        exception  => $self->exception($err->type, $err->info." (cached)"),
                    };
                }
                die $err;
            }
        }

        ### no tree yet - look for a file cache
        if (! $doc->{'_tree'}) {
            $doc->{'modtime'} = (stat $doc->{'_filename'})[9];
            if  ($self->{'COMPILE_DIR'} || $self->{'COMPILE_EXT'}) {
                if ($self->{'COMPILE_DIR'}) {
                    $doc->{'_compile_filename'} = $self->{'COMPILE_DIR'} .'/'. $file;
                } else {
                    $doc->{'_compile_filename'} = $doc->{'_filename'};
                }
                $doc->{'_compile_filename'} .= $self->{'COMPILE_EXT'} if defined($self->{'COMPILE_EXT'});
                $doc->{'_compile_filename'} .= $EXTRA_COMPILE_EXT       if defined $EXTRA_COMPILE_EXT;

                if (-e $doc->{'_compile_filename'} && (stat _)[9] == $doc->{'modtime'}) {
                    require Storable;
                    $doc->{'_tree'} = Storable::retrieve($doc->{'_compile_filename'});
                    $doc->{'compile_was_used'} = 1;
                } else {
                    my $str = $self->slurp($doc->{'_filename'});
                    $doc->{'_content'}  = \$str;
                }
            } else {
                my $str = $self->slurp($doc->{'_filename'});
                $doc->{'_content'}  = \$str;
            }
        }

    }

    ### haven't found a parsed tree yet - parse the content into a tree
    if (! $doc->{'_tree'}) {
        if ($self->{'CONSTANTS'}) {
            my $key = $self->{'CONSTANT_NAMESPACE'} || 'constants';
            $self->{'NAMESPACE'}->{$key} ||= $self->{'CONSTANTS'};
        }

        local $self->{'_component'} = $doc;
        $doc->{'_tree'} = eval { $self->parse_tree($doc->{'_content'}) }
            || do { my $e = $@; $e->doc($doc) if UNIVERSAL::can($e, 'doc') && ! $e->doc; die $e }; # errors die
    }

    ### cache parsed_tree in memory unless asked not to do so
    if (! $doc->{'_is_str_ref'} && (! defined($self->{'CACHE_SIZE'}) || $self->{'CACHE_SIZE'})) {
        $self->{'_documents'}->{$file} ||= $doc;
        $doc->{'cache_time'} = time;

        ### allow for config option to keep the cache size down
        if ($self->{'CACHE_SIZE'}) {
            my $all = $self->{'_documents'};
            if (scalar(keys %$all) > $self->{'CACHE_SIZE'}) {
                my $n = 0;
                foreach my $file (sort {$all->{$b}->{'cache_time'} <=> $all->{$a}->{'cache_time'}} keys %$all) {
                    delete($all->{$file}) if ++$n > $self->{'CACHE_SIZE'};
                }
            }
        }
    }

    ### save a cache on the fileside as asked
    if ($doc->{'_compile_filename'} && ! $doc->{'compile_was_used'}) {
        my $dir = $doc->{'_compile_filename'};
        $dir =~ s|/[^/]+$||;
        if (! -d $dir) {
            require File::Path;
            File::Path::mkpath($dir);
        }
        require Storable;
        Storable::store($doc->{'_tree'}, $doc->{'_compile_filename'});
        utime $doc->{'modtime'}, $doc->{'modtime'}, $doc->{'_compile_filename'};
    }

    return $doc;
}

###----------------------------------------------------------------###

sub play_expr {
    ### allow for the parse tree to store literals
    return $_[1] if ! ref $_[1];

    my $self = shift;
    my $var  = shift;
    my $ARGS = shift || {};
    my $i    = 0;

    ### determine the top level of this particular variable access
    my $ref;
    my $name = $var->[$i++];
    my $args = $var->[$i++];
    if (ref $name) {
        if (! defined $name->[0]) { # operator
            return $self->play_operator($name) if wantarray && $name->[1] eq '..';
            $ref = $self->play_operator($name);
        } else { # a named variable access (ie via $name.foo)
            $name = $self->play_expr($name);
            if (defined $name) {
                return if $QR_PRIVATE && $name =~ $QR_PRIVATE; # don't allow vars that begin with _
                return \$self->{'_vars'}->{$name} if $i >= $#$var && $ARGS->{'return_ref'} && ! ref $self->{'_vars'}->{$name};
                $ref = $self->{'_vars'}->{$name};
            }
        }
    } elsif (defined $name) {
        return if $QR_PRIVATE && $name =~ $QR_PRIVATE; # don't allow vars that begin with _
        return \$self->{'_vars'}->{$name} if $i >= $#$var && $ARGS->{'return_ref'} && ! ref $self->{'_vars'}->{$name};
        $ref = $self->{'_vars'}->{$name};
        if (! defined $ref) {
            $ref = ($name eq 'template' || $name eq 'component') ? $self->{"_$name"} : $VOBJS->{$name};
            $ref = $SCALAR_OPS->{$name} if ! $ref && (! defined($self->{'VMETHOD_FUNCTIONS'}) || $self->{'VMETHOD_FUNCTIONS'});
            $ref = $self->{'_vars'}->{lc $name} if ! defined $ref && $self->{'LOWER_CASE_VAR_FALLBACK'};
        }
    }


    my %seen_filters;
    while (defined $ref) {

        ### check at each point if the rurned thing was a code
        if (UNIVERSAL::isa($ref, 'CODE')) {
            return $ref if $i >= $#$var && $ARGS->{'return_ref'};
            my @results = $ref->($args ? map { $self->play_expr($_) } @$args : ());
            if (defined $results[0]) {
                $ref = ($#results > 0) ? \@results : $results[0];
            } elsif (defined $results[1]) {
                die $results[1]; # TT behavior - why not just throw ?
            } else {
                $ref = undef;
                last;
            }
        }

        ### descend one chained level
        last if $i >= $#$var;
        my $was_dot_call = $ARGS->{'no_dots'} ? 1 : $var->[$i++] eq '.';
        $name            = $var->[$i++];
        $args            = $var->[$i++];

        ### allow for named portions of a variable name (foo.$name.bar)
        if (ref $name) {
            if (ref($name) eq 'ARRAY') {
                $name = $self->play_expr($name);
                if (! defined($name) || ($QR_PRIVATE && $name =~ $QR_PRIVATE) || $name =~ /^\./) {
                    $ref = undef;
                    last;
                }
            } else {
                die "Shouldn't get a ". ref($name) ." during a vivify on chain";
            }
        }
        if ($QR_PRIVATE && $name =~ $QR_PRIVATE) { # don't allow vars that begin with _
            $ref = undef;
            last;
        }

        ### allow for scalar and filter access (this happens for every non virtual method call)
        if (! ref $ref) {
            if ($SCALAR_OPS->{$name}) {                        # normal scalar op
                $ref = $SCALAR_OPS->{$name}->($ref, $args ? map { $self->play_expr($_) } @$args : ());

            } elsif ($LIST_OPS->{$name}) {                     # auto-promote to list and use list op
                $ref = $LIST_OPS->{$name}->([$ref], $args ? map { $self->play_expr($_) } @$args : ());

            } elsif (my $filter = $self->{'FILTERS'}->{$name}    # filter configured in Template args
                     || $FILTER_OPS->{$name}                     # predefined filters in CET
                     || (UNIVERSAL::isa($name, 'CODE') && $name) # looks like a filter sub passed in the stash
                     || $self->list_filters->{$name}) {          # filter defined in Template::Filters

                if (UNIVERSAL::isa($filter, 'CODE')) {
                    $ref = eval { $filter->($ref) }; # non-dynamic filter - no args
                    if (my $err = $@) {
                        $self->throw('filter', $err) if ref($err) !~ /Template::Exception$/;
                        die $err;
                    }
                } elsif (! UNIVERSAL::isa($filter, 'ARRAY')) {
                    $self->throw('filter', "invalid FILTER entry for '$name' (not a CODE ref)");

                } elsif (@$filter == 2 && UNIVERSAL::isa($filter->[0], 'CODE')) { # these are the TT style filters
                    eval {
                        my $sub = $filter->[0];
                        if ($filter->[1]) { # it is a "dynamic filter" that will return a sub
                            ($sub, my $err) = $sub->($self->context, $args ? map { $self->play_expr($_) } @$args : ());
                            if (! $sub && $err) {
                                $self->throw('filter', $err) if ref($err) !~ /Template::Exception$/;
                                die $err;
                            } elsif (! UNIVERSAL::isa($sub, 'CODE')) {
                                $self->throw('filter', "invalid FILTER for '$name' (not a CODE ref)")
                                    if ref($sub) !~ /Template::Exception$/;
                                die $sub;
                            }
                        }
                        $ref = $sub->($ref);
                    };
                    if (my $err = $@) {
                        $self->throw('filter', $err) if ref($err) !~ /Template::Exception$/;
                        die $err;
                    }
                } else { # this looks like our vmethods turned into "filters" (a filter stored under a name)
                    $self->throw('filter', 'Recursive filter alias \"$name\"') if $seen_filters{$name} ++;
                    $var = [$name, 0, '|', @$filter, @{$var}[$i..$#$var]]; # splice the filter into our current tree
                    $i = 2;
                }
                if (scalar keys %seen_filters
                    && $seen_filters{$var->[$i - 5] || ''}) {
                    $self->throw('filter', "invalid FILTER entry for '".$var->[$i - 5]."' (not a CODE ref)");
                }
            } else {
                $ref = undef;
            }

        } else {

            ### method calls on objects
            if ($was_dot_call && UNIVERSAL::can($ref, 'can')) {
                return $ref if $i >= $#$var && $ARGS->{'return_ref'};
                my @args = $args ? map { $self->play_expr($_) } @$args : ();
                my @results = eval { $ref->$name(@args) };
                if ($@) {
                    my $class = ref $ref;
                    die $@ if ref $@ || $@ !~ /Can\'t locate object method "\Q$name\E" via package "\Q$class\E"/;
                } elsif (defined $results[0]) {
                    $ref = ($#results > 0) ? \@results : $results[0];
                    next;
                } elsif (defined $results[1]) {
                    die $results[1]; # TT behavior - why not just throw ?
                } else {
                    $ref = undef;
                    last;
                }
                # didn't find a method by that name - so fail down to hash and array access
            }

            ### hash member access
            if (UNIVERSAL::isa($ref, 'HASH')) {
                if ($was_dot_call && exists($ref->{$name}) ) {
                    return \ $ref->{$name} if $i >= $#$var && $ARGS->{'return_ref'} && ! ref $ref->{$name};
                    $ref = $ref->{$name};
                } elsif ($HASH_OPS->{$name}) {
                    $ref = $HASH_OPS->{$name}->($ref, $args ? map { $self->play_expr($_) } @$args : ());
                } elsif ($ARGS->{'is_namespace_during_compile'}) {
                    return $var; # abort - can't fold namespace variable
                } else {
                    return \ $ref->{$name} if $i >= $#$var && $ARGS->{'return_ref'};
                    $ref = undef;
                }

            ### array access
            } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
                if ($name =~ m{ ^ -? $QR_INDEX $ }ox) {
                    return \ $ref->[$name] if $i >= $#$var && $ARGS->{'return_ref'} && ! ref $ref->[$name];
                    $ref = $ref->[$name];
                } elsif ($LIST_OPS->{$name}) {
                    $ref = $LIST_OPS->{$name}->($ref, $args ? map { $self->play_expr($_) } @$args : ());
                } else {
                    $ref = undef;
                }
            }
        }

    } # end of while

    ### allow for undefinedness
    if (! defined $ref) {
        if ($self->{'_debug_undef'}) {
            my $chunk = $var->[$i - 2];
            $chunk = $self->play_expr($chunk) if ref($chunk) eq 'ARRAY';
            die "$chunk is undefined\n";
        } else {
            $ref = $self->undefined_any($var);
        }
    }

    return $ref;
}

sub set_variable {
    my ($self, $var, $val, $ARGS) = @_;
    $ARGS ||= {};
    my $i = 0;

    ### allow for the parse tree to store literals - the literal is used as a name (like [% 'a' = 'A' %])
    $var = [$var, 0] if ! ref $var;

    ### determine the top level of this particular variable access
    my $ref  = $var->[$i++];
    my $args = $var->[$i++];
    if (ref $ref) {
        ### non-named types can't be set
        return if ref($ref) ne 'ARRAY' || ! defined $ref->[0];

        # named access (ie via $name.foo)
        $ref = $self->play_expr($ref);
        if (defined $ref && (! $QR_PRIVATE || $ref !~ $QR_PRIVATE)) { # don't allow vars that begin with _
            if ($#$var <= $i) {
                return $self->{'_vars'}->{$ref} = $val;
            } else {
                $ref = $self->{'_vars'}->{$ref} ||= {};
            }
        } else {
            return;
        }
    } elsif (defined $ref) {
        return if $QR_PRIVATE && $ref =~ $QR_PRIVATE; # don't allow vars that begin with _
        if ($#$var <= $i) {
            return $self->{'_vars'}->{$ref} = $val;
        } else {
            $ref = $self->{'_vars'}->{$ref} ||= {};
        }
    }

    while (defined $ref) {

        ### check at each point if the returned thing was a code
        if (UNIVERSAL::isa($ref, 'CODE')) {
            my @results = $ref->($args ? map { $self->play_expr($_) } @$args : ());
            if (defined $results[0]) {
                $ref = ($#results > 0) ? \@results : $results[0];
            } elsif (defined $results[1]) {
                die $results[1]; # TT behavior - why not just throw ?
            } else {
                return;
            }
        }

        ### descend one chained level
        last if $i >= $#$var;
        my $was_dot_call = $ARGS->{'no_dots'} ? 1 : $var->[$i++] eq '.';
        my $name         = $var->[$i++];
        my $args         = $var->[$i++];

        ### allow for named portions of a variable name (foo.$name.bar)
        if (ref $name) {
            if (ref($name) eq 'ARRAY') {
                $name = $self->play_expr($name);
                if (! defined($name) || $name =~ /^[_.]/) {
                    return;
                }
            } else {
                die "Shouldn't get a ".ref($name)." during a vivify on chain";
            }
        }
        if ($QR_PRIVATE && $name =~ $QR_PRIVATE) { # don't allow vars that begin with _
            return;
        }

        ### scalar access
        if (! ref $ref) {
            return;

        ### method calls on objects
        } elsif (UNIVERSAL::can($ref, 'can')) {
            my $lvalueish;
            my @args = $args ? map { $self->play_expr($_) } @$args : ();
            if ($i >= $#$var) {
                $lvalueish = 1;
                push @args, $val;
            }
            my @results = eval { $ref->$name(@args) };
            if (! $@) {
                if (defined $results[0]) {
                    $ref = ($#results > 0) ? \@results : $results[0];
                } elsif (defined $results[1]) {
                    die $results[1]; # TT behavior - why not just throw ?
                } else {
                    return;
                }
                return if $lvalueish;
                next;
            }
            my $class = ref $ref;
            die $@ if ref $@ || $@ !~ /Can\'t locate object method "\Q$name\E" via package "\Q$class\E"/;
            # fall on down to "normal" accessors
        }

        ### hash member access
        if (UNIVERSAL::isa($ref, 'HASH')) {
            if ($#$var <= $i) {
                return $ref->{$name} = $val;
            } else {
                $ref = $ref->{$name} ||= {};
                next;
            }

        ### array access
        } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
            if ($name =~ m{ ^ -? $QR_INDEX $ }ox) {
                if ($#$var <= $i) {
                    return $ref->[$name] = $val;
                } else {
                    $ref = $ref->[$name] ||= {};
                    next;
                }
            } else {
                return;
            }

        }

    }

    return;
}

###----------------------------------------------------------------###

sub play_operator {
    my ($self, $tree) = @_;
    ### $tree looks like [undef, '+', 4, 5]

    if ($OP_DISPATCH->{$tree->[1]}) {
        local $^W;
        if ($OP_ASSIGN->{$tree->[1]}) {
            my $val = $OP_DISPATCH->{$tree->[1]}->($self->play_expr($tree->[2]), $self->play_expr($tree->[3]));
            $self->set_variable($tree->[2], $val);
            return $val;
        } else {
            return $OP_DISPATCH->{$tree->[1]}->(@$tree == 3 ? $self->play_expr($tree->[2]) : ($self->play_expr($tree->[2]), $self->play_expr($tree->[3])));
        }
    }

    my $op = $tree->[1];

    ### do custom and short-circuitable operators
    if ($op eq '=') {
        my $val = $self->play_expr($tree->[3]);
        $self->set_variable($tree->[2], $val);
        return $val;

   } elsif ($op eq '||' || $op eq 'or' || $op eq 'OR') {
        my $val = $self->play_expr($tree->[2]) || $self->play_expr($tree->[3]);
        return defined($val) ? $val : '';

    } elsif ($op eq '&&' || $op eq 'and' || $op eq 'AND') {
        my $val = $self->play_expr($tree->[2]) && $self->play_expr($tree->[3]);
        return defined($val) ? $val : '';

    } elsif ($op eq '?') {
        local $^W;
        return $self->play_expr($tree->[2]) ? $self->play_expr($tree->[3]) : $self->play_expr($tree->[4]);

    } elsif ($op eq '~' || $op eq '_') {
        local $^W;
        my $s = '';
        $s .= $self->play_expr($tree->[$_]) for 2 .. $#$tree;
        return $s;

    } elsif ($op eq '[]') {
        return [map {$self->play_expr($tree->[$_])} 2 .. $#$tree];

    } elsif ($op eq '{}') {
        local $^W;
        my @e;
        push @e, $self->play_expr($tree->[$_]) for 2 .. $#$tree;
        return {@e};

    } elsif ($op eq '++') {
        local $^W;
        my $val = 0 + $self->play_expr($tree->[2]);
        $self->set_variable($tree->[2], $val + 1);
        return $tree->[3] ? $val : $val + 1; # ->[3] is set to 1 during parsing of postfix ops

    } elsif ($op eq '--') {
        local $^W;
        my $val = 0 + $self->play_expr($tree->[2]);
        $self->set_variable($tree->[2], $val - 1);
        return $tree->[3] ? $val : $val - 1; # ->[3] is set to 1 during parsing of postfix ops

    } elsif ($op eq '\\') {
        my $var = $tree->[2];

        my $ref = $self->play_expr($var, {return_ref => 1});
        return $ref if ! ref $ref;
        return sub { sub { $$ref } } if ref $ref eq 'SCALAR' || ref $ref eq 'REF';

        my $self_copy = $self;
        eval {require Scalar::Util; Scalar::Util::weaken($self_copy)};

        my $last = ['temp deref key', $var->[-1] ? [@{ $var->[-1] }] : 0];
        return sub { sub { # return a double sub so that the current play_expr will return a coderef
            local $self_copy->{'_vars'}->{'temp deref key'} = $ref;
            $last->[-1] = (ref $last->[-1] ? [@{ $last->[-1] }, @_] : [@_]) if @_;
            return $self->play_expr($last);
        } };
    } elsif ($op eq 'qr') {
        return $tree->[3] ? qr{(?$tree->[3]:$tree->[2])} : qr{$tree->[2]};
    }

    $self->throw('operator', "Un-implemented operation $op");
}

###----------------------------------------------------------------###

sub _vars {
    my $self = shift;
    $self->{'_vars'} = shift if $#_ == 0;
    return $self->{'_vars'} ||= {};
}

sub include_filename {
    my ($self, $file) = @_;
    if ($file =~ m|^/|) {
        $self->throw('file', "$file absolute paths are not allowed (set ABSOLUTE option)") if ! $self->{'ABSOLUTE'};
        return $file if -e $file;
    } elsif ($file =~ m{(^|/)\.\./}) {
        $self->throw('file', "$file relative paths are not allowed (set RELATIVE option)") if ! $self->{'RELATIVE'};
        return $file if -e $file;
    }

    my $paths = $self->{'INCLUDE_PATHS'} ||= do {
        # TT does this everytime a file is looked up - we are going to do it just in time - the first time
        my $paths = $self->{'INCLUDE_PATH'} || [];
        $paths = $paths->()                 if UNIVERSAL::isa($paths, 'CODE');
        $paths = $self->split_paths($paths) if ! UNIVERSAL::isa($paths, 'ARRAY');
        $paths; # return of the do
    };
    foreach my $path (@$paths) {
        return "$path/$file" if -e "$path/$file";
    }

    $self->throw('file', "$file: not found");
}

sub split_paths {
    my ($self, $path) = @_;
    return $path if ref $path;
    my $delim = $self->{'DELIMITER'} || ':';
    $delim = ($delim eq ':' && $^O eq 'MSWin32') ? qr|:(?!/)| : qr|\Q$delim\E|;
    return [split $delim, $path];
}

sub _insert {
    my ($self, $file) = @_;
    return $self->slurp($self->include_filename($file));
}

sub slurp {
    my ($self, $file) = @_;
    open(my $fh, '<', $file) || $self->throw('file', "$file couldn't be opened: $!");
    read $fh, my $txt, -s $file;
    return $txt;
}

sub process_simple {
    my $self = shift;
    my $in   = shift || die "Missing input";
    my $swap = shift || die "Missing variable hash";
    my $out  = shift || die "Missing output string ref";

    eval {
        delete $self->{'_debug_off'};
        delete $self->{'_debug_format'};
        local $self->{'_start_top_level'} = 1;
        $self->_process($in, $swap, $out);
    };
    if (my $err = $@) {
        if ($err->type !~ /stop|return|next|last|break/) {
            $self->{'error'} = $err;
            return;
        }
    }
    return 1;
}

sub error { shift->{'error'} }

###----------------------------------------------------------------###

sub exception {
    my $self = shift;
    my $type = shift;
    my $info = shift;
    return $type if ref($type) =~ /Template::Exception$/;
    if (ref($info) eq 'ARRAY') {
        my $hash = ref($info->[-1]) eq 'HASH' ? pop(@$info) : {};
        if (@$info >= 2 || scalar keys %$hash) {
            my $i = 0;
            $hash->{$_} = $info->[$_] for 0 .. $#$info;
            $hash->{'args'} = $info;
            $info = $hash;
        } elsif (@$info == 1) {
            $info = $info->[0];
        } else {
            $info = $type;
            $type = 'undef';
        }
    }
    return CGI::Ex::Template::Exception->new($type, $info, @_);
}

sub throw { die shift->exception(@_) }

sub context {
    my $self = shift;
    require CGI::Ex::Template::Context;
    return CGI::Ex::Template::Context->new({_template => $self});
}

sub iterator {
    my $self = shift;
    require CGI::Ex::Template::Iterator;
    CGI::Ex::Template::Iterator->new(@_);
}

sub undefined_get {
    my ($self, $ident, $node) = @_;
    return $self->{'UNDEFINED_GET'}->($self, $ident, $node) if $self->{'UNDEFINED_GET'};
    return '';
}

sub undefined_any {
    my ($self, $ident) = @_;
    return $self->{'UNDEFINED_ANY'}->($self, $ident) if $self->{'UNDEFINED_ANY'};
    return;
}

sub list_filters {
    my $self = shift;
    return $self->{'_filters'} ||= eval { require Template::Filters; $Template::Filters::FILTERS } || {};
}

sub list_plugins {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::list_plugins;
}

sub debug_node {
    my ($self, $node) = @_;
    my $info = $self->node_info($node);
    my $format = $self->{'_debug_format'} || $self->{'DEBUG_FORMAT'} || "\n## \$file line \$line : [% \$text %] ##\n";
    $format =~ s{\$(file|line|text)}{$info->{$1}}g;
    return $format;
}

sub node_info {
    my ($self, $node) = @_;
    my $doc = $self->{'_component'};
    my $i = $node->[1];
    my $j = $node->[2] || return ''; # META can be 0
    $doc->{'_content'} ||= do { my $s = $self->slurp($doc->{'_filename'}) ; \$s };
    my $s = substr(${ $doc->{'_content'} }, $i, $j - $i);
    $s =~ s/^\s+//;
    $s =~ s/\s+$//;
    return {
        file => $doc->{'name'},
        line => $self->get_line_number_by_index($doc, $i),
        text => $s,
    };
}

sub get_line_number_by_index {
    my ($self, $doc, $index, $include_char) = @_;
    return 1 if $index <= 0;

    ### get the line offsets for the doc
    my $lines = $doc->{'_line_offsets'} ||= do {
        $doc->{'_content'} ||= do { my $s = $self->slurp($doc->{'_filename'}) ; \$s };
        my $i = 0;
        my @lines = (0);
        while (1) {
            $i = index(${ $doc->{'_content'} }, "\n", $i) + 1;
            last if $i == 0;
            push @lines, $i;
        }
        \@lines;
    };

    ### binary search them (this is fast even on big docs)
    my ($i, $j) = (0, $#$lines);
    if ($index > $lines->[-1]) {
        $i = $j;
    } else {
        while (1) {
            last if abs($i - $j) <= 1;
            my $k = int(($i + $j) / 2);
            $j = $k if $lines->[$k] >= $index;
            $i = $k if $lines->[$k] <= $index;
        }
    }
    return $include_char ? ($i + 1, $index - $lines->[$i]) : $i + 1;
}

###----------------------------------------------------------------###
### long virtual methods or filters
### many of these vmethods have used code from Template/Stash.pm to
### assure conformance with the TT spec.

sub define_syntax {
    my ($self, $name, $sub) = @_;
    $SYNTAX->{$name} = $sub;
    return 1;
}

sub define_operator {
    my ($self, $args) = @_;
    push @$OPERATORS, [@{ $args }{qw(type precedence symbols play_sub)}];
    _build_ops();
    return 1;
}

sub define_directive {
    my ($self, $name, $args) = @_;
    require CGI::Ex::Template::Parse;
    $CGI::Ex::Template::Parse::DIRECTIVES->{$name} = [@{ $args }{qw(parse_sub play_sub is_block is_postop continues no_interp)}];
    return 1;
}

sub define_vmethod {
    my ($self, $type, $name, $sub) = @_;
    if (   $type =~ /scalar|item|text/i) { $SCALAR_OPS->{$name} = $sub }
    elsif ($type =~ /array|list/i ) { $LIST_OPS->{  $name} = $sub }
    elsif ($type =~ /hash/i       ) { $HASH_OPS->{  $name} = $sub }
    elsif ($type =~ /filter/i     ) { $FILTER_OPS->{$name} = $sub }
    else { die "Invalid type vmethod type $type" }
    return 1;
}

sub vmethod_fmt_scalar {
    my $str = shift; $str = ''   if ! defined $str;
    my $pat = shift; $pat = '%s' if ! defined $pat;
    local $^W;
    return @_ ? sprintf($pat, $_[0], $str)
              : sprintf($pat, $str);
}

sub vmethod_fmt_list {
    my $ref = shift || return '';
    my $pat = shift; $pat = '%s' if ! defined $pat;
    my $sep = shift; $sep = ' '  if ! defined $sep;
    local $^W;
    return @_ ? join($sep, map {sprintf $pat, $_[0], $_} @$ref)
              : join($sep, map {sprintf $pat, $_} @$ref);
}

sub vmethod_fmt_hash {
    my $ref = shift || return '';
    my $pat = shift; $pat = "%s\t%s" if ! defined $pat;
    my $sep = shift; $sep = "\n"     if ! defined $sep;
    local $^W;
    return ! @_    ? join($sep, map {sprintf $pat, $_, $ref->{$_}} sort keys %$ref)
         : @_ == 1 ? join($sep, map {sprintf $pat, $_[0], $_, $ref->{$_}} sort keys %$ref) # don't get to pick - it applies to the key
         :           join($sep, map {sprintf $pat, $_[0], $_, $_[1], $ref->{$_}} sort keys %$ref);
}

sub vmethod_chunk {
    my $str  = shift;
    my $size = shift || 1;
    my @list;
    if ($size < 0) { # chunk from the opposite end
        $str = reverse $str;
        $size = -$size;
        unshift(@list, scalar reverse $1) while $str =~ /( .{$size} | .+ )/xg;
    } else {
        push(@list, $1)                   while $str =~ /( .{$size} | .+ )/xg;
    }
    return \@list;
}

sub vmethod_indent {
    my $str = shift; $str = '' if ! defined $str;
    my $pre = shift; $pre = 4  if ! defined $pre;
    $pre = ' ' x $pre if $pre =~ /^\d+$/;
    $str =~ s/^/$pre/mg;
    return $str;
}

sub vmethod_format {
    my $str = shift; $str = ''   if ! defined $str;
    my $pat = shift; $pat = '%s' if ! defined $pat;
    if (@_) {
        return join "\n", map{ sprintf $pat, $_[0], $_ } split(/\n/, $str);
    } else {
        return join "\n", map{ sprintf $pat, $_ } split(/\n/, $str);
    }
}

sub vmethod_list_hash {
    my ($hash, $what) = @_;
    $what = 'pairs' if ! $what || $what !~ /^(keys|values|each|pairs)$/;
    return $HASH_OPS->{$what}->($hash);
}


sub vmethod_match {
    my ($str, $pat, $global) = @_;
    return [] if ! defined $str || ! defined $pat;
    my @res = $global ? ($str =~ /$pat/g) : ($str =~ /$pat/);
    return @res ? \@res : '';
}

sub vmethod_nsort {
    my ($list, $field) = @_;
    return defined($field)
        ? [map {$_->[0]} sort {$a->[1] <=> $b->[1]} map {[$_, (ref $_ eq 'HASH' ? $_->{$field}
                                                               : UNIVERSAL::can($_, $field) ? $_->$field()
                                                               : $_)]} @$list ]
        : [sort {$a <=> $b} @$list];
}

sub vmethod_pick {
    my $ref = shift;
    no warnings;
    my $n   = int(shift);
    $n = 1 if $n < 1;
    my @ind = map { $ref->[ rand @$ref ] } 1 .. $n;
    return $n == 1 ? $ind[0] : \@ind;
}

sub vmethod_repeat {
    my ($str, $n, $join) = @_;
    return '' if ! defined $str || ! length $str;
    $n = 1 if ! defined($n) || ! length $n;
    $join = '' if ! defined $join;
    return join $join, ($str) x $n;
}

### This method is a combination of my submissions along
### with work from Andy Wardley, Sergey Martynoff, Nik Clayton, and Josh Rosenbaum
sub vmethod_replace {
    my ($text, $pattern, $replace, $global) = @_;
    $text      = '' unless defined $text;
    $pattern   = '' unless defined $pattern;
    $replace   = '' unless defined $replace;
    $global    = 1  unless defined $global;
    my $expand = sub {
        my ($chunk, $start, $end) = @_;
        $chunk =~ s{ \\(\\|\$) | \$ (\d+) }{
            $1 ? $1
                : ($2 > $#$start || $2 == 0) ? ''
                : substr($text, $start->[$2], $end->[$2] - $start->[$2]);
        }exg;
        $chunk;
    };
    if ($global) {
        $text =~ s{$pattern}{ $expand->($replace, [@-], [@+]) }eg;
    } else {
        $text =~ s{$pattern}{ $expand->($replace, [@-], [@+]) }e;
    }
    return $text;
}

sub vmethod_sort {
    my ($list, $field) = @_;
    return defined($field)
        ? [map {$_->[0]} sort {$a->[1] cmp $b->[1]} map {[$_, lc(ref $_ eq 'HASH' ? $_->{$field}
                                                                 : UNIVERSAL::can($_, $field) ? $_->$field()
                                                                 : $_)]} @$list ]
        : [map {$_->[0]} sort {$a->[1] cmp $b->[1]} map {[$_, lc $_]} @$list ]; # case insensitive
}

sub vmethod_splice {
    my ($ref, $i, $len, @replace) = @_;
    @replace = @{ $replace[0] } if @replace == 1 && ref $replace[0] eq 'ARRAY';
    if (defined $len) {
        return [splice @$ref, $i || 0, $len, @replace];
    } elsif (defined $i) {
        return [splice @$ref, $i];
    } else {
        return [splice @$ref];
    }
}

sub vmethod_split {
    my ($str, $pat, $lim) = @_;
    $str = '' if ! defined $str;
    if (defined $lim) { return defined $pat ? [split $pat, $str, $lim] : [split ' ', $str, $lim] }
    else              { return defined $pat ? [split $pat, $str      ] : [split ' ', $str      ] }
}

sub vmethod_substr {
    my ($str, $i, $len, $replace) = @_;
    $i ||= 0;
    return substr($str, $i)       if ! defined $len;
    return substr($str, $i, $len) if ! defined $replace;
    substr($str, $i, $len, $replace);
    return $str;
}

sub vmethod_uri {
    my $str = shift;
    utf8::encode($str) if defined &utf8::encode;
    $str =~ s/([^A-Za-z0-9\-_.!~*\'()])/sprintf('%%%02X', ord($1))/eg;
    return $str;
}

sub vmethod_url {
    my $str = shift;
    utf8::encode($str) if defined &utf8::encode;
    $str =~ s/([^;\/?:@&=+\$,A-Za-z0-9\-_.!~*\'()])/sprintf('%%%02X', ord($1))/eg;
    return $str;
}

sub filter_eval {
    my $context = shift;
    my $syntax  = shift;

    return sub {
        ### prevent recursion
        my $t = $context->_template;
        local $t->{'_eval_recurse'} = $t->{'_eval_recurse'} || 0;
        $context->throw('eval_recurse', "MAX_EVAL_RECURSE $MAX_EVAL_RECURSE reached")
            if ++$t->{'_eval_recurse'} > ($t->{'MAX_EVAL_RECURSE'} || $MAX_EVAL_RECURSE);


        my $text = shift;
        local $t->{'SYNTAX'} = $syntax || $t->{'SYNTAX'};
        return $context->process(\$text);
    };
}

sub filter_redirect {
    my ($context, $file, $options) = @_;
    my $path = $context->config->{'OUTPUT_PATH'} || $context->throw('redirect', 'OUTPUT_PATH is not set');
    $context->throw('redirect', 'Invalid filename - cannot include "/../"')
        if $file =~ m{(^|/)\.\./};

    return sub {
        my $text = shift;
        if (! -d $path) {
            require File::Path;
            File::Path::mkpath($path) || $context->throw('redirect', "Couldn't mkpath \"$path\": $!");
        }
        open (my $fh, '>', "$path/$file") || $context->throw('redirect', "Couldn't open \"$file\": $!");
        if (my $bm = (! $options) ? 0 : ref($options) ? $options->{'binmode'} : $options) {
            if (+$bm == 1) { binmode $fh }
            else { binmode $fh, $bm}
        }
        print $fh $text;
        return '';
    };
}

###----------------------------------------------------------------###
###----------------------------------------------------------------###

1;

### See the perldoc in CGI/Ex/Template.pod
