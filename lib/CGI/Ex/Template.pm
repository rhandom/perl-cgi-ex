package CGI::Ex::Template;

###----------------------------------------------------------------###
#  See the perldoc in CGI/Ex/Template.pod
#  Copyright 2007 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

use strict;

our $VERSION = '2.13';

our $PACKAGE_EXCEPTION   = 'CGI::Ex::Template::Exception';
our $PACKAGE_ITERATOR    = 'CGI::Ex::Template::Iterator';
our $PACKAGE_CONTEXT     = 'CGI::Ex::Template::Context';
our $QR_PRIVATE          = qr/^[_.]/;

our $SYNTAX = {
    cet => \&parse_tree_tt3,
    ht  => sub { my $self = shift; local $self->{'V2EQUALS'} = 0; local $self->{'EXPR'} = 0; $self->parse_tree_hte(@_) },
    hte => sub { my $self = shift; local $self->{'V2EQUALS'} = 0; $self->parse_tree_hte(@_) },
    tt3 => \&parse_tree_tt3,
    tt2 => sub { my $self = shift; local $self->{'V2PIPE'} = 1; $self->parse_tree_tt3(@_) },
    tt1 => sub { my $self = shift; local $self->{'V2PIPE'} = 1; local $self->{'V1DOLLAR'} = 1; $self->parse_tree_tt3(@_) },
};

our $TAGS = {
    asp       => ['<%',     '%>'    ], # ASP
    default   => ['\[%',    '%\]'   ], # default
    html      => ['<!--',   '-->'   ], # HTML comments
    mason     => ['<%',     '>'     ], # HTML::Mason
    metatext  => ['%%',     '%%'    ], # Text::MetaText
    php       => ['<\?',    '\?>'   ], # PHP
    star      => ['\[\*',   '\*\]'  ], # TT alternate
    template  => ['\[%',    '%\]'   ], # Normal Template Toolkit
    template1 => ['[\[%]%', '%[%\]]'], # allow TT1 style
    tt2       => ['\[%',    '%\]'   ], # TT2
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

our $DIRECTIVES = {
    #name       parse_sub        play_sub         block    postdir  continue  no_interp
    BLOCK   => [\&parse_BLOCK,   \&play_BLOCK,    1],
    BREAK   => [sub {},          \&play_control],
    CALL    => [\&parse_CALL,    \&play_CALL],
    CASE    => [\&parse_CASE,    undef,           0,       0,       {SWITCH => 1, CASE => 1}],
    CATCH   => [\&parse_CATCH,   undef,           0,       0,       {TRY => 1, CATCH => 1}],
    CLEAR   => [sub {},          \&play_CLEAR],
    '#'     => [sub {},          sub {}],
    CONFIG  => [\&parse_CONFIG,  \&play_CONFIG],
    DEBUG   => [\&parse_DEBUG,   \&play_DEBUG],
    DEFAULT => [\&parse_DEFAULT, \&play_DEFAULT],
    DUMP    => [\&parse_DUMP,    \&play_DUMP],
    ELSE    => [sub {},          undef,           0,       0,       {IF => 1, ELSIF => 1, UNLESS => 1}],
    ELSIF   => [\&parse_IF,      undef,           0,       0,       {IF => 1, ELSIF => 1, UNLESS => 1}],
    END     => [sub {},          sub {}],
    FILTER  => [\&parse_FILTER,  \&play_FILTER,   1,       1],
    '|'     => [\&parse_FILTER,  \&play_FILTER,   1,       1],
    FINAL   => [sub {},          undef,           0,       0,       {TRY => 1, CATCH => 1}],
    FOR     => [\&parse_FOREACH, \&play_FOREACH,  1,       1],
    FOREACH => [\&parse_FOREACH, \&play_FOREACH,  1,       1],
    GET     => [\&parse_GET,     \&play_GET],
    IF      => [\&parse_IF,      \&play_IF,       1,       1],
    INCLUDE => [\&parse_INCLUDE, \&play_INCLUDE],
    INSERT  => [\&parse_INSERT,  \&play_INSERT],
    LAST    => [sub {},          \&play_control],
    LOOP    => [\&parse_LOOP,    \&play_LOOP,     1,       1],
    MACRO   => [\&parse_MACRO,   \&play_MACRO],
    META    => [\&parse_META,    \&play_META],
    NEXT    => [sub {},          \&play_control],
    PERL    => [sub {},          \&play_PERL,     1,       0,       0,        1],
    PROCESS => [\&parse_PROCESS, \&play_PROCESS],
    RAWPERL => [sub {},          \&play_RAWPERL,  1,       0,       0,        1],
    RETURN  => [sub {},          \&play_control],
    SET     => [\&parse_SET,     \&play_SET],
    STOP    => [sub {},          \&play_control],
    SWITCH  => [\&parse_SWITCH,  \&play_SWITCH,   1],
    TAGS    => [\&parse_TAGS,    sub {}],
    THROW   => [\&parse_THROW,   \&play_THROW],
    TRY     => [sub {},          \&play_TRY,      1],
    UNLESS  => [\&parse_UNLESS,  \&play_UNLESS,   1,       1],
    USE     => [\&parse_USE,     \&play_USE],
    VIEW    => [\&parse_VIEW,    \&play_VIEW,     1],
    WHILE   => [\&parse_WHILE,   \&play_WHILE,    1,       1],
    WRAPPER => [\&parse_WRAPPER, \&play_WRAPPER,  1,       1],
    #name       parse_sub        play_sub         block    postdir  continue  no_interp
};

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
our $OP          = {map {my $ref = $_; map {$_ => $ref}      @{$ref->[2]}} grep {$_->[0] ne 'prefix' } @$OPERATORS}; # all non-prefix
our $OP_PREFIX   = {map {my $ref = $_; map {$_ => $ref}      @{$ref->[2]}} grep {$_->[0] eq 'prefix' } @$OPERATORS};
our $OP_DISPATCH = {map {my $ref = $_; map {$_ => $ref->[3]} @{$ref->[2]}} grep {$_->[3]             } @$OPERATORS};
our $OP_ASSIGN   = {map {my $ref = $_; map {$_ => 1}         @{$ref->[2]}} grep {$_->[0] eq 'assign' } @$OPERATORS};
our $OP_POSTFIX  = {map {my $ref = $_; map {$_ => 1}         @{$ref->[2]}} grep {$_->[0] eq 'postfix'} @$OPERATORS}; # bool is postfix
our $OP_TERNARY  = {map {my $ref = $_; map {$_ => 1}         @{$ref->[2]}} grep {$_->[0] eq 'ternary'} @$OPERATORS}; # bool is ternary
sub _op_qr { # no mixed \w\W operators
    my %used;
    my $chrs = join '|', reverse sort map {quotemeta $_} grep {++$used{$_} < 2} grep {! /\{\}|\[\]/} grep {/^\W{2,}$/} @_;
    my $chr  = join '',          sort map {quotemeta $_} grep {++$used{$_} < 2} grep {/^\W$/}     @_;
    my $word = join '|', reverse sort                    grep {++$used{$_} < 2} grep {/^\w+$/}    @_;
    $chr = "[$chr]" if $chr;
    $word = "\\b(?:$word)\\b" if $word;
    return join('|', grep {length} $chrs, $chr, $word) || die "Missing operator regex";
}
sub _build_op_qr        { _op_qr(map {@{ $_->[2] }} grep {$_->[0] ne 'prefix'} @$OPERATORS) }
sub _build_op_qr_prefix { _op_qr(map {@{ $_->[2] }} grep {$_->[0] eq 'prefix'} @$OPERATORS) }
sub _build_op_qr_assign { _op_qr(map {@{ $_->[2] }} grep {$_->[0] eq 'assign'} @$OPERATORS) }
our $QR_OP        = _build_op_qr();
our $QR_OP_PREFIX = _build_op_qr_prefix();
our $QR_OP_ASSIGN = _build_op_qr_assign();

our $QR_DIRECTIVE = '( [a-zA-Z]+\b | \| )';
our $QR_COMMENTS  = '(?-s: \# .* \s*)*';
our $QR_FILENAME  = '([a-zA-Z]]:/|/)? [\w\.][\w\-\.]* (?:/[\w\-\.]+)*';
our $QR_BLOCK     = '\w+\b (?: :\w+\b)* )';
our $QR_NUM       = '(?:\d*\.\d+ | \d+) (?: [eE][+-]\d+ )?';
our $QR_AQ_SPACE  = '(?: \\s+ | \$ | (?=;) )';

our $WHILE_MAX    = 1000;
our $EXTRA_COMPILE_EXT = '.sto';
our $MAX_EVAL_RECURSE  = 50;
our $MAX_MACRO_RECURSE = 50;
our $STAT_TTL          ||= 1;

our @CONFIG_COMPILETIME = qw(SYNTAX ANYCASE INTERPOLATE PRE_CHOMP POST_CHOMP SEMICOLONS V1DOLLAR V2PIPE V2EQUALS);
our @CONFIG_RUNTIME     = qw(DUMP VMETHOD_FUNCTIONS);

eval {require Scalar::Util};
BEGIN {require CGI::Ex::Template::Extra if $ENV{'MOD_PERL'}};

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
            $doc = $self->load_parsed_tree($file) || $self->throw('undef', "Zero length content");;
        }

        ### prevent recursion
        $self->throw('file', "recursion into '$doc->{name}'")
            if ! $self->{'RECURSION'} && $self->{'_in'}->{$doc->{'name'}} && $doc->{'name'} ne 'input text';
        local $self->{'_in'}->{$doc->{'name'}} = 1;

        ### execute the document
        if (! @{ $doc->{'_tree'} }) { # no tags found - just return the content
            $$out_ref = ${ $doc->{'_content'} };
        } else {
            local $self->{'_component'} = $doc;
            local $self->{'_template'}  = $self->{'_top_level'} ? $doc : $self->{'_template'};
            local @{ $self }{@CONFIG_RUNTIME} = @{ $self }{@CONFIG_RUNTIME};
            $self->execute_tree($doc->{'_tree'}, $out_ref);
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

sub load_parsed_tree {
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

        ### allow for predefined blocks that are a code or a string
        if (UNIVERSAL::isa($block, 'CODE')) {
            $block = $block->();
        }
        if (! UNIVERSAL::isa($block, 'HASH')) {
            $self->throw('block', "Unsupported BLOCK type \"$block\"") if ref $block;
            my $copy = $block;
            $block = eval { $self->load_parsed_tree(\$copy) }
                || $self->throw('block', 'Parse error on predefined block');
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
            if ($self->{'EXPOSE_BLOCKS'}
                && ! $self->{'_looking_in_block_file'}) {
                local $self->{'_looking_in_block_file'} = 1;
                my $block_name = '';
                while ($file =~ s|/([^/.]+)$||) {
                    $block_name = length($block_name) ? "$1/$block_name" : $1;
                    my $ref = eval { $self->load_parsed_tree($file) } || next;
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

sub parse_tree {
    my $syntax = $_[0]->{'SYNTAX'} || 'cet';
    my $meth   = $SYNTAX->{$syntax} || $_[0]->throw('parse', "Unknown SYNTAX \"$syntax\"");
    return $meth->(@_);
}

sub parse_tree_tt3 {
    my $self    = shift;
    my $str_ref = shift;
    if (! $str_ref || ! defined $$str_ref) {
        $self->throw('parse.no_string', "No string or undefined during parse");
    }

    my $STYLE = $self->{'TAG_STYLE'} || 'default';
    my $START = $self->{'START_TAG'} || $TAGS->{$STYLE}->[0];
    my $END   = $self->{'END_TAG'}   || $TAGS->{$STYLE}->[1];
    local $self->{'_end_tag'} = $END;

    local @{ $self }{@CONFIG_COMPILETIME} = @{ $self }{@CONFIG_COMPILETIME};

    my @tree;             # the parsed tree
    my $pointer = \@tree; # pointer to current tree to handle nested blocks
    my @state;            # maintain block levels
    local $self->{'_state'} = \@state; # allow for items to introspect (usually BLOCKS)
    local $self->{'_no_interp'} = 0;   # no interpolation in some blocks (usually PERL)
    my @in_view;          # let us know if we are in a view
    my @blocks;           # store blocks for later moving to front
    my @meta;             # place to store any found meta information (to go into META)
    my $post_chomp = 0;   # previous post_chomp setting
    my $continue   = 0;   # flag for multiple directives in the same tag
    my $post_op    = 0;   # found a post-operative DIRECTIVE
    my $capture;          # flag to start capture
    my $func;
    my $node;
    local pos $$str_ref = 0;

    while (1) {
        ### continue looking for information in a semi-colon delimited tag
        if ($continue) {
            $node = [undef, $continue, undef];

        ### find the next opening tag
        } else {
            $$str_ref =~ m{ \G (.*?) $START }gcxs
                || last;

            ### found a text portion - chomp it, interpolate it and store it
            if (length $1) {
                my $text  = $1;
                my $_last = pos $$str_ref;
                if ($post_chomp) {
                    if    ($post_chomp == 1) { $_last += length($1)     if $text =~ s{ ^ ([^\S\n]* \n) }{}x  }
                    elsif ($post_chomp == 2) { $_last += length($1) + 1 if $text =~ s{ ^ (\s+)         }{ }x }
                    elsif ($post_chomp == 3) { $_last += length($1)     if $text =~ s{ ^ (\s+)         }{}x  }
                }
                if (length $text) {
                    push @$pointer, $text;
                    $self->interpolate_node($pointer, $_last) if $self->{'INTERPOLATE'};
                }
            }

            $node = [undef, pos($$str_ref), undef];

            ### take care of whitespace and comments flags
            my $pre_chomp = $$str_ref =~ m{ \G ([+=~-]) }gcx ? $1 : $self->{'PRE_CHOMP'};
            $pre_chomp  =~ y/-=~+/1230/ if $pre_chomp;
            if ($pre_chomp && $pointer->[-1] && ! ref $pointer->[-1]) {
                if    ($pre_chomp == 1) { $pointer->[-1] =~ s{ (?:\n|^) [^\S\n]* \z }{}x  }
                elsif ($pre_chomp == 2) { $pointer->[-1] =~ s{             (\s+) \z }{ }x }
                elsif ($pre_chomp == 3) { $pointer->[-1] =~ s{             (\s+) \z }{}x  }
                splice(@$pointer, -1, 1, ()) if ! length $pointer->[-1]; # remove the node if it is zero length
            }

            ### leading # means to comment the entire section
            if ($$str_ref =~ m{ \G \# }gcx) {
                $$str_ref =~ m{ \G (.*?) ([+~=-]?) ($END) }gcxs # brute force - can't comment tags with nested %]
                    || $self->throw('parse', "Missing closing tag", undef, pos($$str_ref));
                $node->[0] = '#';
                $node->[2] = pos($$str_ref) - length($3);
                push @$pointer, $node;

                $post_chomp = $2;
                $post_chomp ||= $self->{'POST_CHOMP'};
                $post_chomp =~ y/-=~+/1230/ if $post_chomp;
                next;
            }
            $$str_ref =~ m{ \G \s* $QR_COMMENTS }gcxo;
        }

        ### look for DIRECTIVES
        if ($$str_ref =~ m{ \G $QR_DIRECTIVE }gcxo   # find a word
            && ($func = $self->{'ANYCASE'} ? uc($1) : $1)
            && ($DIRECTIVES->{$func}
                || ((pos($$str_ref) -= length $1) && 0))
            ) {                       # is it a directive
            $$str_ref =~ m{ \G \s* $QR_COMMENTS }gcx;

            $node->[0] = $func;

            ### store out this current node level to the appropriate tree location
            # on a post operator - replace the original node with the new one - store the old in the new
            if ($DIRECTIVES->{$func}->[3] && $post_op) {
                my @post_op = @$post_op;
                @$post_op = @$node;
                $node = $post_op;
                $node->[4] = [\@post_op];
            # if there was not a semi-colon - see if semis were required
            } elsif ($post_op && $self->{'SEMICOLONS'}) {
                $self->throw('parse', "Missing semi-colon with SEMICOLONS => 1", undef, $node->[1]);

            # handle directive captures for an item like "SET foo = BLOCK"
            } elsif ($capture) {
                push @{ $capture->[4] }, $node;
                undef $capture;

            # normal nodes
            } else{
                push @$pointer, $node;
            }

            ### parse any remaining tag details
            $node->[3] = eval { $DIRECTIVES->{$func}->[0]->($self, $str_ref, $node) };
            if (my $err = $@) {
                $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                die $err;
            }
            $node->[2] = pos $$str_ref;

            ### anything that behaves as a block ending
            if ($func eq 'END' || $DIRECTIVES->{$func}->[4]) { # [4] means it is a continuation block (ELSE, CATCH, etc)
                if (! @state) {
                    $self->throw('parse', "Found an $func tag while not in a block", $node, pos($$str_ref));
                }
                my $parent_node = pop @state;

                if ($func ne 'END') {
                    pop @$pointer; # we will store the node in the parent instead
                    $parent_node->[5] = $node;
                    my $parent_type = $parent_node->[0];
                    if (! $DIRECTIVES->{$func}->[4]->{$parent_type}) {
                        $self->throw('parse', "Found unmatched nested block", $node, pos($$str_ref));
                    }
                }

                ### restore the pointer up one level (because we hit the end of a block)
                $pointer = (! @state) ? \@tree : $state[-1]->[4];

                ### normal end block
                if ($func eq 'END') {
                    if ($parent_node->[0] eq 'BLOCK') { # move BLOCKS to front
                        if (defined($parent_node->[3]) && @in_view) {
                            push @{ $in_view[-1] }, $parent_node;
                        } else {
                            push @blocks, $parent_node;
                        }
                        if ($pointer->[-1] && ! $pointer->[-1]->[6]) {
                            splice(@$pointer, -1, 1, ());
                        }
                    } elsif ($parent_node->[0] eq 'VIEW') {
                        my $ref = { map {($_->[3] => $_->[4])} @{ pop @in_view }};
                        unshift @{ $parent_node->[3] }, $ref;
                    } elsif ($DIRECTIVES->{$parent_node->[0]}->[5]) { # allow no_interp to turn on and off
                        $self->{'_no_interp'}--;
                    }

                ### continuation block - such as an elsif
                } else {
                    push @state, $node;
                    $pointer = $node->[4] ||= [];
                }

            ### handle block directives
            } elsif ($DIRECTIVES->{$func}->[2] && ! $post_op) {
                    push @state, $node;
                    $pointer = $node->[4] ||= []; # allow future parsed nodes before END tag to end up in current node
                    push @in_view, [] if $func eq 'VIEW';
                    $self->{'_no_interp'}++ if $DIRECTIVES->{$node->[0]}->[5] # allow no_interp to turn on and off

            } elsif ($func eq 'TAGS') {
                ($START, $END) = @{ $node->[3] };

                ### allow for one more closing tag of the old style
                if ($$str_ref =~ m{ \G ([+~=-]?) $self->{'_end_tag'} }gcxs) {
                    $post_chomp = $1 || $self->{'POST_CHOMP'};
                    $post_chomp =~ y/-=~+/1230/ if $post_chomp;
                    $continue = 0;
                    $post_op  = 0;
                    $self->{'_end_tag'} = $END; # need to keep track so parse_expr knows when to stop
                    next;
                }
                $self->{'_end_tag'} = $END;

            } elsif ($func eq 'META') {
                unshift @meta, %{ $node->[3] }; # first defined win
                $node->[3] = undef;             # only let these be defined once - at the front of the tree
            }

        ### allow for bare variable getting and setting
        } elsif (defined(my $var = $self->parse_expr($str_ref))) {
            if ($post_op && $self->{'SEMICOLONS'}) {
                $self->throw('parse', "Missing semi-colon with SEMICOLONS => 1", undef, $node->[1]);
            }
            push @$pointer, $node;
            if ($$str_ref =~ m{ \G ($QR_OP_ASSIGN) >? (?! [+=~-]? $END) \s* $QR_COMMENTS }gcx) {
                $node->[0] = 'SET';
                $node->[3] = eval { $DIRECTIVES->{'SET'}->[0]->($self, $str_ref, $node, $1, $var) };
                if (my $err = $@) {
                    $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                    die $err;
                }
            } else {
                $node->[0] = 'GET';
                $node->[3] = $var;
            }
            $node->[2] = pos $$str_ref;
        }

        ### look for the closing tag
        if ($$str_ref =~ m{ \G (?: ; \s* $QR_COMMENTS)? ([+=~-]?) $END }gcxs) {
            $post_chomp = $1 || $self->{'POST_CHOMP'};
            $post_chomp =~ y/-=~+/1230/ if $post_chomp;
            $continue = 0;
            $post_op  = 0;
            next;
        }

        ### semi-colon = end of statement - we will need to continue parsing this tag
        if ($$str_ref =~ m{ \G ; \s* $QR_COMMENTS }gcxo) {
            $post_op   = 0;

        ### we are flagged to start capturing the output of the next directive - set it up
        } elsif ($node->[6]) {
            $post_op = 0;
            $capture = $node;

        ### allow next directive to be post-operative (or not)
        } else {
            $post_op = $node;
        }

        ### no closing tag yet - no need to get an opening tag on next loop
        $self->throw('parse', "Not sure how to handle tag", $node, pos($$str_ref)) if $continue == pos $$str_ref;
        $continue = pos $$str_ref;
    }

    ### cleanup the tree
    unshift(@tree, @blocks) if @blocks;
    unshift(@tree, ['META', 0, 0, {@meta}]) if @meta;
    $self->throw('parse', "Missing END directive", $state[-1], pos($$str_ref)) if @state > 0;

    ### pull off the last text portion - if any
    if (pos($$str_ref) != length($$str_ref)) {
        my $text  = substr $$str_ref, pos($$str_ref);
        my $_last = pos($$str_ref);
        if ($post_chomp) {
            if    ($post_chomp == 1) { $_last += length($1)     if $text =~ s{ ^ ([^\S\n]* \n) }{}x  }
            elsif ($post_chomp == 2) { $_last += length($1) + 1 if $text =~ s{ ^ (\s+)         }{ }x }
            elsif ($post_chomp == 3) { $_last += length($1)     if $text =~ s{ ^ (\s+)         }{}x  }
        }
        if (length $text) {
            push @$pointer, $text;
            $self->interpolate_node($pointer, $_last) if $self->{'INTERPOLATE'};
        }
    }

    return \@tree;
}

sub parse_tree_hte {
    require CGI::Ex::Template::HTE;
    &CGI::Ex::Template::HTE::parse_tree_hte;
}

sub parse_expr {
    my $self    = shift;
    my $str_ref = shift;
    my $ARGS    = shift || {};
    my $is_aq   = $ARGS->{'auto_quote'} ? 1 : 0;
    my $mark    = pos $$str_ref;

    ### allow for custom auto_quoting (such as hash constructors)
    if ($is_aq) {
        if ($$str_ref =~ m{ \G $ARGS->{'auto_quote'} }gcx) {
            return $1;

        ### allow for auto-quoted $foo
        } elsif ($$str_ref =~ m{ \G \$ (\w+\b (?:\.\w+\b)*) \s* $QR_COMMENTS }gcxo) {
            my $name = $1;
            if ($$str_ref !~ m{ \G \( }gcx || $name =~ /^(?:qw|m|\d)/) {
                return $self->parse_expr(\$name);
            }
            ### this is a little cryptic/odd - but TT allows items in
            ### autoquote position to only be prefixed by a $ - gross
            ### so we will defer to the regular parsing - but after the $
            pos($$str_ref) = $mark + 1;
            $is_aq = undef; # but don't allow operators - false flag handed down

        ### allow for ${foo.bar} type constructs
        } elsif ($$str_ref =~ m{ \G \$\{ \s* }gcx) {
            my $var = $self->parse_expr($str_ref);
            $$str_ref =~ m{ \G \s* \} \s* $QR_COMMENTS }gcxo
                || $self->throw('parse', 'Missing close "}" from "${"', undef, pos($$str_ref));
            return $var;
        }
    }


    ### test for leading prefix operators
    my $has_prefix;
    while (! $is_aq && $$str_ref =~ m{ \G ($QR_OP_PREFIX) }gcxo) {
        push @{ $has_prefix }, $1;
        $$str_ref =~ m{ \G \s* $QR_COMMENTS }gcxo;
    }

    my @var;
    my $is_literal;
    my $is_namespace;
    my $already_parsed_args;

    ### allow hex
    if ($$str_ref =~ m{ \G 0x ( [a-fA-F0-9]+ ) \s* $QR_COMMENTS }gcxo) {
        my $number = eval { hex $1 } || 0;
        push @var, \ $number;
        $is_literal = 1;

    ### allow for numbers
    } elsif ($$str_ref =~ m{ \G ( $QR_NUM ) \s* $QR_COMMENTS }gcxo) {
        my $number = $1;
        push @var, \ $number;
        $is_literal = 1;

    ### allow for quoted array constructor
    } elsif (! $is_aq && $$str_ref =~ m{ \G qw ([^\w\s]) \s* }gcxo) {
        my $quote = $1;
        $quote =~ y|([{<|)]}>|;
        $$str_ref =~ m{ \G (.*?) (?<!\\) \Q$quote\E \s* $QR_COMMENTS }gcxs
            || $self->throw('parse.missing.array_close', "Missing close \"$quote\"", undef, pos($$str_ref));
        my $str = $1;
        $str =~ s{ ^ \s+ }{}x;
        $str =~ s{ \s+ $ }{}x;
        $str =~ s{ \\ \Q$quote\E }{$quote}gx;
        push @var, [undef, '[]', split /\s+/, $str];

    ### allow for regex constructor
    } elsif (! $is_aq && $$str_ref =~ m{ \G / }gcx) {
        $$str_ref =~ m{ \G (.*?) (?<! \\) / ([msixeg]*) \s* $QR_COMMENTS }gcxos
            || $self->throw('parse', 'Unclosed regex tag "/"', undef, pos($$str_ref));
        my ($str, $opts) = ($1, $2);
        $self->throw('parse', 'e option not allowed on regex',   undef, pos($$str_ref)) if $opts =~ /e/;
        $self->throw('parse', 'g option not supported on regex', undef, pos($$str_ref)) if $opts =~ /g/;
        $str =~ s|\\n|\n|g;
        $str =~ s|\\t|\t|g;
        $str =~ s|\\r|\r|g;
        $str =~ s|\\\/|\/|g;
        $str =~ s|\\\$|\$|g;
        $self->throw('parse', "Invalid regex: $@", undef, pos($$str_ref)) if ! eval { "" =~ /$str/; 1 };
        push @var, [undef, 'qr', $str, $opts];

    ### looks like a normal variable start
    } elsif ($$str_ref =~ m{ \G (\w+) \s* $QR_COMMENTS }gcxo) {
        push @var, $1;
        $is_namespace = 1 if $self->{'NAMESPACE'} && $self->{'NAMESPACE'}->{$1};

    ### allow for literal strings
    } elsif ($$str_ref =~ m{ \G ([\"\']) }gcx) {
        my $quote = $1;
        $$str_ref =~ m{ \G (.*?) (?<! \\) $quote \s* $QR_COMMENTS }gcxs
            || $self->throw('parse', "Unclosed quoted string ($1)", undef, pos($$str_ref));
        my $str = $1;
        if ($quote eq "'") { # no interpolation on single quoted strings
            $str =~ s{ \\\' }{\'}xg;
            push @var, \ $str;
            $is_literal = 1;
        } else {
            $str =~ s/\\n/\n/g;
            $str =~ s/\\t/\t/g;
            $str =~ s/\\r/\r/g;
            $str =~ s/\\"/"/g;
            my @pieces = $is_aq
                ? split(m{ (?: ^ | (?<!\\)) (\$\w+            | \$\{ .*? (?<!\\) \}) }x, $str)  # autoquoted items get a single $\w+ - no nesting
                : split(m{ (?: ^ | (?<!\\)) (\$\w+ (?:\.\w+)* | \$\{ .*? (?<!\\) \}) }x, $str);
            my $n = 0;
            foreach my $piece (@pieces) {
                $piece =~ s/\\\$/\$/g;
                $piece =~ s/\\//g;
                next if ! ($n++ % 2);
                next if $piece !~ m{ ^ \$ (\w+ (?:\.\w+)*) $ }x
                    && $piece !~ m{ ^ \$\{ \s* (.*?) (?<!\\) \} $ }x;
                my $name = $1;
                $name =~ s/\\\}/\}/g;
                $piece = $self->parse_expr(\$name);
           }
            @pieces = grep {defined && length} @pieces;
            if (@pieces == 1 && ! ref $pieces[0]) {
                push @var, \ $pieces[0];
                $is_literal = 1;
            } elsif (! @pieces) {
                push @var, \ '';
                $is_literal = 1;
            } else {
                push @var, [undef, '~', @pieces];
            }
        }
        if ($is_aq) {
            return ${ $var[0] } if $is_literal;
            push @var, 0;
            return \@var;
        }

    ### allow for leading $foo type constructs
    } elsif ($$str_ref =~ m{ \G \$ (\w+) \b \s* $QR_COMMENTS }gcxo) {
        if ($self->{'V1DOLLAR'}) {
            push @var, $1;
            $is_namespace = 1 if $self->{'NAMESPACE'} && $self->{'NAMESPACE'}->{$1};
        } else {
            push @var, [$1, 0];
        }

    ### allow for ${foo.bar} type constructs
    } elsif ($$str_ref =~ m{ \G \$\{ \s* }gcx) {
        push @var, $self->parse_expr($str_ref);
        $$str_ref =~ m{ \G \s* \} \s* $QR_COMMENTS }gcxo
            || $self->throw('parse', 'Missing close "}" from "${"', undef, pos($$str_ref));

    ### looks like an array constructor
    } elsif (! $is_aq && $$str_ref =~ m{ \G \[ \s* $QR_COMMENTS }gcxo) {
        local $self->{'_operator_precedence'} = 0; # reset presedence
        my $arrayref = [undef, '[]'];
        while (defined(my $var = $self->parse_expr($str_ref))) {
            push @$arrayref, $var;
            $$str_ref =~ m{ \G , \s* $QR_COMMENTS }gcxo;
        }
        $$str_ref =~ m{ \G \] \s* $QR_COMMENTS }gcxo
            || $self->throw('parse.missing.square_bracket', "Missing close \]", undef, pos($$str_ref));
        push @var, $arrayref;

    ### looks like a hash constructor
    } elsif (! $is_aq && $$str_ref =~ m{ \G \{ \s* $QR_COMMENTS }gcxo) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $hashref = [undef, '{}'];
        while (defined(my $key = $self->parse_expr($str_ref, {auto_quote => "(\\w+\\b) (?! \\.) \\s* $QR_COMMENTS"}))) {
            $$str_ref =~ m{ \G = >? \s* $QR_COMMENTS }gcxo;
            my $val = $self->parse_expr($str_ref);
            push @$hashref, $key, $val;
            $$str_ref =~ m{ \G , \s* $QR_COMMENTS }gcxo;
        }
        $$str_ref =~ m{ \G \} \s* $QR_COMMENTS }gcxo
            || $self->throw('parse.missing.curly_bracket', "Missing close \}", undef, pos($$str_ref));
        push @var, $hashref;

    ### looks like a paren grouper
    } elsif (! $is_aq && $$str_ref =~ m{ \G \( \s* $QR_COMMENTS }gcxo) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $var = $self->parse_expr($str_ref, {allow_parened_ops => 1});

        $$str_ref =~ m{ \G \) \s* $QR_COMMENTS }gcxo
            || $self->throw('parse.missing.paren', "Missing close \)", undef, pos($$str_ref));

        $self->throw('parse', 'Paren group cannot be followed by an open paren', undef, pos($$str_ref))
            if $$str_ref =~ m{ \G \( }gcx;

        $already_parsed_args = 1;
        if (! ref $var) {
            push @var, \$var, 0;
            $is_literal = 1;
        } elsif (! defined $var->[0]) {
            push @var, $var, 0;
        } else {
            push @var, @$var;
        }

    ### nothing to find - return failure
    } else {
        pos($$str_ref) = $mark if $is_aq || $has_prefix;
        return;
    }

    # auto_quoted thing was too complicated
    if ($is_aq) {
        pos($$str_ref) = $mark;
        return;
    }

    ### looks for args for the initial
    if ($already_parsed_args) {
        # do nothing
    } elsif ($$str_ref =~ m{ \G \( \s* $QR_COMMENTS }gcxo) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $args = $self->parse_args($str_ref, {is_parened => 1});
        $$str_ref =~ m{ \G \) \s* $QR_COMMENTS }gcxo
            || $self->throw('parse.missing.paren', "Missing close \)", undef, pos($$str_ref));
        push @var, $args;
    } else {
        push @var, 0;
    }


    ### allow for nested items
    while ($$str_ref =~ m{ \G ( \.(?!\.) | \|(?!\|) ) }gcx) {
        if ($1 eq '|' && $self->{'V2PIPE'}) {
            pos($$str_ref) -= 1;
            last;
        }

        push(@var, $1) if ! $ARGS->{'no_dots'};

        $$str_ref =~ m{ \G \s* $QR_COMMENTS }gcxo;

        ### allow for interpolated variables in the middle - one.$foo.two
        if ($$str_ref =~ m{ \G \$ (\w+) \b \s* $QR_COMMENTS }gcxo) {
            push @var, $self->{'V1DOLLAR'} ? $1 : [$1, 0];

        ### or one.${foo.bar}.two
        } elsif ($$str_ref =~ m{ \G \$\{ \s* }gcx) {
            push @var, $self->parse_expr($str_ref);
            $$str_ref =~ m{ \G \s* \} \s* $QR_COMMENTS }gcxo
                || $self->throw('parse', 'Missing close "}" from "${"', undef, pos($$str_ref));

        ### allow for names
        } elsif ($$str_ref =~ m{ \G (-? \w+) \s* $QR_COMMENTS }gcxo) {
            push @var, $1;

        } else {
            $self->throw('parse', "Not sure how to continue parsing", undef, pos($$str_ref));
        }

        ### looks for args for the nested item
        if ($$str_ref =~ m{ \G \( \s* $QR_COMMENTS }gcxo) {
            local $self->{'_operator_precedence'} = 0; # reset precedence
            my $args = $self->parse_args($str_ref, {is_parened => 1});
            $$str_ref =~ m{ \G \) \s* $QR_COMMENTS }gcxo
                || $self->throw('parse.missing.paren', "Missing close \)", undef, pos($$str_ref));
            push @var, $args;
        } else {
            push @var, 0;
        }

    }

    ### flatten literals and constants as much as possible
    my $var;
    if ($is_literal) {
        $var = ${ $var[0] };
        if ($#var != 1) {
            $var[0] = [undef, '~', $var];
            $var = \@var;
        }
    } elsif ($is_namespace) {
        $var = $self->play_expr(\@var, {is_namespace_during_compile => 1});
    } else {
        $var = \@var;
    }

    ### allow for all "operators"
    if (! $self->{'_operator_precedence'} && defined $is_aq) {
        my $tree;
        my $found;
        while (1) {
            my $mark = pos $$str_ref;
            if ($self->{'_end_tag'} && $$str_ref =~ m{ \G [+=~-]? $self->{'_end_tag'} }gcx) {
                pos($$str_ref) = $mark;
                last;
            } elsif ($$str_ref !~ m{ \G ($QR_OP) }gcxo) {
                pos($$str_ref) = $mark;
                last;
            }
            if ($OP_ASSIGN->{$1} && ! $ARGS->{'allow_parened_ops'}) { # only allow assignment in parens
                pos($$str_ref) = $mark;
                last;
            }
            local $self->{'_operator_precedence'} = 1;
            my $op = $1;
            $op = 'eq' if $op eq '==' && (! defined($self->{'V2EQUALS'}) || $self->{'V2EQUALS'});
            $op = 'ne' if $op eq '!=' && (! defined($self->{'V2EQUALS'}) || $self->{'V2EQUALS'});

            $$str_ref =~ m{ \G \s* $QR_COMMENTS }gcxo;

            ### allow for postfix - doesn't check precedence - someday we might change - but not today (only affects post ++ and --)
            if ($OP_POSTFIX->{$op}) {
                $var = [[undef, $op, $var, 1], 0]; # cheat - give a "second value" to postfix ops
                next;

            ### allow for prefix operator precedence
            } elsif ($has_prefix && $OP->{$op}->[1] < $OP_PREFIX->{$has_prefix->[-1]}->[1]) {
                if ($tree) {
                    if ($#$tree == 1) { # only one operator - keep simple things fast
                        $var = [[undef, $tree->[0], $var, $tree->[1]], 0];
                    } else {
                        unshift @$tree, $var;
                        $var = $self->apply_precedence($tree, $found);
                    }
                    undef $tree;
                    undef $found;
                }
                $var = [[undef, $has_prefix->[-1], $var ], 0];
                if (! @$has_prefix) { undef $has_prefix } else { pop @$has_prefix }
            }

            ### add the operator to the tree
            my $var2 =  $self->parse_expr($str_ref, {from_here => 1});
            $self->throw('parse', 'Missing variable after "'.$op.'"', undef, pos($$str_ref)) if ! defined $var2;
            push (@{ $tree ||= [] }, $op, $var2);
            $found->{$OP->{$op}->[1]}->{$op} = 1; # found->{precedence}->{op}
        }

        ### if we found operators - tree the nodes by operator precedence
        if ($tree) {
            if (@$tree == 2) { # only one operator - keep simple things fast
                $var = [[undef, $tree->[0], $var, $tree->[1]], 0];
            } else {
                unshift @$tree, $var;
                $var = $self->apply_precedence($tree, $found);
            }
        }
    }

    ### allow for prefix on non-chained variables
    if ($has_prefix) {
        $var = [[undef, $_, $var], 0] for reverse @$has_prefix;
    }

    return $var;
}

### this is used to put the parsed variables into the correct operations tree
sub apply_precedence {
    my ($self, $tree, $found) = @_;

    my @var;
    my $trees;
    ### look at the operators we found in the order we found them
    for my $prec (sort keys %$found) {
        my $ops = $found->{$prec};
        local $found->{$prec};
        delete $found->{$prec};

        ### split the array on the current operators for this level
        my @ops;
        my @exprs;
        for (my $i = 1; $i <= $#$tree; $i += 2) {
            next if ! $ops->{ $tree->[$i] };
            push @ops, $tree->[$i];
            push @exprs, [splice @$tree, 0, $i, ()];
            shift @$tree;
            $i = -1;
        }
        next if ! @exprs; # this iteration didn't have the current operator
        push @exprs, $tree if scalar @$tree; # add on any remaining items

        ### simplify sub expressions
        for my $node (@exprs) {
            if (@$node == 1) {
                $node = $node->[0]; # single item - its not a tree
            } elsif (@$node == 3) {
                $node = [[undef, $node->[1], $node->[0], $node->[2]], 0]; # single operator - put it straight on
            } else {
                $node = $self->apply_precedence($node, $found); # more complicated - recurse
            }
        }

        ### assemble this current level

        ### some rules:
        # 1) items at the same precedence level must all be either right or left or ternary associative
        # 2) ternary items cannot share precedence with anybody else.
        # 3) there really shouldn't be another operator at the same level as a postfix
        my $type = $OP->{$ops[0]}->[0];

        if ($type eq 'ternary') {
            my $op = $OP->{$ops[0]}->[2]->[0]; # use the first op as what we are using

            ### return simple ternary
            if (@exprs == 3) {
                $self->throw('parse', "Ternary operator mismatch") if $ops[0] ne $op;
                $self->throw('parse', "Ternary operator mismatch") if ! $ops[1] || $ops[1] eq $op;
                return [[undef, $op, @exprs], 0];
            }


            ### reorder complex ternary - rare case
            while ($#ops >= 1) {
                ### if we look starting from the back - the first lead ternary op will always be next to its matching op
                for (my $i = $#ops; $i >= 0; $i --) {
                    next if $OP->{$ops[$i]}->[2]->[1] eq $ops[$i];
                    my ($op, $op2) = splice @ops, $i, 2, (); # remove the pair of operators
                    my $node = [[undef, $op, @exprs[$i .. $i + 2]], 0];
                    splice @exprs, $i, 3, $node;
                }
            }
            return $exprs[0]; # at this point the ternary has been reduced to a single operator

        } elsif ($type eq 'right' || $type eq 'assign') {
            my $val = $exprs[-1];
            $val = [[undef, $ops[$_ - 1], $exprs[$_], $val], 0] for reverse (0 .. $#exprs - 1);
            return $val;

        } else {
            my $val = $exprs[0];
            $val = [[undef, $ops[$_ - 1], $val, $exprs[$_]], 0] for (1 .. $#exprs);
            return $val;

        }
    }

    $self->throw('parse', "Couldn't apply precedence");
}

### look for arguments - both positional and named
sub parse_args {
    my $self    = shift;
    my $str_ref = shift;
    my $ARGS    = shift || {};

    my @args;
    my @named;
    my $name;
    my $end = $self->{'_end_tag'} || '(?!)';
    while (1) {
        my $mark = pos $$str_ref;

        ### look to see if the next thing is a directive or a closing tag
        if (! $ARGS->{'is_parened'}
            && ! $ARGS->{'require_arg'}
            && $$str_ref =~ m{ \G $QR_DIRECTIVE (?: \s+ | (?: \s* $QR_COMMENTS (?: ;|[+=~-]?$end))) }gcxo
            && ((pos($$str_ref) = $mark) || 1)                  # always revert
            && $DIRECTIVES->{$self->{'ANYCASE'} ? uc($1) : $1}  # looks like a directive - we are done
            ) {
            last;
        }
        if ($$str_ref =~ m{ \G [+=~-]? $end }gcx) {
            pos($$str_ref) = $mark;
            last;
        }

        ### find the initial arg
        my $name;
        if ($ARGS->{'allow_bare_filenames'}) {
            $name = $self->parse_expr($str_ref, {auto_quote => "
              ($QR_FILENAME               # file name
              | $QR_BLOCK                 # or block
                (?= [+=~-]? $end          # an end tag
                  | \\s*[+,;]             # followed by explicit + , or ;
                  | \\s+ (?! [\\s=])      # or space not before an =
                )  \\s* $QR_COMMENTS"});
            # filenames can be separated with a "+" - why a "+" ?
            if ($$str_ref =~ m{ \G \+ (?! [+=~-]? $end) \s* $QR_COMMENTS }gcxo) {
                push @args, $name;
                $ARGS->{'require_arg'} = 1;
                next;
            }
        }
        if (! defined $name) {
            $name = $self->parse_expr($str_ref);
            if (! defined $name) {
                if ($ARGS->{'require_arg'} && ! @args && ! $ARGS->{'positional_only'} && ! @named) {
                    $self->throw('parse', 'Argument required', undef, pos($$str_ref));
                } else {
                    last;
                }
            }
        }

        $$str_ref =~ m{ \G \s* $QR_COMMENTS }gcxo;

        ### see if it is named or positional
        if ($$str_ref =~ m{ \G = >? \s* $QR_COMMENTS }gcxo) {
            $self->throw('parse', 'Named arguments not allowed', undef, $mark) if $ARGS->{'positional_only'};
            my $val = $self->parse_expr($str_ref);
            $name = $name->[0] if ref($name) && @$name == 2 && ! $name->[1]; # strip a level of indirection on named arguments
            push @named, $name, $val;
        } else {
            push @args, $name;
        }

        ### look for trailing comma
        $ARGS->{'require_arg'} = ($$str_ref =~ m{ \G , \s* $QR_COMMENTS }gcxo) || 0;
    }

    ### allow for named arguments to be added at the front (if asked)
    if ($ARGS->{'named_at_front'}) {
        unshift @args, [[undef, '{}', @named], 0];
    } elsif (scalar @named) { # only add at end - if there are some
        push @args,    [[undef, '{}', @named], 0]
    }

    return \@args;
}

### allow for looking for $foo or ${foo.bar} in TEXT "nodes" of the parse tree.
sub interpolate_node {
    my ($self, $tree, $offset) = @_;
    return if $self->{'_no_interp'};

    ### split on variables while keeping the variables
    my @pieces = split m{ (?: ^ | (?<! \\)) (\$\w+ (?:\.\w+)* | \$\{ .*? (?<!\\) \}) }x, $tree->[-1];
    if ($#pieces <= 0) {
        $tree->[-1] =~ s{ \\ ([\"\$]) }{$1}xg;
        return;
    }

    my @sub_tree;
    my $n = 0;
    foreach my $piece (@pieces) {
        $offset += length $piece; # we track the offset to make sure DEBUG has the right location
        $piece =~ s{ \\ ([\"\$]) }{$1}xg;
        if (! ($n++ % 2)) { # odds will always be text chunks
            next if ! length $piece;
            push @sub_tree, $piece;
        } elsif ($piece =~ m{ ^ \$ (\w+ (?:\.\w+)*) $ }x
                 || $piece =~ m{ ^ \$\{ \s* (.*?) (?<!\\) \} $ }x) {
            my $name = $1;
            $name =~ s/\\\}/\}/g;
            push @sub_tree, ['GET', $offset - length($piece), $offset, $self->parse_expr(\$name)];
        } else {
            $self->throw('parse', "Parse error during interpolate node");
        }
    }

    ### replace the tree
    splice @$tree, -1, 1, @sub_tree;
}

###----------------------------------------------------------------###

sub execute_tree {
    my ($self, $tree, $out_ref) = @_;

    # node contains (0: DIRECTIVE,
    #                1: start_index,
    #                2: end_index,
    #                3: parsed tag details,
    #                4: sub tree for block types
    #                5: continuation sub trees for sub continuation block types (elsif, else, etc)
    #                6: flag to capture next directive
    for my $node (@$tree) {
        ### text nodes are just the bare text
        if (! ref $node) {
            $$out_ref .= $node if defined $node;
            next;
        }

        $$out_ref .= $self->debug_node($node) if $self->{'_debug_dirs'} && ! $self->{'_debug_off'};

        $DIRECTIVES->{$node->[0]}->[1]->($self, $node->[3], $node, $out_ref);
    }
}

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
        if ($ARGS->{'is_namespace_during_compile'}) {
            $ref = $self->{'NAMESPACE'}->{$name};
        } else {
            return if $QR_PRIVATE && $name =~ $QR_PRIVATE; # don't allow vars that begin with _
            return \$self->{'_vars'}->{$name} if $i >= $#$var && $ARGS->{'return_ref'} && ! ref $self->{'_vars'}->{$name};
            $ref = $self->{'_vars'}->{$name};
            if (! defined $ref) {
                $ref = ($name eq 'template' || $name eq 'component') ? $self->{"_$name"} : $VOBJS->{$name};
                $ref = $SCALAR_OPS->{$name} if ! $ref && (! defined($self->{'VMETHOD_FUNCTIONS'}) || $self->{'VMETHOD_FUNCTIONS'});
            }
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
                if ($name =~ m{ ^ -? $QR_NUM $ }ox) {
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

sub is_empty_named_args {
    my ($self, $hash_ident) = @_;
    # [[undef, '{}', 'key1', 'val1', 'key2, 'val2'], 0]
    return @{ $hash_ident->[0] } <= 2;
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
            if ($name =~ m{ ^ -? $QR_NUM $ }ox) {
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

sub parse_BLOCK {
    my ($self, $str_ref, $node) = @_;

    my $end = $self->{'_end_tag'} || '(?!)';
    my $block_name = $self->parse_expr($str_ref, {auto_quote => "
              ($QR_FILENAME               # file name
              | $QR_BLOCK                 # or block
                (?= [+=~-]? $end          # an end tag
                  | \\s*[+,;]             # followed by explicit + , or ;
                  | \\s+ (?! [\\s=])      # or space not before an =
                )  \\s* $QR_COMMENTS"});

    return '' if ! defined $block_name;

    my $prepend = join "/", map {$_->[3]} grep {ref($_) && $_->[0] eq 'BLOCK'} @{ $self->{'_state'} || {} };
    return $prepend ? "$prepend/$block_name" : $block_name;
}

sub play_BLOCK {
    my ($self, $block_name, $node, $out_ref) = @_;

    ### store a named reference - but do nothing until something processes it
    $self->{'BLOCKS'}->{$block_name} = {
        _tree => $node->[4],
        name  => $self->{'_component'}->{'name'} .'/'. $block_name,
    };

    return;
}

sub parse_CALL { $DIRECTIVES->{'GET'}->[0]->(@_) }

sub play_CALL {
    my ($self, $ident, $node) = @_;
    my $var = $self->play_expr($ident);
    $var = $self->undefined_get($ident, $node) if ! defined $var;
    return;
}

sub parse_CASE {
    my ($self, $str_ref) = @_;
    return if $$str_ref =~ m{ \G DEFAULT \s* }gcx;
    return $self->parse_expr($str_ref);
}

sub parse_CATCH {
    my ($self, $str_ref) = @_;
    return $self->parse_expr($str_ref, {auto_quote => "(\\w+\\b (?: \\.\\w+\\b)*) $QR_AQ_SPACE \\s* $QR_COMMENTS"});
}

sub play_control {
    my ($self, $undef, $node) = @_;
    $self->throw(lc($node->[0]), 'Control exception', $node);
}

sub play_CLEAR {
    my ($self, $undef, $node, $out_ref) = @_;
    $$out_ref = '';
    return;
}

sub parse_CONFIG {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::parse_CONFIG;
}

sub play_CONFIG {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_CONFIG;
}

sub parse_DEBUG {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::parse_DEBUG;
}

sub play_DEBUG {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_DEBUG;
}

sub parse_DEFAULT { $DIRECTIVES->{'SET'}->[0]->(@_) }

sub play_DEFAULT {
    my ($self, $set) = @_;
    foreach (@$set) {
        my ($op, $set, $default) = @$_;
        next if ! defined $set;
        my $val = $self->play_expr($set);
        if (! $val) {
            $default = defined($default) ? $self->play_expr($default) : '';
            $self->set_variable($set, $default);
        }
    }
    return;
}

sub parse_DUMP {
    my ($self, $str_ref) = @_;
    return $self->parse_args($str_ref, {named_at_front => 1});
}

sub play_DUMP {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_DUMP;
}

sub parse_FILTER {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::parse_FILTER;
}

sub play_FILTER {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_FILTER;
}

sub parse_FOREACH {
    my ($self, $str_ref) = @_;
    my $items = $self->parse_expr($str_ref);
    my $var;
    if ($$str_ref =~ m{ \G (= | [Ii][Nn]\b) \s* }gcx) {
        $var = [@$items];
        $items = $self->parse_expr($str_ref);
    }
    return [$var, $items];
}

sub play_FOREACH {
    my ($self, $ref, $node, $out_ref) = @_;

    ### get the items - make sure it is an arrayref
    my ($var, $items) = @$ref;

    $items = $self->play_expr($items);
    return '' if ! defined $items;

    if (ref($items) !~ /Iterator$/) {
        $items = $self->iterator($items);
    }

    my $sub_tree = $node->[4];

    local $self->{'_vars'}->{'loop'} = $items;

    ### if the FOREACH tag sets a var - then nothing but the loop var gets localized
    if (defined $var) {
        my ($item, $error) = $items->get_first;
        while (! $error) {

            $self->set_variable($var, $item);

            ### execute the sub tree
            eval { $self->execute_tree($sub_tree, $out_ref) };
            if (my $err = $@) {
                if (UNIVERSAL::isa($err, $PACKAGE_EXCEPTION)) {
                    if ($err->type eq 'next') {
                        ($item, $error) = $items->get_next;
                        next;
                    }
                    last if $err->type =~ /last|break/;
                }
                die $err;
            }

            ($item, $error) = $items->get_next;
        }
        die $error if $error && $error != 3; # Template::Constants::STATUS_DONE;
    ### if the FOREACH tag doesn't set a var - then everything gets localized
    } else {

        ### localize variable access for the foreach
        my $swap = $self->{'_vars'};
        local $self->{'_vars'} = my $copy = {%$swap};

        ### iterate use the iterator object
        #foreach (my $i = $items->index; $i <= $#$vals; $items->index(++ $i)) {
        my ($item, $error) = $items->get_first;
        while (! $error) {

            if (ref($item) eq 'HASH') {
                @$copy{keys %$item} = values %$item;
            }

            ### execute the sub tree
            eval { $self->execute_tree($sub_tree, $out_ref) };
            if (my $err = $@) {
                if (UNIVERSAL::isa($err, $PACKAGE_EXCEPTION)) {
                    if ($err->type eq 'next') {
                        ($item, $error) = $items->get_next;
                        next;
                    }
                    last if $err->type =~ /last|break/;
                }
                die $err;
            }

            ($item, $error) = $items->get_next;
        }
        die $error if $error && $error != 3; # Template::Constants::STATUS_DONE;
    }

    return;
}

sub parse_GET {
    my ($self, $str_ref) = @_;
    my $ref = $self->parse_expr($str_ref);
    $self->throw('parse', "Missing variable name", undef, pos($$str_ref)) if ! defined $ref;
    return $ref;
}

sub play_GET {
    my ($self, $ident, $node, $out_ref) = @_;
    my $var = $self->play_expr($ident);
    if (defined $var) {
        $$out_ref .= $var;
    } else {
        $var = $self->undefined_get($ident, $node);
        $$out_ref .= $var if defined $var;
    }
    return;
}

sub parse_IF {
    my ($self, $str_ref) = @_;
    return $self->parse_expr($str_ref);
}

sub play_IF {
    my ($self, $var, $node, $out_ref) = @_;

    my $val = $self->play_expr($var);
    if ($val) {
        my $body_ref = $node->[4] ||= [];
        $self->execute_tree($body_ref, $out_ref);
        return;
    }

    while ($node = $node->[5]) { # ELSE, ELSIF's
        if ($node->[0] eq 'ELSE') {
            my $body_ref = $node->[4] ||= [];
            $self->execute_tree($body_ref, $out_ref);
            return;
        }
        my $var = $node->[3];
        my $val = $self->play_expr($var);
        if ($val) {
            my $body_ref = $node->[4] ||= [];
            $self->execute_tree($body_ref, $out_ref);
            return;
        }
    }
    return;
}

sub parse_INCLUDE { $DIRECTIVES->{'PROCESS'}->[0]->(@_) }

sub play_INCLUDE {
    my ($self, $str_ref, $node, $out_ref) = @_;

    ### localize the swap
    my $swap = $self->{'_vars'} || {};
    local $self->{'_vars'} = {%$swap};

    ### localize the blocks
    my $blocks = $self->{'BLOCKS'} || {};
    local $self->{'BLOCKS'} = {%$blocks};

    return $DIRECTIVES->{'PROCESS'}->[1]->($self, $str_ref, $node, $out_ref);
}

sub parse_INSERT { $DIRECTIVES->{'PROCESS'}->[0]->(@_) }

sub play_INSERT {
    my ($self, $args, $node, $out_ref) = @_;

    my ($named, @files) = @$args;

    foreach my $name (@files) {
        my $filename = $self->play_expr($name);
        $$out_ref .= $self->_insert($filename);
    }

    return;
}

sub parse_LOOP {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::parse_LOOP;
}

sub play_LOOP {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_LOOP;
}

sub parse_MACRO {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::parse_MACRO;
}

sub play_MACRO {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_MACRO;
}

sub parse_META {
    my ($self, $str_ref) = @_;
    my $args = $self->parse_args($str_ref, {named_at_front => 1});
    my $hash;
    return $hash if ($hash = $self->play_expr($args->[0])) && UNIVERSAL::isa($hash, 'HASH');
    return undef;
}


sub play_META {
    my ($self, $hash) = @_;
    return if ! $hash;
    my @keys = keys %$hash;

    my $ref;
    if ($self->{'_top_level'}) {
        $ref = $self->{'_template'} ||= {};
    } else {
        $ref = $self->{'_component'} ||= {};
    }

    @{ $ref }{ @keys } = @{ $hash }{ @keys };
    return;
}

sub play_PERL {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_PERL;
}

sub parse_PROCESS {
    my ($self, $str_ref) = @_;

    return $self->parse_args($str_ref, {
        named_at_front       => 1,
        allow_bare_filenames => 1,
        require_arg          => 1,
    });
}

sub play_PROCESS {
    my ($self, $info, $node, $out_ref) = @_;
    if ($self->{'NO_INCLUDES'}) {
        $self->throw('file', "NO_INCLUDES was set during a $node->[0] directive");
    }

    my ($args, @files) = @$info;

    ### set passed args
    # [[undef, '{}', 'key1', 'val1', 'key2', 'val2'], 0]
    $args = $args->[0];
    foreach (my $i = 2; $i < @$args; $i+=2) {
        my $key = $args->[$i];
        my $val = $self->play_expr($args->[$i+1]);
        if (ref($key) && @$key == 2 && $key->[0] eq 'import' && UNIVERSAL::isa($val, 'HASH')) { # import ?! - whatever
            foreach my $key (keys %$val) {
                $self->set_variable([$key,0], $val->{$key});
            }
            next;
        }
        $self->set_variable($key, $val);
    }

    ### iterate on any passed block or filename
    foreach my $ref (@files) {
        next if ! defined $ref;
        my $filename = $self->play_expr($ref);
        my $out = ''; # have temp item to allow clear to correctly clear

        ### normal blocks or filenames
        if (! ref $filename) {
            eval { $self->_process($filename, $self->{'_vars'}, \$out) }; # restart the swap - passing it our current stash

        ### allow for $template which is used in some odd instances
        } else {
            my $doc;
            if ($ref->[0] eq 'template') {
                $doc = $filename;
            } else {
                $doc = $self->play_expr($ref);
                if (ref($doc) ne 'HASH' || ! $doc->{'_tree'}) {
                    $self->throw('process', "Passed item doesn't appear to be a valid document");
                }
            }
            $self->throw('process', "Recursion detected in $node->[0] \$template") if $self->{'_process_dollar_template'};
            local $self->{'_process_dollar_template'} = 1;
            local $self->{'_component'} = $filename;
            return if ! $doc->{'_tree'};

            ### execute and trim
            eval { $self->execute_tree($doc->{'_tree'}, \$out) };
            if ($self->{'TRIM'}) {
                $out =~ s{ \s+ $ }{}x;
                $out =~ s{ ^ \s+ }{}x;
            }

            ### handle exceptions
            if (my $err = $@) {
                $err = $self->exception('undef', $err) if ref($err) !~ /Template::Exception$/;
                $err->doc($doc) if $doc && $err->can('doc') && ! $err->doc;
            }

        }

        ### append any output
        $$out_ref .= $out;
        if (my $err = $@) {
            die $err if ref($err) !~ /Template::Exception$/ || $err->type !~ /return/;
        }
    }

    return;
}

sub play_RAWPERL {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_RAWPERL;
}

sub parse_SET {
    my ($self, $str_ref, $node, $initial_op, $initial_var) = @_;
    my @SET;
    my $func;

    if ($initial_op) {
        if ($$str_ref =~ m{ \G $QR_DIRECTIVE }gcx                # find a word
            && ((pos($$str_ref) -= length($1)) || 1)             # always revert
            && $DIRECTIVES->{$self->{'ANYCASE'} ? uc $1 : $1}) { # make sure its a directive - if so set up capturing
            $node->[6] = 1;                                      # set a flag to keep parsing
            my $val = $node->[4] ||= [];                         # setup storage
            return [[$initial_op, $initial_var, $val]];
        } else { # get a normal variable
            return [[$initial_op, $initial_var, $self->parse_expr($str_ref)]];
        }
    }

    while (1) {
        my $set = $self->parse_expr($str_ref);
        last if ! defined $set;

        if ($$str_ref =~ m{ \G ($QR_OP_ASSIGN) >? \s* }gcx) {
            my $op = $1;
            if ($$str_ref =~ m{ \G $QR_DIRECTIVE }gcx                # find a word
                && ((pos($$str_ref) -= length($1)) || 1)             # always revert
                && $DIRECTIVES->{$self->{'ANYCASE'} ? uc $1 : $1}) { # make sure its a directive - if so set up capturing
                $node->[6] = 1;                                      # set a flag to keep parsing
                my $val = $node->[4] ||= [];                         # setup storage
                push @SET, [$op, $set, $val];
                last;
            } else { # get a normal variable
                push @SET, [$op, $set, $self->parse_expr($str_ref)];
            }
        } else {
            push @SET, ['=', $set, undef];
        }
    }

    return \@SET;
}

sub play_SET {
    my ($self, $set, $node) = @_;
    foreach (@$set) {
        my ($op, $set, $val) = @$_;
        if (! defined $val) { # not defined
            # do nothing - allow for setting to undef
        } elsif ($node->[4] && $val == $node->[4]) { # a captured directive
            my $sub_tree = $node->[4];
            $sub_tree = $sub_tree->[0]->[4] if $sub_tree->[0] && $sub_tree->[0]->[0] eq 'BLOCK';
            $val = '';
            $self->execute_tree($sub_tree, \$val);
        } else { # normal var
            $val = $self->play_expr($val);
        }

        if ($OP_DISPATCH->{$op}) {
            local $^W;
            $val = $OP_DISPATCH->{$op}->($self->play_expr($set), $val);
        }

        $self->set_variable($set, $val);
    }
    return;
}

sub parse_SWITCH { $DIRECTIVES->{'GET'}->[0]->(@_) }

sub play_SWITCH {
    my ($self, $var, $node, $out_ref) = @_;

    my $val = $self->play_expr($var);
    $val = '' if ! defined $val;
    ### $node->[4] is thrown away

    my $default;
    while ($node = $node->[5]) { # CASES
        my $var = $node->[3];
        if (! defined $var) {
            $default = $node->[4];
            next;
        }

        my $val2 = $self->play_expr($var);
        $val2 = [$val2] if ! UNIVERSAL::isa($val2, 'ARRAY');
        for my $test (@$val2) { # find matching values
            next if ! defined $val && defined $test;
            next if defined $val && ! defined $test;
            if ($val ne $test) { # check string-wise first - then numerical
                next if $val  !~ m{ ^ -? $QR_NUM $ }ox;
                next if $test !~ m{ ^ -? $QR_NUM $ }ox;
                next if $val != $test;
            }

            my $body_ref = $node->[4] ||= [];
            $self->execute_tree($body_ref, $out_ref);
            return;
        }
    }

    if ($default) {
        $self->execute_tree($default, $out_ref);
    }

    return;
}

sub parse_TAGS {
    my ($self, $str_ref, $node) = @_;

    my ($start, $end);
    if ($$str_ref =~ m{ \G (\w+) \s* $QR_COMMENTS }gcxs) {
        my $ref = $TAGS->{lc $1} || $self->throw('parse', "Invalid TAGS name \"$1\"", undef, pos($$str_ref));
        ($start, $end) = @$ref;

    } else {
        local $self->{'_operator_precedence'} = 1; # prevent operator matching
        $start = $$str_ref =~ m{ \G (?= [\'\"\/]) }gcx
            ? $self->parse_expr($str_ref)
            : $self->parse_expr($str_ref, {auto_quote => "(\\S+) \\s+ $QR_COMMENTS"})
            || $self->throw('parse', "Invalid opening tag in TAGS", undef, pos($$str_ref));
        $end   = $$str_ref =~ m{ \G (?= [\'\"\/]) }gcx
            ? $self->parse_expr($str_ref)
            : $self->parse_expr($str_ref, {auto_quote => "(\\S+) \\s* $QR_COMMENTS"})
            || $self->throw('parse', "Invalid closing tag in TAGS", undef, pos($$str_ref));
        for my $tag ($start, $end) {
            $tag = $self->play_expr($tag);
            $tag = quotemeta($tag) if ! ref $tag;
        }
    }
    return [$start, $end];
}

sub parse_THROW {
    my ($self, $str_ref, $node) = @_;
    my $name = $self->parse_expr($str_ref, {auto_quote => "(\\w+\\b (?: \\.\\w+\\b)*) $QR_AQ_SPACE \\s* $QR_COMMENTS"});
    $self->throw('parse.missing', "Missing name in THROW", $node, pos($$str_ref)) if ! $name;
    my $args = $self->parse_args($str_ref, {named_at_front => 1});
    return [$name, $args];
}

sub play_THROW {
    my ($self, $ref, $node) = @_;
    my ($name, $args) = @$ref;

    $name = $self->play_expr($name);

    my ($named, @args) = @$args;
    push @args, $named if ! $self->is_empty_named_args($named); # add named args back on at end - if there are some

    @args = map { $self->play_expr($_) } @args;
    $self->throw($name, \@args, $node); # dies
    return; # but return just in case
}

sub play_TRY {
    my ($self, $foo, $node, $out_ref) = @_;
    my $out = '';

    my $body_ref = $node->[4];
    eval { $self->execute_tree($body_ref, \$out) };
    my $err = $@;

    if (! $node->[5]) { # no catch or final
        if (! $err) { # no final block and no error
            $$out_ref .= $out;
            return;
        }
        $self->throw('parse.missing', "Missing CATCH block", $node);
    }
    if ($err) {
        $err = $self->exception('undef', $err) if ref($err) !~ /Template::Exception$/;
        if ($err->type =~ /stop|return/) {
            $$out_ref .= $out;
            die $err;
        }
    }

    ### loop through the nested catch and final blocks
    my $catch_body_ref;
    my $last_found;
    my $type = $err ? $err->type : '';
    my $final;
    while ($node = $node->[5]) { # CATCH
        if ($node->[0] eq 'FINAL') {
            $final = $node->[4];
            next;
        }
        next if ! $err;
        my $name = $self->play_expr($node->[3]);
        $name = '' if ! defined $name || lc($name) eq 'default';
        if ($type =~ / ^ \Q$name\E \b /x
            && (! defined($last_found) || length($last_found) < length($name))) { # more specific wins
            $catch_body_ref = $node->[4] || [];
            $last_found     = $name;
        }
    }

    ### play the best catch block
    if ($err) {
        if (! $catch_body_ref) {
            $$out_ref .= $out;
            die $err;
        }
        local $self->{'_vars'}->{'error'} = $err;
        local $self->{'_vars'}->{'e'}     = $err;
        eval { $self->execute_tree($catch_body_ref, \$out) };
        if (my $err = $@) {
            $$out_ref .= $out;
            die $err;
        }
    }

    ### the final block
    $self->execute_tree($final, \$out) if $final;

    $$out_ref .= $out;

    return;
}

sub parse_UNLESS {
    my $ref = $DIRECTIVES->{'IF'}->[0]->(@_);
    return [[undef, '!', $ref], 0];
}

sub play_UNLESS { return $DIRECTIVES->{'IF'}->[1]->(@_) }

sub parse_USE {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::parse_USE;
}

sub play_USE {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_USE;
}

sub parse_VIEW {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::parse_VIEW;
}

sub play_VIEW {
    require CGI::Ex::Template::Extra;
    &CGI::Ex::Template::Extra::play_VIEW;
}

sub parse_WHILE { $DIRECTIVES->{'IF'}->[0]->(@_) }

sub play_WHILE {
    my ($self, $var, $node, $out_ref) = @_;
    return if ! defined $var;

    my $sub_tree = $node->[4];

    ### iterate use the iterator object
    my $count = $WHILE_MAX;
    while (--$count > 0) {

        $self->play_expr($var) || last;

        ### execute the sub tree
        eval { $self->execute_tree($sub_tree, $out_ref) };
        if (my $err = $@) {
            if (UNIVERSAL::isa($err, $PACKAGE_EXCEPTION)) {
                next if $err->type =~ /next/;
                last if $err->type =~ /last|break/;
            }
            die $err;
        }
    }
    die "WHILE loop terminated (> $WHILE_MAX iterations)\n" if ! $count;

    return;
}

sub parse_WRAPPER { $DIRECTIVES->{'PROCESS'}->[0]->(@_) }

sub play_WRAPPER {
    my ($self, $args, $node, $out_ref) = @_;
    my $sub_tree = $node->[4] || return;

    my ($named, @files) = @$args;

    my $out = '';
    $self->execute_tree($sub_tree, \$out);

    foreach my $name (reverse @files) {
        local $self->{'_vars'}->{'content'} = $out;
        $out = '';
        $DIRECTIVES->{'INCLUDE'}->[1]->($self, [$named, $name], $node, \$out);
    }

    $$out_ref .= $out;
    return;
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

sub process {
    my ($self, $in, $swap, $out, @ARGS) = @_;
    delete $self->{'error'};

    my $args;
    $args = ($#ARGS == 0 && UNIVERSAL::isa($ARGS[0], 'HASH')) ? {%{$ARGS[0]}} : {@ARGS} if scalar @ARGS;
    $self->DEBUG("set binmode\n") if $args->{'binmode'}; # holdover for TT2 tests

    ### get the content
    my $content;
    if (ref $in) {
        if (UNIVERSAL::isa($in, 'SCALAR')) { # reference to a string
            $content = $in;
        } elsif (UNIVERSAL::isa($in, 'CODE')) {
            $content = $in->();
            $content = \$content;
        } else { # should be a file handle
            local $/ = undef;
            $content = <$in>;
            $content = \$content;
        }
    } else {
        ### should be a filename
        $content = $in;
    }


    ### prepare block localization
    my $blocks = $self->{'BLOCKS'} ||= {};


    ### do the swap
    my $output = '';
    eval {

        ### localize the stash
        $swap ||= {};
        my $var1 = $self->{'_vars'} ||= {};
        my $var2 = $self->{'VARIABLES'} || $self->{'PRE_DEFINE'} || {};
        $var1->{'global'} ||= {}; # allow for the "global" namespace - that continues in between processing
        my $copy = {%$var2, %$var1, %$swap};

        local $self->{'BLOCKS'} = $blocks = {%$blocks}; # localize blocks - but save a copy to possibly restore
        local $self->{'_template'};

        delete $self->{'_debug_off'};
        delete $self->{'_debug_format'};

        ### handle pre process items that go before every document
        my $pre = '';
        if ($self->{'PRE_PROCESS'}) {
            $self->_load_template_meta($content);
            foreach my $name (@{ $self->split_paths($self->{'PRE_PROCESS'}) }) {
                $self->_process($name, $copy, \$pre);
            }
        }

        ### process the central file now - catching errors to allow for the ERROR config
        eval {
            ### handle the PROCESS config - which loads another template in place of the real one
            if (exists $self->{'PROCESS'}) {
                $self->_load_template_meta($content);
                foreach my $name (@{ $self->split_paths($self->{'PROCESS'}) }) {
                    next if ! length $name;
                    $self->_process($name, $copy, \$output);
                }

                ### handle "normal" content
            } else {
                local $self->{'_start_top_level'} = 1;
                $self->_process($content, $copy, \$output);
            }
        };

        ### catch errors with ERROR config
        if (my $err = $@) {
            $err = $self->exception('undef', $err) if ref($err) !~ /Template::Exception$/;
            die $err if $err->type =~ /stop|return/;
            my $catch = $self->{'ERRORS'} || $self->{'ERROR'} || die $err;
            $catch = {default => $catch} if ! ref $catch;
            my $type = $err->type;
            my $last_found;
            my $file;
            foreach my $name (keys %$catch) {
                my $_name = (! defined $name || lc($name) eq 'default') ? '' : $name;
                if ($type =~ / ^ \Q$_name\E \b /x
                    && (! defined($last_found) || length($last_found) < length($_name))) { # more specific wins
                    $last_found = $_name;
                    $file       = $catch->{$name};
                }
            }

            ### found error handler - try it out
            if (defined $file) {
                $output = '';
                local $copy->{'error'} = local $copy->{'e'} = $err;
                $self->_process($file, $copy, \$output);
            }
        }

        ### handle wrapper directives
        if (exists $self->{'WRAPPER'}) {
            $self->_load_template_meta($content);
            foreach my $name (reverse @{ $self->split_paths($self->{'WRAPPER'}) }) {
                next if ! length $name;
                local $copy->{'content'} = $output;
                my $out = '';
                $self->_process($name, $copy, \$out);
                $output = $out;
            }
        }

        $output = $pre . $output if length $pre;

        ### handle post process items that go after every document
        if ($self->{'POST_PROCESS'}) {
            $self->_load_template_meta($content);
            foreach my $name (@{ $self->split_paths($self->{'POST_PROCESS'}) }) {
                $self->_process($name, $copy, \$output);
            }
        }

    };
    if (my $err = $@) {
        $err = $self->exception('undef', $err) if ref($err) !~ /Template::Exception$/;
        if ($err->type !~ /stop|return|next|last|break/) {
            $self->{'error'} = $err;
            return;
        }
    }



    ### clear blocks as asked (AUTO_RESET) defaults to on
    $self->{'BLOCKS'} = $blocks if exists($self->{'AUTO_RESET'}) && ! $self->{'AUTO_RESET'};

    ### send the content back out
    $out ||= $self->{'OUTPUT'};
    if (ref $out) {
        if (UNIVERSAL::isa($out, 'CODE')) {
            $out->($output);
        } elsif (UNIVERSAL::can($out, 'print')) {
            $out->print($output);
        } elsif (UNIVERSAL::isa($out, 'SCALAR')) { # reference to a string
            $$out = $output;
        } elsif (UNIVERSAL::isa($out, 'ARRAY')) {
            push @$out, $output;
        } else { # should be a file handle
            print $out $output;
        }
    } elsif ($out) { # should be a filename
        my $file;
        if ($out =~ m|^/|) {
            if (! $self->{'ABSOLUTE'}) {
                $self->{'error'} = $self->throw('file', "ABSOLUTE paths disabled");
            } else {
                $file = $out;
            }
        } elsif ($out =~ m|^\.\.?/|) {
            if (! $self->{'RELATIVE'}) {
                $self->{'error'} = $self->throw('file', "RELATIVE paths disabled");
            } else {
                $file = $out;
            }
        } else {
            if (! $self->{'OUTPUT_PATH'}) {
                $self->{'error'} = $self->throw('file', "OUTPUT_PATH not set");
            } else {
                $file = $self->{'OUTPUT_PATH'} . '/' . $out;
            }
        }
        if ($file) {
            if (open my $fh, '>', $file) {
                if (my $bm = $args->{'binmode'}) {
                    if (+$bm == 1) { binmode $fh }
                    else           { binmode $fh, $bm }
                }
                print $fh $output;
            } else {
                $self->{'error'} = $self->throw('file', "$out couldn't be opened for writing: $!");
            }
        }
    } else {
        print $output;
    }

    return if $self->{'error'};
    return 1;
}

sub error { shift->{'error'} }

sub DEBUG {
    my $self = shift;
    print STDERR "DEBUG: ", @_;
}

sub _load_template_meta {
    my $self = shift;
    return if $self->{'_template'}; # only do once as need

    eval {
        ### load the meta data for the top document
        ### this is needed by some of the custom handlers such as PRE_PROCESS and POST_PROCESS
        my $content = shift;
        my $doc     = $self->{'_template'} = $self->load_parsed_tree($content) || {};
        my $meta    = ($doc->{'_tree'} && ref($doc->{'_tree'}->[0]) && $doc->{'_tree'}->[0]->[0] eq 'META')
            ? $doc->{'_tree'}->[0]->[3] : {};

        $self->{'_template'} = $doc;
        @{ $doc }{keys %$meta} = values %$meta;
    };

    return;
}


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
    return $PACKAGE_EXCEPTION->new($type, $info, @_);
}

sub throw { die shift->exception(@_) }

sub context {
    my $self = shift;
    require CGI::Ex::Template::Extra;
    return CGI::Ex::Template::Context->new({_template => $self});
}

sub iterator {
    my $self = shift;
    $PACKAGE_ITERATOR->new(@_);
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
    return $SYNTAX->{$name} = $sub;
}

sub define_operator {
    my ($self, $name, $args) = @_;
    return $OPERATORS->{$name} = [@{ $args }{qw(type precedence symbols play_sub)}];
}

sub define_directive {
    my ($self, $name, $args) = @_;
    return $DIRECTIVES->{$name} = [@{ $args }{qw(parse_sub play_sub is_block is_postop continues no_interp)}];
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

sub dump_parse {
    my $obj = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift : __PACKAGE__->new;
    my $str = shift;
    require Data::Dumper;
    return Data::Dumper::Dumper($obj->parse_tree(\$str));
}

sub dump_parse_expr {
    my $obj = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift : __PACKAGE__->new;
    my $str = shift;
    require Data::Dumper;
    return Data::Dumper::Dumper($obj->parse_expr(\$str));
}

###----------------------------------------------------------------###
### support for few HTML::Template and HTML::Template::Expr calling syntax

sub register_function {
    my ($name, $sub) = @_;
    $SCALAR_OPS->{$name} = $sub;
}

sub param {
    require CGI::Ex::Template::HTE;
    &CGI::Ex::Template::HTE::param;
}

sub output {
    require CGI::Ex::Template::HTE;
    &CGI::Ex::Template::HTE::output;
}

sub clear_param { shift->{'param'} = {} }

sub query { shift->throw('query', "Not implemented in CGI::Ex::Template") }

sub new_file       { my $class = shift; my $in = shift; $class->new(source => $in, type => 'filename',   @_) }
sub new_scalar_ref { my $class = shift; my $in = shift; $class->new(source => $in, type => 'scalarref',  @_) }
sub new_array_ref  { my $class = shift; my $in = shift; $class->new(source => $in, type => 'arrayref',   @_) }
sub new_filehandle { my $class = shift; my $in = shift; $class->new(source => $in, type => 'filehandle', @_) }

###----------------------------------------------------------------###

package CGI::Ex::Template::Exception;

use overload
    '""' => \&as_string,
    bool => sub { defined shift },
    fallback => 1;

sub new {
    my ($class, $type, $info, $node, $pos, $doc) = @_;
    return bless [$type, $info, $node, $pos, $doc], $class;
}

sub type { shift->[0] }

sub info { shift->[1] }

sub node {
    my $self = shift;
    $self->[2] = shift if @_;
    $self->[2];
}

sub offset {
    my $self = shift;
    $self->[3] = shift if @_;
    $self->[3];
}

sub doc {
    my $self = shift;
    $self->[4] = shift if @_;
    $self->[4];
}

sub as_string {
    my $self = shift;
    if ($self->type =~ /^parse/) {
        if (my $doc = $self->doc) {
            my ($line, $char) = CGI::Ex::Template->get_line_number_by_index($doc, $self->offset, 'include_char');
            return $self->type ." error - $doc->{'name'} line $line char $char: ". $self->info;
        } else {
            return $self->type .' error - '. $self->info .' (At char '. $self->offset .')';
        }
    } else {
        return $self->type .' error - '. $self->info;
    }
}

###----------------------------------------------------------------###

package CGI::Ex::Template::Iterator;

sub new {
    my ($class, $items) = @_;
    $items = [] if ! defined $items;
    if (UNIVERSAL::isa($items, 'HASH')) {
	$items = [ map { {key => $_, value => $items->{ $_ }} } sort keys %$items ];
    } elsif (UNIVERSAL::can($items, 'as_list')) {
	$items = $items->as_list;
    } elsif (! UNIVERSAL::isa($items, 'ARRAY')) {
        $items = [$items];
    }
    return bless [$items, 0], $class;
}

sub get_first {
    my $self = shift;
    return (undef, 3) if ! @{ $self->[0] };
    return ($self->[0]->[$self->[1] = 0], undef);
}

sub get_next {
    my $self = shift;
    return (undef, 3) if ++ $self->[1] > $#{ $self->[0] };
    return ($self->items->[$self->[1]], undef);
}

sub items { shift->[0] }

sub index { shift->[1] }

sub max { $#{ shift->[0] } }

sub size { shift->max + 1 }

sub count { shift->index + 1 }

sub number { shift->index + 1 }

sub first { (shift->index == 0) || 0 }

sub last { my $self = shift; return ($self->index == $self->max) || 0 }

sub prev {
    my $self = shift;
    return undef if $self->index <= 0;
    return $self->items->[$self->index - 1];
}

sub next {
    my $self = shift;
    return undef if $self->index >= $self->max;
    return $self->items->[$self->index + 1];
}

###----------------------------------------------------------------###

1;

### See the perldoc in CGI/Ex/Template.pod
