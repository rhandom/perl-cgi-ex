package CGI::Ex::Template;

###----------------------------------------------------------------###
#  See the perldoc in CGI/Ex/Template.pod
#  Copyright 2006 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

use strict;
use constant trace => $ENV{'CET_TRACE'} || 0; # enable for low level tracing
use vars qw($VERSION
            $TAGS
            $SCALAR_OPS $HASH_OPS $LIST_OPS $FILTER_OPS
            $DIRECTIVES $QR_DIRECTIVE

            $OPERATORS
            $OP_UNARY
            $OP_BINARY
            $OP_TRINARY
            $OP_DISPATCH

            $QR_OP
            $QR_OP_UNARY
            $QR_OP_PARENED

            $QR_COMMENTS
            $QR_FILENAME
            $QR_AQ_NOTDOT
            $QR_AQ_SPACE
            $QR_PRIVATE

            $PACKAGE_EXCEPTION $PACKAGE_ITERATOR $PACKAGE_CONTEXT $PACKAGE_STASH $PACKAGE_PERL_HANDLE
            $WHILE_MAX
            $EXTRA_COMPILE_EXT
            $DEBUG
            );

BEGIN {
    $VERSION = '2.00';

    $PACKAGE_EXCEPTION   = 'CGI::Ex::Template::Exception';
    $PACKAGE_ITERATOR    = 'CGI::Ex::Template::Iterator';
    $PACKAGE_CONTEXT     = 'CGI::Ex::Template::_Context';
    $PACKAGE_STASH       = 'CGI::Ex::Template::_Stash';
    $PACKAGE_PERL_HANDLE = 'CGI::Ex::Template::EvalPerlHandle';

    $TAGS ||= {
        default  => ['[%',   '%]'],  # default
        template => ['[%',   '%]'],  # default
        metatext => ['%%',   '%%'],  # Text::MetaText
        star     => ['[*',   '*]'],  # TT alternate
        php      => ['<?',   '?>'],  # PHP
        asp      => ['<%',   '%>'],  # ASP
        mason    => ['<%',   '>' ],  # HTML::Mason
        html     => ['<!--', '-->'], # HTML comments
    };

    $SCALAR_OPS ||= {
        chunk    => \&vmethod_chunk,
        collapse => sub { local $_ = $_[0]; s/^\s+//; s/\s+$//; s/\s+/ /g; $_ },
        defined  => sub { 1 },
        indent   => \&vmethod_indent,
        'format' => \&vmethod_format,
        hash     => sub { {value => $_[0]} },
        html     => sub { local $_ = $_[0]; s/&/&amp;/g; s/</&lt;/g; s/>/&gt;/g; s/\"/&quot;/g; $_ },
        lcfirst  => sub { lcfirst $_[0] },
        length   => sub { defined($_[0]) ? length($_[0]) : 0 },
        lower    => sub { lc $_[0] },
        match    => \&vmethod_match,
        null     => sub { '' },
        remove   => sub { vmethod_replace(shift, shift, '', 1) },
        repeat   => \&vmethod_repeat,
        replace  => \&vmethod_replace,
        search   => sub { my ($str, $pat) = @_; return $str if ! defined $str || ! defined $pat; return $str =~ /$pat/ },
        size     => sub { 1 },
        split    => \&vmethod_split,
        stderr   => sub { print STDERR $_[0]; '' },
        substr   => sub { my ($str, $i, $len) = @_; defined($len) ? substr($str, $i, $len) : substr($str, $i) },
        trim     => sub { local $_ = $_[0]; s/^\s+//; s/\s+$//; $_ },
        ucfirst  => sub { ucfirst $_[0] },
        upper    => sub { uc $_[0] },
        uri      => sub { local $_ = $_[0]; s/([^;\/?:@&=+\$,A-Za-z0-9\-_.!~*\'()])/sprintf('%%%02X', ord($1))/eg; $_ },
    };

    $FILTER_OPS ||= { # generally - non-dynamic filters belong in scalar ops
        eval     => [\&filter_eval, 1],
        evaltt   => [\&filter_eval, 1],
        file     => [\&filter_redirect, 1],
        redirect => [\&filter_redirect, 1],
    };

    $LIST_OPS ||= {
        first   => sub { my ($ref, $i) = @_; return $ref->[0] if ! $i; return [@{$ref}[0 .. $i - 1]]},
        grep    => sub { my ($ref, $pat) = @_; [grep {/$pat/} @$ref] },
        hash    => sub { my ($list, $i) = @_; defined($i) ? {map {$i++ => $_} @$list} : {@$list} },
        join    => sub { my ($ref, $join) = @_; $join = ' ' if ! defined $join; local $^W; return join $join, @$ref },
        last    => sub { my ($ref, $i) = @_; return $ref->[-1] if ! $i; return [@{$ref}[-$i .. -1]]},
        list    => sub { $_[0] },
        max     => sub { $#{ $_[0] } },
        merge   => sub { my $ref = shift; return [ @$ref, grep {defined} map {ref eq 'ARRAY' ? @$_ : undef} @_ ] },
        nsort   => \&vmethod_nsort,
        pop     => sub { pop @{ $_[0] } },
        push    => sub { my $ref = shift; push @$ref, @_; return '' },
        reverse => sub { [ reverse @{ $_[0] } ] },
        shift   => sub { shift  @{ $_[0] } },
        size    => sub { scalar @{ $_[0] } },
        slice   => sub { my ($ref, $a, $b) = @_; $a ||= 0; $b = $#$ref if ! defined $b; return [@{$ref}[$a .. $b]] },
        sort    => \&vmethod_sort,
        splice  => \&vmethod_splice,
        unique  => sub { my %u; return [ grep { ! $u{$_} ++ } @{ $_[0] } ] },
        unshift => sub { my $ref = shift; unshift @$ref, @_; return '' },
    };

    $HASH_OPS ||= {
        defined => sub { return '' if ! defined $_[1]; defined $_[0]->{ $_[1] } },
        delete  => sub { return '' if ! defined $_[1]; delete  $_[0]->{ $_[1] } },
        each    => sub { [%{ $_[0] }] },
        exists  => sub { return '' if ! defined $_[1]; exists $_[0]->{ $_[1] } },
        hash    => sub { $_[0] },
        import  => sub { my ($a, $b) = @_; return '' if ref($b) ne 'HASH'; @{$a}{keys %$b} = values %$b; '' },
        keys    => sub { [keys %{ $_[0] }] },
        list    => sub { [$_[0]] },
        pairs   => sub { [map { {key => $_, value => $_[0]->{$_}} } keys %{ $_[0] } ] },
        nsort   => sub { my $ref = shift; [sort {$ref->{$a}    <=> $ref->{$b}   } keys %$ref] },
        size    => sub { scalar keys %{ $_[0] } },
        sort    => sub { my $ref = shift; [sort {lc $ref->{$a} cmp lc $ref->{$b}} keys %$ref] },
        values  => sub { [values %{ $_[0] }] },
    };

    $DIRECTIVES = {
        #name       #parse_sub       #play_sub        #block   #postdir #continue #move_to_front
        BLOCK   => [\&parse_BLOCK,   \&play_BLOCK,    1,       0,       0,        1],
        BREAK   => [sub {},          \&play_control],
        CALL    => [\&parse_CALL,    \&play_CALL],
        CASE    => [\&parse_CASE,    undef,           0,       0,       {SWITCH => 1, CASE => 1}],
        CATCH   => [\&parse_CATCH,   undef,           0,       0,       {TRY => 1, CATCH => 1}],
        CLEAR   => [sub {},          \&play_CLEAR],
        '#'     => [sub {},          sub {}],
        DEBUG   => [\&parse_DEBUG,   \&play_DEBUG],
        DEFAULT => [\&parse_DEFAULT, \&play_DEFAULT],
        DUMP    => [\&parse_DUMP,    \&play_DUMP],
        ELSE    => [sub {},          undef,           0,       0,       {IF => 1, ELSIF => 1, UNLESS => 1}],
        ELSIF   => [\&parse_IF,      undef,           0,       0,       {IF => 1, ELSIF => 1, UNLESS => 1}],
        END     => [undef,           sub {}],
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
        MACRO   => [\&parse_MACRO,   \&play_MACRO],
        META    => [undef,           sub {}],
        METADEF => [undef,           \&play_METADEF],
        NEXT    => [sub {},          \&play_control],
        PERL    => [\&parse_PERL,    \&play_PERL,     1],
        PROCESS => [\&parse_PROCESS, \&play_PROCESS],
        RAWPERL => [\&parse_PERL,    \&play_RAWPERL,  1],
        RETURN  => [sub {},          \&play_control],
        SET     => [\&parse_SET,     \&play_SET],
        STOP    => [sub {},          \&play_control],
        SWITCH  => [\&parse_SWITCH,  \&play_SWITCH,   1],
        TAGS    => [undef,           sub {}],
        THROW   => [\&parse_THROW,   \&play_THROW],
        TRY     => [sub {},          \&play_TRY,      1],
        UNLESS  => [\&parse_UNLESS,  \&play_UNLESS,   1,       1],
        USE     => [\&parse_USE,     \&play_USE],
        WHILE   => [\&parse_IF,      \&play_WHILE,    1,       1],
        WRAPPER => [\&parse_WRAPPER, \&play_WRAPPER,  1,       1],
        #name       #parse_sub       #play_sub        #block   #postdir #continue #move_to_front
    };
    $QR_DIRECTIVE = qr{ ^ (\w+|\|) (?= $|[\s;\#]) }x;

    ### setup the operator parsing
    $OPERATORS ||= [
        # name => # order, precedence, symbols, only_in_parens, sub to create
        [2, 96, ['**', '^', 'pow'],  0, sub {     $_[0] ** $_[1]                  } ],
        [1, 93, ['!'],               0, sub {   ! $_[0]                           } ],
        [1, 93, ['-'],               0, sub { @_ == 1 ? 0 - $_[0] : $_[0] - $_[1] } ],
        [2, 90, ['*'],               0, sub {     $_[0] *  $_[1]                  } ],
        [2, 90, ['/'],               0, sub {     $_[0] /  $_[1]                  } ],
        [2, 90, ['div', 'DIV'],      0, sub { int($_[0] /  $_[1])                 } ],
        [2, 90, ['%', 'mod', 'MOD'], 0, sub {     $_[0] %  $_[1]                  } ],
        [2, 85, ['+'],               0, sub {     $_[0] +  $_[1]                  } ],
        [2, 85, ['-'],               0, sub { @_ == 1 ? 0 - $_[0] : $_[0] - $_[1] } ],
        [2, 85, ['_', '~'],          0, sub { join "", @_                         } ],
        [2, 80, ['<'],               0, sub {     $_[0] <  $_[1]                  } ],
        [2, 80, ['>'],               0, sub {     $_[0] >  $_[1]                  } ],
        [2, 80, ['<='],              0, sub {     $_[0] <= $_[1]                  } ],
        [2, 80, ['>='],              0, sub {     $_[0] >= $_[1]                  } ],
        [2, 80, ['lt'],              0, sub {     $_[0] lt $_[1]                  } ],
        [2, 80, ['gt'],              0, sub {     $_[0] gt $_[1]                  } ],
        [2, 80, ['le'],              0, sub {     $_[0] le $_[1]                  } ],
        [2, 80, ['ge'],              0, sub {     $_[0] ge $_[1]                  } ],
        [2, 75, ['==', 'eq'],        0, sub {     $_[0] eq $_[1]                  } ],
        [2, 75, ['!=', 'ne'],        0, sub {     $_[0] ne $_[1]                  } ],
        [2, 70, ['&&'],              0, undef                                       ],
        [2, 65, ['||'],              0, undef                                       ],
        [2, 60, ['..'],              0, sub {     $_[0] .. $_[1]                  } ],
        [3, 55, ['?', ':'],          0, undef                                       ],
        [2, 52, ['='],               1, undef                                       ],
        [1, 50, ['not', 'NOT'],      0, sub {   ! $_[0]                           } ],
        [2, 45, ['and', 'AND'],      0, undef                                       ],
        [2, 40, ['or', 'OR'],        0, undef                                       ],
        [0,  0, ['hash'],            0, sub { return {@_};                        } ],
        [0,  0, ['array'],           0, sub { return [@_]                         } ],
    ];
    $OP_DISPATCH ||= {map {my $ref = $_; map {$_ => $ref->[4]} @{$ref->[2]}}                @$OPERATORS};
    $OP_UNARY    ||= {map {my $ref = $_; map {$_ => $ref} @{$ref->[2]}} grep {$_->[0] == 1} @$OPERATORS};
    $OP_BINARY   ||= {map {my $ref = $_; map {$_ => $ref} @{$ref->[2]}} grep {$_->[0] == 2} @$OPERATORS};
    $OP_TRINARY  ||= {map {my $ref = $_; map {$_ => $ref} @{$ref->[2]}} grep {$_->[0] == 3} @$OPERATORS};
    sub _op_qr { # no mixed \w\W operators
        my %used;
        my $chrs = join '|', map {quotemeta $_} grep {++$used{$_} < 2} grep {/^\W{2,}$/} @_;
        my $chr  = join '',  map {quotemeta $_} grep {++$used{$_} < 2} grep {/^\W$/}     @_;
        my $word = join '|',                    grep {++$used{$_} < 2} grep {/^\w+$/}    @_;
        $chr = "[$chr]" if $chr;
        $word = "\\b(?:$word)\\b" if $word;
        return join('|', grep {length} $chrs, $chr, $word) || die "Missing operator regex";
    }
    sub _build_op_qr       { _op_qr(sort map {@{ $_->[2] }} grep {$_->[0] > 1 && ! $_->[3]} @$OPERATORS) } # all binary, trinary, non-parened ops
    sub _build_op_qr_unary { _op_qr(sort map {@{ $_->[2] }} grep {$_->[0] == 1            } @$OPERATORS) } # unary operators
    sub _build_op_qr_paren { _op_qr(sort map {@{ $_->[2] }} grep {                 $_->[3]} @$OPERATORS) } # paren
    $QR_OP         ||= _build_op_qr();
    $QR_OP_UNARY   ||= _build_op_qr_unary();
    $QR_OP_PARENED ||= _build_op_qr_paren();


    $QR_COMMENTS  = '(?-s: \# .* \s*)*';
    $QR_FILENAME  = '([a-zA-Z]]:/|/)? [\w\-\.]+ (?:/[\w\-\.]+)*';
    $QR_AQ_NOTDOT = "(?! \\s* $QR_COMMENTS \\.)";
    $QR_AQ_SPACE  = '(?: \\s+ | \$ | (?=[;+]) )'; # the + comes into play on filenames
    $QR_PRIVATE   ||= qr/^_/;

    $WHILE_MAX    ||= 1000;
    $EXTRA_COMPILE_EXT = '.sto';
};

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $args  = ref($_[0]) ? { %{ shift() } } : {@_};
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
        ### load the document
        $doc = $self->load_parsed_tree($file) || $self->throw('undef', "Zero length content");;

        ### prevent recursion
        $self->throw('file', "recursion into '$doc->{name}'")
            if ! $self->{'RECURSION'} && $self->{'_in'}->{$doc->{'name'}} && $doc->{'name'} ne 'input text';
        local $self->{'_in'}->{$doc->{'name'}} = 1;

        ### execute the document
        if (! @{ $doc->{'_tree'} }) { # no tags found - just return the content
            $$out_ref = ${ $doc->{'_content'} };
        } else {
            local $self->{'_vars'}->{'component'} = $doc;
            $self->{'_vars'}->{'template'}  = $doc if $self->{'_top_level'};
            $self->execute_tree($doc->{'_tree'}, $out_ref);
            delete $self->{'_vars'}->{'template'} if $self->{'_top_level'};
        }
    };

    ### trim whitespace from the beginning and the end of a block or template
    if ($self->{'TRIM'}) {
        substr($$out_ref, $i, length($$out_ref) - $i) =~ s{ \s+ $ }{}x; # tail first
        substr($$out_ref, $i, length($$out_ref) - $i) =~ s{ ^ \s+ }{}x;
    }

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

    ### looks like a string reference
    if (ref $file) {
        $doc->{'_content'}   = $file;
        $doc->{'name'}       = 'input text';
        $doc->{'is_str_ref'} = 1;

    ### looks like a previously cached-in-memory document
    } elsif ($self->{'_documents'}->{$file}
             && (   ($self->{'_documents'}->{$file}->{'_cache_time'} == time) # don't stat more than once a second
                 || ($self->{'_documents'}->{$file}->{'modtime'}
                     == (stat $self->{'_documents'}->{$file}->{'_filename'})[9]))) {
        $doc = $self->{'_documents'}->{$file};
        $doc->{'_cache_time'} = time;
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
                        next if $node->[0] eq 'METADEF';
                        last if $node->[0] ne 'BLOCK';
                        next if $block_name ne $node->[3];
                        $doc->{'_content'} = $ref->{'_content'};
                        $doc->{'_tree'}    = $node->[4];
                        $doc->{'modtime'} = $ref->{'modtime'};
                        $file = $ref->{'name'};
                        last;
                    }
                }
                die $err if ! $doc->{'_tree'};
            } elsif ($self->{'DEFAULT'}) {
                $doc->{'_filename'} = eval { $self->include_filename($self->{'DEFAULT'}) } || die $err;
            } else {
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

        local $self->{'_vars'}->{'component'} = $doc;
        $doc->{'_tree'} = $self->parse_tree($doc->{'_content'}); # errors die
    }

    ### cache parsed_tree in memory unless asked not to do so
    if (! $doc->{'is_str_ref'} && (! defined($self->{'CACHE_SIZE'}) || $self->{'CACHE_SIZE'})) {
        $self->{'_documents'}->{$file} ||= $doc;
        $doc->{'_cache_time'} = time;

        ### allow for config option to keep the cache size down
        if ($self->{'CACHE_SIZE'}) {
            my $all = $self->{'_documents'};
            if (scalar(keys %$all) > $self->{'CACHE_SIZE'}) {
                my $n = 0;
                foreach my $file (sort {$all->{$b}->{'_cache_time'} <=> $all->{$a}->{'_cache_time'}} keys %$all) {
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

sub parse_tree {
    my $self    = shift;
    my $str_ref = shift;
    if (! $str_ref || ! defined $$str_ref) {
        $self->throw('parse.no_string', "No string or undefined during parse");
    }

    my $STYLE = $self->{'TAG_STYLE'} || 'default';
    my $START = $self->{'START_TAG'} || $TAGS->{$STYLE}->[0];
    my $END   = $self->{'END_TAG'}   || $TAGS->{$STYLE}->[1];
    my $len_s = length $START;
    my $len_e = length $END;

    my @tree;             # the parsed tree
    my $pointer = \@tree; # pointer to current tree to handle nested blocks
    my @state;            # maintain block levels
    local $self->{'_state'} = \@state; # allow for items to introspect (usually BLOCKS)
    local $self->{'_in_perl'};         # no interpolation in perl
    my @move_to_front;    # items that need to be declared first (usually BLOCKS)
    my @meta;             # place to store any found meta information (to go into METADEF)
    my $i = 0;            # start index
    my $j = 0;            # end index
    my $last = 0;         # previous end index
    my $post_chomp = 0;   # previous post_chomp setting
    my $continue;         # multiple directives in the same tag
    my $post_op;          # found a post-operative DIRECTIVE
    my $capture;          # flag to start capture
    my $func;
    my $node;
    my $tag;
    while (1) {
        ### continue looking for information in a semi-colon delimited tag
        if ($continue) {
            $i = $continue;
            $node = [undef, $i, $j];

        ### look through the string using index
        } else {
            $i = index($$str_ref, $START, $last);
            last if $i == -1; # no start tag found - we are done
            if ($last != $i) { # found a text portion - chomp it, interpolate it and store it
                my $text  = substr($$str_ref, $last, $i - $last);
                my $_last = $last;
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
            $j = index($$str_ref, $END, $i + $len_s);
            $last = $j + $len_e;
            if ($j == -1) { # missing closing tag
                $last = length($$str_ref);
                last;
            }
            $tag = substr($$str_ref, $i + $len_s, $j - ($i + $len_s));
            $node = [undef, $i + $len_s, $j];

            ### take care of whitespace and comments flags
            my $pre_chomp = $tag =~ s{ ^ ([+=~-]) }{}x ? $1 : $self->{'PRE_CHOMP'};
            $post_chomp   = $tag =~ s{ ([+=~-]) $ }{}x ? $1 : $self->{'POST_CHOMP'};
            $pre_chomp  =~ y/-=~+/1230/ if $pre_chomp;
            $post_chomp =~ y/-=~+/1230/ if $post_chomp;
            if ($pre_chomp && $pointer->[-1] && ! ref $pointer->[-1]) {
                if    ($pre_chomp == 1) { $pointer->[-1] =~ s{ (?:\n|^) [^\S\n]* \z }{}x  }
                elsif ($pre_chomp == 2) { $pointer->[-1] =~ s{             (\s+) \z }{ }x }
                elsif ($pre_chomp == 3) { $pointer->[-1] =~ s{             (\s+) \z }{}x  }
                splice(@$pointer, -1, 1, ()) if ! length $pointer->[-1]; # remove the node if it is zero length
            }
            if ($tag =~ /^\#/) { # leading # means to comment the entire section
                $node->[0] = '#';
                push @$pointer, $node;
                next;
            }
            $tag =~ s{ ^ \s+ $QR_COMMENTS }{}ox;
        }

        if (! length $tag) {
            undef $continue;
            undef $post_op;
            next;
        }

        ### look for DIRECTIVES
        if ($tag =~ $QR_DIRECTIVE     # find a word
            && $DIRECTIVES->{$1} ) {  # is it a directive
            $node->[0] = $func = $1;
            $tag =~ s{ ^ (\w+ | \|) \s* $QR_COMMENTS }{}ox;

            ### store out this current node level
            if ($post_op) { # on a post operator - replace the original node with the new one - store the old in the new
                my @post_op = @$post_op;
                @$post_op = @$node;
                $node = $post_op;
                $node->[4] = [\@post_op];
            } elsif ($capture) {
                # do nothing - it will be handled further down
            } else{
                push @$pointer, $node;
            }

            ### anything that behaves as a block ending
            if ($func eq 'END' || $DIRECTIVES->{$func}->[4]) { # [4] means it is a continuation block (ELSE, CATCH, etc)
                if (! @state) {
                    $self->throw('parse', "Found an $func tag while not in a block", $node);
                }
                my $parent_node = pop @state;

                if ($func ne 'END') {
                    pop @$pointer; # we will store the node in the parent instead
                    $parent_node->[5] = $node;
                    my $parent_type = $parent_node->[0];
                    if (! $DIRECTIVES->{$func}->[4]->{$parent_type}) {
                        $self->throw('parse', "Found unmatched nested block", $node, 0);
                    }
                }

                ### restore the pointer up one level (because we hit the end of a block)
                $pointer = (! @state) ? \@tree : $state[-1]->[4];

                ### normal end block
                if ($func eq 'END') {
                    if ($DIRECTIVES->{$parent_node->[0]}->[5]) { # move things like BLOCKS to front
                        push @move_to_front, $parent_node;
                        if ($pointer->[-1] && ! $pointer->[-1]->[6]) { # capturing doesn't remove the var
                            splice(@$pointer, -1, 1, ());
                        }
                    } elsif ($parent_node->[0] =~ /PERL$/) {
                        delete $self->{'_in_perl'};
                    }

                ### continuation block - such as an elsif
                } else {
                    $node->[3] = eval { $DIRECTIVES->{$func}->[0]->($self, \$tag, $node) };
                    if (my $err = $@) {
                        $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                        die $err;
                    }
                    push @state, $node;
                    $pointer = $node->[4] ||= [];
                }

            } elsif ($func eq 'TAGS') {
                if ($tag =~ / ^ (\w+) /x && $TAGS->{$1}) {
                    $tag =~ s{ ^ (\w+) \s* $QR_COMMENTS }{}ox;
                    ($START, $END) = @{ $TAGS->{$1} };
                } elsif ($tag =~ s{ ^ (\S+) \s+ (\S+) \s* $QR_COMMENTS }{}ox) {
                    ($START, $END) = ($1, $2);
                }
                $len_s = length $START;
                $len_e = length $END;

            } elsif ($func eq 'META') {
                my $args = $self->parse_args(\$tag);
                my $hash;
                if (($hash = $self->vivify_args($args)->[-1])
                    && UNIVERSAL::isa($hash, 'HASH')) {
                    unshift @meta, %$hash; # first defined win
                }

            ### all other "normal" tags
            } else {
                $node->[3] = eval { $DIRECTIVES->{$func}->[0]->($self, \$tag, $node) };
                if (my $err = $@) {
                    $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                    die $err;
                }
                if ($DIRECTIVES->{$func}->[2] && ! $post_op) { # this looks like a block directive
                    push @state, $node;
                    $pointer = $node->[4] ||= [];
                }
            }

        ### allow for bare variable getting and setting
        } elsif (defined(my $var = $self->parse_variable(\$tag))) {
            push @$pointer, $node;
            if ($tag =~ s{ ^ = >? \s* $QR_COMMENTS }{}ox) {
                $node->[0] = 'SET';
                $node->[3] = eval { $DIRECTIVES->{'SET'}->[0]->($self, \$tag, $node, $var) };
                if (my $err = $@) {
                    $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                    die $err;
                }
            } else {
                $node->[0] = 'GET';
                $node->[3] = $var;
            }

        } else { # error
            my $all  = substr($$str_ref, $i + $len_s, $j - ($i + $len_s));
            $all =~ s/^\s+//;
            $all =~ s/\s+$//;
            $self->throw('parse', "Not sure how to handle tag \"$all\"", $node);
        }

        ### we now have the directive to capture for an item like "SET foo = BLOCK" - store it
        if ($capture) {
            my $parent_node = $capture;
            push @{ $parent_node->[4] }, $node;
            undef $capture;
        }

        ### we are flagged to start capturing the output of the next directive - set it up
        if ($node->[6]) {
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $post_op   = undef;
            $capture   = $node;

        ### semi-colon = end of statement - we will need to continue parsing this tag
        } elsif ($tag =~ s{ ^ ; \s* $QR_COMMENTS }{}ox) {
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $post_op   = undef;

        ### looking at a post operator ([% u FOREACH u IN [1..3] %])
        } elsif ($tag =~ $QR_DIRECTIVE         # find a word
                 && $DIRECTIVES->{$1}          # is it a directive
                 && $DIRECTIVES->{$1}->[3]) {  # it is a post operative directive
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $post_op   = $node;

        } else { # error
            $self->throw('parse', "Found trailing info \"$tag\"", $node) if length $tag;
            $continue = undef;
            $post_op  = undef;
        }
    }

    if (@move_to_front) {
        unshift @tree, @move_to_front;
    }
    if (@meta) {
        unshift @tree, ['METADEF', 0, 0, {@meta}];
    }

    if ($#state > -1) {
        $self->throw('parse.missing.end', "Missing END", $state[-1], 0);
    }

    ### pull off the last text portion - if any
    if ($last != length($$str_ref)) {
        my $text  = substr($$str_ref, $last, length($$str_ref) - $last);
        my $_last = $last;
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
            warn "NODE: TEXT\n" if trace;
            $$out_ref .= $node if defined $node;
            next;
        }

        warn "NODE: $node->[0] (char $node->[1])\n" if trace;
        $$out_ref .= $self->debug_node($node) if $self->{'_debug_dirs'} && ! $self->{'_debug_off'};

        my $val = $DIRECTIVES->{$node->[0]}->[1]->($self, $node->[3], $node, $out_ref);
        $$out_ref .= $val if defined $val;
    }
}

###----------------------------------------------------------------###

sub parse_variable {
    my $self    = shift;
    my $str_ref = shift;
    my $ARGS    = shift || {};

    ### allow for custom auto_quoting (such as hash constructors)
    if ($ARGS->{'auto_quote'}) {
        if ($$str_ref =~ $ARGS->{'auto_quote'}) {
            my $str = $1;
            substr($$str_ref, 0, length($str), '');
            $$str_ref =~ s{ ^ \s* $QR_COMMENTS }{}ox;
            return $str;
        ### allow for auto-quoted $foo or ${foo.bar} type constructs
        } elsif ($$str_ref =~ s{ ^ \$ (\w+ (?:\.\w+)*) \b \s* $QR_COMMENTS }{}ox
                 || $$str_ref =~ s{ ^ \$\{ \s* ([^\}]+) \} \s* $QR_COMMENTS }{}ox) {
            my $name = $1;
            return $self->parse_variable(\$name);
        }
    }

    my $copy = $$str_ref; # copy while parsing to allow for errors

    ### test for leading unary operators
    my $has_unary;
    if ($copy =~ s{ ^ ($QR_OP_UNARY) \s* $QR_COMMENTS }{}ox) {
        return if $ARGS->{'auto_quote'}; # auto_quoted thing was too complicated
        $has_unary = $1;
    }

    my @var;
    my $is_literal;
    my $is_namespace;

    ### allow for numbers
    if ($copy =~ s{ ^ ( (?:\d*\.\d+ | \d+) ) \s* $QR_COMMENTS }{}ox) {
        my $number = $1;
        push @var, \ $number;
        $is_literal = 1;

    ### looks like a normal variable start
    } elsif ($copy =~ s{ ^ (\w+) \s* $QR_COMMENTS }{}ox) {
        push @var, $1;
        $is_namespace = 1 if $self->{'NAMESPACE'} && $self->{'NAMESPACE'}->{$1};

    ### allow for literal strings
    } elsif ($copy =~ s{ ^ ([\"\']) (|.*?[^\\]) \1 \s* $QR_COMMENTS }{}sox) {
        if ($1 eq "'") { # no interpolation on single quoted strings
            my $str = $2;
            $str =~ s{ \\\' }{\'}xg;
            push @var, \ $str;
            $is_literal = 1;
        } else {
            my $str = $2;
            $str =~ s/\\n/\n/g;
            $str =~ s/\\t/\t/g;
            $str =~ s/\\r/\r/g;
            $str =~ s/\\([\"\$])/$1/g;
            my @pieces = $ARGS->{'auto_quote'}
                ? split(m{ (\$\w+            | \$\{ [^\}]+ \}) }x, $str)  # autoquoted items get a single $\w+ - no nesting
                : split(m{ (\$\w+ (?:\.\w+)* | \$\{ [^\}]+ \}) }x, $str);
            my $n = 0;
            foreach my $piece (@pieces) {
                next if ! ($n++ % 2);
                next if $piece !~ m{ ^ \$ (\w+ (?:\.\w+)*) $ }x
                    && $piece !~ m{ ^ \$\{ \s* ([^\}]+) \} $ }x;
                my $name = $1;
                $piece = $self->parse_variable(\$name);
            }
            @pieces = grep {defined && length} @pieces;
            if (@pieces == 1 && ! ref $pieces[0]) {
                push @var, \ $pieces[0];
                $is_literal = 1;
            } elsif (! @pieces) {
                push @var, \ '';
                $is_literal = 1;
            } else {
                push @var, \ ['~', @pieces];
            }
        }
        if ($ARGS->{'auto_quote'}){
            $$str_ref = $copy;
            return ${ $var[0] } if $is_literal;
            push @var, 0;
            return \@var;
        }

    ### allow for leading $foo or ${foo.bar} type constructs
    } elsif ($copy =~ s{ ^ \$ (\w+) \b \s* $QR_COMMENTS }{}ox
        || $copy =~ s{ ^ \$\{ \s* ([^\}]+) \} \s* $QR_COMMENTS }{}ox) {
        my $name = $1;
        push @var, $self->parse_variable(\$name);

    ### looks like an array constructor
    } elsif ($copy =~ s{ ^ \[ \s* $QR_COMMENTS }{}ox) {
        local $self->{'_operator_precedence'} = 0; # reset presedence
        my $arrayref = ['array'];
        while (defined(my $var = $self->parse_variable(\$copy))) {
            push @$arrayref, $var;
            $copy =~ s{ ^ , \s* $QR_COMMENTS }{}ox;
        }
        $copy =~ s{ ^ \] \s* $QR_COMMENTS }{}ox
            || $self->throw('parse.missing.square_bracket', "Missing close \]", undef, length($$str_ref) - length($copy));
        push @var, \ $arrayref;

    ### looks like a hash constructor
    } elsif ($copy =~ s{ ^ \{ \s* $QR_COMMENTS }{}ox) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $hashref = ['hash'];
        while (defined(my $key = $self->parse_variable(\$copy, {auto_quote => qr{ ^ (\w+) $QR_AQ_NOTDOT }xo}))) {
            $copy =~ s{ ^ = >? \s* $QR_COMMENTS }{}ox;
            my $val = $self->parse_variable(\$copy);
            push @$hashref, $key, $val;
            $copy =~ s{ ^ , \s* $QR_COMMENTS }{}ox;
        }
        $copy =~ s{ ^ \} \s* $QR_COMMENTS }{}ox
            || $self->throw('parse.missing.curly_bracket', "Missing close \} ($copy)", undef, length($$str_ref) - length($copy));
        push @var, \ $hashref;

    ### looks like a paren grouper
    } elsif ($copy =~ s{ ^ \( \s* $QR_COMMENTS }{}ox) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $var = $self->parse_variable(\$copy, {allow_parened_ops => 1});
        $copy =~ s{ ^ \) \s* $QR_COMMENTS }{}ox
            || $self->throw('parse.missing.paren', "Missing close \)", undef, length($$str_ref) - length($copy));
        @var = @$var;
        pop(@var); # pull off the trailing args of the paren group

    ### nothing to find - return failure
    } else {
        return;
    }

    return if $ARGS->{'auto_quote'}; # auto_quoted thing was too complicated

    ### looks for args for the initial
    if ($copy =~ s{ ^ \( \s* $QR_COMMENTS }{}ox) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $args = $self->parse_args(\$copy);
        $copy =~ s{ ^ \) \s* $QR_COMMENTS }{}ox
            || $self->throw('parse.missing.paren', "Missing close \)", undef, length($$str_ref) - length($copy));
        push @var, $args;
    } else {
        push @var, 0;
    }

    ### allow for nested items
    while ($copy =~ s{ ^ ( \.(?!\.) | \|(?!\|) ) \s* $QR_COMMENTS }{}ox) {
        push(@var, $1) if ! $ARGS->{'no_dots'};

        ### allow for interpolated variables in the middle - one.$foo.two or one.${foo.bar}.two
        if ($copy =~ s{ ^ \$(\w+) \s* $QR_COMMENTS }{}ox
            || $copy =~ s{ ^ \$\{ \s* ([^\}]+)\} \s* $QR_COMMENTS }{}ox) {
            my $name = $1;
            my $var = $self->parse_variable(\$name);
            push @var, $var;
        } elsif ($copy =~ s{ ^ (\w+) \s* $QR_COMMENTS }{}ox) {
            push @var, $1;
        } else {
            $self->throw('parse', "Not sure how to continue parsing on \"$copy\" ($$str_ref)");
        }

        ### looks for args for the nested item
        if ($copy =~ s{ ^ \( \s* $QR_COMMENTS }{}ox) {
            local $self->{'_operator_precedence'} = 0; # reset precedence
            my $args = $self->parse_args(\$copy);
            $copy =~ s{ ^ \) \s* $QR_COMMENTS }{}ox
                || $self->throw('parse.missing.paren', "Missing close \)", undef, length($$str_ref) - length($copy));
            push @var, $args;
        } else {
            push @var, 0;
        }

    }

    ### flatten literals and constants as much as possible
    my $var = ($is_literal && $#var == 1) ? ${ $var[0] }
            : $is_namespace               ? $self->get_variable(\@var, {is_namespace_during_compile => 1})
            :                               \@var;

    ### allow for all "operators"
    if (! $self->{'_operator_precedence'}) {
        my $tree;
        my $found;
        while ($copy =~ s{ ^ ($QR_OP) \s* $QR_COMMENTS }{}ox ## look for operators - then move along
               || ($ARGS->{'allow_parened_ops'}
                   && $copy =~ s{ ^ ($QR_OP_PARENED) \s* $QR_COMMENTS }{}ox) ) {
            local $self->{'_operator_precedence'} = 1;
            my $op   = $1;
            my $var2 = $self->parse_variable(\$copy);

            ### allow for unary operator precedence
            if ($has_unary && (($OP_BINARY->{$op} || $OP_TRINARY->{$op})->[1] < $OP_UNARY->{$has_unary}->[1])) {
                if ($tree) {
                    if ($#$tree == 1) { # only one operator - keep simple things fast
                        $var = [\ [$tree->[0], $var, $tree->[1]], 0];
                    } else {
                        unshift @$tree, $var;
                        $var = $self->apply_precedence($tree, $found);
                    }
                    undef $tree;
                    undef $found;
                }
                $var = [ \ [ $has_unary, $var ], 0 ];
                undef $has_unary;
            }

            ### add the operator to the tree
            push (@{ $tree ||= [] }, $op, $var2);
            my $ref = $OP_BINARY->{$op} || $OP_TRINARY->{$op};
            $found->{$op} = $ref->[1];
        }

        ### if we found operators - tree the nodes by operator precedence
        if ($tree) {
            if (@$tree == 2) { # only one operator - keep simple things fast
                $var = [\ [$tree->[0], $var, $tree->[1]], 0];
            } else {
                unshift @$tree, $var;
                $var = $self->apply_precedence($tree, $found);
            }
        }
    }

    ### allow for unary on non-chained variables
    if ($has_unary) {
        $var = [ \ [ $has_unary, $var ], 0 ];
    }

    $$str_ref = $copy; # commit the changes
    return $var;
}

### this is used to put the parsed variables into the correct operations tree
sub apply_precedence {
    my ($self, $tree, $found) = @_;

    my @var;
    my $trees;
    ### look at the operators we found in the order we found them
    for my $op (sort {$found->{$a} <=> $found->{$b}} keys %$found) {
        local $found->{$op};
        delete $found->{$op};
        my @trees;
        my @trinary;

        ### split the array on the current operator
        for (my $i = 0; $i <= $#$tree; $i ++) {
            my $is_trinary = $OP_TRINARY->{$op} && grep {$_ eq $tree->[$i]} @{ $OP_TRINARY->{$op}->[2] };
            next if $tree->[$i] ne $op && ! $is_trinary;
            push @trees, [splice @$tree, 0, $i, ()]; # everything up to the operator
            push @trinary, $tree->[0] if $is_trinary;
            shift @$tree; # pull off the operator
            $i = -1;
        }
        next if ! @trees; # this iteration didn't have the current operator
        push @trees, $tree if scalar @$tree; # elements after last operator

        ### now - for this level split on remaining operators, or add the variable to the tree
        for my $node (@trees) {
            if (@$node == 1) {
                $node = $node->[0]; # single item - its not a tree
            } elsif (@$node == 3) {
                $node = [ \ [ $node->[1], $node->[0], $node->[2] ], 0 ]; # single operator - put it straight on
            } else {
                $node = $self->apply_precedence($node, $found); # more complicated - recurse
            }
        }

        ### return binary
        if ($OP_BINARY->{$op}) {
            my $val = $trees[-1];
            $val = [ \ [ $op, $trees[$_], $val ], 0 ] for reverse (0 .. $#trees - 1); # reverse order - helps out ||
            return $val;
        }

        ### return simple trinary
        if (@trinary == 2) {
            return [ \ [ $op, @trees ], 0 ];
        }

        ### reorder complex trinary - rare case
        while ($#trinary >= 1) {
            ### if we look starting from the back - the first lead trinary op will always be next to its matching op
            for (my $i = $#trinary; $i >= 0; $i --) {
                next if $OP_TRINARY->{$trinary[$i]}->[2]->[1] eq $trinary[$i];
                my ($op, $op2) = splice @trinary, $i, 2, (); # remove the pair of operators
                my $node = [ \ [$op, @trees[$i .. $i + 2] ], 0 ];
                splice @trees, $i, 3, $node;
            }
        }
        return $trees[0]; # at this point the trinary has been reduced to a single operator

    }

    $self->throw('parse', "Couldn't apply precedence");
}

### look for arguments - both positional and named
sub parse_args {
    my $self    = shift;
    my $str_ref = shift;
    my $ARGS    = shift || {};
    my $copy    = $$str_ref;

    my @args;
    my @named;
    while (length $$str_ref) {
        my $copy = $$str_ref;
        if (defined(my $name = $self->parse_variable(\$copy, {auto_quote => qr{ ^ (\w+) $QR_AQ_NOTDOT }xo}))
            && $copy =~ s{ ^ = >? \s* $QR_COMMENTS }{}ox) {
            $self->throw('parse', 'Named arguments not allowed') if $ARGS->{'positional_only'};
            my $val = $self->parse_variable(\$copy);
            $copy =~ s{ ^ , \s* $QR_COMMENTS }{}ox;
            push @named, $name, $val;
            $$str_ref = $copy;
        } elsif (defined(my $arg = $self->parse_variable($str_ref))) {
            push @args, $arg;
            $$str_ref =~ s{ ^ , \s* $QR_COMMENTS }{}ox;
        } else {
            last;
        }
    }

    ### allow for named arguments to be added also
    push @args, [\ ['hash', @named], 0] if scalar @named;

    return \@args;
}

### allow for looking for $foo or ${foo.bar} in TEXT "nodes" of the parse tree.
sub interpolate_node {
    my ($self, $tree, $offset) = @_;
    return if $self->{'_in_perl'};

    ### split on variables while keeping the variables
    my @pieces = split m{ (?: ^ | (?<! \\)) (\$\w+ (?:\.\w+)* | \$\{ [^\}]+ \}) }x, $tree->[-1];
    if ($#pieces <= 0) {
        $tree->[-1] =~ s{ \\ ([\"\$]) }{$1}xg;
        return;
    }

    my @sub_tree;
    my $n = 0;
    foreach my $piece (@pieces) {
        $offset += length $piece; # we track the offset to make sure DEBUG has the right location
        if (! ($n++ % 2)) { # odds will always be text chunks
            next if ! length $piece;
            $piece =~ s{ \\ ([\"\$]) }{$1}xg;
            push @sub_tree, $piece;
        } elsif ($piece =~ m{ ^ \$ (\w+ (?:\.\w+)*) $ }x
                 || $piece =~ m{ ^ \$\{ \s* ([^\}]+) \} $ }x) {
            my $name = $1;
            push @sub_tree, ['GET', $offset - length($piece), $offset, $self->parse_variable(\$name)];
        } else {
            $self->throw('parse', "Parse error during interpolate node");
        }
    }

    ### replace the tree
    splice @$tree, -1, 1, @sub_tree;
}

###----------------------------------------------------------------###

sub get_variable {
    ### allow for the parse tree to store literals
    return $_[1] if ! ref $_[1];

    my $self = shift;
    my $var  = shift;
    my $ARGS = shift || {};
    my $i    = 0;

    ### determine the top level of this particular variable access
    my $ref  = $var->[$i++];
    my $args = $var->[$i++];
    warn "get_variable: begin \"$ref\"\n" if trace;
    if (ref $ref) {
        if (ref($ref) eq 'SCALAR') { # a scalar literal
            $ref = $$ref;
        } elsif (ref($ref) eq 'REF') { # operator
            return $self->play_operator($$ref) if ${ $ref }->[0] eq '..';
            $ref = $self->play_operator($$ref);
        } else { # a named variable access (ie via $name.foo)
            $ref = $self->get_variable($ref);
            if (defined $ref) {
                return if $ref =~ $QR_PRIVATE; # don't allow vars that begin with _
                $ref = $self->{'_vars'}->{$ref};
            }
        }
    } elsif (defined $ref) {
        if ($ARGS->{'is_namespace_during_compile'}) {
            $ref = $self->{'NAMESPACE'}->{$ref};
        } else {
            return if $ref =~ $QR_PRIVATE; # don't allow vars that begin with _
            $ref = $self->{'_vars'}->{$ref};
        }
    }


    my %seen_filters;
    while (defined $ref) {

        ### check at each point if the rurned thing was a code
        if (UNIVERSAL::isa($ref, 'CODE')) {
            my @results = $ref->($args ? @{ $self->vivify_args($args) } : ());
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
        my $name         = $var->[$i++];
        my $args         = $var->[$i++];
        warn "get_variable: nested \"$name\"\n" if trace;

        ### allow for named portions of a variable name (foo.$name.bar)
        if (ref $name) {
            if (ref($name) eq 'ARRAY') {
                $name = $self->get_variable($name);
                if (! defined($name) || $name =~ $QR_PRIVATE || $name =~ /^\./) {
                    $ref = undef;
                    last;
                }
            } else {
                die "Shouldn't get a ". ref($name) ." during a vivify on chain";
            }
        }
        if ($name =~ $QR_PRIVATE) { # don't allow vars that begin with _
            $ref = undef;
            last;
        }

        ### allow for scalar and filter access (this happens for every non virtual method call)
        if (! ref $ref) {
            if ($SCALAR_OPS->{$name}) {                        # normal scalar op
                $ref = $SCALAR_OPS->{$name}->($ref, $args ? @{ $self->vivify_args($args) } : ());

            } elsif ($LIST_OPS->{$name}) {                     # auto-promote to list and use list op
                $ref = $LIST_OPS->{$name}->([$ref], $args ? @{ $self->vivify_args($args) } : ());

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
                            ($sub, my $err) = $sub->($self->context, $args ? @{ $self->vivify_args($args) } : ());
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
            if (UNIVERSAL::can($ref, 'can')) {
                my @args = $args ? @{ $self->vivify_args($args) } : ();
                my @results = eval { $ref->$name(@args) };
                if ($@) {
                    die $@ if ref $@ || $@ !~ /Can\'t locate object method/;
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
                    $ref = $ref->{$name};
                } elsif ($HASH_OPS->{$name}) {
                    $ref = $HASH_OPS->{$name}->($ref, $args ? @{ $self->vivify_args($args) } : ());
                } elsif ($ARGS->{'is_namespace_during_compile'}) {
                    return $var; # abort - can't fold namespace variable
                } else {
                    $ref = undef;
                }

            ### array access
            } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
                if ($name =~ /^\d+$/) {
                    $ref = ($name > $#$ref) ? undef : $ref->[$name];
                } else {
                    $ref = (! $LIST_OPS->{$name}) ? undef : $LIST_OPS->{$name}->($ref, $args ? @{ $self->vivify_args($args) } : ());
                }
            }
        }

    } # end of while

    ### allow for undefinedness
    if (! defined $ref) {
        if ($self->{'_debug_undef'}) {
            my $chunk = $var->[$i - 2];
            $chunk = $self->get_variable($chunk) if ref($chunk) eq 'ARRAY';
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
        if (ref($ref) eq 'ARRAY') { # named access (ie via $name.foo)
            $ref = $self->get_variable($ref);
            if (defined $ref && $ref !~ $QR_PRIVATE) { # don't allow vars that begin with _
                if ($#$var <= $i) {
                    $self->{'_vars'}->{$ref} = $val;
                    return;
                } else {
                    $ref = $self->{'_vars'}->{$ref} ||= {};
                }
            } else {
                return;
            }
        } else { # all other types can't be set
            return;
        }
    } elsif (defined $ref) {
        return if $ref =~ $QR_PRIVATE; # don't allow vars that begin with _
        if ($#$var <= $i) {
            $self->{'_vars'}->{$ref} = $val;
            return;
        } else {
            $ref = $self->{'_vars'}->{$ref} ||= {};
        }
    }

    ### let the top level thing be a code block
    if (UNIVERSAL::isa($ref, 'CODE')) {
        return;
    }

    ### vivify the chained levels
    while (defined $ref && $#$var > $i) {
        my $was_dot_call = $ARGS->{'no_dots'} ? 1 : $var->[$i++] eq '.';
        my $name         = $var->[$i++];
        my $args         = $var->[$i++];

        ### allow for named portions of a variable name (foo.$name.bar)
        if (ref $name) {
            if (ref($name) eq 'ARRAY') {
                $name = $self->get_variable($name);
                if (! defined($name) || $name =~ /^[_.]/) {
                    $ref = undef;
                    next;
                }
            } else {
                die "Shouldn't get a ".ref($name)." during a vivify on chain";
            }
        }
        if ($name =~ $QR_PRIVATE) { # don't allow vars that begin with _
            return;
        }

        ### method calls on objects
        if (UNIVERSAL::can($ref, 'can')) {
            my $lvalueish;
            my @args = $args ? @{ $self->vivify_args($args) } : ();
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
                    $ref = undef;
                }
                return if $lvalueish;
                next;
            }
            die $@ if ref $@ || $@ !~ /Can\'t locate object method/;
            # fall on down to "normal" accessors
        }

        ### hash member access
        if (UNIVERSAL::isa($ref, 'HASH')) {
            if ($#$var <= $i) {
                $ref->{$name} = $val;
                return;
            } else {
                $ref = $ref->{$name} ||= {};
                next;
            }

        ### array access
        } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
            if ($name =~ /^\d+$/) {
                if ($#$var <= $i) {
                    $ref->[$name] = $val;
                    return;
                } else {
                    $ref = $ref->[$name] ||= {};
                    next;
                }
            } else {
                return;
            }

        ### scalar access
        } elsif (! ref($ref) && defined($ref)) {
            return;
        }

        ### check at each point if the returned thing was a code
        if (defined($ref) && UNIVERSAL::isa($ref, 'CODE')) {
            my @results = $ref->($args ? @{ $self->vivify_args($args) } : ());
            if (defined $results[0]) {
                $ref = ($#results > 0) ? \@results : $results[0];
            } elsif (defined $results[1]) {
                die $results[1]; # TT behavior - why not just throw ?
            } else {
                return;
            }
        }

    }

    return $ref;
}

sub vivify_args {
    my $self = shift;
    my $vars = shift;
    return [map {$self->get_variable($_)} @$vars];
}

###----------------------------------------------------------------###

sub play_operator {
    my $self = shift;
    my $tree = shift;

    if ($OP_DISPATCH->{$tree->[0]}) {
        my @args = map { $self->get_variable($tree->[$_]) } 1 .. $#$tree;
        local $^W;
        return $OP_DISPATCH->{$tree->[0]}->(@args);
    }

    my $op = $tree->[0];

    ### do custom and short-circuitable operators
    if ($op eq '=') {
        my $val = $self->get_variable($tree->[2]);
        $self->set_variable($tree->[1], $val);
        return $val;

   } elsif ($op eq '||' || $op eq 'or' || $op eq 'OR') {
        return $self->get_variable($tree->[1]) || $self->get_variable($tree->[2]) || '';

    } elsif ($op eq '&&' || $op eq 'and' || $op eq 'AND') {
        my $var = $self->get_variable($tree->[1]) && $self->get_variable($tree->[2]);
        return $var ? $var : 0;

    } elsif ($op eq '?') {
        local $^W;
        return $self->get_variable($tree->[1]) ? $self->get_variable($tree->[2]) : $self->get_variable($tree->[3]);
    }

    $self->throw('operator', "Un-implemented operation $op");
}

###----------------------------------------------------------------###

sub parse_BLOCK {
    my ($self, $tag_ref, $node) = @_;

    my $block_name = '';
    if ($$tag_ref =~ s{ ^ (\w+ (?: :\w+)*) \s* (?! [\.\|]) }{}x
        || $$tag_ref =~ s{ ^ '(|.*?[^\\])' \s* (?! [\.\|]) }{}x
        || $$tag_ref =~ s{ ^ "(|.*?[^\\])" \s* (?! [\.\|]) }{}x
        ) {
        $block_name = $1;
        ### allow for nested blocks to have nested names
        my @names = map {$_->[3]} grep {$_->[0] eq 'BLOCK'} @{ $self->{'_state'} };
        $block_name = join("/", @names, $block_name) if scalar @names;
    }

    return $block_name;
}

sub play_BLOCK {
    my ($self, $block_name, $node, $out_ref) = @_;

    ### store a named reference - but do nothing until something processes it
    $self->{'BLOCKS'}->{$block_name} = {
        _tree => $node->[4],
        name  => $self->{'_vars'}->{'component'}->{'name'} .'/'. $block_name,
    };

    return;
}

sub parse_CALL { $DIRECTIVES->{'GET'}->[0]->(@_) }

sub play_CALL { $DIRECTIVES->{'GET'}->[1]->(@_); return }

sub parse_CASE {
    my ($self, $tag_ref) = @_;
    return if $$tag_ref =~ s{ ^ DEFAULT \s* }{}x;
    return $self->parse_variable($tag_ref);
}

sub parse_CATCH {
    my ($self, $tag_ref) = @_;
    return $self->parse_variable($tag_ref, {auto_quote => qr{ ^ (\w+ (?: \.\w+)*) $QR_AQ_SPACE }xo});
}

sub play_control {
    my ($self, $undef, $node) = @_;
    $self->throw(lc($node->[0]), 'Control exception', $node);
}

sub play_CLEAR {
    my ($self, $undef, $node, $out_ref) = @_;
    $$out_ref = '';
}

sub parse_DEBUG {
    my ($self, $tag_ref) = @_;
    $$tag_ref =~ s{ ^ (on | off | format) \s* }{}xi || $self->throw('parse', "Unknown DEBUG option");
    my $ret = [lc($1)];
    if ($ret->[0] eq 'format') {
        $$tag_ref =~ s{ ^ ([\"\']) (|.*?[^\\]) \1 \s* }{}xs || $self->throw('parse', "Missing format string");
        $ret->[1] = $2;
    }
    return $ret;
}

sub play_DEBUG {
    my ($self, $ref) = @_;
    if ($ref->[0] eq 'on') {
        delete $self->{'_debug_off'};
    } elsif ($ref->[0] eq 'off') {
        $self->{'_debug_off'} = 1;
    } elsif ($ref->[0] eq 'format') {
        $self->{'_debug_format'} = $ref->[1];
    }
}

sub parse_DEFAULT { $DIRECTIVES->{'SET'}->[0]->(@_) }

sub play_DEFAULT {
    my ($self, $set) = @_;
    foreach (@$set) {
        my ($set, $default) = @$_;
        next if ! defined $set;
        my $val = $self->get_variable($set);
        if (! $val) {
            $default = defined($default) ? $self->get_variable($default) : '';
            $self->set_variable($set, $default);
        }
    }
    return;
}

sub parse_DUMP {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref);
    return $ref;
}

sub play_DUMP {
    my ($self, $ident, $node) = @_;
    require Data::Dumper;
    my $info = $self->node_info($node);
    my $out;
    my $var;
    if ($ident) {
        $out = Data::Dumper::Dumper($self->get_variable($ident));
        $var = $info->{'text'};
        $var =~ s/^[+\-~=]?\s*DUMP\s+//;
        $var =~ s/\s*[+\-~=]?$//;
    } else {
        my @were_never_here = (qw(template component), grep {$_ =~ $QR_PRIVATE} keys %{ $self->{'_vars'} });
        local @{ $self->{'_vars'} }{ @were_never_here };
        delete @{ $self->{'_vars'} }{ @were_never_here };
        $out = Data::Dumper::Dumper($self->{'_vars'});
        $var = 'EntireStash';
    }
    if ($ENV{'REQUEST_METHOD'}) {
        $out =~ s/</&lt;/g;
        $out = "<pre>$out</pre>";
        $out =~ s/\$VAR1/$var/;
        $out = "<b>DUMP: File \"$info->{file}\" line $info->{line}</b>$out";
    } else {
        $out =~ s/\$VAR1/$var/;
    }

    return $out;
}

sub parse_FILTER {
    my ($self, $tag_ref) = @_;
    my $name = '';
    if ($$tag_ref =~ s{ ^ ([^\W\d]\w*) \s* = \s* }{}x) {
        $name = $1;
    }

    my $filter = $self->parse_variable($tag_ref);
    $filter = '' if ! defined $filter;

    return [$name, $filter];
}

sub play_FILTER {
    my ($self, $ref, $node, $out_ref) = @_;
    my ($name, $filter) = @$ref;

    return '' if ! @$filter;

    $self->{'FILTERS'}->{$name} = $filter if length $name;

    my $sub_tree = $node->[4];

    ### play the block
    my $out = '';
    eval { $self->execute_tree($sub_tree, \$out) };
    die $@ if $@ && ref($@) !~ /Template::Exception$/;

    my $var = [\$out, 0, '|', @$filter]; # make a temporary var out of it


    return $DIRECTIVES->{'GET'}->[1]->($self, $var, $node, $out_ref);
}

sub parse_FOREACH {
    my ($self, $tag_ref) = @_;
    my $items = $self->parse_variable($tag_ref);
    my $var;
    if ($$tag_ref =~ s{ ^ (= | [Ii][Nn]\b) \s* }{}x) {
        $var = [@$items];
        $items = $self->parse_variable($tag_ref);
    }
    return [$var, $items];
}

sub play_FOREACH {
    my ($self, $ref, $node, $out_ref) = @_;

    ### get the items - make sure it is an arrayref
    my ($var, $items) = @$ref;

    $items = $self->get_variable($items);
    return '' if ! defined $items;

    if (ref($items) !~ /Iterator$/) {
        $items = $PACKAGE_ITERATOR->new($items);
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

    return undef;
}

sub parse_GET {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref);
    $self->throw('parse', "Missing variable name") if ! defined $ref;
    return $ref;
}

sub play_GET {
    my ($self, $ident, $node) = @_;
    my $var = $self->get_variable($ident);
    return (! defined $var) ? $self->undefined_get($ident, $node) : $var;
}

sub parse_IF {
    my ($self, $tag_ref) = @_;
    return $self->parse_variable($tag_ref);
}

sub play_IF {
    my ($self, $var, $node, $out_ref) = @_;

    my $val = $self->get_variable($var);
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
        my $val = $self->get_variable($var);
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
    my ($self, $tag_ref, $node, $out_ref) = @_;

    ### localize the swap
    my $swap = $self->{'_vars'};
    local $self->{'_vars'} = {%$swap};

    ### localize the blocks
    my $blocks = $self->{'BLOCKS'};
    local $self->{'BLOCKS'} = {%$blocks};

    my $str = $DIRECTIVES->{'PROCESS'}->[1]->($self, $tag_ref, $node, $out_ref);

    return $str;
}

sub parse_INSERT { $DIRECTIVES->{'PROCESS'}->[0]->(@_) }

sub play_INSERT {
    my ($self, $var, $node, $out_ref) = @_;
    my ($names, $args) = @$var;

    foreach my $name (@$names) {
        my $filename = $self->get_variable($name);
        $$out_ref .= $self->_insert($filename);
    }

    return;
}

sub parse_MACRO {
    my ($self, $tag_ref, $node) = @_;
    my $copy = $$tag_ref;

    my $name = $self->parse_variable(\$copy, {auto_quote => qr{ ^ (\w+) $QR_AQ_NOTDOT }xo});
    $self->throw('parse', "Missing macro name") if ! defined $name;
    if (! ref $name) {
        $name = [ $name, 0 ];
    }

    my $args;
    if ($copy =~ s{ ^ \( \s* }{}x) {
        $args = $self->parse_args(\$copy, {positional_only => 1});
        $copy =~ s { ^ \) \s* }{}x || $self->throw('parse.missing', "Missing close ')'");
    }

    $node->[6] = 1;           # set a flag to keep parsing
    $$tag_ref = $copy;
    return [$name, $args];
}

sub play_MACRO {
    my ($self, $ref, $node, $out_ref) = @_;
    my ($name, $args) = @$ref;

    ### get the sub tree
    my $sub_tree = $node->[4];
    if (! $sub_tree || ! $sub_tree->[0]) {
        $self->set_variable($name, undef);
        return;
    } elsif ($sub_tree->[0]->[0] eq 'BLOCK') {
        $sub_tree = $sub_tree->[0]->[4];
    }

    my $self_copy = $self->weak_copy;

    ### install a closure in the stash that will handle the macro
    $self->set_variable($name, sub {
        ### macros localize
        my $copy = $self_copy->{'_vars'};
        local $self_copy->{'_vars'}= {%$copy};

        ### set arguments
        my $named = pop(@_) if $_[-1] && UNIVERSAL::isa($_[-1],'HASH') && $#_ > $#$args;
        my @positional = @_;
        foreach my $var (@$args) {
            $self_copy->set_variable($var, shift(@positional));
        }
        foreach my $name (sort keys %$named) {
            $self_copy->set_variable([$name, 0], $named->{$name});
        }

        ### finally - run the sub tree
        my $out = '';
        $self_copy->execute_tree($sub_tree, \$out);
        return $out;
    });

    return;
}

sub play_METADEF {
    my ($self, $hash) = @_;
    my $ref;
    if ($self->{'_top_level'}) {
        $ref = $self->{'_vars'}->{'template'} ||= {};
    } else {
        $ref = $self->{'_vars'}->{'component'} ||= {};
    }
    foreach my $key (keys %$hash) {
        next if $key eq 'name' || $key eq 'modtime';
        $ref->{$key} = $hash->{$key};
    }
    return;
}

sub parse_PERL { shift->{'_in_perl'} = 1; return }

sub play_PERL {
    my ($self, $info, $node, $out_ref) = @_;
    $self->throw('perl', 'EVAL_PERL not set') if ! $self->{'EVAL_PERL'};

    ### fill in any variables
    my $perl = $node->[4] || return;
    my $out  = '';
    $self->execute_tree($perl, \$out);
    $out = $1 if $out =~ /^(.+)$/s; # blatant untaint - shouldn't use perl anyway

    ### try the code
    my $err;
    eval {
        package CGI::Ex::Template::Perl;

        my $context = $self->context;
        my $stash   = $context->stash;

        ### setup a fake handle
        local *PERLOUT;
        tie *PERLOUT, $CGI::Ex::Template::PACKAGE_PERL_HANDLE, $out_ref;
        my $old_fh = select PERLOUT;

        eval $out;
        $err = $@;

        ### put the handle back
        select $old_fh;

    };
    $err ||= $@;


    if ($err) {
        $self->throw('undef', $err) if ref($err) !~ /Template::Exception$/;
        die $err;
    }

    return;
}

sub parse_PROCESS {
    my ($self, $tag_ref) = @_;
    my $info = [[], []];
    while (defined(my $filename = $self->parse_variable($tag_ref, {
                       auto_quote => qr{ ^ ($QR_FILENAME | \w+ (?: :\w+)* ) $QR_AQ_SPACE }xo,
                   }))) {
        push @{$info->[0]}, $filename;
        last if $$tag_ref !~ s{ ^ \+ \s* }{}x;
    }

    ### allow for post process variables
    while (length $$tag_ref) {
        last if $$tag_ref =~ / ^ (\w+) (?: ;|$|\s)/x && $DIRECTIVES->{$1}; ### looks like a directive - we are done

        my $var = $self->parse_variable($tag_ref);
        last if ! defined $var;
        if ($$tag_ref !~ s{ ^ = >? \s* }{}x) {
            $self->throw('parse.missing.equals', 'Missing equals while parsing args');
        }

        my $val = $self->parse_variable($tag_ref);
        push @{$info->[1]}, [$var, $val];
        $$tag_ref =~ s{ ^ , \s* $QR_COMMENTS }{}ox if $val;
    }

    return $info;
}

sub play_PROCESS {
    my ($self, $info, $node, $out_ref) = @_;

    my ($files, $args) = @$info;

    ### set passed args
    foreach (@$args) {
        my ($key, $val) = @$_;
        $val = $self->get_variable($val);
        if (ref($key) && @$key == 2 && $key->[0] eq 'import' && UNIVERSAL::isa($val, 'HASH')) { # import ?! - whatever
            foreach my $key (keys %$val) {
                $self->set_variable([$key,0], $val->{$key});
            }
            next;
        }
        $self->set_variable($key, $val);
    }

    ### iterate on any passed block or filename
    foreach my $ref (@$files) {
        next if ! defined $ref;
        my $filename = $self->get_variable($ref);
        my $out = ''; # have temp item to allow clear to correctly clear

        ### normal blocks or filenames
        if (! ref $filename) {
            eval { $self->_process($filename, $self->{'_vars'}, \$out) }; # restart the swap - passing it our current stash

        ### allow for $template which is used in some odd instances
        } else {
            $self->throw('process', "Unable to process document $filename") if $ref->[0] ne 'template';
            $self->throw('process', "Recursion detected in $node->[0] \$template") if $self->{'_process_dollar_template'};
            local $self->{'_process_dollar_template'} = 1;
            local $self->{'_vars'}->{'component'} = my $doc = $filename;
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
    my ($self, $info, $node, $out_ref) = @_;
    $self->throw('perl', 'EVAL_PERL not set') if ! $self->{'EVAL_PERL'};

    ### fill in any variables
    my $tree = $node->[4] || return;
    my $perl  = '';
    $self->execute_tree($tree, \$perl);
    $perl = $1 if $perl =~ /^(.+)$/s; # blatant untaint - shouldn't use perl anyway

    ### try the code
    my $err;
    my $output = '';
    eval {
        package CGI::Ex::Template::Perl;

        my $context = $self->context;
        my $stash   = $context->stash;

        eval $perl;
        $err = $@;
    };
    $err ||= $@;

    $$out_ref .= $output;

    if ($err) {
        $self->throw('undef', $err) if ref($err) !~ /Template::Exception$/;
        die $err;
    }

    return;
}

sub parse_SET {
    my ($self, $tag_ref, $node, $initial_var) = @_;
    my @SET;
    my $copy = $$tag_ref;
    my $func;
    while (length $$tag_ref) {
        my $set;
        my $get_val;
        my $val;
        if ($initial_var) {
            $set = $initial_var;
            undef $initial_var;
            $get_val = 1;
        } else {
            $set = $self->parse_variable($tag_ref);
            last if ! defined $set;
            $get_val = $$tag_ref =~ s{ ^ = >? \s* }{}x;
        }
        if (! $get_val) { # no next val
            $val = undef;
        } elsif ($$tag_ref =~ $QR_DIRECTIVE   # find a word
                 && $DIRECTIVES->{$1}) {      # is it a directive - if so set up capturing
            $node->[6] = 1;           # set a flag to keep parsing
            $val = $node->[4] ||= []; # setup storage
            push @SET, [$set, $val];
            last;
        } else { # get a normal variable
            $val = $self->parse_variable($tag_ref);
        }
        push @SET, [$set, $val];
    }
    return \@SET;
}

sub play_SET {
    my ($self, $set, $node) = @_;
    foreach (@$set) {
        my ($set, $val) = @$_;
        if (! defined $val) { # not defined
            $val = '';
        } elsif ($node->[4] && $val == $node->[4]) { # a captured directive
            my $sub_tree = $node->[4];
            $sub_tree = $sub_tree->[0]->[4] if $sub_tree->[0] && $sub_tree->[0]->[0] eq 'BLOCK';
            $val = '';
            $self->execute_tree($sub_tree, \$val);
        } else { # normal var
            $val = $self->get_variable($val);
        }

        $self->set_variable($set, $val);
    }
    return;
}

sub parse_SWITCH { $DIRECTIVES->{'GET'}->[0]->(@_) }

sub play_SWITCH {
    my ($self, $var, $node, $out_ref) = @_;

    my $val = $self->get_variable($var);
    $val = '' if ! defined $val;
    ### $node->[4] is thrown away

    my $default;
    while ($node = $node->[5]) { # CASES
        my $var = $node->[3];
        if (! defined $var) {
            $default = $node->[4];
            next;
        }

        my $val2 = $self->get_variable($var);
        $val2 = [$val2] if ! UNIVERSAL::isa($val2, 'ARRAY');
        for my $test (@$val2) { # find matching values
            next if ! defined $val && defined $test;
            next if defined $val && ! defined $test;
            if ($val ne $test) { # check string-wise first - then numerical
                next if $val  !~ /^ -? (?: \d*\.\d+ | \d+) $/x;
                next if $test !~ /^ -? (?: \d*\.\d+ | \d+) $/x;
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

sub parse_THROW {
    my ($self, $tag_ref, $node) = @_;
    my $name = $self->parse_variable($tag_ref, {auto_quote => qr{ ^ (\w+ (?: \.\w+)*) $QR_AQ_SPACE }xo});
    $self->throw('parse.missing', "Missing name in THROW", $node) if ! $name;
    my $args = $self->parse_args($tag_ref);
    return [$name, $args];
}

sub play_THROW {
    my ($self, $ref, $node) = @_;
    my ($name, $args) = @$ref;
    $name = $self->get_variable($name);
    my @args = $args ? @{ $self->vivify_args($args) } : ();
    $self->throw($name, \@args, $node);
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
        my $name = $self->get_variable($node->[3]);
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
    return [ \ [ '!', $ref ], 0 ];
}

sub play_UNLESS { return $DIRECTIVES->{'IF'}->[1]->(@_) }

sub parse_USE {
    my ($self, $tag_ref) = @_;

    my $var;
    my $copy = $$tag_ref;
    if (defined(my $_var = $self->parse_variable(\$copy, {auto_quote => qr{ ^ (\w+) $QR_AQ_NOTDOT }xo}))
        && $copy =~ s{ ^ = >? \s* $QR_COMMENTS }{}ox) {
        $var = $_var;
        $$tag_ref = $copy;
    }

    $copy = $$tag_ref;
    my $module = $self->parse_variable(\$copy, {auto_quote => qr{ ^ (\w+ (?: (?:\.|::) \w+)*) $QR_AQ_NOTDOT }xo});
    $self->throw('parse', "Missing plugin name while parsing $$tag_ref") if ! defined $module;
    $module =~ s/\./::/g;

    my $args;
    my $open = $copy =~ s{ ^ \( \s* $QR_COMMENTS }{}ox;
    $args = $self->parse_args(\$copy);

    if ($open) {
        $copy =~ s { ^ \) \s* $QR_COMMENTS }{}ox || $self->throw('parse.missing', "Missing close ')'");
    }

    $$tag_ref = $copy;
    return [$var, $module, $args];
}

sub play_USE {
    my ($self, $ref, $node, $out_ref) = @_;
    my ($var, $module, $args) = @$ref;

    ### get the stash storage location - default to the module
    $var = $module if ! defined $var;
    my @var = map {($_, 0, '.')} split /(?:\.|::)/, $var;
    pop @var; # remove the trailing '.'

    ### look for a plugin_base
    my $base = $self->{'PLUGIN_BASE'} || 'Template::Plugin'; # I'm not maintaining plugins - leave that to TT
    my $package = $self->{'PLUGINS'}->{$module} ? $self->{'PLUGINS'}->{$module}
       : $self->{'PLUGIN_FACTORY'}->{$module} ? $self->{'PLUGIN_FACTORY'}->{$module}
       : "${base}::${module}";
    my $require = "$package.pm";
    $require =~ s|::|/|g;

    ### try and load the module - fall back to bare module if allowed
    my $obj;
    if ($self->{'PLUGIN_FACTORY'}->{$module} || eval {require $require}) {
        my $shape   = $package->load;
        my $context = $self->context;
        my @args    = $args ? @{ $self->vivify_args($args) } : ();
        $obj = $shape->new($context, @args);
    } elsif (lc($module) eq 'iterator') { # use our iterator if none found (TT's works just fine)
        $obj = $PACKAGE_ITERATOR->new($args ? $self->get_variable($args->[0]) : []);
    } elsif (my @packages = grep {lc($package) eq lc($_)} @{ $self->list_plugins({base => $base}) }) {
        foreach my $package (@packages) {
            my $require = "$package.pm";
            $require =~ s|::|/|g;
            eval {require $require} || next;
            my $shape   = $package->load;
            my $context = $self->context;
            my @args    = $args ? @{ $self->vivify_args($args) } : ();
            $obj = $shape->new($context, @args);
        }
    } elsif ($self->{'LOAD_PERL'}) {
        my $require = "$module.pm";
        $require =~ s|::|/|g;
        if (eval {require $require}) {
            my @args = $args ? @{ $self->vivify_args($args) } : ();
            $obj = $module->new(@args);
        }
    }
    if (! defined $obj) {
        my $err = "$module: plugin not found";
        $self->throw('plugin', $err);
    }

    ### all good
    $self->set_variable(\@var, $obj);

    return;
}

sub play_WHILE {
    my ($self, $var, $node, $out_ref) = @_;
    return '' if ! defined $var;

    my $sub_tree = $node->[4];

    ### iterate use the iterator object
    my $count = $WHILE_MAX;
    while (--$count > 0) {

        $self->get_variable($var) || last;

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

    return undef;
}

sub parse_WRAPPER { $DIRECTIVES->{'INCLUDE'}->[0]->(@_) }

sub play_WRAPPER {
    my ($self, $var, $node, $out_ref) = @_;
    my $sub_tree = $node->[4] || return;

    my ($names, $args) = @$var;

    my $out = '';
    $self->execute_tree($sub_tree, \$out);

    foreach my $name (reverse @$names) {
        local $self->{'_vars'}->{'content'} = $out;
        $out = '';
        $DIRECTIVES->{'INCLUDE'}->[1]->($self, [[$name], $args], $node, \$out);
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
        $self->throw('file', "$file ABSOLUTE paths disabled") if ! $self->{'ABSOLUTE'};
        return $file if -e $file;
    } elsif ($file =~ m|^\./|) {
        $self->throw('file', "$file RELATIVE paths disabled") if ! $self->{'RELATIVE'};
        return $file if -e $file;
    }

    my $paths = $self->{'INCLUDE_PATHS'} ||= do {
        # TT does this everytime a file is looked up - we are going to do it just in time - the first time
        my $paths = $self->{'INCLUDE_PATH'} || $self->throw('file', "INCLUDE_PATH not set");
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
    local *FH;
    open(FH, "<$file") || $self->throw('file', "$file couldn't be opened: $!");
    read FH, my $txt, -s $file;
    close FH;
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
    $self->DEBUG("set binmode\n") if $DEBUG && $args->{'binmode'}; # holdover for TT2 tests

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
        local $copy->{'template'};

        local $self->{'BLOCKS'} = $blocks = {%$blocks}; # localize blocks - but save a copy to possibly restore

        delete $self->{'_debug_off'};
        delete $self->{'_debug_format'};

        ### handle pre process items that go before every document
        if ($self->{'PRE_PROCESS'}) {
            foreach my $name (@{ $self->split_paths($self->{'PRE_PROCESS'}) }) {
                my $out = '';
                $self->_process($name, $copy, \$out);
                $output = $out . $output;
            }
        }

        ### handle the process config - which loads a template in place of the real one
        if (exists $self->{'PROCESS'}) {
            ### load the meta data for the top document
            my $doc  = $self->load_parsed_tree($content) || {};
            my $meta = ($doc->{'_tree'} && ref($doc->{'_tree'}->[0]) && $doc->{'_tree'}->[0]->[0] eq 'METADEF')
                ? $doc->{'_tree'}->[0]->[3] : {};

            $copy->{'template'} = $doc;
            @{ $doc }{keys %$meta} = values %$meta;

            ### process any other templates
            foreach my $name (@{ $self->split_paths($self->{'PROCESS'}) }) {
                next if ! length $name;
                $self->_process($name, $copy, \$output);
            }

        ### handle "normal" content
        } else {
            local $self->{'_start_top_level'} = 1;
            $self->_process($content, $copy, \$output);
        }


        ### handle post process items that go after every document
        if ($self->{'POST_PROCESS'}) {
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
            local *FH;
            if (open FH, ">$file") {
                if (my $bm = $args->{'binmode'}) {
                    if (+$bm == 1) { binmode FH }
                    else           { binmode FH, $bm }
                }
                print FH $output;
                close FH;
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

###----------------------------------------------------------------###

sub exception {
    my ($self, $type, $info, $node) = @_;
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
    return $PACKAGE_EXCEPTION->new($type, $info, $node);
}

sub throw { die shift->exception(@_) }

sub context {
    my $self = shift;
    return bless {_template => $self}, $PACKAGE_CONTEXT; # a fake context
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
    my $self = shift;
    my $args = shift || {};
    my $base = $args->{'base'} || '';

    return $self->{'_plugins'}->{$base} ||= do {
        my @plugins;

        $base =~ s|::|/|g;
        my @dirs = grep {-d $_} map {"$_/$base"} @INC;

        foreach my $dir (@dirs) {
            require File::Find;
            File::Find::find(sub {
                my $mod = $base .'/'. ($File::Find::name =~ m|^ $dir / (.*\w) \.pm $|x ? $1 : return);
                $mod =~ s|/|::|g;
                push @plugins, $mod;
            }, $dir);
        }

        \@plugins; # return of the do
    };
}

### get a copy of self without circular refs for use in closures
sub weak_copy {
    my $self = shift;
    my $self_copy;
    if (eval { require Scalar::Util }
        && defined &Scalar::Util::weaken) {
        $self_copy = $self;
        Scalar::Util::weaken($self_copy);
    } else {
        $self_copy = bless {%$self}, ref($self); # hackish way to avoid circular refs on old perls (pre 5.8)
    }
    return $self_copy;
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
    my $doc = $self->{'_vars'}->{'component'};
    my $i = $node->[1];
    my $j = $node->[2] || return ''; # METADEF can be 0
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
    my ($self, $doc, $index) = @_;
    ### get the line offsets for the doc
    my $lines = $doc->{'line_offsets'} ||= do {
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
    return $#$lines + 1 if $index > $lines->[-1];
    my ($i, $j) = (0, $#$lines);
    while (1) {
        return $i + 1 if abs($i - $j) <= 1;
        my $k = int(($i + $j) / 2);
        $j = $k if $lines->[$k] >= $index;
        $i = $k if $lines->[$k] <= $index;
    }
}

###----------------------------------------------------------------###
### long virtual methods or filters
### many of these vmethods have used code from Template/Stash.pm to
### assure conformance with the TT spec.

sub define_vmethod {
    my ($self, $type, $name, $sub) = @_;
    if (   $type =~ /scalar|item/i) { $SCALAR_OPS->{$name} = $sub }
    elsif ($type =~ /array|list/i ) { $LIST_OPS->{  $name} = $sub }
    elsif ($type =~ /hash/i       ) { $HASH_OPS->{  $name} = $sub }
    elsif ($type =~ /filter/i     ) { $FILTER_OPS->{$name} = $sub }
    else {
        die "Invalid type vmethod type $type";
    }
    return 1;
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
    return join "\n", map{ sprintf $pat, $_ } split(/\n/, $str);
}

sub vmethod_match {
    my ($str, $pat, $global) = @_;
    return [] if ! defined $str || ! defined $pat;
    my @res = $global ? ($str =~ /$pat/g) : ($str =~ /$pat/);
    return (@res >= 2) ? \@res : (@res == 1) ? $res[0] : '';
}

sub vmethod_nsort {
    my ($list, $field) = @_;
    return defined($field)
        ? [map {$_->[0]} sort {$a->[1] <=> $b->[1]} map {[$_, (ref $_ eq 'HASH' ? $_->{$field}
                                                               : UNIVERSAL::can($_, $field) ? $_->$field()
                                                               : $_)]} @$list ]
        : [sort {$a <=> $b} @$list];
}

sub vmethod_repeat {
    my ($str, $n, $join) = @_;
    return if ! length $str;
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
    } else {
        return [splice @$ref, $i || 0];
    }
}

sub vmethod_split {
    my ($str, $pat, @args) = @_;
    $str = '' if ! defined $str;
    return defined $pat ? [split $pat, $str, @args] : [split ' ', $str, @args];
}

sub filter_eval {
    my $context = shift;
    return sub {
        my $text = shift;
        return $context->process(\$text);
    };
}

sub filter_redirect {
    my ($context, $file, $options) = @_;
    my $path = $context->config->{'OUTPUT_PATH'} || $context->throw('redirect', 'OUTPUT_PATH is not set');

    return sub {
        my $text = shift;
        if (! -d $path) {
            require File::Path;
            File::Path::mkpath($path) || $context->throw('redirect', "Couldn't mkpath \"$path\": $!");
        }
        local *FH;
        open (FH, ">$path/$file") || $context->throw('redirect', "Couldn't open \"$file\": $!");
        if (my $bm = (! $options) ? 0 : ref($options) ? $options->{'binmode'} : $options) {
            if (+$bm == 1) { binmode FH }
            else { binmode FH, $bm}
        }
        print FH $text;
        close FH;
        return '';
    };
}

###----------------------------------------------------------------###

sub dump_parse {
    my $obj = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift : __PACKAGE__->new;
    my $str = shift;
    require Data::Dumper;
    return Data::Dumper::Dumper($obj->parse_variable(\$str));
}

###----------------------------------------------------------------###

package CGI::Ex::Template::Exception;

use overload
    '""' => \&as_string,
    bool => sub { defined shift },
    fallback => 1;

sub new {
    my ($class, $type, $info, $node, $pos, $str_ref) = @_;
    return bless [$type, $info, $node, $pos, $str_ref], $class;
}

sub type { shift->[0] }

sub info { shift->[1] }

sub node {
    my $self = shift;
    $self->[2] = shift if $#_ == 0;
    $self->[2];
}

sub offset { shift->[3] || 0 }

sub doc {
    my $self = shift;
    $self->[4] = shift if $#_ == 0;
    $self->[4];
}

sub as_string {
    my $self = shift;
    my $msg  = $self->type .' error - '. $self->info;
    if (my $node = $self->node) {
#        $msg .= " (In tag $node->[0] starting at char ".($node->[1] + $self->offset).")";
    }
    return $msg;
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

package CGI::Ex::Template::_Context;

use vars qw($AUTOLOAD);

sub _template { shift->{'_template'} || die "Missing _template" }

sub config { shift->_template }

sub stash {
    my $self = shift;
    return $self->{'stash'} ||= bless {_template => $self->_template}, $CGI::Ex::Template::PACKAGE_STASH;
}

sub insert { shift->_template->_insert(@_) }

sub eval_perl { shift->_template->{'EVAL_PERL'} }

sub process {
    my $self = shift;
    my $ref  = shift;
    my $vars = $self->_template->_vars;
    my $out  = '';
    $self->_template->_process($ref, $vars, \$out);
    return $out;
}

sub include {
    my $self = shift;
    my $file = shift;
    my $args = shift || {};

    $self->_template->set_variable($_, $args->{$_}) for keys %$args;

    my $out = ''; # have temp item to allow clear to correctly clear
    eval { $self->_template->_process($file, $self->{'_vars'}, \$out) };
    if (my $err = $@) {
        die $err if ref($err) !~ /Template::Exception$/ || $err->type !~ /return/;
    }

    return $out;
}

sub define_filter {
    my ($self, $name, $filter, $is_dynamic) = @_;
    $filter = [ $filter, 1 ] if $is_dynamic;
    $self->define_vmethod('filter', $name, $filter);
}

sub filter {
    my ($self, $name, $args, $alias) = @_;
    my $t = $self->_template;

    my $filter;
    if (! ref $name) {
        $filter = $t->{'FILTERS'}->{$name} || $CGI::Ex::Template::FILTER_OPS->{$name} || $CGI::Ex::Template::SCALAR_OPS->{$name};
        $t->throw('filter', $name) if ! $filter;
    } elsif (UNIVERSAL::isa($name, 'CODE') || UNIVERSAL::isa($name, 'ARRAY')) {
        $filter = $name;
    } elsif (UNIVERSAL::can($name, 'factory')) {
        $filter = $name->factory || $t->throw($name->error);
    } else {
        $t->throw('undef', "$name: filter not found");
    }

    if (UNIVERSAL::isa($filter, 'ARRAY')) {
        $filter = ($filter->[1]) ? $filter->[0]->($t->context, @$args) : $filter->[0];
    } elsif ($args && @$args) {
        my $sub = $filter;
        $filter = sub { $sub->(shift, @$args) };
    }

    $t->{'FILTERS'}->{$alias} = $filter if $alias;

    return $filter;
}

sub define_vmethod { shift->_template->define_vmethod(@_) }

sub throw {
    my ($self, $type, $info) = @_;

    if (UNIVERSAL::isa($type, $CGI::Ex::Template::PACKAGE_EXCEPTION)) {
	die $type;
    } elsif (defined $info) {
	$self->_template->throw($type, $info);
    } else {
	$self->_template->throw('undef', $type);
    }
}

sub AUTOLOAD { shift->_template->throw('not_implemented', "The method $AUTOLOAD has not been implemented") }

sub DESTROY {}

###----------------------------------------------------------------###

package CGI::Ex::Template::_Stash;

use vars qw($AUTOLOAD);

sub _template { shift->{'_template'} || die "Missing _template" }

sub get {
    my ($self, $var) = @_;
    if (! ref $var) {
        if ($var =~ /^\w+$/) {  $var = [$var, 0] }
        else {                  $var = $self->_template->parse_variable(\$var, {no_dots => 1}) }
    }
    return $self->_template->get_variable($var, {no_dots => 1});
}

sub set {
    my ($self, $var, $val) = @_;
    if (! ref $var) {
        if ($var =~ /^\w+$/) {  $var = [$var, 0] }
        else {                  $var = $self->_template->parse_variable(\$var, {no_dots => 1}) }
    }
    $self->_template->set_variable($var, $val, {no_dots => 1});
    return $val;
}

sub AUTOLOAD { shift->_template->throw('not_implemented', "The method $AUTOLOAD has not been implemented") }

sub DESTROY {}

###----------------------------------------------------------------###

package CGI::Ex::Template::EvalPerlHandle;

sub TIEHANDLE {
    my ($class, $out_ref) = @_;
    return bless [$out_ref], $class;
}

sub PRINT {
    my $self = shift;
    ${ $self->[0] } .= $_ for grep {defined && length} @_;
    return 1;
}

###----------------------------------------------------------------###

1;

### See the perldoc in CGI/Ex/Template.pod
