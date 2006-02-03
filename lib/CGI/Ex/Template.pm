package CGI::Ex::Template;

=head1

CGI::Ex::Template - Lightweight TT2/3 engine

=cut

use CGI::Ex::Dump qw(debug);
use strict;
use vars qw($TAGS
            $SCALAR_OPS $HASH_OPS $LIST_OPS
            $DIRECTIVES $QR_DIRECTIVE
            $OPERATORS $OP_UNARY $OP_TRINARY $OP_FUNC $QR_OP $QR_OP_UNARY
            $QR_FILENAME $QR_AQ_NOTDOT $QR_AQ_SPACE
            $WHILE_MAX
            $EXTRA_COMPILE_EXT
            );

BEGIN {
    $TAGS ||= {
        template => ['[%',   '%]'],  # default
        metatext => ['%%',   '%%'],  # Text::MetaText
        star     => ['[*',   '*]'],  # TT alternate
        php      => ['<?',   '?>'],  # PHP
        asp      => ['<%',   '%>'],  # ASP
        mason    => ['<%',   '>' ],  # HTML::Mason
        html     => ['<!--', '-->'], # HTML comments
    };

    $SCALAR_OPS = {
        indent  => \&vmethod_indent,
        hash    => sub { {value => $_[0]} },
        length  => sub { defined($_[0]) ? length($_[0]) : 0 },
        match   => \&vmethod_match,
        repeat  => \&vmethod_repeat,,
        replace => \&vmethod_replace,
        size    => sub { 1 },
        split   => \&vmethod_split,
        substr  => sub { my ($str, $i, $len) = @_; defined($len) ? substr($str, $i, $len) : substr($str, $i) },
    };

    $LIST_OPS = {
        grep    => sub { my ($ref, $pat) = @_; [grep {/$pat/} @$ref] },
        hash    => sub { my ($list, $i) = @_; defined($i) ? {map {$i++ => $_} @$list} : {@$list} },
        join    => sub { my ($ref, $join) = @_; $join = ' ' if ! defined $join; return join $join, @$ref },
        list    => sub { $_[0] },
        max     => sub { $#{ $_[0] } },
        nsort   => sub { [sort {$a->[1] <=> $b->[1]} @{ $_[0] } ] },
        pop     => sub { pop @{ $_[0] } },
        push    => sub { my $ref = shift; push @$ref, @_; return '' },
        reverse => sub { [ reverse @{ $_[0] } ] },
        shift   => sub { shift @{ $_[0] } },
        size    => sub { $#{ $_[0] } + 1 },
        sort    => sub { [map {$_->[0]} sort {$a->[1] cmp $b->[1]} map {[$_, lc $_]} @{ $_[0] } ] }, # case insensitive
        unshift => sub { my $ref = shift; unshift @$ref, @_; return '' },
    };

    $HASH_OPS = {
        defined => sub { return '' if ! defined $_[1]; defined $_[0]->{ $_[1] } },
        delete  => sub { return '' if ! defined $_[1]; delete  $_[0]->{ $_[1] } },
        each    => sub { [each %{ $_[0] }] },
        exists  => sub { return '' if ! defined $_[1]; exists $_[0]->{ $_[1] } },
        hash    => sub { $_[0] },
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
        META    => [\&parse_META,    \&play_META],
        NEXT    => [sub {},          \&play_control],
        PERL    => [\&parse_PERL,    \&play_PERL,     1],
        PROCESS => [\&parse_PROCESS, \&play_PROCESS],
        RETURN  => [sub {},          \&play_control],
        SET     => [\&parse_SET,     \&play_SET],
        STOP    => [sub {},          \&play_control],
        SWITCH  => [\&parse_SWITCH,  \&play_SWITCH,   1],
        TAGS    => [undef,           sub {}],
        THROW   => [\&parse_THROW,   \&play_THROW],
        TRY     => [sub {},          \&play_TRY,      1],
        UNLESS  => [\&parse_UNLESS,  \&play_UNLESS,   1,       1],
        USE     => [\&parse_USE,     \&play_USE],
        WHILE   => [\&parse_WHILE,   \&play_WHILE,    1,       1],
        WRAPPER => [\&parse_WRAPPER, \&play_WRAPPER,  1],
        #name       #parse_sub       #play_sub        #block   #postdir
    };
    $QR_DIRECTIVE = qr{ ^ (\w+|\|) (?= \s|$|;) }x;

    $OPERATORS ||= {qw(**  99   ^   99   pow 99
                       !   95   unary_minus  95
                       *   90   /   90   div 90   DIV 90
                       %   90   mod 90   MOD 90
                       +   85   -   85   _   85   ~   85
                       <   80   >   80   <=  80   >=  80
                       lt  80   gt  80   le  80   ge  80
                       ==  75   !=  75   eq  75   ne  75
                       &&  70
                       ||  65
                       ..  60
                       ?   55
                       not 50   NOT 50
                       and 45   AND 45
                       or  40   OR  40
                       hashref 1 arrayref 1
                       )};
    $OP_UNARY   ||= {'!' => '!', 'not' => '!', 'NOT' => '!', 'unary_minus' => '-'};
    $OP_TRINARY ||= {'?' => ':'};
    $OP_FUNC    ||= {};
    sub _op_qr { # no mixed \w\W operators
        my %used;
        my $chrs = join '|', map {quotemeta $_} grep {++$used{$_} < 2} grep {/^\W{2,}$/} @_;
        my $chr  = join '',  map {quotemeta $_} grep {++$used{$_} < 2} grep {/^\W$/}     @_;
        my $word = join '|',                    grep {++$used{$_} < 2} grep {/^\w+$/}    @_;
        $chr = "[$chr]" if $chr;
        $word = "\\b(?:$word)\\b" if $word;
        return join('|', grep {length} $chrs, $chr, $word) || die "Missing operator regex";
    }
    sub _build_op_qr       { _op_qr(sort(grep {! $OP_UNARY->{$_}} keys(%$OPERATORS), values(%$OP_TRINARY))) }
    sub _build_op_qr_unary { _op_qr(sort %$OP_UNARY) } # grab keys and values
    $QR_OP       ||= _build_op_qr();
    $QR_OP_UNARY ||= _build_op_qr_unary();

    $QR_FILENAME  = '([a-zA-Z]]:/|/)? [\w\-\.]+ (?:/[\w\-\.]+)*';
    $QR_AQ_NOTDOT = '(?! \s* \.)';
    $QR_AQ_SPACE  = '(?: \s+ | $ | ;)';

    $WHILE_MAX   = 1000;
    $EXTRA_COMPILE_EXT = '.sto';
};

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $args  = ref($_[0]) ? { %{ shift() } } : {@_};
  return bless $args, $class;
}

###----------------------------------------------------------------###

sub _swap {
    my $self = shift;
    my $file = shift;
    local $self->{'_vars'} = shift || {};
    my $out_ref = shift || $self->throw('undef', "Missing output");
    local $self->{'_top_level'} = delete $self->{'_start_top_level'};
    my $i = length $$out_ref;

    ### parse and execute
    my $doc;
    eval {
        ### load the document
        $doc = $self->load_parsed_tree($file) || $self->throw('undef', "Zero length content");;

        ### prevent recursion
        $self->throw('recursion', "Recursive call into $doc->{name}")
            if ! $self->{'RECURSION'} && $self->{'_in'}->{$doc->{'name'}};
        local $self->{'_in'}->{$doc->{'name'}} = 1;

        ### execute the document
        if ($#{ $doc->{'tree'} } == -1) { # no tags found - just return the content
            $$out_ref = ${ $doc->{'content'} };
        } else {
            local $self->{'_template'} = $doc;
            $self->{'_vars'}->{'template'} = $doc if $self->{'_top_level'};
            $self->execute_tree($doc->{'tree'}, $out_ref);
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
        $err = $self->exception('undef', $err) if ! UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception');
        $err->doc($doc) if $doc && $err->can('doc') && ! $err->doc;
        die $err;
    }

    return 1;
}

sub load_parsed_tree {
    my $self = shift;
    my $file = shift;
    return if ! defined $file;

    my $doc = {name => $file};

    ### looks like a string reference
    if (ref $file) {
        $doc->{'content'} = $file;
        $doc->{'name'}    = 'input text';
        $doc->{'is_ref'}  = 1;

    ### looks like a previously cached-in-memory document
    } elsif ($self->{'_documents'}->{$file}) {
        $doc = $self->{'_documents'}->{$file};
        $doc->{'cache_time'} = time if $self->{'CACHE_SIZE'};
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
        $doc->{'tree'} = $block->{'tree'} || $self->throw('block', "Invalid block definition (missing tree)");
        return $doc;


    ### go and look on the file system
    } else {
        $doc->{'filename'} = eval { $self->include_filename($file) };
        if (my $err = $@) {
            ### allow for blocks in other files
            if ($self->{'EXPOSE_BLOCKS'}
                && ! $self->{'_looking_in_block_file'}) {
                local $self->{'_looking_in_block_file'} = 1;
                my $block_name = '';
                while ($file =~ s|/([^/.]+)$||) {
                    $block_name = length($block_name) ? "$1/$block_name" : $1;
                    my $ref = eval { $self->load_parsed_tree($file) } || next;
                    my $_tree = $ref->{'tree'};
                    foreach my $node (@$_tree) {
                        next if ! ref $node;
                        next if $block_name ne $node->[3];
                        $doc->{'content'} = $ref->{'content'};
                        $doc->{'tree'}    = $node->[4];
                        $doc->{'modtime'} = $ref->{'modtime'};
                        $file = $ref->{'name'};
                        last;
                    }
                }
                die $err if ! $doc->{'tree'};
            } elsif ($self->{'DEFAULT'}) {
                $doc->{'filename'} = eval { $self->include_filename($self->{'DEFAULT'}) } || die $err;
            } else {
                die $err;
            }
        }

        ### no tree yet - look for a file cache
        if (! $doc->{'tree'}) {
            $doc->{'modtime'} = (stat $doc->{'filename'})[9];
            if  ($self->{'COMPILE_DIR'} || $self->{'COMPILE_EXT'}) {
                if ($self->{'COMPILE_DIR'}) {
                    $doc->{'compile_filename'} = $self->{'COMPILE_DIR'} .'/'. $file;
                } else {
                    $doc->{'compile_filename'} = $doc->{'filename'};
                }
                $doc->{'compile_filename'} .= $self->{'COMPILE_EXT'} if defined($self->{'COMPILE_EXT'});
                $doc->{'compile_filename'} .= $EXTRA_COMPILE_EXT       if defined $EXTRA_COMPILE_EXT;

                if (-e $doc->{'compile_filename'} && (stat _)[9] == $doc->{'modtime'}) {
                    require Storable;
                    $doc->{'tree'} = Storable::retrieve($doc->{'compile_filename'});
                    $doc->{'compile_was_used'} = 1;
                } else {
                    my $str = $self->slurp($doc->{'filename'});
                    $doc->{'content'}  = \$str;
                }
            } else {
                my $str = $self->slurp($doc->{'filename'});
                $doc->{'content'}  = \$str;
            }
        }

    }

    ### haven't found a parsed tree yet
    if (! $doc->{'tree'}) {
        if ($self->{'CONSTANTS'}) {
            my $key = $self->{'CONSTANT_NAMESPACE'} || 'constants';
            $self->{'NAMESPACE'}->{$key} ||= $self->{'CONSTANTS'};
        }

        local $self->{'_template'} = $doc;
        $doc->{'tree'} = $self->parse_tree($doc->{'content'}); # errors die
    }

    ### cache parsed_tree in memory unless asked not to do so
    if (! $doc->{'is_ref'} && (! defined($self->{'CACHE_SIZE'}) || $self->{'CACHE_SIZE'})) {
        $self->{'_documents'}->{$file} ||= $doc;

        ### allow for config option to keep the cache size down
        if ($self->{'CACHE_SIZE'}) {
            $doc->{'cache_time'} = time;
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
    if ($doc->{'compile_filename'} && ! $doc->{'compile_was_used'}) {
        my $dir = $doc->{'compile_filename'};
        $dir =~ s|/[^/]+$||;
        if (! -d $dir) {
            require File::Path;
            File::Path::mkpath($dir);
        }
        require Storable;
        Storable::store($doc->{'tree'}, $doc->{'compile_filename'});
        utime $doc->{'modtime'}, $doc->{'modtime'}, $doc->{'compile_filename'};
    }

    return $doc;
}

sub parse_tree {
    my $self    = shift;
    my $str_ref = shift;
    if (! $str_ref || ! defined $$str_ref) {
        $self->throw('parse.no_string', "No string or undefined during parse");
    }

    my $STYLE = $self->{'TAG_STYLE'} || 'template';
    my $START = $self->{'START_TAG'} || $TAGS->{$STYLE}->[0];
    my $END   = $self->{'END_TAG'}   || $TAGS->{$STYLE}->[1];
    my $len_s = length $START;
    my $len_e = length $END;

    my @tree;             # the parsed tree
    my $pointer = \@tree; # pointer to current tree to handle nested blocks
    my @state;            # maintain block levels
    local $self->{'_state'} = \@state; # allow for items to introspect (usually BLOCKS)
    my @move_to_front;    # items that need to be declared first
    my $i = 0;            # start index
    my $j = 0;            # end index
    my $last = 0;         # previous end index
    my $post_chomp = 0;   # previous post_chomp setting
    my $continue;         # multiple directives in the same tag
    my $postop;           # found a post-operative DIRECTIVE
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
            last if $i == -1;
            if ($last != $i) {
                my $text  = substr($$str_ref, $last, $i - $last);
                my $_last = $last;
                if ($post_chomp) {
                    if    ($post_chomp == 1) { $_last += length($1)     if $text =~ s{ ^ ([^\S\n]* \n) }{}x  }
                    elsif ($post_chomp == 2) { $_last += length($1) + 1 if $text =~ s{ ^ ([^\S\n]* \n) }{ }x }
                    elsif ($post_chomp == 3) { $_last += length($1)     if $text =~ s{ ^ (\s+)         }{}x  }
                    elsif ($post_chomp == 4) { $_last += length($1) + 1 if $text =~ s{ ^ (\s+)         }{ }x }
                }
                if (length $text) {
                    push @$pointer, $text;
                    $self->interpolate_node($pointer, -1, $_last) if $self->{'INTERPOLATE'};
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

            ### take care of whitespace and comments
            my $pre_chomp = $tag =~ s{ ^ ([+=~^-]) }{}x ? $1 : $self->{'PRE_CHOMP'};
            $post_chomp   = $tag =~ s{ ([+=~^-]) $ }{}x ? $1 : $self->{'POST_CHOMP'};
            $pre_chomp  =~ y/-=~^+/12340/ if $pre_chomp;
            $post_chomp =~ y/-=~^+/12340/ if $post_chomp;
            if ($pre_chomp && $pointer->[-1] && ! ref $pointer->[-1]) {
                if    ($pre_chomp == 1) { $pointer->[-1] =~ s{ (?:\n|^) [^\S\n]* \z }{}x  }
                elsif ($pre_chomp == 2) { $pointer->[-1] =~ s{ (?:\n|^) [^\S\n]* \z }{ }x }
                elsif ($pre_chomp == 3) { $pointer->[-1] =~ s{             (\s+) \z }{}x  }
                elsif ($pre_chomp == 4) { $pointer->[-1] =~ s{             (\s+) \z }{ }x }
                splice(@$pointer, -1, 1, ()) if ! length $pointer->[-1]; # remove the node if it is zero length
            }
            if ($tag =~ /^\#/) { # leading # means to comment the entire section
                $node->[0] = '#';
                push @$pointer, $node;
                next;
            }
            $tag =~ s{ (?<! \\) \# .* $ }{}xmg; # remove trailing comments
            $tag =~ s{ ^ \s+ }{}x;
        }

        if (! length $tag) {
            undef $continue;
            undef $postop;
            next;
        }

        ### look for DIRECTIVES
        if ($tag =~ $QR_DIRECTIVE                         # find a word
            && ($func = $self->{'ANYCASE'} ? uc($1) : $1) # case ?
            && $DIRECTIVES->{$func} ) {                   # is it a directive
            $tag =~ s{ ^ \w+ \s* }{}x;
            $node->[0] = $func;

            ### store out this current node level
            if ($postop) { # on a post operator - replace the original node with the new one
                my @postop = @$postop;
                @$postop = @$node;
                $node = $postop;
                $node->[4] = [\@postop];
            } elsif ($capture) {
                # do nothing
            } else{
                push @$pointer, $node;
            }

            ### anything that behaves as a block ending
            if ($func eq 'END' || $DIRECTIVES->{$func}->[4]) {
                if ($#state == -1) {
                    $self->throw('parse', "Found an $func while not in a block", $node);
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

                ### restore the pointer up one level
                $pointer = ($#state == -1) ? \@tree : $state[0]->[4];

                ### normal end block
                if ($func eq 'END') {
                    if ($DIRECTIVES->{$parent_node->[0]}->[5]) { # move things like BLOCKS to front
                        push @move_to_front, $parent_node;
                        if ($pointer->[-1] && ! $pointer->[-1]->[6]) { # capturing doesn't remove the var
                            splice(@$pointer, -1, 1, ());
                        }
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
                    $tag =~ s{ ^ (\w+) \s* }{}x;
                    ($START, $END) = @{ $TAGS->{$1} };
                } elsif ($tag =~ s{ ^ (\S+) \s+ (\S+) \s* }{}x) {
                    ($START, $END) = ($1, $2);
                }
                $len_s = length $START;
                $len_e = length $END;

            } else {
                $node->[3] = eval { $DIRECTIVES->{$func}->[0]->($self, \$tag, $node) };
                if (my $err = $@) {
                    $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                    die $err;
                }
                if ($DIRECTIVES->{$func}->[2] && ! $postop) {
                    push @state, $node;
                    $pointer = $node->[4] ||= [];
                }
            }

        ### allow for bare variable getting and setting
        } elsif (defined(my $var = $self->parse_variable(\$tag))) {
            push @$pointer, $node;
            if ($tag =~ s{ ^ = \s* }{}x) {
                $node->[0] = 'SET';
                $node->[3] = eval { $DIRECTIVES->{'SET'}->[0]->($self, \$tag, $node, $var) };
                if (my $err = $@) {
                    $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                    die $err;
                }
            } else {
                #die "Found trailing info during variable access \"$tag" if $tag;
                $node->[0] = 'GET';
                $node->[3] = $var;
            }

        } else { # error
            my $all  = substr($$str_ref, $i + $len_s, $j - ($i + $len_s));
            $all =~ s/^\s+//;
            $all =~ s/\s+$//;
            $self->throw('parse', "Not sure how to handle tag \"$all\"");
        }

        ### we now have the directive to capture - store it
        if ($capture) {
            my $parent_node = $capture;
            push @{ $parent_node->[4] }, $node;
            undef $capture;
        }

        ### we are capturing the output of the next directive - set it up
        if ($node->[6]) {
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $postop    = undef;
            $capture   = $node;

        ### semi-colon = end of statement
        } elsif ($tag =~ s{ ^ ; \s* }{}x) {
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $postop    = undef;

        ### looking at a postoperator
        } elsif ($tag =~ $QR_DIRECTIVE                         # find a word
                 && ($func = $self->{'ANYCASE'} ? uc($1) : $1) # case ?
                 && $DIRECTIVES->{$func}                       # is it a directive
                 && $DIRECTIVES->{$func}->[3]) {               # it is a postoperative directive
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $postop    = $node;

        } else { # error
            $self->throw('parse', "Found trailing info \"$tag\"", $node) if length $tag;
            $continue = undef;
            $postop   = undef;
        }
    }

    if ($#move_to_front != -1) {
        unshift @tree, @move_to_front;
    }

    if ($#state >  -1) {
        $self->throw('parse.missing.end', "Missing END", $state[-1], 0);
    }

    ### pull off the last text portion - if any
    if ($last != length($$str_ref)) {
        my $text  = substr($$str_ref, $last, length($$str_ref) - $last);
        my $_last = $last;
        if ($post_chomp) {
            if    ($post_chomp == 1) { $_last += length($1)     if $text =~ s{ ^ ([^\S\n]* \n) }{}x  }
            elsif ($post_chomp == 2) { $_last += length($1) + 1 if $text =~ s{ ^ ([^\S\n]* \n) }{ }x }
            elsif ($post_chomp == 3) { $_last += length($1)     if $text =~ s{ ^ (\s+)         }{}x  }
            elsif ($post_chomp == 4) { $_last += length($1) + 1 if $text =~ s{ ^ (\s+)         }{ }x }
        }
        if (length $text) {
            push @$pointer, $text;
            $self->interpolate_node($pointer, -1, $_last) if $self->{'INTERPOLATE'};
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
            $$out_ref .= $node if defined $node;
            next;
        }

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
            $$str_ref =~ s{ ^ \s+ }{}x;
            return $str;
        }
    }

    my $copy = $$str_ref; # copy while parsing to allow for errors

    ### test for leading unary operators
    my $has_unary;
    if ($copy =~ s{ ^ ($QR_OP_UNARY) \s* }{}xo) {
        return if $ARGS->{'auto_quote'}; # auto_quoted thing was too complicated
        $has_unary = $1;
        if (! $OP_UNARY->{$has_unary}) {
            my $ref = $self->{'_op_unary_backref'} ||= {reverse %$OP_UNARY};
            $has_unary = $ref->{$has_unary} || $self->throw("Couldn't find canonical name of unary \"$1\"");
        }
    }

    my @var;
    my $is_literal;
    my $is_namespace;

    ### allow for numbers
    if ($copy =~ s{ ^ ( (?:\d*\.\d+ | \d+) ) \s* }{}x) {
        my $number = $1;
        push @var, \ $number;
        $is_literal = 1;

    ### looks like a normal variable start
    } elsif ($copy =~ s{ ^ (\w+) \s* }{}x) {
        push @var, $1;
        $is_namespace = 1 if $self->{'NAMESPACE'} && $self->{'NAMESPACE'}->{$1};

    ### allow for literal strings
    } elsif ($copy =~ s{ ^ ([\"\']) (|.*?[^\\]) \1 \s* }{}xs) {
        if ($1 eq "'") { # no interpolation on single quoted strings
            my $str = $2;
            push @var, \ $str;
            $is_literal = 1;
        } else {
            my @pieces = $ARGS->{'auto_quote'}
                ? split(m{ (\$\w+            | \$\{ [^\}]+ \}) }x, $2)  # autoquoted items get a single $\w+ - no nesting
                : split(m{ (\$\w+ (?:\.\w+)* | \$\{ [^\}]+ \}) }x, $2);
            my $n = 0;
            foreach my $piece (@pieces) {
                next if ! ($n++ % 2);
                next if $piece !~ m{ ^ \$ (\w+ (?:\.\w+)*) $ }x
                    && $piece !~ m{ ^ \$\{ \s* ([^\}]+) \} $ }x;
                my $name = $1;
                $piece = $self->parse_variable(\$name);
            }
            @pieces = grep {defined && length} @pieces;
            if ($#pieces == -1) {
                push @var, \ '';
                $is_literal = 1;
            } elsif ($#pieces == 0 && ! ref $pieces[0]) {
                push @var, \ $pieces[0];
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
    } elsif ($copy =~ s{ ^ \$ (\w+) \b \s* }{}x
        || $copy =~ s{ ^ \$\{ \s* ([^\}]+) \} \s* }{}x) {
        my $name = $1;
        push @var, $self->parse_variable(\$name);
        if ($ARGS->{'auto_quote'}){
            $$str_ref = $copy;
            return $var[-1];
        }

    ### looks like an array constructor
    } elsif ($copy =~ s{ ^ \[ \s* }{}x) {
        local $self->{'_operator_precedence'} = 0; # reset presedence
        my $arrayref = ['arrayref'];
        while (defined(my $var = $self->parse_variable(\$copy))) {
            push @$arrayref, $var;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \] \s* }{}x || $self->throw('parse.missing.square', "Missing close \]", undef,
                                                          length($$str_ref) - length($copy));
        push @var, \ $arrayref;

    ### looks like a hash constructor
    } elsif ($copy =~ s{ ^ \{ \s* }{}x) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $hashref = ['hashref'];
        while (defined(my $key = $self->parse_variable(\$copy, {auto_quote => qr{ ^ (\w+) $QR_AQ_NOTDOT }xo}))) {
            $copy =~ s{ ^ => \s* }{}x;
            my $val = $self->parse_variable(\$copy);
            push @$hashref, $key, $val;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \} \s* }{}x || $self->throw('parse.missing.curly', "Missing close \} ($copy)", undef,
                                                          length($$str_ref) - length($copy));
        push @var, \ $hashref;

    ### looks like a paren grouper
    } elsif ($copy =~ s{ ^ \( \s* }{}x) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $var = $self->parse_variable(\$copy);
        $copy =~ s{ ^ \) \s* }{}x || $self->throw('parse.missing.paren', "Missing close \)", undef,
                                                          length($$str_ref) - length($copy));
        @var = @$var;
        pop(@var); # pull off the trailing args of the paren group

    ### nothing to find - return failure
    } else {
        return;
    }

    return if $ARGS->{'auto_quote'}; # auto_quoted thing was too complicated

    ### looks for args for the initial
    if ($copy =~ s{ ^ \( \s* }{}x) {
        local $self->{'_operator_precedence'} = 0; # reset precedence
        my $args = $self->parse_args(\$copy);
        $copy =~ s{ ^ \) \s* }{}x || $self->throw('parse.missing.paren', "Missing close \)", undef,
                                                          length($$str_ref) - length($copy));
        push @var, $args;
    } else {
        push @var, 0;
    }

    ### allow for nested items
    while ($copy =~ s{ ^ ( \.(?!\.) | \|(?!\|) ) \s* }{}x) {
        push @var, $1;

        ### allow for interpolated variables in the middle - one.$foo.two or one.${foo.bar}.two
        if ($copy =~ s{ ^ \$(\w+) \s* }{}x
            || $copy =~ s{ ^ \$\{ \s* ([^\}]+)\} \s* }{}x) {
            my $name = $1;
            my $var = $self->parse_variable(\$name);
            push @var, $var;
        } elsif ($copy =~ s{ ^ (\w+) \s* }{}x) {
            push @var, $1;
        } else {
            $self->throw('parse', "Not sure how to continue parsing on \"$copy\" ($$str_ref)");
        }

        ### looks for args for the nested item
        if ($copy =~ s{ ^ \( \s* }{}x) {
            local $self->{'_operator_precedence'} = 0; # reset precedence
            my $args = $self->parse_args(\$copy);
            $copy =~ s{ ^ \) \s* }{}x || $self->throw('parse.missing.paren', "Missing close \)", undef,
                                                              length($$str_ref) - length($copy));
            push @var, $args;
        } else {
            push @var, 0;
        }

    }

    ### flatten literals and constants as much as possible
    my $var = ($is_literal && $#var == 1) ? ${ $var[0] }
            : $is_namespace               ? $self->vivify_variable(\@var, {is_namespace_during_compile => 1})
            :                               \@var;

    ### allow for all "operators"
    if (! $self->{'_operator_precedence'}) {
        my $tree;
        my $found;
        while ($copy =~ s{ ^ ($QR_OP) \s* }{}ox) {
            local $self->{'_operator_precedence'} = 1;
            my $op   = $1;
            my $var2 = $self->parse_variable(\$copy);
            ### allow for unary operator precedence
            if ($has_unary && ($OPERATORS->{$op} && $OPERATORS->{$op} < $OPERATORS->{$has_unary})) {
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
                my $op_unary = $OP_UNARY->{$has_unary} || $has_unary;
                $var = [ \ [ $op_unary, $var ], 0 ];
                undef $has_unary;
            }

            ### add the operator to the tree
            push (@{ $tree ||= [] }, $op, $var2);
            $found->{$op} = $OPERATORS->{$op} if exists $OPERATORS->{$op};
        }

        ### if we found operators - tree the nodes by operator precedence
        if ($tree) {
            if ($#$tree == 1) { # only one operator - keep simple things fast
                $var = [\ [$tree->[0], $var, $tree->[1]], 0];
            } else {
                unshift @$tree, $var;
                $var = $self->apply_precedence($tree, $found);
            }
        }
    }

    ### allow for unary on non-chained variables
    if ($has_unary) {
        my $op_unary = $OP_UNARY->{$has_unary} || $has_unary;
        $var = [ \ [ $op_unary, $var ], 0 ];
    }

#    debug $var, $copy;
    $$str_ref = $copy; # commit the changes
    return $var;
}

sub apply_precedence {
    my ($self, $tree, $found) = @_;

    my @var;
    my $trees;
    for my $op (sort {$found->{$a} <=> $found->{$b}} keys %$found) {
        local $found->{$op};
        delete $found->{$op};
        my @trees;
        my @trinary;
        for (my $i = 0; $i <= $#$tree; $i ++) {
            next if $tree->[$i] ne $op && (! exists $OP_TRINARY->{$op} || $OP_TRINARY->{$op} ne $tree->[$i]);
            push @trees, [splice @$tree, 0, $i, ()]; # everything up to the operator
            push @trinary, $tree->[0] if exists $OP_TRINARY->{$op};
            shift @$tree; # pull off the operator
            $i = -1;
        }
        next if $#trees == -1;
        push @trees, $tree if $#$tree != -1; # elements after last operator
        for (@trees) {
            if ($#$_ == 0) {
                $_ = $_->[0]; # single item - its not a tree
            } elsif ($#$_ == 2) {
                $_ = [ \ [ $_->[1], $_->[0], $_->[2] ], 0 ]; # single operator - put it straight on
            } else {
                $_ = $self->apply_precedence($_, $found); # more complicated - recurse
            }
        }

        return [ \ [ $op, @trees ], 0 ] if $#trinary <= 1;

        ### reorder complex trinary - rare case
        while ($#trinary >= 1) {
            ### if we look - starting from the back the first lead trinary op will always be next to its matching op
            for (my $i = $#trinary; $i >= 0; $i --) {
                next if ! exists $OP_TRINARY->{$trinary[$i]};
                my ($op, $op2) = splice @trinary, $i, 2, (); # remove the pair of operators
                my $node = [ \ [$op, @trees[$i .. $i + 2] ], 0 ];
                splice @trees, $i, 3, $node;
            }
        }
        return $trees[0]; # at this point the trinary has been reduced to a single operator

    }

    $self->throw('parse', "Couldn't apply precedence");
}


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
            && $copy =~ s{ ^ =>? \s* }{}x) {
            $self->throw('parse', 'Named arguments not allowed') if $ARGS->{'positional_only'};
            my $val = $self->parse_variable(\$copy);
            $copy =~ s{ ^ , \s* }{}x;
            push @named, $name, $val;
            $$str_ref = $copy;
        } elsif (defined(my $arg = $self->parse_variable($str_ref))) {
            push @args, $arg;
            $$str_ref =~ s{ ^ , \s* }{}x;
        } else {
            last;
        }
    }

    ### allow for named arguments to be added also
    push @args, [\ ['hashref', @named], 0] if $#named != -1;

    return \@args;
}

sub interpolate_node {
    my ($self, $tree, $index, $offset) = @_;

    my @pieces = split m{ (\$\w+ (?:\.\w+)* | \$\{ [^\}]+ \}) }x, $tree->[$index];
    return if $#pieces <= 0;

    my @sub_tree;
    my $n = 0;
    foreach my $piece (@pieces) {
        if (! ($n++ % 2)) {
            next if ! length $piece;
            push @sub_tree, $piece;
        } elsif ($piece =~ m{ ^ \$ (\w+ (?:\.\w+)*) $ }x
                 || $piece =~ m{ ^ \$\{ \s* ([^\}]+) \} $ }x) {
            my $name = $1;
            push @sub_tree, ['GET', $offset, $offset + length($piece), $self->parse_variable(\$name)];
        } else {
            $self->throw('parse', "Parse error during interpolate node");
        }
        $offset += length $piece;
    }

    ### replace the tree
    splice @$tree, -1, 1, @sub_tree;
}

###----------------------------------------------------------------###

sub vivify_variable {
    ### allow for the parse tree to store literals
    return $_[1] if ! ref $_[1];

    my $self = shift;
    my $var  = shift;
    my $ARGS = shift || {};
    my $i    = 0;
    my $generated_list;

    ### determine the top level of this particular variable access
    my $ref  = $var->[$i++];
    my $args = $var->[$i++];
    if (ref $ref) {
        if (ref($ref) eq 'SCALAR') { # a scalar literal
            return if $ARGS->{'set_var'};
            $ref = $$ref;
        } elsif (ref($ref) eq 'REF') { # operator
            return if $ARGS->{'set_var'};
            $generated_list = 1 if ${ $ref }->[0] eq '..';
            $ref = $self->play_operator($$ref);
        } else { # a named variable access (ie via $name.foo)
            $ref = $self->vivify_variable($ref);
            if (defined $ref) {
                return if $ref =~ /^_/; # don't allow vars that begin with _
                if ($ARGS->{'set_var'}) {
                    if ($#$var <= $i) {
                        $self->{'_vars'}->{$ref} = $ARGS->{'var_val'};
                        return;
                    } else {
                        $self->{'_vars'}->{$ref} ||= {};
                    }
                }
                $ref = $self->{'_vars'}->{$ref};
            } else {
                return if $ARGS->{'set_var'};
            }
        }
    } elsif (defined $ref) {
        if ($ARGS->{'is_namespace_during_compile'}) {
            $ref = $self->{'NAMESPACE'}->{$ref};
        } else {
            return if $ref =~ /^_/; # don't allow vars that begin with _
            if ($ARGS->{'set_var'}) {
                if ($#$var <= $i) {
                    $self->{'_vars'}->{$ref} = $ARGS->{'var_val'};
                    return;
                } else {
                    $self->{'_vars'}->{$ref} ||= {};
                }
            }
            $ref = $self->{'_vars'}->{$ref};
        }
    }

    ### let the top level thing be a code block
    if (UNIVERSAL::isa($ref, 'CODE')) {
        return if $ARGS->{'set_var'} && $#$var <= $i;
        my @results = $ref->($args ? @{ $self->vivify_args($args) } : ());
        if (defined $results[0]) {
            $ref = ($#results > 0) ? \@results : $results[0];
        } elsif (defined $results[1]) {
            die $results[1]; # grr - TT behavior - why not just throw in the plugin ?
        } else {
            $ref = undef;
        }
    }

    ### vivify the chained levels
    my $seen_filters;
    while (defined $ref && $#$var > $i) {
        my $was_dot_call = $var->[$i++] eq '.';
        my $name         = $var->[$i++];
        my $args         = $var->[$i++];


        ### allow for named portions of a variable name (foo.$name.bar)
        if (ref $name) {
            if (ref($name) eq 'SCALAR') {
                die "Shouldn't get a SCALAR during a vivify on chain";
            } elsif (ref($name) eq 'REF') {
                die "Shouldn't get a REF during a vivify on chain";
            } else {
                $name = $self->vivify_variable($name);
                if (! defined $name) {
                    $ref = undef;
                    next;
                }
            }
        }

        ### method calls on objects
        if (UNIVERSAL::can($ref, 'can')) {
            my @results = eval { $ref->$name($args ? @{ $self->vivify_args($args) } : ()) };
            if (! $@) {
                if (defined $results[0]) {
                    $ref = ($#results > 0) ? \@results : $results[0];
                } elsif (defined $results[1]) {
                    die $results[1]; # grr - TT behavior - why not just throw ?
                } else {
                    $ref = undef;
                }
                next;
            }
            die $@ if ref $@ || $@ !~ /Can\'t locate object method/;
            # fall on down to "normal" accessors
        }

        ### hash member access
        if (UNIVERSAL::isa($ref, 'HASH')) {
            return if $name =~ /^_/; # don't allow vars that begin with _
            if ($ARGS->{'set_var'}) {
                if ($#$var <= $i) {
                    $ref->{$name} = $ARGS->{'var_val'};
                    return;
                } else {
                    $ref = $ref->{$name} ||= {};
                    next;
                }
            } elsif ($was_dot_call && exists($ref->{$name}) ) {
                $ref = $ref->{$name};
            } elsif (my $code = $self->hash_op($name)) {
                return if $ARGS->{'set_var'};
                $ref = $code->($ref, $args ? @{ $self->vivify_args($args) } : ());
                next;
            } elsif ($ARGS->{'is_namespace_during_compile'}) {
                return $var; # abort - can't fold namespace variable
            } else {
                $ref = undef;
            }

        ### array access
        } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
            if ($name =~ /^\d+$/) {
                if ($ARGS->{'set_var'}) {
                    if ($#$var <= $i) {
                        $ref->[$name] = $ARGS->{'var_val'};
                        return;
                    } else {
                        $ref = $ref->[$name] ||= {};
                        next;
                    }
                } elsif ($name <= $#$ref) {
                    $ref = $ref->[$name];
                } else {
                    $ref = undef;
                }
            } elsif (my $code = $self->list_op($name)) {
                return if $ARGS->{'set_var'};
                $ref = $code->($ref, $args ? @{ $self->vivify_args($args) } : ());
                next;
            } else {
                $ref = undef;
            }

        ### scalar access
        } elsif (! ref($ref) && defined($ref)) {
            if (UNIVERSAL::isa($name, 'CODE')) { ### looks like a filter access
                $ref = $name->($ref);
            } elsif (my $code = $self->scalar_op($name)) {
                return if $ARGS->{'set_var'};
                $ref = $code->($ref, $args ? @{ $self->vivify_args($args) } : ());
                next;
            } elsif ($code = $self->list_op($name)) {
                return if $ARGS->{'set_var'};
                $ref = $code->([$ref], $args ? @{ $self->vivify_args($args) } : ());
                next;
            } elsif (my $filter = $self->{'FILTERS'}->{$name} || $self->list_filters->{$name}) {
                return if $ARGS->{'set_var'};
                $filter = [$filter, 0] if UNIVERSAL::isa($filter, 'CODE');
                if ($#$filter == 1 && UNIVERSAL::isa($filter->[0], 'CODE')) { # these are the TT style filters
                    my $sub = $filter->[0];
                    if ($filter->[1]) {
                        $sub = $sub->({}, $args ? @{ $self->vivify_args($args) } : ()); # dynamic filter (empty context)
                    }
                    $ref = $sub->($ref);
                } else { # this looks like our vmethods turned into "filters"
                    $seen_filters ||= {};
                    if ($seen_filters->{$name} ++) {
                        $ref = undef;
                        next;
                    }
                    $var = ['|', @$filter, splice(@$var, $i)];
                    $i = 0;
                }
            } else {
                $ref = undef;
            }
        }

        ### check at each point if the rurned thing was a code
        if (defined($ref) && UNIVERSAL::isa($ref, 'CODE')) {
            my @results = $ref->($args ? @{ $self->vivify_args($args) } : ());
            if (defined $results[0]) {
                $ref = ($#results > 0) ? \@results : $results[0];
            } elsif (defined $results[1]) {
                die $results[1]; # grr - TT behavior - why not just throw ?
            } else {
                $ref = undef;
            }
        }

    }

    #debug $ref;

    if ($generated_list && $ARGS->{'list_context'} && UNIVERSAL::isa($ref, 'ARRAY')) {
        return @$ref;
    }
    return $ref;
}

sub vivify_args {
    my $self = shift;
    my $vars = shift;
    my $args = shift || {};
    return [map {$self->vivify_variable($_, $args)} @$vars];
}

###----------------------------------------------------------------###

sub play_operator {
    my $self = shift;
    my $tree = shift;
    my $ARGS = shift || {};
    my $op = $tree->[0];
    $tree = [@$tree[1..$#$tree]];

    ### allow for operator function override
    if (exists $OP_FUNC->{$op}) {
        return $OP_FUNC->{$op}->($self, $op, $tree, $ARGS);
    }

    ### do constructors and short-circuitable operators
    if ($op eq '~' || $op eq '_') {
        return join "", grep {defined} @{ $self->vivify_args($tree) };
    } elsif ($op eq 'arrayref') {
        return $self->vivify_args($tree, {list_context => 1});
    } elsif ($op eq 'hashref') {
        my $args = $self->vivify_args($tree);
        push @$args, undef if ! ($#$args % 2);
        return {@$args};
    } elsif ($op eq '?') {
        if ($self->vivify_variable($tree->[0])) {
            return defined($tree->[1]) ? $self->vivify_variable($tree->[1]) : undef;
        } else {
            return defined($tree->[2]) ? $self->vivify_variable($tree->[2]) : undef;
        }
    } elsif ($op eq '||' || $op eq 'or' || $op eq 'OR') {
        for my $node (@$tree) {
            my $var = $self->vivify_variable($node);
            return $var if $var;
        }
        return '';
    } elsif ($op eq '&&' || $op eq 'and' || $op eq 'AND') {
        my $var;
        for my $node (@$tree) {
            $var = $self->vivify_variable($node);
            return 0 if ! $var;
        }
        return $var;
    } elsif ($op eq '!') {
        my $var = ! $self->vivify_variable($tree->[0]);
        return defined($var) ? $var : '';
    }

    ### equality operators
    local $^W = 0;
    my $n = $self->vivify_variable($tree->[0]);
    $tree = [@$tree[1..$#$tree]];
    if ($op eq '==')    { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n eq $_) }; return 1 }
    elsif ($op eq '!=') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n ne $_) }; return 1 }
    elsif ($op eq 'eq') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n eq $_) }; return 1 }
    elsif ($op eq 'ne') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n ne $_) }; return 1 }
    elsif ($op eq '<')  { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n <  $_); $n = $_ }; return 1 }
    elsif ($op eq '>')  { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n >  $_); $n = $_ }; return 1 }
    elsif ($op eq '<=') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n <= $_); $n = $_ }; return 1 }
    elsif ($op eq '>=') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n >= $_); $n = $_ }; return 1 }
    elsif ($op eq 'lt') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n lt $_); $n = $_ }; return 1 }
    elsif ($op eq 'gt') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n gt $_); $n = $_ }; return 1 }
    elsif ($op eq 'le') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n le $_); $n = $_ }; return 1 }
    elsif ($op eq 'ge') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n ge $_); $n = $_ }; return 1 }

    ### numeric operators
    my $args = $self->vivify_args($tree);
    if ($#$args == -1) {
        if ($op eq '-') { return - $n }
        $self->throw('operator', "Not enough args for operator \"$op\"");
    }
    if ($op eq '..')        { return [($n||0) .. ($args->[-1]||0)] }
    elsif ($op eq '+')      { $n +=  $_ for @$args; return $n }
    elsif ($op eq '-')      { $n -=  $_ for @$args; return $n }
    elsif ($op eq '*')      { $n *=  $_ for @$args; return $n }
    elsif ($op eq '/')      { $n /=  $_ for @$args; return $n }
    elsif ($op eq 'div'
           || $op eq 'DIV') { $n = int($n / $_) for @$args; return $n }
    elsif ($op eq '%'
           || $op eq 'mod'
           || $op eq 'MOD') { $n %=  $_ for @$args; return $n }
    elsif ($op eq '**'
           || $op eq 'pow') { $n **= $_ for @$args; return $n }

    $self->throw('operator', "Un-implemented operation $op");
}

###----------------------------------------------------------------###

sub scalar_op {
    my ($self, $name) = @_;
    $SCALAR_OPS->{$name} = $_[2] if $#_ == 2;
    return $SCALAR_OPS->{$name};
}

sub list_op {
    my ($self, $name) = @_;
    $LIST_OPS->{$name} = $_[2] if $#_ == 2;
    return $LIST_OPS->{$name};
}

sub hash_op {
    my ($self, $name) = @_;
    $HASH_OPS->{$name} = $_[2] if $#_ == 2;
    return $HASH_OPS->{$name};
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
        $block_name = join("/", @names, $block_name) if $#names > -1;
    }

    return $block_name;
}

sub play_BLOCK {
    my ($self, $block_name, $node, $out_ref) = @_;

    ### store a named reference - but do nothing until something processes it
    $self->{'BLOCKS'}->{$block_name} = {
        tree    => $node->[4],
        name    => $self->{'_template'}->{'name'} .'/'. $block_name,
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
    $self->throw($node->[0], 'Control exception', $node);
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
        my $val = $self->vivify_variable($set);
        if (! $val) {
            $default = defined($default) ? $self->vivify_variable($default) : '';
            $self->vivify_variable($set, {
                set_var => 1,
                var_val => $default,
            });
        }
    }
    return;
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

    return '' if $#$filter == -1;

    $self->{'FILTERS'}->{$name} = $filter if length $name;

    my $sub_tree = $node->[4];

    ### play the block
    my $out = '';
    eval { $self->execute_tree($sub_tree, \$out) };
    die $@ if $@ && ! UNIVERSAL::isa($@, 'CGI::Ex::Template::Exception');

    my $var = [\$out, 0, '|', @$filter]; # make a temporary var out of it

    return $DIRECTIVES->{'GET'}->[1]->($self, $var, $node, $out_ref);
}

sub parse_FOREACH {
    my ($self, $tag_ref) = @_;
    my $items = $self->parse_variable($tag_ref);
    my $var;
    if ($$tag_ref =~ s{ ^ (= | IN\b) \s* }{}x) {
        $var = [@$items];
        $items = $self->parse_variable($tag_ref);
    }
    return [$var, $items];
}

sub play_FOREACH {
    my ($self, $ref, $node, $out_ref) = @_;

    ### get the items - make sure it is an arrayref
    my ($var, $items) = @$ref;

    $items = $self->vivify_variable($items);
    return '' if ! defined $items;

    if (! UNIVERSAL::isa($items, 'CGI::Ex::Template::Iterator')) {
        $items = CGI::Ex::Template::Iterator->new($items);
    }

    my $sub_tree = $node->[4];
    my $vals     = $items->items;

    ### if the FOREACH tag sets a var - then nothing gets localized
    if (defined $var) {
        $self->{'_vars'}->{'loop'} = $items;
        foreach my $i ($items->index .. $#$vals) {
            $items->index($i);
            my $item = $vals->[$i];

            $self->vivify_variable($var, {
                set_var => 1,
                var_val => $item,
            });


            ### execute the sub tree
            eval { $self->execute_tree($sub_tree, $out_ref) };
            if (my $err = $@) {
                if (UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception')) {
                    next if $err->type =~ /NEXT/;
                    last if $err->type =~ /LAST|BREAK/;
                }
                die $err;
            }
        }
    ### if the FOREACH tag doesn't set a var - then everything gets localized
    } else {

        ### localize variable access for the foreach
        my $swap = $self->{'_vars'};
        local $self->{'_vars'} = my $copy = {%$swap};
        $copy->{'loop'} = $items;

        ### iterate use the iterator object
        #foreach (my $i = $items->index; $i <= $#$vals; $items->index(++ $i)) {
        foreach my $i ($items->index .. $#$vals) {
            $items->index($i);
            my $item = $vals->[$i];

            if (ref($item) eq 'HASH') {
                @$copy{keys %$item} = values %$item;
            }

            ### execute the sub tree
            eval { $self->execute_tree($sub_tree, $out_ref) };
            if (my $err = $@) {
                if (UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception')) {
                    next if $err->type =~ /NEXT/;
                    last if $err->type =~ /LAST|BREAK/;
                }
                die $err;
            }
        }

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
    my ($self, $ident) = @_;
    my $var = $self->vivify_variable($ident);
    $var = $self->undefined($ident) if ! defined $var;
    my $ref = ref $var;
    return $var if ! $ref;
    return '' if $ref eq 'ARRAY' || $ref eq 'SCALAR' || $ref eq 'HASH';
    return $var;
}

sub parse_IF {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref);
    return $ref;
}

sub play_IF {
    my ($self, $var, $node, $out_ref) = @_;

    my $val = $self->vivify_variable($var);
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
        my $val = $self->vivify_variable($var);
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

sub parse_INSERT {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref, {auto_quote => qr{ ^ ($QR_FILENAME) $QR_AQ_SPACE }xo});
    return $ref;
}

sub play_INSERT {
    my ($self, $var) = @_;
    return '' if ! $var;
    my $filename = $self->vivify_variable($var);

    return $self->include_file($filename);
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
        $self->vivify_var($name, {set_var => 1, var_val => undef});
        return;
    } elsif ($sub_tree->[0]->[0] eq 'BLOCK') {
        $sub_tree = $sub_tree->[0]->[4];
    }

    my $self_copy; # get a copy of self without circular refs
    if (eval { require Scalar::Util }
        && defined &Scalar::Util::weaken) {
        $self_copy = $self;
        Scalar::Util::weaken($self_copy);
    } else {
        $self_copy = bless {%$self}, ref($self); # hackish way to avoid circular refs on old perls (pre 5.8)
    }

    ### install a closure in the stash that will handle the macro
    $self->vivify_variable($name, {
        set_var => 1,
        var_val => sub {
            ### macros localize
            my $copy = $self_copy->{'_vars'};
            local $self_copy->{'_vars'}= {%$copy};

            ### set arguments
            my $named = pop(@_) if $_[-1] && UNIVERSAL::isa($_[-1],'HASH') && $#_ > $#$args;
            my @positional = @_;
            foreach my $var (@$args) {
                $self_copy->vivify_variable($var, {set_var => 1, var_val => shift(@positional)});
            }
            foreach my $name (sort keys %$named) {
                $self_copy->vivify_variable([$name, 0], {set_var => 1, var_val => $named->{$name}});
            }

            ### finally - run the sub tree
            my $out = '';
            $self_copy->execute_tree($sub_tree, \$out);
            return $out;
        },
    });

    return;
}

sub parse_META {
    my ($self, $tag_ref) = @_;

    return $self->parse_args($tag_ref);
}

sub play_META {
    my ($self, $args) = @_;
    return if ! $args;
    return if ! $self->{'_top_level'};

    my $hash = $self->vivify_args($args)->[-1] || return;
    return if ! UNIVERSAL::isa($hash, 'HASH');

    my $ref = $self->{'_vars'}->{'template'} ||= {};
    foreach my $key (keys %$hash) {
        next if $key eq 'name' || $key eq 'modtime';
        $ref->{$key} = $hash->{$key};
    }

    return;
}

sub parse_PERL {}

sub play_PERL {
    my ($self, $info, $node, $out_ref) = @_;
    $self->throw('perl', 'EVAL_PERL not set') if ! $self->{'EVAL_PERL'};

    ### fill in any variables
    my $perl = $node->[4] || return;
    my $out  = '';
    $self->execute_tree($perl, \$out);

    ### setup a fake handle
    my $handle = $self->eval_perl_handle($out_ref);
    local *OUTPUT = $handle;
    my $old_fh = select OUTPUT;

    ### try the code
    eval { eval $out };
    my $err = $@;

    ### put the handle back
    select $old_fh;

    if ($err) {
        $err = $self->exception('undef', $err) if ! UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception');
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
        if ($$tag_ref !~ s{ ^ = \s* }{}x) {
            $self->throw('parse.missing.equals', 'Missing equals while parsing args');
        }
        my $val = $self->parse_variable($tag_ref);
        push @{$info->[1]}, [$var, $val];
    }

    return $info;
}

sub play_PROCESS {
    my ($self, $info, $node, $out_ref) = @_;

    my ($files, $args) = @$info;

    ### set passed args
    foreach (@$args) {
        my ($key, $val) = @$_;
        $val = $self->vivify_variable($val);
        $self->vivify_variable($key, {
            set_var => 1,
            var_val => $val,
        });
    }

    ### iterate on any passed block or filename
    foreach my $ref (@$files) {
        next if ! defined $ref;
        my $filename = $self->vivify_variable($ref);

        my $out = '';
        eval {
            $self->_swap($filename, $self->{'_vars'}, \$out); # restart the swap - passing it our current stash
        };
        $$out_ref .= $out if length $out;
        if (my $err = $@) {
            die $err if ! UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception') || $err->type !~ /RETURN/;
        }
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
            $get_val = $$tag_ref =~ s{ ^ = \s* }{}x;
        }
        if (! $get_val) { # no next val
            $val = undef;
        } elsif ($$tag_ref =~ $QR_DIRECTIVE                    # find a word
                 && ($func = $self->{'ANYCASE'} ? uc($1) : $1) # case ?
                 && $DIRECTIVES->{$func}) {                    # is it a directive - if so set up capturing
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
            $val = $self->vivify_variable($val);
        }

        $self->vivify_variable($set, {
            set_var => 1,
            var_val => $val,
        });
    }
    return;
}

sub parse_SWITCH { $DIRECTIVES->{'GET'}->[0]->(@_) }

sub play_SWITCH {
    my ($self, $var, $node, $out_ref) = @_;

    my $val = $self->vivify_variable($var);
    $val = '' if ! defined $val;
    ### $node->[4] is thrown away

    my $default;
    while ($node = $node->[5]) { # CASES
        my $var = $node->[3];
        if (! defined $var) {
            $default = $node->[4];
            next;
        }

        my $val2 = $self->vivify_variable($var);
        $val2 = [$val2] if ! UNIVERSAL::isa($val2, 'ARRAY');
        for my $test (@$val2) { # find matching values
            next if ! defined $val && defined $test;
            next if defined $val && ! defined $test;
            if ($val ne $test) { # check string wise first - then numerical
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
    my $msg  = $self->parse_variable($tag_ref);
    return [$name, $msg];
}

sub play_THROW {
    my ($self, $ref, $node) = @_;
    my ($name, $info) = @$ref;
    $name = $self->vivify_variable($name);
    $self->throw($name, $info, $node);
}

sub play_TRY {
    my ($self, $foo, $node, $out_ref) = @_;

    my $body_ref = $node->[4];
    eval { $self->execute_tree($body_ref, $out_ref) };
    my $err = $@;

    if (! $node->[5]) { # no catch or final
        return if ! $err; # no final block and no error
        $$out_ref = ''; # hack for tt behavior
        $self->throw('parse.missing', "Missing CATCH block", $node);
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
        my $name = $self->vivify_variable($node->[3]);
        $name = '' if ! defined $name;
        if ($type =~ / ^ \Q$name\E \b /x
            && (! defined($last_found) || length($last_found) < length($name))) { # more specific wins
            $catch_body_ref = $node->[4] || [];
            $last_found     = $name;
        }
    }

    ### play the best catch block
    if ($err) {
        die $err if ! $catch_body_ref;
        local $self->{'_vars'}->{'error'} = $err;
        local $self->{'_vars'}->{'e'}     = $err;
        $self->execute_tree($catch_body_ref, $out_ref);
    }

    ### the final block
    $self->execute_tree($final, $out_ref) if $final;

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
        && $copy =~ s{ ^ = \s* }{}x) {
        $var = $_var;
        $$tag_ref = $copy;
    }

    $copy = $$tag_ref;
    my $module = $self->parse_variable(\$copy, {auto_quote => qr{ ^ (\w+ (?: (?:\.|::) \w+)*) $QR_AQ_NOTDOT }xo});
    $self->throw('parse', "Missing plugin name while parsing $$tag_ref") if ! defined $module;
    $module =~ s/\./::/g;

    my $args;
    if ($copy =~ s{ ^ \( \s* }{}x) {
        $args = $self->parse_args(\$copy);
        $copy =~ s { ^ \) \s* }{}x || $self->throw('parse.missing', "Missing close ')'");
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
    my $package = $self->{'PLUGINS'}->{$module} ? $self->{'PLUGINS'}->{$module} : "${base}::${module}";
    my $require = "$package.pm";
    $require =~ s|::|/|g;

    ### try and load the module - fall back to bare module if allowed
    my $obj;
    if (eval {require $require}) {
        my $shape   = $package->load;
        my $context = bless {_template => $self}, 'CGI::Ex::Template::_Context'; # a fake context
        my @args    = $args ? @{ $self->vivify_args($args) } : ();
        $obj = $shape->new($context, @args);
    } elsif (my @packages = grep {lc($package) eq lc($_)} @{ $self->list_plugins({base => $base}) }) {
        foreach my $package (@packages) {
            my $require = "$package.pm";
            $require =~ s|::|/|g;
            eval {require $require} || next;
            my $shape   = $package->load;
            my $context = bless {_template => $self}, 'CGI::Ex::Template::_Context'; # a fake context
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
    if (! $obj) {
        my $err = $@ || "Unknown error while loading $module";
        $self->throw('plugin', $err);
    }

    ### all good
    $self->vivify_variable(\@var, {
        set_var => 1,
        var_val => $obj,
    });

    return;
}

sub parse_WHILE {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $var;
    my $var2;
    if ($copy =~ s{ ^ \( \s* }{}x
        && defined($var = $self->parse_variable(\$copy))
        && $copy =~ s{ ^ = \s* }{}x) {
        $var2 = $self->parse_variable(\$copy);
        $copy =~ s{ ^ \) \s* }{}x || $self->throw('parse', "Missing close \")\"");
        $$tag_ref = $copy;
    } else {
        $var2 = $self->parse_variable($tag_ref);
    }
    return [$var, $var2];
}

sub play_WHILE {
    my ($self, $ref, $node, $out_ref) = @_;

    ### get the items - make sure it is an arrayref
    my ($var, $var2) = @$ref;
    return '' if ! defined $var2;

    my $sub_tree = $node->[4];

    ### iterate use the iterator object
    my $max = $WHILE_MAX;
    while (--$max > 0) {

        my $value = $self->vivify_variable($var2);

        ### update vars as needed
        $self->vivify_variable($var, {
            set_var => 1,
            var_val => $value,
        }) if defined $var;

        last if ! $value;

        ### execute the sub tree
        eval { $self->execute_tree($sub_tree, $out_ref) };
        if (my $err = $@) {
            if (UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception')) {
                next if $err->type =~ /NEXT/;
                last if $err->type =~ /LAST|BREAK/;
            }
            die $err;
        }
    }

    return undef;
}

sub parse_WRAPPER { $DIRECTIVES->{'INCLUDE'}->[0]->(@_) }

sub play_WRAPPER {
    my ($self, $var, $node, $out_ref) = @_;
    my $sub_tree = $node->[4] || return;

    my $out = '';
    $self->execute_tree($sub_tree, \$out);

    local $self->{'_vars'}->{'content'} = $out;
    return $DIRECTIVES->{'INCLUDE'}->[1]->(@_)
}

###----------------------------------------------------------------###

sub stash {
    my $self = shift;
    $self->{'_vars'} = shift if $#_ == 0;
    return $self->{'_vars'} ||= {};
}

sub include_path {
    my $self = shift;
    return $self->{'INCLUDE_PATH'} ||= [];
}

sub include_filename {
    my ($self, $file) = @_;
    if ($file =~ m|^/|) {
        $self->throw('file', "$file ABSOLUTE paths disabled") if ! $self->{'ABSOLUTE'};
        return $file if -e $file;
    } elsif ($file =~ m|^\./|) {
        $self->throw('file', "$file RELATIVE paths disabled") if ! $self->{'RELATIVE'};
        return $file if -e $file;
    } else {
        my $paths = $self->include_path;
        if (! ref $paths) {
            my $delim = $self->{'DELIMITER'} || ':';
            $delim = ($delim eq ':' && $^O eq 'MSWin32') ? qr|:(?!/)| : qr|\Q$delim\E|;
            $paths = [split $delim, $paths];
        }
        foreach my $item (@$paths) {
            my @path = UNIVERSAL::isa($item, 'CODE')  ? $item->()
                     : UNIVERSAL::can($item, 'paths') ? $item->paths
                     :                                  ($item);
            @path = map {ref($_) ? @$_ : $_} @path;
            foreach my $path (@path) {
                return "$path/$file" if -e "$path/$file";
            }
        }
    }

    $self->throw('file', "$file: not found");
}

sub include_file {
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

sub process {
    my ($self, $in, $swap, $out) = @_;

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
        local $self->{'_start_top_level'} = 1;

        ### "enable" debugging - we only support DEBUG_DIRS
        local $self->{'_debug_dirs'} = $self->{'DEBUG'}
            && ($self->{'DEBUG'} =~ /^\d+$/ ? $self->{'DEBUG'} & 8 : $self->{'DEBUG'} =~ /dirs|all/);
        delete $self->{'_debug_off'};
        delete $self->{'_debug_format'};

        return $self->_swap($content, $copy, \$output);
    };
    if (my $err = $@) {
        if ($err->type !~ /STOP|RETURN|NEXT|LAST|BREAK/) {
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

###----------------------------------------------------------------###

sub exception {
    my $self = shift;
    my $pkg  = $self->{'exception_package'} || 'CGI::Ex::Template::Exception';
    return $pkg->new(@_);
}

sub throw { die shift->exception(@_) }

sub eval_perl_handle {
    my ($self, $out_ref) = @_;
    my $handle = CGI::Ex::Template::EvalPerlHandle->load($out_ref);
}

sub undefined { ''}

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

sub debug_node {
    my ($self, $node) = @_;
    my $doc = $self->{'_template'};
    my $i = $node->[1];
    my $j = $node->[2];
    $doc->{'content'} ||= do { my $s = $self->slurp($doc->{'filename'}) ; \$s };
    my $format = $self->{'_debug_format'} || $self->{'DEBUG_FORMAT'} || "\n## \$file line \$line : [% \$text %] ##\n";
    $format =~ s{\$file}{$doc->{name}}g;
    $format =~ s{\$line}{ $self->get_line_number_by_index($doc, $i) }eg;
    $format =~ s{\$text}{ my $s = substr(${ $doc->{'content'} }, $i, $j - $i); $s=~s/^\s+//; $s=~s/\s+$//; $s }eg;
    return $format;
}

sub get_line_number_by_index {
    my ($self, $doc, $index) = @_;
    ### get the line offsets for the doc
    my $lines = $doc->{'line_offsets'} ||= do {
        $doc->{'content'} ||= do { my $s = $self->slurp($doc->{'filename'}) ; \$s };
        my $i = 0;
        my @lines = (0);
        while (1) {
            $i = index(${ $doc->{'content'} }, "\n", $i) + 1;
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
### long virtual methods

sub vmethod_indent {
    my $str = shift; $str = '' if ! defined $str;
    my $n   = shift; $n   = 4  if ! defined $n;
    my $ind = ' ' x $n;
    $str =~ s/^/$ind/mg;
    return $str;
}

sub vmethod_match {
    my ($str, $pat, $global) = @_;
    return [] if ! defined $str || ! defined $pat;
    return [$str =~ /$pat/g] if $global;
    return [$str =~ /$pat/ ];
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

sub vmethod_split {
    my ($str, $pat, @args) = @_;
    $str = ''  if ! defined $str;
    $pat = ' ' if ! defined $pat;
    return [split $pat, $str, @args];
}

###----------------------------------------------------------------###

package CGI::Ex::Template::Exception;

use overload '""' => \&as_string;
use overload bool => sub { defined shift };

sub new {
    my ($class, $type, $msg, $node, $pos, $str_ref) = @_;
    return bless [$type, $msg, $node, $pos, $str_ref], $class;
}

sub type { shift->[0] }

sub info { shift->[1] || '' }

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
        $msg .= " (In tag $node->[0] starting at char ".($node->[1] + $self->offset).")";
    }
    return $msg;
}

###----------------------------------------------------------------###

package CGI::Ex::Template::Iterator;

sub new {
    my ($class, $items) = @_;
    $items = [] if ! defined $items;
    $items = [$items] if ! UNIVERSAL::isa($items, 'ARRAY');
    return bless {items => $items, i => 0}, $class;
}

sub items { shift->{'items'} }

sub index {
    my $self = shift;
    $self->{'i'} = shift if $#_ == 0;
    return $self->{'i'};
}

sub max { $#{ shift->items } }

sub size { shift->max + 1 }

sub count { shift->index + 1 }

sub first { (shift->index == 0) || 0 }

sub last { my $self = shift; return ($self->index == $self->max) || 0 }

sub prev {
    my $self = shift;
    return undef if $self->index == -1;
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

sub AUTOLOAD { shift->_template->throw('not_implemented', "The method $AUTOLOAD has not been implemented") }

sub DESTROY {}

###----------------------------------------------------------------###

package CGI::Ex::Template::EvalPerlHandle;

sub load {
    my ($class, $out_ref) = @_;
    tie(*OUTPUT, $class, $out_ref);
    return \*OUTPUT;
}

sub TIEHANDLE {
    my ($class, $out_ref) = @_;
    return bless [$out_ref], $class;
}

sub PRINT {
    my $self = shift;
    ${ $self->[0] } .= $_ for grep {defined && length} @_;
    return 1;
}

sub PRINTF { shift->PRINT(sprintf @_) }

###----------------------------------------------------------------###

1;

__END__

=head1 SYNOPSIS

  None yet.

=head1 DESCRIPTION

=head1 TODO

    Benchmark text processing
    Benchmark FOREACH types again
    Allow USE to use our Iterator
    Get several test suites to pass

=head1 DIRECTIVES

This section containts the alphabetical list of DIRECTIVES available in
the TT language.  For further discussion and examples, please refer to the
TT directives documentation.


=over 4

=item BLOCK



=item BREAK

Alias for LAST.  Used for exiting FOREACH and WHILE loops.

=item CALL

Calls the variable (and any underlying coderefs) as in the GET method, but
always returns an empty string.

=item CASE

Used with the SWITCH directive.  See the L</"SWITCH"> directive.

=item CATCH

Used with the TRY directive.  See the L</"TRY"> directive.

=item CLEAR

Clears any of the content currently generated in the innermost block
or template.  This can be useful when used in conjuction with the TRY
statement to clear generated content if an error occurs later.

=item DEBUG

Used to reset the DEBUG_FORMAT configuration variable, or to turn
DEBUG statements on or off.  This only has effect if the DEBUG_DIRS or
DEBUG_ALL flags were passed to the DEBUG configuration variable.

    [% DEBUG format '($file) (line $line) ($text)' %]
    [% DEBUG on %]
    [% DEBUG off %]

=item DEFAULT

Similar to SET, but only sets the value if a previous value was not
defined or was zero length.

    [% DEFAULT foo = 'bar' %][% foo %] => 'bar'

    [% foo = 'baz' %][% DEFAULT foo = 'bar' %][% foo %] => 'baz'

=item ELSE

Used with the IF directive.  See the L</"IF"> directive.

=item ELSIF

Used with the IF directive.  See the L</"IF"> directive.

=item END

Used to end a block directive.

=item FILTER



=item '|'

Alias for the FILTER directive.  Note that | is similar to the
'.' in CGI::Ex::Template.  Therefore a pipe cannot be used directly after a
variable name in some situations (the pipe will act only on that variable).
This is the behavior employed by TT3.

=item FINAL

Used with the TRY directive.  See the L</"TRY"> directive.

=item FOR

Alias for FOREACH

=item FOREACH



=item GET



=item IF (IF / ELSIF / ELSE)

Allows for conditional testing.  Expects an expression as its only
argument.  If the expression is true, the contents of its block are
processed.  If false, the processor looks for an ELSIF block.  If an
ELSIF's expression is true then it is processed.  Finally it looks for
an ELSE block which is processed if none of the IF or ELSIF's
expressions were true.

    [% IF a == b %]A equaled B[% END %]

    [% IF a == b -%]
        A equaled B
    [%- ELSIF a == c -%]
        A equaled C
    [%- ELSE -%]
        Couldn't determine that A equaled anything.
    [%- END %]

=item INCLUDE



=item INSERT



=item LAST

Used to exit out of a WHILE or FOREACH loop.

=item MACRO



=item META



=item NEXT

Used to go to the next iteration of a WHILE or FOREACH loop.

=item PERL



=item PROCESS



=item RETURN

Used to exit the innermost block or template and continue processing
in the surrounding block or template.

=item SET



=item STOP

Used to exit the entire process method (out of all blocks and templates).
No content will be processed beyond this point.

=item SWITCH



=item TAGS

Change the type of enclosing braces used to delineate template tags.  This
remains in effect until the end of the enclosing block or template or until
the next TAGS directive.  Either a named set of tags must be supplied, or
two tags themselves must be supplied.

    [% TAGS html %]

    [% TAGS <!-- --> %]

The named tags are (duplicated from TT):

    template => ['[%',   '%]'],  # default
    metatext => ['%%',   '%%'],  # Text::MetaText
    star     => ['[*',   '*]'],  # TT alternate
    php      => ['<?',   '?>'],  # PHP
    asp      => ['<%',   '%>'],  # ASP
    mason    => ['<%',   '>' ],  # HTML::Mason
    html     => ['<!--', '-->'], # HTML comments


=item THROW



=item TRY



=item UNLESS



=item USE



=item WHILE




=item WRAPPER

Block directive.  Processes contents of its block and then passes them
in the [% content %] variable to the block or filename listed in the
WRAPPER tag.

    [% WRAPPER foo %]
    My content to be processed.[% a = 2 %]
    [% END %]

    [% BLOCK foo %]
    A header ([% a %]).
    [% content %]
    A footer ([% a %]).
    [% END %]

This would print.

    A header (2).
    My content to be processed.
    A footer (2).


=back



=head1 OPERATORS

The following operators are available in CGI::Ex::Template.  Except
where noted these are the same operators available in TT.  They are
listed in the order of their precedence (the higher the precedence the
tighter it binds).

=over 4

=item C<**  ^  pow>

Binary.  X raised to the Y power.  This isn't available in TT 2.14.

    [% 2 ** 3 %] => 8

=item C<!>

Unary not.  Negation of the value.

=item C<-  unary_minus>

Unary minus.  Returns the value multiplied by -1.  The operator
"unary_minus" is used internally by CGI::Ex::Template to provide for -
to be listed in the precedence table twice.

    [% a = 1 ; b = -a ; b %] => -1

=item C<*>

Binary. Multiplication.

=item C</  div  DIV>

Binary. Division.  Note that / is floating point division, but div and
DIV are integer division.

=item C<%  mod  MOD>

Binary. Modulus.

   [% 15 % 8 %] => 7

=item C<+>

Binary.  Addition.

=item C<->

Binary.  Minus.

=item C<_  ~>

Binary.  String concatenation.

=item C<< <  >  <=  >= >>

Binary.  Numerical comparators.

=item C<lt  gt  le  ge>

Binary.  String comparators.

=item C<==  eq>

Binary.  Equality test.  TT chose to use Perl's eq for both operators.

=item C<!= ne>

Binary.  Non-equality test.  TT chose to use Perl's ne for both
operators.

=item C<&&>

Multiple arity.  And.  Both values must be true.  If all values are true, the last
value is returned as the truth value.

    [% 2 && 3 && 4 %] => 4

=item C<||>

Multiple arity.  Or.  The first true value is returned.

    [% 0 || '' || 7 %] => 7

=item C<..>

Binary.  Range creator.  Returns an arrayref containing the values
between and including the first and last arguments.

    [% t = [1 .. 5] %] => variable t contains an array with 1,2,3,4, and 5

In CGI::Ex::Template, because .. is an operator that returns an array, you may
also specify the previous example as the following (this does not work in TT):

    [% t = 1 .. 5 %] => variable t contains an array with 1,2,3,4, and 5

CGI::Ex::Template provides special functionality to allow the arrayref
returned by .. to be expanded fully into a [] construtor as in C<[1 .. 5]>.
Because of this it is possible to place multiple ranges in the
same [] constructor.  This is not available in TT.

    [% t = [1..3, 5..7] %] => variable t contains an array with 1,2,3,5,6 and 7

=item C<? :>

Trinary.  Can be nested with other ?: pairs.

    [% 1 ? 2 : 3 %] => 2
    [% 0 ? 2 : 3 %] => 3

=item C<not  NOT>

Lower precedence version of the '!' operator.

=item C<and  AND>

Lower precedence version of the '&&' operator.

=item C<or OR>

Lower precedence version of the '||' operator.

=item C<hashref>

Multiple arity.  This operator is not used in TT.  It is used internally
by CGI::Ex::Template to delay the creation of a hashref until the
execution of the compiled template.

=item C<arrayref>

Multiple arity.  This operator is not used in TT.  It is used internally
by CGI::Ex::Template to delay the creation of an arrayref until the
execution of the compiled template.

=back



=head1 CONFIGURATION

The following TT2 configuration variables are supported (in
alphabetical order).  Note: for further discussion you can refer to
the TT config documentation.

These variables should be passed to the "new" constructor.

   my $obj = CGI::Ex::Template->new(
       VARIABLES  => \%hash_of_variables,
       AUTO_RESET => 0,
       TRIM       => 1,
       POST_CHOMP => 2,
       PRE_CHOMP  => 1,
   );


=over 4

=item ABSOLUTE

Boolean.  Default false.  Are absolute paths allowed for included files.

=item ANYCASE

Boolean.  Default false.  Allow directive names to be in any case.
See the note about DIRECTIVE names in the differences from TT section.

=item AUTO_RESET

Boolean.  Default 1.  Clear blocks that were set during the process method.

=item BLOCKS - no Template::Documents support

A hashref of blocks that can be used by the process method.

   BLOCKS => {
       block_1 => sub { ... }, # coderef that returns a block
       block_2 => 'A String',  # simple string
   },

Note that a Template::Document cannot be supplied as a value (TT
supports this).  However, it is possible to supply a value that is
equal to the hashref returned by the load_parsed_tree method.

=item CACHE_SIZE

Number of compiled templates to keep in memory.  Default undef.
Undefined means to allow all templates to cache.  A value of 0 will
force no caching.  The cache mechanism will clear templates that have
not been used recently.

=item COMPILE_DIR

Base directory to store compiled templates.  Default undef. Compiled
templates will only be stored if one of COMPILE_DIR and COMPILE_EXT is
set.

=item COMPILE_EXT

Extension to add to stored compiled template filenames.  Default undef.

=item CONSTANTS

Hashref.  Used to define variables that will be "folded" into the
compiled template.  Variables defined here cannot be overridden.

    CONSTANTS => {my_constant => 42},

    A template containing:

    [% constants.my_constant %]

    Will have the value 42 compiled in.

Constants defined in this way can be chained as in [%
constant.foo.bar.baz %] but may only interpolate values that are set
before the compile process begins.  This goes one step beyond TT in
that any variable set in VARIABLES, or PRE_DEFINE, or passed to the
process method are allowed - they are not in TT.  Variables defined in
the template are not available during the compile process.

    GOOD:

    CONSTANTS => {
        foo  => {
            bar => {baz => 42},
            bim => 57,
        },
        bing => 'baz',
        bang => 'bim',
    },
    VARIABLES => {
        bam  => 'bar',
    },

    In the template

    [% constants.foo.${constants.bang} %]

    Will correctly print 42.

    GOOD (non-tt behavior)

    [% constants.foo.$bam.${constants.bing} %]

    Will correctly print 42.  TT would print '' as the value of $bam
    is not yet defined in the TT engine.


    BAD:

    In the template:

    [% bam = 'somethingelse' %]
    [% constants.foo.$bam.${constants.bing} %]

    Will still print 42 because the value of bam used comes from
    variables defined before the template was compiled.

=item CONSTANT_NAMESPACE


=item DEBUG



=item DEBUG_FORMAT



=item DEFAULT



=item DELIMITER



=item END_TAG



=item EVAL_PERL



=item FILTERS



=item INCLUDE_PATH



=item INTERPOLATE



=item LOAD_PERL



=item NAMESPACE - no Template::Namespace::Constants support



=item OUTPUT



=item OUTPUT_PATH



=item PLUGINS



=item PLUGIN_BASE



=item POST_CHOMP



=item PRE_CHOMP



=item PRE_DEFINE



=item RECURSION



=item RELATIVE



=item START_TAG



=item TAG_STYLE


=item TRIM

Remove leading and trailing whitespace from blocks and templates.
This operation is performed after all enclosed template tags have
been executed.

=item VARIABLES

A hashref of variables to initialize the template stash with.  These
variables are available for use in any of the executed templates.

=back



=head1 UNSUPPORTED TT CONFIGURATION

=over 4

=item PRE_PROCESS

=item POST_PROCESS

=item PROCESS

=item WRAPPER

=item ERROR

=item V1DOLLAR

=item LOAD_TEMPLATES

CGI::Ex::Template has its own mechanism for loading and storing
compiled templates.  TT would use a Template::Provider that would
return a Template::Document.  The closest thing in CGI::Ex::Template
is the load_parsed_template method.  There is no immediate plan to
support the TT behavior.

=item LOAD_PLUGINS

CGI::Ex::Template uses its own mechanism for loading plugins.  TT
would use a Template::Plugins object to load plugins requested via the
USE directive.  The functionality for doing this in CGI::Ex::Template
is contained in the list_plugins method and the play_USE method.  There
is no immediate plan to support the TT behavior.

Full support is offered for the PLUGINS and LOAD_PERL configuration items.

Also note that CGI::Ex::Template only natively supports the Iterator plugin.
Any of the other plugins requested will need to provided by installing
Template::Toolkit or the appropriate module.

=item LOAD_FILTERS

CGI::Ex::Template uses its own mechanism for loading filters.  TT
would use the Template::Filters object to load filters requested via the
FILTER directive.  The functionality for doing this in CGI::Ex::Template
is contained in the list_filters method and the vivify_variable method.

Full support is offered for the FILTERS configuration item.

=item TOLERANT

This option is used by the LOAD_TEMPLATES and LOAD_PLUGINS options and
is not available in CGI::Ex::Template.

=item SERVICE

CGI::Ex::Template has no concept of service.

=item CONTEXT

CGI::Ex::Template provides its own pseudo context object to plugins,
filters, and perl blocks.  The CGI::Ex::Template model doesn't really
allow for a separate context.  CGI::Ex::Template IS the context.

=item STASH

CGI::Ex::Template manages its own stash of variables.  A pseudo stash
object is available via the pseudo context object for use in plugins,
filters, and perl blocks.

=item PARSER

CGI::Ex::Template has its own built in parser.  It is available via
the parse_tree method.

=item GRAMMAR

CGI::Ex::Template maintains its own grammar.  The grammar is defined
in the parse_tree method and the callbacks listed in the global
$DIRECTIVES hashref.

=back


=head1 VARIABLE PARSE TREE

CGI::Ex::Template parses templates into an tree of operations.  Even
variable access is parsed into a tree.  This is done in a manner
somewhat similar to the way that TT operates except that nested
variables such as foo.bar|baz contain the '.' or '|' in between each
name level.  The following table shows a variable and the
corresponding parsed tree.

    one                [ 'one',  0 ]
    one()              [ 'one',  [] ]
    one.two            [ 'one',  0, '.', 'two',  0 ]
    one|two            [ 'one',  0, '|', 'two',  0 ]
    one.$two           [ 'one',  0, '.', ['two', 0 ], 0 ]
    one(two)           [ 'one',  [ ['two', 0] ] ]
    one.${two().three} [ 'one',  0, '.', ['two', [], '.', 'three', 0], 0]
    2.34               2.34
    "one"              "one"
    "one"|length       [ \"one", 0, '|', 'length', 0 ]
    "one $a two"       [ \ [ '~', 'one ', ['a', 0], ' two' ], 0 ]
    [0, 1, 2]          [ \ [ 'arrayref', 0, 1, 2 ], 0 ]
    [0, 1, 2].size     [ \ [ 'arrayref', 0, 1, 2 ], 0, '.', 'size', 0 ]
    ['a', a, $a ]      [ \ [ 'arrayref', 'a', ['a', 0], [['a', 0], 0] ], 0]
    {a  => 'b'}        [ \ [ 'hashref',  'a', 'b' ], 0 ]
    {a  => 'b'}.size   [ \ [ 'hashref',  'a', 'b' ], 0, '.', 'size', 0 ]
    {$a => b}          [ \ [ 'hashref',  ['a', 0], ['b', 0] ], 0 ]
    1 + 2 + 3 + 4      [ \ [ '+', 1, 2, 3, 4 ], 0]
    a + b              [ \ [ '+', ['a', 0], ['b', 0] ], 0 ]
    a * (b + c)        [ \ [ '*', ['a', 0], [ \ ['+', ['b', 0], ['c', 0]], 0 ]], 0 ]
    (a + b)            [ \ [ '+', ['a', 0], ['b', 0] ]], 0 ]
    (a + b) * c        [ \ [ '*', [ \ [ '+', ['a', 0], ['b', 0] ], 0 ], ['c', 0] ], 0 ]
    a ? b : c          [ \ [ '?', ['a', 0], ['b', 0], ['c', 0] ], 0 ]
    a || b || c        [ \ [ '||', ['a', 0], ['b', 0], ['c', 0] ], 0 ]
    ! a                [ \ [ '!', ['a', 0] ], 0 ]

Some notes on the parsing.

    Operators are parsed as part of the variable and become part of the variable tree.

    Operators are stored in the variable tree using a reference to the arrayref - which
    allows for quickly decending the parsed variable tree and determining that the next
    node is an operator.

    () can be used at any point to disambiguate precedence.

    "Variables" that appear to be literal strings or literal numbers
    are returned as the literal (no operator tree).

    "Variables" that are literals that have an operator operating on them are stored
    as a reference to that literal (scalar ref).

The following perl can be typed at the command line to view the parsed variable tree.

    perl -e 'my $a = "\"one \$a two\"";
       use CGI::Ex::Template;
       use Data::Dumper;
       print Dumper(CGI::Ex::Template->new->parse_variable(\$a));'

=head1 AUTHOR

Paul Seamons <mail at seamons dot com>

=cut

