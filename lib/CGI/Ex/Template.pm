package CGI::Ex::Template;

=head1

CGI::Ex::Template - Lightweight TT2/3 engine

=cut

use CGI::Ex::Dump qw(debug);
use strict;
use vars qw(@INCLUDE_PATH
            $TAGS
            $SCALAR_OPS $HASH_OPS $LIST_OPS
            $DIRECTIVES
            $OPERATORS $OP_UNARY $OP_TRINARY $OP_FUNC $OP_QR $OP_QR_UNARY
            $QR_FILENAME
            $MAX_RECURSE
            $WHILE_MAX
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

    ### list out the virtual methods
    $SCALAR_OPS = {
        hash    => sub { {value => $_[0]} },
        length  => sub { defined($_[0]) ? length($_[0]) : 0 },
        list    => sub { [ $_[0] ] },
        match   => sub {
            my ($str, $pat, $global) = @_;
            return [] if ! defined $str || ! defined $pat;
            return [$str =~ /$pat/g] if $global;
            return [$str =~ /$pat/ ];
        },
        repeat  => sub {
            my ($str, $n, $join) = @_;
            return if ! length $str;
            $n = 1 if ! defined($n) || ! length $n;
            $join = '' if ! defined $join;
            return join $join, ($str) x $n;
        },
        replace => sub {
            my ($str, $pat, $replace) = @_;
            return undef if ! defined $str || ! defined $pat;
            $replace = '' if ! defined $replace;
            $str =~ s/$pat/$replace/g;
            return $str;
        },
        size    => sub { 1 },
    };

    $LIST_OPS = {
        grep    => sub { my ($ref, $pat) = @_; [grep {/$pat/} @$ref] },
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
        list    => sub { [map { {key => $_, value => $_[0]->{$_}} } keys %{ $_[0] } ] },
        nsort   => sub { my $ref = shift; [sort {$ref->{$a}    <=> $ref->{$b}   } keys %$ref] },
        size    => sub { scalar keys %{ $_[0] } },
        sort    => sub { my $ref = shift; [sort {lc $ref->{$a} cmp lc $ref->{$b}} keys %$ref] },
        values  => sub { [values %{ $_[0] }] },
    };

    $DIRECTIVES = {
        BLOCK   => {
            parse  => \&parse_BLOCK,
            play   => \&play_BLOCK,
            block  => 1,
            move_to_front => 1,
        },
        BREAK   => { control => 1 },
        CALL    => {
            parse  => \&parse_CALL,
            play   => \&play_CALL,
        },
        CASE    => {
            parse => \&parse_CASE,
            play  => sub {},
            continue_block => {SWITCH => 1, CASE => 1},
        },
        CATCH   => {
            parse => \&parse_CATCH,
            play  => sub {},
            continue_block => {TRY => 1, CATCH => 1},
        },
        CLEAR   => { control => 1 },
        COMMENT => {
            parse  => sub {},
            play   => sub {},
        },
        DEFAULT => {
            parse  => \&parse_DEFAULT,
            play   => \&play_DEFAULT,
        },
        DUMP    => {
            parse  => \&parse_DUMP,
            play   => \&play_DUMP,
        },
        ELSE    => {
            parse => sub {},
            play  => sub {},
            continue_block => {IF => 1, ELSIF => 1, UNLESS => 1},
        },
        ELSIF   => {
            parse => \&parse_IF,
            play  => sub {},
            continue_block => {IF => 1, ELSIF => 1, UNLESS => 1},
        },
        END     => {}, # builtin that cannot be overridden
        FILTER  => {
            parse  => \&parse_FILTER,
            play   => \&play_FILTER,
            block  => 1,
            postop => 1,
        },
        FOR     => {
            parse  => \&parse_FOREACH,
            play   => \&play_FOREACH,
            block  => 1,
            postop => 1,
        },
        FOREACH => {
            parse  => \&parse_FOREACH,
            play   => \&play_FOREACH,
            block  => 1,
            postop => 1,
        },
        GET     => {
            parse  => \&parse_GET,
            play   => \&play_GET,
        },
        IF      => {
            parse  => \&parse_IF,
            play   => \&play_IF,
            block  => 1,
            postop => 1,
        },
        INCLUDE => {
            parse  => \&parse_INCLUDE,
            play   => \&play_INCLUDE,
        },
        INSERT  => {
            parse  => \&parse_INSERT,
            play   => \&play_INSERT,
        },
        LAST    => { control => 1 },
        MACRO   => {
            parse  => \&parse_MACRO,
            play   => \&play_MACRO,
        },
        NEXT    => { control => 1 },
        PROCESS => {
            parse  => \&parse_PROCESS,
            play   => \&play_PROCESS,
        },
        RETURN  => { control => 1 },
        SET     => {
            parse  => \&parse_SET,
            play   => \&play_SET,
        },
        STOP    => { control => 1 },
        SWITCH  => {
            parse  => \&parse_SWITCH,
            play   => \&play_SWITCH,
            block  => 1,
        },
        TAGS    => {}, # builtin that cannot be overridden
        THROW   => {
            parse  => \&parse_THROW,
            play   => \&play_THROW,
        },
        TRY     => {
            parse  => sub {},
            play   => \&play_TRY,
            block  => 1,
        },
        UNLESS  => {
            parse  => \&parse_UNLESS,
            play   => \&play_UNLESS,
            block  => 1,
            postop => 1,
        },
        USE     => {
            parse  => \&parse_USE,
            play   => \&play_USE,
        },
        WHILE   => {
            parse  => \&parse_WHILE,
            play   => \&play_WHILE,
            block  => 1,
            postop => 1,
        },
        WRAPPER => {
            parse  => \&parse_WRAPPER,
            play   => \&play_WRAPPER,
            block  => 1,
        },
    };

    $OPERATORS ||= {qw(**  99   ^  99   pow 99
                       !   95   unary_minus 95
                       *   90   /  90   div 90   %  90   mod    90
                       +   85   -  85   _   85   ~  85   concat 85
                       <   80   >  80   <=  80   >= 80
                       lt  80   gt 80   le  80   ge 80
                       ==  75   != 75   eq  75   ne 75
                       &&  70
                       ||  65
                       ..  60
                       ?   55
                       not 50
                       and 45
                       or  40
                       hashref 1 arrayref 1
                       )};
    $OP_UNARY   ||= {'!' => '!', 'not' => '!', 'unary_minus' => '-'};
    $OP_TRINARY ||= {'?' => ':'};
    $OP_FUNC    ||= {};
    sub _op_qr { # no mixed \w\W operators
        my %used;
        my $chrs = join '|', map {quotemeta $_} grep {++$used{$_} < 2} grep {/^\W{2,}$/} @_;
        my $chr  = join '',  map {quotemeta $_} grep {++$used{$_} < 2} grep {/^\W$/}     @_;
        my $word = join '|',                    grep {++$used{$_} < 2} grep {/^\w+$/}    @_;
        $chr = "[$chr]" if $chr;
        return join('|', grep {length} $chrs, $chr, $word) || die "Missing operator regex";
    }
    sub _build_op_qr       { _op_qr(sort(grep {! $OP_UNARY->{$_}} keys(%$OPERATORS), values(%$OP_TRINARY))) }
    sub _build_op_qr_unary { _op_qr(sort %$OP_UNARY) } # grab keys and values
    $OP_QR       ||= _build_op_qr();
    $OP_QR_UNARY ||= _build_op_qr_unary();

    $QR_FILENAME = qr{(?i: [a-z]:/|/)? [\w\-\.]+ (?:/[\w\-\.]+)* }x;
    $MAX_RECURSE = 50;
    $WHILE_MAX   = 1000;
};

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $args  = ref($_[0]) ? shift : {@_};
  return bless $args, $class;
}

###----------------------------------------------------------------###

sub swap {
    my $self = shift;
    my $file = shift;
    my $tree;
    my $str_ref;
    local $self->{'_swap'}  = shift || {};
    local $self->{'_state'} = {};


    ### look for cached components
    my $store_file;
    if (ref $file) {
        $str_ref = $file;
        undef $file;

    } elsif ($self->{'_documents'}->{$file}) {
        ($str_ref, $tree) = @{ $self->{'_documents'}->{$file} };

    } else {
        my $content = $self->include_file($file);
        $str_ref = \$content;

        if ($self->{'COMPILE_DIR'} || $self->{'COMPILE_EXT'}) {
            $store_file = $file . $self->{'COMPILE_EXT'} .'.sto' if $self->{'COMPILE_EXT'};
            $store_file = ($self->{'COMPILE_DIR'})
                ? $self->{'COMPILE_DIR'} .'/'. $store_file
                : $self->include_file($file) . $self->{'COMPILE_EXT'} .'.sto';
            if (-e $store_file && -M _ == -M $file) {
                require Storable;
                $tree = Storable::retrieve($store_file);
                undef $store_file;
            }
        }

    }

    if (! $tree && $self->{'CONSTANTS'}) {
        my $key = $self->{'CONSTANTS_NAMESPACE'} || 'constants';
        $self->{'NAMESPACE'}->{$key} = $self->{'CONSTANTS'};
    }

    ### parse and execute
    my $out = '';
    eval {
        $tree ||= $self->parse_tree($str_ref) || [];
        if ($#$tree == -1) {
            $out = $$str_ref;
        } else {
            $self->execute_tree($tree, $str_ref, \$out);
        }
    };

    ### cache if asked
    if ($file && (! defined($self->{'CACHE_SIZE'}) || $self->{'CACHE_SIZE'})) { 
        $self->{'_documents'}->{$file} ||= [$str_ref, $tree];

        ### allow for config option to keep the cache size down
        if ($self->{'CACHE_SIZE'}) {
            my $cache = $self->{'_documents'};
            $cache->{$file}->[2] = time;
            if (scalar(keys %$cache) > $self->{'CACHE_SIZE'}) {
                my $n = 0;
                foreach my $file (sort {$cache->{$b}->[2] <=> $cache->{$a}->[2]} keys %$cache) {
                    $n ++;
                    delete($cache->{$file}) if $n > $self->{'CACHE_SIZE'};
                }
            }
        }
    }


    ### save a cache as asked
    if ($store_file && $tree) {
        my $dir = $store_file;
        $dir =~ s|/[^/]+$||;
        if (! -d $dir) {
            require File::Path;
            File::Path::mkpath($dir);
        }
        require Storable;
        Storable::store($tree, $store_file);
        my $mtime = (stat $file)[9];
        utime $mtime, $mtime, $store_file;
    }

    ### all good
    return $out if ! $@;

    ### handle exceptions
    my $err = $@;

    $err = $self->exception('undef', $err) if ! UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception');
    $err->str_ref(\$_[0]);
    if ($err->type !~ /STOP|RETURN|NEXT|LAST|BREAK/) {
        eval { die $err };
        return '';
    }
    return $out; # some directives may get us here - but still contain content
}

sub parse_tree {
    my $self    = shift;
    my $str_ref = shift;
    if (! $str_ref || ! defined $$str_ref) {
        die $self->exception('parse.no_string', "No string or undefined during parse");
    }

    my $STYLE = $self->{'TAG_STYLE'} || 'template';
    my $START = $self->{'START_TAG'} || $TAGS->{$STYLE}->[0];
    my $END   = $self->{'END_TAG'}   || $TAGS->{$STYLE}->[1];
    my $len_s = length $START;
    my $len_e = length $END;

    my @tree;             # the parsed tree
    my @state;            # maintain block levels
    my $pointer = \@tree; # pointer to current block storage
    my @move_to_front;    # items that need to be declared first
    my $i = 0;            # start index
    my $j = 0;            # end index
    my $last = 0;         # previous end index
    my $post_chomp = 0;   # previous post_chomp setting
    my $continue;         # multiple directives in the same tag
    my $postop;           # found a post-operative DIRECTIVE
    my @capture;          # capture the output of a directive
    my $capture;          # flag to start capture
    my $tag;
    my $node;
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
                push @$pointer, ['TEXT', $last, $i, [0, $post_chomp]];
                $self->interpolate_node($pointer, -1) if $self->{'INTERPOLATE'};
            }
            $j = index($$str_ref, $END, $i + $len_s);
            $last = $j + $len_e;
            if ($j == -1) { # missing closing tag
                $last = length($$str_ref);
                last;
            }
            $tag = substr($$str_ref, $i + $len_s, $j - ($i + $len_s));
            $node = [undef, $i + $len_s, $j];
        }

        ### take care of whitespace
        if ($tag =~ s/^(\#?)-/$1/ || ($self->{'PRE_CHOMP'} && $tag !~ s/^(\#?)\+/$1/)) {
            $pointer->[-1]->[3]->[0] = 1 if $pointer->[-1] && $pointer->[-1]->[0] eq 'TEXT';
            $node->[1] ++;
        }
        if ($tag =~ s/-$// || ($self->{'POST_CHOMP'} && $tag !~ s/\+$//)) {
            $post_chomp = 1;
            $node->[2] --;
        } else {
            $post_chomp = $self->{'POST_CHOMP'} || 0;
        }
        if ($tag =~ /^\#/) { # leading # means to comment the entire section
            $node->[0] = 'COMMENT';
            push @$pointer, $node;
            next;
        }
        $tag =~ s{ (?<! \\) \# .* $ }{}xmg; # remove trailing comments
        $tag =~ s{ ^ \s+ }{}x;
        if (! length $tag) {
            undef $continue;
            undef $postop;
            next;
        }

        ### look for DIRECTIVES
        if ($tag =~ / ^ (\w+) (?: ;|$|\s)/x && $DIRECTIVES->{$1}) {

            my $func = $node->[0] = $1;
            $tag =~ s{ ^ \w+ \s* }{}x;

            ### store out this current node level
            if ($postop) { # on a post operator - replace the original node with the new one
                my @postop = @$postop;
                @$postop = @$node;
                $node = $postop;
                $node->[4] = [\@postop];
            } else {
                push @$pointer, $node;
            }

            ### anything that behaves as a block ending
            if ($func eq 'END' || $DIRECTIVES->{$func}->{'continue_block'}) {
                if ($#state == -1) {
                    die $self->exception('parse', "Found an $func while not in a block", $node);
                }
                my $parent_node = pop @state;

                if ($func ne 'END') {
                    pop @$pointer; # we will store the node in the parent instead
                    $parent_node->[5] = $node;
                    my $parent_type = $parent_node->[0];
                    if (! $DIRECTIVES->{$func}->{'continue_block'}->{$parent_type}) {
                        die $self->exception('parse', "Found unmatched nested block", $node, 0);
                    }
                }

                ### restore the pointer up one level
                $pointer = ($#state == -1) ? \@tree : $state[0]->[4];

                ### normal end block
                if ($func eq 'END') {
                    if ($DIRECTIVES->{$parent_node->[0]}->{'move_to_front'}) { # move things like BLOCKS to front
                        push @move_to_front, $parent_node;
                        splice @$pointer, -1, 1, ();
                    }

                ### continuation block - such as an elsif
                } else {
                    $node->[3] = eval { $DIRECTIVES->{$func}->{'parse'}->($self, \$tag, $func, $node) };
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

            } elsif ($DIRECTIVES->{$func}->{'control'}) {
                # do nothing
            } else {
                $node->[3] = eval { $DIRECTIVES->{$func}->{'parse'}->($self, \$tag, $func, $node) };
                if (my $err = $@) {
                    $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                    die $err;
                }
                if ($DIRECTIVES->{$func}->{'block'} && ! $postop) {
                    push @state, $node;
                    $pointer = $node->[4] ||= [];
                }
            }

        ### allow for bare variable getting and setting
        } elsif (defined(my $var = $self->parse_variable(\$tag))) {
            push @$pointer, $node;
            if ($tag =~ s{ ^ = \s* }{}x) {
                $node->[0] = 'SET';
                $node->[3] = eval { $DIRECTIVES->{'SET'}->{'parse'}->($self, \$tag, 'SET', $node, $var) };
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
            die $self->exception('parse', "Not sure how to handle tag \"$all\"");
        }


        ### we need to store the captured directive
        if ($capture) {
            undef $capture;
            if (! $capture[-1]) {
                die $self->exception('parse', "Die bad DIRECTIVE capturing setup", $node);
            } elsif ($node->[6]) {
                die $self->exception('parse', "Nested DIRECTIVE capturing attempted", $node);
            }
            my $storage = pop @capture; # pull off the most recent
            push @$storage, $node;
            pop @$pointer;
        }

        ### we are capturing the output of the next directive - set it up
        if (delete $node->[6]) {
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $postop    = undef;
            $capture   = 1;
            push @capture, $node->[4] ||= [];

        ### semi-colon = end of statement
        } elsif ($tag =~ s{ ^ ; \s* }{}x) {
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $postop    = undef;

        ### looking at a postoperator
        } elsif ($tag =~ / ^ (\w+) (?: ;|$|\s)/x && $DIRECTIVES->{$1}
                 && $DIRECTIVES->{$1}->{'postop'}) {
            $continue  = $j - length $tag;
            $node->[2] = $continue;
            $postop    = $node;

        } else { # error
            die $self->exception('parse', "Found trailing info \"$tag\"", $node) if length $tag;
            $continue = undef;
            $postop   = undef;
        }
    }

    if ($#move_to_front != -1) {
        unshift @tree, @move_to_front;
    }

    if ($#state >  -1) {
        die $self->exception('parse.missing.end', "Missing END", $state[-1], 0);
    }

    push @tree, ['TEXT', $last, length($$str_ref), [0, $post_chomp]] if $last != length($$str_ref);
    $self->interpolate_node(\@tree, -1) if $self->{'INTERPOLATE'};

    return \@tree;
}

sub exception {
    my $self = shift;
    my $pkg  = $self->{'exception_package'} || 'CGI::Ex::Template::Exception';
    return $pkg->new(@_);
}

sub execute_tree {
    my ($self, $tree, $template_ref, $out_ref) = @_;

    # node contains (0: DIRECTIVE,
    #                1: start_index,
    #                2: end_index,
    #                3: parsed tag details,
    #                4: sub tree for block types
    #                5: continuation sub trees for sub continuation block types (elsif, else, etc)
    #                6: flag to capture next directive
    for my $node (@$tree) {
        if ($node->[0] eq 'TEXT') {
            if (! defined $node->[4]) {
                $node->[4] = substr($$template_ref, $node->[1], $node->[2] - $node->[1]);

                $node->[4] =~ s{ (?:\n|^) [^\S\n]* \z }{}xm   if $node->[3]->[0]; # pre_chomp
                $node->[4] =~ s{ \G [^\S\n]* (?:\n?$|\n) }{}x if $node->[3]->[1]; # post_chomp
            }
            $$out_ref .= $node->[4];
            next;
        }

        ### allow for the null directives
        if ($node->[0] eq 'END' || $node->[0] eq 'TAGS') {
            next;

        ### allow for control directives
        } elsif ($DIRECTIVES->{$node->[0]}->{'control'}) {
            if ($node->[0] eq 'CLEAR') {
                $$out_ref = '';
                next;
            }
            die $self->exception($node->[0], 'Control exception', $node);

        ### normal directive
        } else {
            my $val = $DIRECTIVES->{$node->[0]}->{'play'}->($self, $node->[3], $node, $template_ref, $out_ref);
            $$out_ref .= $val if defined $val;
        }

    }
}

###----------------------------------------------------------------###

sub parse_variable {
    my $self    = shift;
    my $str_ref = shift;
    my $ARGS    = shift || {};

    ### allow for custom auto_quoting (such as hash constructors)
    if (my $quote_qr = $ARGS->{'auto_quote'}) {
        if ($$str_ref =~ s{ ^ ($quote_qr) \s* (?! \.) }{}x) { # auto-quoted - not followed by a dot
            my $str = $1;
            return $str;
            #return [\$str, 0];
        } elsif ($$str_ref =~ s{ ^ \$ (\w+) \s* (?! \.) }{}x # auto-quoted dollars
                 || $$str_ref =~ s{ ^ \$\{ \s* ([^\}]+) \} \s* (?! \.) }{}x) {
            my $name = $1;
            return [$name, 0];
        }
    }

    my $copy = $$str_ref; # copy while parsing to allow for errors

    ### test for leading unary operators
    my $has_unary;
    if ($copy =~ s{ ^ ($OP_QR_UNARY) \s* }{}xo) {
        return if $ARGS->{'auto_quote'}; # auto_quoted thing was too complicated
        $has_unary = $1;
        if (! $OP_UNARY->{$has_unary}) {
            my $ref = $self->{'_op_unary_backref'} ||= {reverse %$OP_UNARY};
            $has_unary = $ref->{$has_unary} || die "Couldn't find canonical name of unary \"$1\"";
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
            my @pieces = split m{ (\$\w+\b | \$\{ [^\}]+ \}) }x, $2;
            foreach my $piece (@pieces) {
                if ($piece =~ m{ ^ \$ (\w+) $ }x
                    || $piece =~ m{ ^ \$\{ \s* ([^\}]+) \} $ }x) {
                    my $name = $1;
                    $piece = $self->parse_variable(\$name);
                }
            }
            @pieces = grep {defined && length} @pieces;
            if ($#pieces == -1) {
                push @var, \ '';
                $is_literal = 1;
            } else {
                #push @var, \ ['concat', map {ref($_) ? $_ : [\$_, 0]} @pieces];
                push @var, \ ['concat', @pieces];
            }
        }

    ### allow for leading $foo or ${foo.bar} type constructs
    } elsif ($copy =~ s{ ^ \$ (\w+) \b \s* }{}x
        || $copy =~ s{ ^ \$\{ \s* ([^\}]+) \} \s* }{}x) {
        my $name = $1;
        push @var, $self->parse_variable(\$name);

    ### looks like an array constructor
    } elsif ($copy =~ s{ ^ \[ \s* }{}x) {
        local $self->{'_state'}->{'operator_precedence'} = 0; # reset presedence
        my $arrayref = ['arrayref'];
        while (defined(my $var = $self->parse_variable(\$copy))) {
            push @$arrayref, $var;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \] \s* }{}x || die $self->exception('parse.missing.square', "Missing close \]", undef,
                                                          length($$str_ref) - length($copy));
        push @var, \ $arrayref;

    ### looks like a hash constructor
    } elsif ($copy =~ s{ ^ \{ \s* }{}x) {
        local $self->{'_state'}->{'operator_precedence'} = 0; # reset precedence
        my $hashref = ['hashref'];
        while (defined(my $key = $self->parse_variable(\$copy, {auto_quote => qr/\w+/}))) {
            $copy =~ s{ ^ => \s* }{}x;
            my $val = $self->parse_variable(\$copy);
            push @$hashref, $key, $val;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \} \s* }{}x || die $self->exception('parse.missing.curly', "Missing close \} ($copy)", undef,
                                                          length($$str_ref) - length($copy));
        push @var, \ $hashref;

    ### looks like a paren grouper
    } elsif ($copy =~ s{ ^ \( \s* }{}x) {
        local $self->{'_state'}->{'operator_precedence'} = 0; # reset precedence
        my $var = $self->parse_variable(\$copy);
        $copy =~ s{ ^ \) \s* }{}x || die $self->exception('parse.missing.paren', "Missing close \)", undef,
                                                          length($$str_ref) - length($copy));
        @var = @$var;
        pop(@var); # pull off the trailing args of the paren group

    ### nothing to find - return failure
    } else {
        return;
    }

    ### looks for args for the initial
    if ($copy =~ s{ ^ \( \s* }{}x) {
        local $self->{'_state'}->{'operator_precedence'} = 0; # reset precedence
        my $args = $self->parse_args(\$copy);
        $copy =~ s{ ^ \) \s* }{}x || die $self->exception('parse.missing.paren', "Missing close \)", undef,
                                                          length($$str_ref) - length($copy));
        push @var, $args;
    } else {
        push @var, 0;
    }

    ### allow for nested items
    while ($copy =~ s{ ^ ( \.(?!\.) | \|(?!\|) ) \s* }{}x) {
        return if $ARGS->{'auto_quote'}; # auto_quoted thing was too complicated
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
            die "Not sure how to continue parsing on \"$copy\" ($$str_ref)";
        }

        ### looks for args for the nested item
        if ($copy =~ s{ ^ \( \s* }{}x) {
            local $self->{'_state'}->{'operator_precedence'} = 0; # reset precedence
            my $args = $self->parse_args(\$copy);
            $copy =~ s{ ^ \) \s* }{}x || die $self->exception('parse.missing.paren', "Missing close \)", undef,
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
    if (! $self->{'_state'}->{'operator_precedence'}) {
        my $tree;
        my $found;
        while ($copy =~ s{ ^ ($OP_QR) \s* }{}ox) {
            return if $ARGS->{'auto_quote'}; # auto_quoted thing was too complicated
            local $self->{'_state'}->{'operator_precedence'} = 1;
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

    die "Couldn't apply precedence";
}

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
                if ($ARGS->{'set_var'}) {
                    if ($#$var <= $i) {
                        $self->{'_swap'}->{$ref} = $ARGS->{'var_val'};
                        return;
                    } else {
                        $self->{'_swap'}->{$ref} ||= {};
                    }
                }
                $ref = $self->{'_swap'}->{$ref};
            } else {
                return if $ARGS->{'set_var'};
            }
        }
    } elsif (defined $ref) {
        if ($ARGS->{'is_namespace_during_compile'}) {
            $ref = $self->{'NAMESPACE'}->{$ref};
        } else {
            if ($ARGS->{'set_var'}) {
                if ($#$var <= $i) {
                    $self->{'_swap'}->{$ref} = $ARGS->{'var_val'};
                    return;
                } else {
                    $self->{'_swap'}->{$ref} ||= {};
                }
            }
            $ref = $self->{'_swap'}->{$ref};
        }
    }

    ### let the top level thing be a code block
    if (UNIVERSAL::isa($ref, 'CODE')) {
        return if $ARGS->{'set_var'} && $#$var <= $i;
        my @results = $ref->($args ? $self->vivify_args($args) : ());
        $ref = ($#results > 0) ? \@results : $results[0];
    }

    ### vivify the chained levels
    my $seen_filters;
    while (defined $ref && $#$var > $i) {
        my $was_dot_call = $var->[$i++] eq '.';
        my $name         = $var->[$i++];
        my $args         = $var->[$i++];

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

        if (UNIVERSAL::can($ref, $name)) {
            my @results = $ref->$name($args ? $self->vivify_args($args) : ());
            $ref = ($#results > 0) ? \@results : $results[0];
            next;

        } elsif (UNIVERSAL::isa($ref, 'HASH')) {
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
                $ref = $code->($ref, $args ? $self->vivify_args($args) : ());
                next;
            } elsif ($ARGS->{'is_namespace_during_compile'}) {
                return $var; # abort - can't fold namespace variable
            } else {
                $ref = undef;
            }

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
                $ref = $code->($ref, $args ? $self->vivify_args($args) : ());
                next;
            } else {
                $ref = undef;
            }

        } elsif (! ref($ref) && defined($ref)) {
            if (my $code = $self->scalar_op($name)) {
                return if $ARGS->{'set_var'};
                $ref = $code->($ref, $args ? $self->vivify_args($args) : ());
                next;
            } elsif (my $filter = $self->{'FILTERS'}->{$name}) {
                $seen_filters ||= {};
                if ($seen_filters->{$name} ++) {
                    $ref = undef;
                    next;
                }
                $var = ['|', @$filter, splice(@$var, $i)];
                $i = 0;
            } else {
                $ref = undef;
            }
        }

        if (defined($ref) && UNIVERSAL::isa($ref, 'CODE')) {
            my @results = $ref->($args ? $self->vivify_args($args) : ());
            $ref = ($#results > 0) ? \@results : $results[0];
        }

    }

    #debug $ref;

    if ($generated_list && $ARGS->{'list_context'} && UNIVERSAL::isa($ref, 'ARRAY')) {
        return @$ref;
    }
    return $ref;
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
        if (defined(my $name = $self->parse_variable(\$copy, {auto_quote => qr/\w+/}))
            && $copy =~ s{ ^ = \s* }{}x) {
            die $self->exception('parse', 'Named arguments not allowed') if $ARGS->{'positional_only'};
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

sub vivify_args {
    my $self = shift;
    my $vars = shift;
    my $args = shift || {};
    return map {$self->vivify_variable($_, $args)} @$vars;
}

###----------------------------------------------------------------###

sub play_operator {
    my $self = shift;
    my $tree = shift;
    my $args = shift || {};
    my $op = $tree->[0];
    $tree = [@$tree[1..$#$tree]];

    ### allow for operator function override
    if (exists $OP_FUNC->{$op}) {
        return $OP_FUNC->{$op}->($self, $op, $tree, $args);
    }

    ### do constructors and short-circuitable operators
    if ($op eq 'concat' || $op eq '~' || $op eq '_') {
        return join "", grep {defined} $self->vivify_args($tree);
    } elsif ($op eq 'arrayref') {
        my @vals = $self->vivify_args($tree, {list_context => 1});
        return [@vals];
    } elsif ($op eq 'hashref') {
        my @args = $self->vivify_args($tree);
        push @args, undef if ! ($#args % 2);
        return {@args};
    } elsif ($op eq '?') {
        if ($self->vivify_variable($tree->[0])) {
            return defined($tree->[1]) ? $self->vivify_variable($tree->[1]) : undef;
        } else {
            return defined($tree->[2]) ? $self->vivify_variable($tree->[2]) : undef;
        }
    } elsif ($op eq '||' || $op eq 'or') {
        for my $node (@$tree) {
            my $var = $self->vivify_variable($node);
            return $var if $var;
        }
        return '';
    } elsif ($op eq '&&' || $op eq 'and') {
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
    if ($op eq '==')    { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n == $_) }; return 1 }
    elsif ($op eq '!=') { for (@$tree) { $_ = $self->vivify_variable($_); return '' if ! ($n != $_) }; return 1 }
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
    my @args = $self->vivify_args($tree);
    if ($#args == -1) {
        if ($op eq '-') { return - $n }
        die "Not enough args for operator \"$op\"";
    }
    if ($op eq '..')        { return [($n||0) .. ($args[-1]||0)] }
    elsif ($op eq '+')      { $n +=  $_ for @args; return $n }
    elsif ($op eq '-')      { $n -=  $_ for @args; return $n }
    elsif ($op eq '*')      { $n *=  $_ for @args; return $n }
    elsif ($op eq '/'
           || $op eq 'div') { $n /=  $_ for @args; return $n }
    elsif ($op eq '**'
           || $op eq 'pow') { $n **= $_ for @args; return $n }

    die "Un-implemented operation $op";
}

###----------------------------------------------------------------###

sub undefined {''}

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
    my ($self, $tag_ref) = @_;

    my $name = '';
    if ($$tag_ref =~ s{ ^ (\w+) \s* (?! [\.\|]) }{}x) {
        $name = $1;
    }

    return $name;
}

sub play_BLOCK {
    my ($self, $name, $node, $template_ref, $out_ref) = @_;

    my $body_ref = $node->[4] || [];
    $self->{'BLOCKS'}->{$name} = $body_ref; # store a named reference - but do nothing until something processes it

    return;
}

sub parse_CALL { $DIRECTIVES->{'GET'}->{'parse'}->(@_) }

sub play_CALL { $DIRECTIVES->{'GET'}->{'play'}->(@_); return }

sub parse_CASE {
    my ($self, $tag_ref) = @_;
    return if $$tag_ref =~ s{ ^ DEFAULT \s* }{}x;
    return $self->parse_variable($tag_ref);
}

sub parse_CATCH {
    my ($self, $tag_ref) = @_;
    return $self->parse_variable($tag_ref, {auto_quote => qr/\w+ (?: \.\w+)*/x});
}

sub parse_DEFAULT {
    my ($self, $tag_ref) = @_;
    return $DIRECTIVES->{'SET'}->{'parse'}->($self, $tag_ref);
}

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

sub parse_DUMP {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->parse_variable($tag_ref);
    my $val = $self->vivify_variable($ref);
    require Data::Dumper;
    my $str = Data::Dumper::Dumper($val);
    $str =~ s/\$VAR1/$copy/g;
    return $str;
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
    my ($self, $ref, $node, $template_ref, $out_ref) = @_;
    my ($name, $filter) = @$ref;

    return '' if $#$filter == -1;

    $self->{'FILTERS'}->{$name} = $filter if length $name;

    my $sub_tree = $node->[4];

    ### play the block
    my $out = '';
    eval { $self->execute_tree($sub_tree, $template_ref, \$out) };
    die $@ if $@ && ! UNIVERSAL::isa($@, 'CGI::Ex::Template::Exception');

    my $var = [\$out, 0, '|', @$filter]; # make a temporary var out of it

    return $DIRECTIVES->{'GET'}->{'play'}->($self, $var, $node, $template_ref, $out_ref);
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
    my ($self, $ref, $node, $template_ref, $out_ref) = @_;

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
        $self->{'_swap'}->{'loop'} = $items;
        foreach my $i ($items->index .. $#$vals) {
            $items->index($i);
            my $item = $vals->[$i];

            $self->vivify_variable($var, {
                set_var => 1,
                var_val => $item,
            });

            ### execute the sub tree
            eval { $self->execute_tree($sub_tree, $template_ref, $out_ref) };
            if ($@) {
                if (UNIVERSAL::isa($@, 'CGI::Ex::Template::Exception')) {
                    next if $@->[0] =~ /NEXT/;
                    last if $@->[0] =~ /LAST|BREAK/;
                }
                die $@;
            }
        }
    ### if the FOREACH tag doesn't set a var - then everything gets localized
    } else {

        ### localize variable access for the foreach
        my $swap = $self->{'_swap'};
        local $self->{'_swap'} = my $copy = {%$swap};
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
            eval { $self->execute_tree($sub_tree, $template_ref, $out_ref) };
            if ($@) {
                if (UNIVERSAL::isa($@, 'CGI::Ex::Template::Exception')) {
                    next if $@->[0] =~ /NEXT/;
                    last if $@->[0] =~ /LAST|BREAK/;
                }
                die $@;
            }
        }

    }

    return undef;
}

sub parse_GET {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref);
    die $self->exception('parse', "Missing variable name") if ! defined $ref;
    return $ref;
}

sub play_GET {
    my ($self, $ref) = @_;
    my $var = $self->vivify_variable($ref);
    return ref($var) || ! defined($var) ? '' : $var;
}

sub parse_IF {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref);
    return $ref;
}

sub play_IF {
    my ($self, $var, $node, $template_ref, $out_ref) = @_;

    my $val = $self->vivify_variable($var);
    if ($val) {
        my $body_ref = $node->[4] ||= [];
        $self->execute_tree($body_ref, $template_ref, $out_ref);
        return;
    }

    while ($node = $node->[5]) { # ELSE, ELSIF's
        if ($node->[0] eq 'ELSE') {
            my $body_ref = $node->[4] ||= [];
            $self->execute_tree($body_ref, $template_ref, $out_ref);
            return;
        }
        my $var = $node->[3];
        my $val = $self->vivify_variable($var);
        if ($val) {
            my $body_ref = $node->[4] ||= [];
            $self->execute_tree($body_ref, $template_ref, $out_ref);
            return;
        }
    }
    return;
}

sub parse_INCLUDE { $DIRECTIVES->{'PROCESS'}->{'parse'}->(@_) }

sub play_INCLUDE {
    my ($self, $tag_ref, $node, $template_ref, $out_ref) = @_;

    ### localize the swap
    my $swap = $self->{'_swap'};
    local $self->{'_swap'} = {%$swap};

    my $str = $DIRECTIVES->{'PROCESS'}->{'play'}->($self, $tag_ref, $node, $template_ref, $out_ref);

    return $str;
}

sub parse_INSERT {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref, {auto_quote => $QR_FILENAME});
    return $ref;
}

sub play_INSERT {
    my ($self, $var) = @_;
    return '' if ! $var;
    my $filename = $self->vivify_variable($var);

    return $self->include_file($filename);
}

sub parse_MACRO {
    my ($self, $tag_ref, $func, $node) = @_;
    my $copy = $$tag_ref;

    my $name = $self->parse_variable(\$copy, {auto_quote => qr/\w+/x});
    die $self->exception('parse', "Missing macro name") if ! defined $name;
    if (! ref $name) {
        $name = [ $name, 0 ];
    }

    my $args;
    if ($copy =~ s{ ^ \( \s* }{}x) {
        $args = $self->parse_args(\$copy, {positional_only => 1});
        $copy =~ s { ^ \) \s* }{}x || die $self->exception('parse.missing', "Missing close ')'");
    }

    $node->[6] = 1;           # set a flag to keep parsing
    $$tag_ref = $copy;
    return [$name, $args];
}

sub play_MACRO {
    my ($self, $ref, $node, $template_ref, $out_ref) = @_;
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
        $self_copy = {%$self}; # hackish way to avoid circular refs on old perls (pre 5.8)
    }

    ### install a closure in the stash that will handle the macro
    $self->vivify_variable($name, {
        set_var => 1,
        var_val => sub {
            ### macros localize
            my $copy = $self_copy->{'_swap'};
            local $self_copy->{'_swap'}= {%$copy};

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
            $self_copy->execute_tree($sub_tree, $template_ref, \$out);
            return $out;
        },
    });

    return;
}

sub parse_PROCESS {
    my ($self, $tag_ref) = @_;
    my $info = [[], []];
    while (defined(my $filename = $self->parse_variable($tag_ref, {auto_quote => qr/$QR_FILENAME|\w+/}))) {
        push @{$info->[0]}, $filename;
        last if $$tag_ref !~ s{ ^ \+ \s* }{}x;
    }

    ### allow for post process variables
    while (length $$tag_ref) {
        last if $$tag_ref =~ / ^ (\w+) (?: ;|$|\s)/x && $DIRECTIVES->{$1}; ### looks like a directive - we are done

        my $var = $self->parse_variable($tag_ref);
        last if ! defined $var;
        if ($$tag_ref !~ s{ ^ = \s* }{}x) {
            die $self->exception('parse.missing.equals', 'Missing equals while parsing args');
        }
        my $val = $self->parse_variable($tag_ref);
        push @{$info->[1]}, [$var, $val];
    }

    return $info;
}

sub play_PROCESS {
    my ($self, $info, $node, $template_ref, $out_ref) = @_;

    my ($files, $args) = @$info;

    $self->{'_state'}->{'recurse'} ||= 0;
    $self->{'_state'}->{'recurse'} ++;
    if ($self->{'_state'}->{'recurse'} >= $MAX_RECURSE) {
        my $func = $node->[0];
        die "MAX_RECURSE $MAX_RECURSE reached during $func";
    }

    ### set passed args
    foreach (@$args) {
        my ($key, $val) = @$_;
        $self->vivify_variable($key, {
            set_var => 1,
            var_val => $val,
        });
    }

    ### iterat on any passed block or filename
    foreach my $ref (@$files) {
        next if ! defined $ref;
        my $filename = $self->vivify_variable($ref);

        ### handle filenames that look like blocks
        if (my $body_ref = $self->{'BLOCKS'}->{$filename}) {
            my $out = '';
            eval { $self->execute_tree($body_ref, $template_ref, \$out) };
            $$out_ref .= $out;
            if ($@) {
                my $err = $@;
                die $err if ! UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception') || $err->type !~ /RETURN/;
            }
        } else {
            my $str = $self->swap($filename, $self->{'_swap'}); # restart the swap - passing it our current stash
            $$out_ref .= $str if defined $str;
        }
    }

    $self->{'_state'}->{'recurse'} --;
    return;
}

sub parse_SET {
    my ($self, $tag_ref, $func, $node, $initial_var) = @_;
    my @SET;
    my $copy = $$tag_ref;
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
        } elsif ($$tag_ref =~ / ^ (\w+) (?: ;|$|\s)/x && $DIRECTIVES->{$1}) { # next val is a directive - set up capturing
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
    my ($self, $set, $node, $template_ref) = @_;
    foreach (@$set) {
        my ($set, $val) = @$_;
        if (! defined $val) { # not defined
            $val = '';
        } elsif ($node->[4] && $val == $node->[4]) { # a captured directive
            my $sub_tree = $node->[4];
            $sub_tree = $sub_tree->[0]->[4] if $sub_tree->[0] && $sub_tree->[0]->[0] eq 'BLOCK';
            $val = '';
            $self->execute_tree($sub_tree, $template_ref, \$val);
            debug $val;
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

sub parse_SWITCH { $DIRECTIVES->{'GET'}->{'parse'}->(@_) }

sub play_SWITCH {
    my ($self, $var, $node, $template_ref, $out_ref) = @_;

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
            $self->execute_tree($body_ref, $template_ref, $out_ref);
            return;
        }
    }

    if ($default) {
        $self->execute_tree($default, $template_ref, $out_ref);
    }

    return;
}

sub parse_THROW {
    my ($self, $tag_ref, $func, $node) = @_;
    my $name = $self->parse_variable($tag_ref, {auto_quote => qr/\w+ (?: \.\w+)*/x});
    die $self->exception('parse.missing', "Missing name in THROW", $node) if ! $name;
    my $msg  = $self->parse_variable($tag_ref);
    return [$name, $msg];
}

sub play_THROW {
    my ($self, $ref, $node) = @_;
    my ($name, $info) = @$ref;
    $name = $self->vivify_variable($name);
    die $self->exception($name, $info, $node);
}

sub play_TRY {
    my ($self, $foo, $node, $template_ref, $out_ref) = @_;

    my $body_ref = $node->[4];
    eval { $self->execute_tree($body_ref, $template_ref, $out_ref) };

    return if ! $@;
    my $err = $@;

    if (! $node->[5]) {
        $$out_ref = ''; # hack;
        die $self->exception('parse.missing', "Missing CATCH block", $node);
    }

    my $catch_body_ref;
    my $last_found;
    my $type = $err->type;
    while ($node = $node->[5]) { # CATCH
        my $name = $self->vivify_variable($node->[3]);
        $name = '' if ! defined $name;
        if ($type =~ / ^ \Q$name\E \b /x
            && (! defined($last_found) || length($last_found) < length($name))) { # more specific wins
            $catch_body_ref = $node->[4] || [];
            $last_found     = $name;
        }
    }

    die $err if ! $catch_body_ref;

    local $self->{'_swap'}->{'error'} = $err;
    $self->execute_tree($catch_body_ref, $template_ref, $out_ref);
    return;
}

sub parse_UNLESS {
    my $ref = $DIRECTIVES->{'IF'}->{'parse'}->(@_);
    return [ \ [ '!', $ref ], 0 ];
}

sub play_UNLESS { return $DIRECTIVES->{'IF'}->{'play'}->(@_) }

sub parse_USE {
    my ($self, $tag_ref) = @_;

    my $var;
    my $copy = $$tag_ref;
    if (defined(my $_var = $self->parse_variable(\$copy, {auto_quote => qr/\w+/}))
        && $copy =~ s{ ^ = \s* }{}x) {
        $var = $_var;
        $$tag_ref = $copy;
    }

    $copy = $$tag_ref;
    my $module = $self->parse_variable(\$copy, {auto_quote => qr/\w+ (?: (?:\.|::) \w+)*/x});
    die $self->exception('parse', "Missing plugin name while parsing $$tag_ref") if ! defined $module;
    $module =~ s/\./::/g;

    my $args;
    if ($copy =~ s{ ^ \( \s* }{}x) {
        $args = $self->parse_args(\$copy);
        $copy =~ s { ^ \) \s* }{}x || die $self->exception('parse.missing', "Missing close ')'");
    }

    $$tag_ref = $copy;
    return [$var, $module, $args];
}

sub play_USE {
    my ($self, $ref, $node, $template_ref, $out_ref) = @_;
    my ($var, $module, $args) = @$ref;

    ### get the stash storage location - default to the module
    $var = $module if ! defined $var;
    my @var = map {($_, 0, '.')} split /(?:\.|::)/, $var;
    pop @var; # remove the trailing '.'

    ### look for a plugin_base
    my $base = $self->{'PLUGIN_BASE'} || 'Template::Plugin'; # I'm not maintaining plugins - leave that to TT
    my $package = "${base}::${module}";
    my $require = "$package.pm";
    $require =~ s|::|/|g;

    ### try and load the module - fall back to bare module if allowed
    my $obj;
    if (eval {require $require}) {
        my $shape   = $package->load;
        my $context = bless {_template => $self}, 'CGI::Ex::Template::_Context'; # a fake context
        my @args    = $args ? $self->vivify_args($args) : ();
        $obj = $shape->new($context, @args);
    } elsif ($self->{'LOAD_PERL'}) {
        my $require = "$module.pm";
        $require =~ s|::|/|g;
        if (eval {require $require}) {
            my @args = $args ? $self->vivify_args($args) : ();
            $obj = $module->new(@args);
        }
    }
    if (! $obj) {
        my $err = $@ || 'Unknown error while loading $module';
        die $self->exception('plugin', $err);
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
        $copy =~ s{ ^ \) \s* }{}x || die "Missing close \")\"";
        $$tag_ref = $copy;
    } else {
        $var2 = $self->parse_variable($tag_ref);
    }
    return [$var, $var2];
}

sub play_WHILE {
    my ($self, $ref, $node, $template_ref, $out_ref) = @_;

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
        eval { $self->execute_tree($sub_tree, $template_ref, $out_ref) };
        if ($@) {
            if (UNIVERSAL::isa($@, 'CGI::Ex::Template::Exception')) {
                next if $@->[0] =~ /NEXT/;
                last if $@->[0] =~ /LAST|BREAK/;
            }
            die $@;
        }
    }

    return undef;
}

sub parse_WRAPPER { $DIRECTIVES->{'INCLUDE'}->{'parse'}->(@_) }

sub play_WRAPPER {
    my ($self, $var, $node, $template_ref, $out_ref) = @_;
    my $sub_tree = $node->[4] || return;

    my $out = '';
    $self->execute_tree($sub_tree, $template_ref, \$out);

    local $self->{'_swap'}->{'content'} = $out;
    return $DIRECTIVES->{'INCLUDE'}->{'play'}->(@_)
}

###----------------------------------------------------------------###

sub stash { shift->{'_swap'} ||= {} }

sub include_path {
    my $self = shift;
    return $self->{'INCLUDE_PATH'} ||= [@INCLUDE_PATH];
}

sub include_filename {
    my ($self, $file) = @_;
    if ($file =~ m|^/|) {
        die $self->exception('file', "$file ABSOLUTE paths disabled") if ! $self->{'ABSOLUTE'};
        return $file if -e $file;
    } elsif ($file =~ m|^\./|) {
        die $self->exception('file', "$file RELATIVE paths disabled") if ! $self->{'RELATIVE'};
        return $file if -e $file;
    } else {
        my $paths = $self->include_path;
        $paths = [$paths] if ! ref $paths;
        foreach my $path (@$paths) {
            return "$path/$file" if -e "$path/$file";
        }
    }
    die $self->exception('file', "$file not found");
}

sub include_file {
    my ($self, $file) = @_;
    my $full = $self->include_filename($file);
    open(my $fh, "<$full") || die $self->exception('file', "$file couldn't be opened: $!");
    read $fh, my $txt, -s $full;
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

    ### localize the stash
    $swap ||= {};
    my $stash = $self->stash;
    my $copy = {%$stash, %$swap};

    ### do the swap
    $content = $self->swap($content, $copy);
    $self->{'error'} = $@;

    ### put it back out
    $out ||= $self->{'OUTPUT'};
    if (ref $out) {
        if (UNIVERSAL::isa($out, 'CODE')) {
            $out->($content);
        } elsif (UNIVERSAL::can($out, 'print')) {
            $out->print($content);
        } elsif (UNIVERSAL::isa($out, 'SCALAR')) { # reference to a string
            $$out = $content;
        } elsif (UNIVERSAL::isa($out, 'ARRAY')) {
            push @$out, $content;
        } else { # should be a file handle
            print $out $content;
        }
    } elsif ($out) { # should be a filename
        my $file;
        if ($out =~ m|^/|) {
            die $self->exception('file', "ABSOLUTE paths disabled") if ! $self->{'ABSOLUTE'};
            $file = $out;
        } elsif ($out =~ m|^\./|) {
            die $self->exception('file', "RELATIVE paths disabled") if ! $self->{'RELATIVE'};
            $file = $out;
        } else {
            die $self->exception('file', "OUTPUT_PATH not set") if ! $self->{'OUTPUT_PATH'};
            $file = $self->{'OUTPUT_PATH'} . '/' . $out;
        }
        open(my $fh, ">$file") || die $self->exception('file', "$out couldn't be opened for writing: $!");
        print $fh $content;
    } else {
        print $content;
    }

    return if $self->{'error'};
    return 1;
}

sub error { shift->{'error'} }

###----------------------------------------------------------------###

package CGI::Ex::Template::Exception;

use overload '""' => \&as_string;
use overload bool => sub { defined shift };

sub new {
    my ($class, $type, $msg, $node, $pos, $str_ref) = @_;
    return bless [$type, $msg, $node, $pos, $str_ref], $class;
}

sub type { shift->[0] }

sub msg { shift->[1] || '' }

sub node {
    my $self = shift;
    $self->[2] = shift if $#_ == 0;
    $self->[2];
}

sub offset { shift->[3] || 0 }

sub str_ref {
    my $self = shift;
    $self->[4] = shift if $#_ == 0;
    $self->[4];
}

sub as_string {
    my $self = shift;
    my $msg  = $self->type .' error - '. $self->msg;
    if (my $node = $self->node) {
        $msg .= " (In tag $node->[0] starting at char ".($node->[1] + $self->offset).")";
    }
    $msg .= "\n" if $msg !~ /\n\z/;
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

sub AUTOLOAD { shift->_template->exception('not_implemented', "The method $AUTOLOAD has not been implemented") }

sub DESTROY {}

###----------------------------------------------------------------###

1;

__END__

=head1 SYNOPSIS

  None yet.

=head1 DESCRIPTION

=head1 TODO

    Benchmark text processing
    Add META
    Add FINAL
    Look at other configs
    Allow USE to use our Iterator
    Allow for Interpolate
    Get several test suites to pass
    Add remaining filters

=head1 OPERATORS

    # ..
    # ()
    # && and
    # || or
    # _ ~ concat
    # +
    # -
    # / div
    # *
    # % mod
    # ^ ** pow
    # ! not
    # arrayref
    # hashref
    # >
    # >=
    # <
    # <=
    # ==
    # !=
    # gt
    # ge
    # lt
    # le
    # eq
    # ne

=head1 VARIABLE PARSE TREE

    one                [ 'one',  0 ]
    one()              [ 'one',  [] ]
    one(two)           [ 'one',  [ ['two', 0] ] ]
    one.two            [ 'one',  0, '.', 'two',  0 ]
    one|two            [ 'one',  0, '|', 'two',  0 ]
    one.$two           [ 'one',  0, '.', ['two', 0 ] ]
    one.${two().three} [ 'one',  0, '.', ['two', [], '.', 'three', 0], 0]
    "one"              "one"
    2.34               2.34
    "one"|length       [ \"one", 0, '|', 'length', 0 ]
    "one $a two"       [ \ [ 'concat', [\ 'one ', 0], ['a', 0], [\ ' two', 0 ] ], 0 ]
    [0,1,2]            [ \ [ 'arrayref', 0, 1, 2 ], 0 ]
    [0,1,2].size       [ \ [ 'arrayref', 0, 1, 2 ], 0, '.', 'size', 0 ]
    ['a', a, $a ]      [ \ [ 'arrayref', 'a', ['a', 0], [['a', 0], 0] ], 0]
    {a  => 'b'}        [ \ [ 'hashref',  'a', 'b' ], 0 ]
    {a  => 'b'}.size   [ \ [ 'hashref',  'a', 'b' ], 0, '.', 'size', 0 ]
    {$a => b}          [ \ [ 'hashref',  ['a', 0], ['b', 0] ], 0 ]
    1 + 2 + 3 + 4      [ \ [ '+', 1, 2, 3, 4 ], 0]
    a + b              [ \ [ '+', ['a', 0], ['b', 0] ], 0 ]
    a * (b + c)        [ \ [ '*', ['a', 0], [ \ ['+', ['b', 0], ['c', 0]], 0 ]], 0 ]
    (a + b)            [ \ [ '+', ['a', 0], ['b', 0] ]], 0 ]
    (a + b) * c        [ \ [ '*', [ \ [ '+', ['a', 0], ['b', 0] ], 0 ], ['c', 0] ], 0 ]

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut

