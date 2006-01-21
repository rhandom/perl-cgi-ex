package CGI::Ex::Template;

=head1

CGI::Ex::Template - Lightweight TT2 parser

=cut

use CGI::Ex::Dump qw(debug);
use strict;
use vars qw(@INCLUDE_PATH
            $TAGS
            $SCALAR_OPS $HASH_OPS $LIST_OPS
            $DIRECTIVES
            $OPERATORS
            $OP_TRINARY
            $OP_FUNC
            $OP_QR
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
        WHILE => {
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
                     !   95
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
    $OP_TRINARY ||= {'?' => ':'};
    $OP_FUNC    ||= {};
    sub _op_qr { # no mixed \w\W operators
        my $chrs = join '|', map {quotemeta $_} grep {/^\W{2,}$/} @_;
        my $chr  = join '',  map {quotemeta $_} grep {/^\W$/} @_;
        my $word = join '|', grep {/^\w+$/} @_;
        return "$chrs|[$chr]|$word";
    }
    $OP_QR ||= _op_qr(sort(keys(%$OPERATORS), values(%$OP_TRINARY)));

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

        if ($self->{'COMPILE_DIR'} && $self->{'COMPILE_EXT'}) {
            $store_file = $self->{'COMPILE_DIR'} .'/'. $file . $self->{'COMPILE_EXT'} .'.sto';
            if (-e $store_file && -M _ == -M $file) {
                require Storable;
                $tree = Storable::retrieve($store_file);
                undef $store_file;
            }
        }
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

    if ($file && ! $self->{'no_cache'}) {
        $self->{'_documents'}->{$file} ||= [$str_ref, $tree];
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

    die $err if ! UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception');
    $err->str_ref(\$_[0]);
    eval { die $err };
    return '' if $err->type !~ /STOP|RETURN|NEXT|LAST|BREAK/;
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

    my @tree;           # the parsed tree
    my @state;          # maintain block levels
    my @move_to_front;  # items that need to be declared first
    my $i = 0;          # start index
    my $j = 0;          # end index
    my $last = 0;       # previous end index
    my $post_chomp = 0; # previous post_chomp setting
    my $continue;       # multiple directives in the same tag
    my $postop;
    my $tag;
    my $level;
    while (1) {
        ### continue looking for information in a semi-colon delimited tag
        if ($continue) {
            $i = $j - length $tag;
            $level = [undef, $i, $j];

        ### look through the string using index
        } else {
            $i = index($$str_ref, $START, $last);
            last if $i == -1;
            push @tree, ['TEXT', $last, $i, [0, $post_chomp]] if $last != $i;
            $j = index($$str_ref, $END, $i + $len_s);
            $last = $j + $len_e;
            if ($j == -1) { # missing closing tag
                $last = length($$str_ref);
                last;
            }
            $tag = substr($$str_ref, $i + $len_s, $j - ($i + $len_s));
            $level = [undef, $i + $len_s, $j];
        }

        ### take care of whitespace
        if ($tag =~ s/^(\#?)-/$1/ || ($self->{'PRE_CHOMP'} && $tag !~ s/^(\#?)\+/$1/)) {
            $tree[-1]->[3]->[0] = 1 if $tree[-1] && $tree[-1]->[0] eq 'TEXT';
            $level->[1] ++;
        }
        if ($tag =~ s/-$// || ($self->{'POST_CHOMP'} && $tag !~ s/\+$//)) {
            $post_chomp = 1;
            $level->[2] --;
        } else {
            $post_chomp = $self->{'POST_CHOMP'};
        }
        if ($tag =~ /^\#/) { # leading # means to comment the entire section
            $level->[0] = 'COMMENT';
            push @tree, $level;
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

            ### store out this current node level
            my $func = $level->[0] = $1;
            $tag =~ s{ ^ \w+ \s* }{}x;
            push @tree, $level if ! $postop;

            ### anything that behaves as a block ending
            if ($func eq 'END' || $DIRECTIVES->{$func}->{'continue_block'}) {
                if ($#state == -1) {
                    die $self->exception('parse', "Found an $func while not in a block", $level);
                } else {
                    ### store any child nodes into the parent node
                    my $parent_level = pop @state;
                    my $start_index = $parent_level->[4] = $i + $len_s;
                    my $j = $#tree;
                    for ( ; $j >= 0; $j--) {
                        last if defined($tree[$j]->[4]) && $tree[$j]->[4] == $start_index;
                    }
                    my @sub_tree = splice @tree, $j + 1, $#tree - ($j + 1), (); # remove from main tree - but store
                    my $storage = $parent_level->[5] ||= [];
                    @$storage = @sub_tree;
                    if ($DIRECTIVES->{$parent_level->[0]}->{'move_to_front'}) {
                        push @move_to_front, splice(@tree, -2, 2, ());
                    }

                    if ($DIRECTIVES->{$func}->{'continue_block'}) {
                        my $parent_type = $parent_level->[0];
                        if (! $DIRECTIVES->{$func}->{'continue_block'}->{$parent_type}) {
                            die $self->exception('parse', "Found unmatched nested block", $level, 0);
                        }
                        $level->[3] = eval { $DIRECTIVES->{$func}->{'parse'}->($self, \$tag, $func, $level) };
                        if ($@) {
                            $@->node($level) if UNIVERSAL::can($@, 'node') && ! $@->node;
                            die $@;
                        }
                        $level->[4] = -1;
                        $level->[5] = [];
                        push @state, $level;
                        $parent_level->[6] = $level;
                    }
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
                if ($DIRECTIVES->{$func}->{'block'} && ! $postop) {
                    $level->[4] = -1;
                    $level->[5] = [];
                    push @state, $level;
                }
                $level->[3] = eval { $DIRECTIVES->{$func}->{'parse'}->($self, \$tag, $func, $level) };
                if (my $err = $@) {
                    $err->node($level) if UNIVERSAL::can($err, 'node') && ! $err->node;
                    die $err;
                }
                $postop->[7] = $level if $postop;
            }

        ### allow for bare variable getting and setting
        } elsif (my $var = $self->parse_variable(\$tag)) {
            push @tree, $level;
            if ($tag =~ s{ ^ = \s* }{}x) {
                eval {
                    my $val = $self->parse_variable(\$tag);
                    $tag =~ s{ ^ ; \s* }{}x;
                    my $SET = $DIRECTIVES->{'SET'}->{'parse'}->($self, \$tag, 'SET', $level) || [];
                    unshift @$SET, [$var, $val];
                    $level->[0] = 'SET';
                    $level->[3] = $SET;
                };
                if ($@) {
                    $@->node($level) if UNIVERSAL::can($@, 'node') && ! $@->node;
                    die $@;
                }
            } else {
                #die "Found trailing info during variable access \"$tag" if $tag;
                $level->[0] = 'GET';
                $level->[3] = $var;
            }

        } else {
            my $all  = substr($$str_ref, $i + $len_s, $j - ($i + $len_s));
            $all =~ s/^\s+//;
            $all =~ s/\s+$//;
            die "Not sure how to handle tag \"$all\"";
        }

        ### look for more directives in the tag
        if ($tag =~ s{ ^ ; \s* }{}x) {
            $continue = 1;
            $postop   = undef;
        } elsif ($tag =~ / ^ (\w+) \s /x && $DIRECTIVES->{$1} && $DIRECTIVES->{$1}->{'postop'}) {
            $continue = 1;
            $postop   = $level;
        } else {
            debug $level if length $tag;
            die "Found trailing info \"$tag\"" if length $tag;
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
    return undef if $#tree  == -1;

    push @tree, ['TEXT', $last, length($$str_ref), [0, $post_chomp]] if $last != length($$str_ref);

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
    #                4: start position marker for matching END directives
    #                5: sub tree for block types
    #                6: continuation sub trees for some block types
    #                7: post operations holder
    my $val;
    for my $node (@$tree) {
        if ($node->[0] eq 'TEXT') {
            $val = substr($$template_ref, $node->[1], $node->[2] - $node->[1]);

            $val =~ s{ (?:\n|^) [^\S\n]* \z }{}xm   if $node->[3]->[0]; # pre_chomp
            $val =~ s{ \G [^\S\n]* (?:\n?$|\n) }{}x if $node->[3]->[1]; # post_chomp
            $$out_ref .= $val;
            next;
        }

        ### had a post operator - turn it inside out - TODO - it would be nice to do this to the original tree
        if ($node->[7]) {
            while (my $post_node = delete $node->[7]) {
                $post_node->[5] = [$node];
                $node = $post_node;
            }
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
            $val = $DIRECTIVES->{$node->[0]}->{'play'}->($self, $node->[3], $node, $template_ref, $out_ref);
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
            return [\$str, 0];
        } elsif ($$str_ref =~ s{ ^ \$ (\w+) \b \s* }{}x # auto-quoted dollars
                 || $$str_ref =~ s{ ^ \$\{ \s* ([^\}]+) \} \s* }{}x) {
            my $name = $1;
            return [$name, 0];
        }
    }

    my @var;
    my $copy = $$str_ref;

    ### allow for leading $foo or ${foo.bar} type constructs
    if ($copy =~ s{ ^ \$ (\w+) \b \s* }{}x
        || $copy =~ s{ ^ \$\{ \s* ([^\}]+) \} \s* }{}x) {
        my $name = $1;
        push @var, $self->parse_variable(\$name);

    ### allow for numbers
    } elsif ($copy =~ s{ ^ (-? (?:\d*\.\d+ | \d+) ) \s* }{}x) {
        my $number = $1;
        push @var, \$number;

    ### allow for literal strings
    } elsif ($copy =~ s{ ^ ([\"\']) (|.*?[^\\]) \1 \s* }{}xs) {
        if ($1 eq "'") { # no interpolation on single quoted strings
            my $str = $2;
            push @var, \$str;
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
            } else {
                push @var, \ ['concat', map {ref($_) ? $_ : [\$_, 0]} @pieces];
            }
        }

    ### looks like an array constructor
    } elsif ($copy =~ s{ ^ \[ \s* }{}x) {
        local $self->{'_state'}->{'operator_precedence'} = 0; # reset presedence
        my $arrayref = ['arrayref'];
        while (my $var = $self->parse_variable(\$copy)) {
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
        while (my $key = $self->parse_variable(\$copy, {auto_quote => qr/\w+/})) {
            $copy =~ s{ ^ => \s* }{}x;
            my $val = $self->parse_variable(\$copy);
            push @$hashref, $key, $val;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \} \s* }{}x || die $self->exception('parse.missing.curly', "Missing close \}", undef,
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

    ### looks like a normal variable start
    } elsif ($copy =~ s{ ^ (\w+) \s* }{}x) {
        push @var, $1;

    ### nothing to find - return failure
    } else {
        return undef;
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

    ### allow for all "operators"
    if (! $self->{'_state'}->{'operator_precedence'}) {
        my $tree;
        my $found;
        while ($copy =~ s{ ^ ($OP_QR) \s* }{}ox) {
            local $self->{'_state'}->{'operator_precedence'} = 1;
            my $op   = $1;
            my $var2 = $self->parse_variable(\$copy);
            push (@{ $tree ||= [] }, $op, $var2);
            $found->{$op} = $OPERATORS->{$op} if exists $OPERATORS->{$op};
        }

        ### if we found operators - tree the nodes by operator precedence
        if ($tree) {
            if ($#$tree == 1) { # only one operator - keep simple things fast
                @var = (\ [$tree->[0], [@var], $tree->[1]], 0);
            } else {
                unshift @$tree, [@var];
                @var = @{ $self->apply_precedence($tree, $found) };
            }
        }
    }

#    debug \@var, $copy;
    $$str_ref = $copy; # commit the changes
    return \@var;
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
    my $self = shift;
    my $var  = shift;
    my $ARGS = shift || {};
    my $i    = 0;
    my $generated_list;

    ### determine the top level of this particular variable access
    my $ref  = $var->[$i++];
    my $args = $var->[$i++];
    if (ref $ref) {
        if (ref($ref) eq 'SCALAR') {
            return if $ARGS->{'set_var'};
            $ref = $$ref;
        } elsif (ref($ref) eq 'REF') {
            return if $ARGS->{'set_var'};
            $generated_list = 1 if ${ $ref }->[0] eq '..';
            $ref = $self->play_operator($$ref);
        } else {
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

    if ($ARGS->{'list_context'} && $generated_list && UNIVERSAL::isa($ref, 'ARRAY')) {
        return @$ref;
    }
    return $ref;
}

sub parse_args {
    my $self    = shift;
    my $str_ref = shift;
    my $ARGS    = shift || {};

    my @args;
    while (my $var = $self->parse_variable($str_ref)) {
        push @args, $var;
        $$str_ref =~ s{ ^ , \s* }{}x;
    }
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
            return $tree->[1] ? $self->vivify_variable($tree->[1]) : undef;
        } else {
            return $tree->[2] ? $self->vivify_variable($tree->[2]) : undef;
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
    return $SCALAR_OPS->{$name};
}

sub list_op {
    my ($self, $name) = @_;
    return $LIST_OPS->{$name};
}

sub hash_op {
    my ($self, $name) = @_;
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

    my $body_ref = $node->[5] || [];
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
        next if ! $set;
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

    my $filter = $self->parse_variable($tag_ref) || [];

    return [$name, $filter];
}

sub play_FILTER {
    my ($self, $ref, $node, $template_ref, $out_ref) = @_;
    my ($name, $filter) = @$ref;

    return '' if $#$filter == -1;

    $self->{'FILTERS'}->{$name} = $filter if length $name;

    my $sub_tree = $node->[5];

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
    my $set_loop;
    if (! UNIVERSAL::isa($items, 'CGI::Ex::Template::Iterator')) {
        $items = CGI::Ex::Template::Iterator->new($items);
        $set_loop = 1;
    }

    my $prev_val = defined($var) ? $self->vivify_variable($var) : undef;
    my $sub_tree = $node->[5];

    ### localize variable access for the foreach
    my $stash = $self->{'_swap'};
    my @keys  = keys %$stash;
    my %keys = map {$_ => 1} @keys;
    local @$stash{@keys} = values %$stash;
    $stash->{'loop'} = $items if $set_loop;

    ### iterate use the iterator object
    my $vals = $items->items;
    #foreach (my $i = $items->index; $i <= $#$vals; $items->index(++ $i)) {
    foreach my $i ($items->index .. $#$vals) {
        $items->index($i);
        my $item = $vals->[$i];

        ### update vars as needed
        if (defined $var) {
            $self->vivify_variable($var, {
                set_var => 1,
                var_val => $item,
            });
        } elsif (ref($item) eq 'HASH') {
            @$stash{keys %$item} = values %$item;
        }

        ### execute the sub tree
        eval { $self->execute_tree($sub_tree, $template_ref, $out_ref) };
        if ($@) {
            if (UNIVERSAL::isa($@, 'CGI::Ex::Template::Exception')) {
                next if $@->[0] =~ /NEXT/;
                last if $@->[0] =~ /LAST|BREAK/;
            }
            delete @$stash{grep {!$keys{$_}} keys %$stash};
            die $@;
        }


    }

    ### remove items added to stash during this run
    delete @$stash{grep {!$keys{$_}} keys %$stash};

    $self->vivify_variable($var, {
        set_var => 1,
        var_val => $prev_val,
    }) if defined $var;

    return undef;
}

sub parse_GET {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref);
    die $self->exception('parse', "Missing variable name") if ! $ref;
    return $ref;
}

sub play_GET {
    my ($self, $ref) = @_;
    my $var = $self->vivify_variable($ref);
    return ref($var) ? '' : $var;
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
        my $body_ref = $node->[5] ||= [];
        $self->execute_tree($body_ref, $template_ref, $out_ref);
        return;
    }

    while ($node = $node->[6]) { # ELSE, ELSIF's
        if ($node->[0] eq 'ELSE') {
            my $body_ref = $node->[5] ||= [];
            $self->execute_tree($body_ref, $template_ref, $out_ref);
            return;
        }
        my $var = $node->[3];
        my $val = $self->vivify_variable($var);
        if ($val) {
            my $body_ref = $node->[5] ||= [];
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
    my @keys  = keys %$swap;
    local @$swap{@keys} = values %$swap; # note that we are only "cloning" one level deep

    my $str = $DIRECTIVES->{'PROCESS'}->{'play'}->($self, $tag_ref, $node, $template_ref, $out_ref);

    ### kill added keys
    my %keys = map {$_ => 1} @keys;
    delete @$swap{grep {!$keys{$_}} keys %$swap};

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

sub parse_PROCESS {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref, {auto_quote => qr/$QR_FILENAME|\w+/});
    return $ref;
}

sub play_PROCESS {
    my ($self, $var, $node, $template_ref, $out_ref) = @_;

    return undef if ! $var;
    my $filename = $self->vivify_variable($var);

    $self->{'_state'}->{'recurse'} ||= 0;
    $self->{'_state'}->{'recurse'} ++;
    if ($self->{'_state'}->{'recurse'} >= $MAX_RECURSE) {
        my $func = $node->[0];
        die "MAX_RECURSE $MAX_RECURSE reached during $func on $filename";
    }

    ### see if the filename is an existing block name
    if (my $body_ref = $self->{'BLOCKS'}->{$filename}) {
        my $out = '';
        eval { $self->execute_tree($body_ref, $template_ref, \$out) };
        $$out_ref .= $out;
        if ($@) {
            my $err = $@;
            die $err if ! UNIVERSAL::isa($err, 'CGI::Ex::Template::Exception') || $err->type !~ /RETURN/;
        }
        $self->{'_state'}->{'recurse'} --;
        return;
    }

    my $str = $self->swap($filename, $self->{'_swap'}); # restart the swap - passing it our current stash

    $self->{'_state'}->{'recurse'} --;

    $$out_ref .= $str if defined $str;
    return;
}

sub parse_SET {
    my ($self, $tag_ref) = @_;
    my @SET;
    my $copy = $$tag_ref;
    while (length $$tag_ref) {
        my $set = $self->parse_variable($tag_ref);
        die "Couldn't find variable on SET on $copy" if ! $set;
        my $val;
        if ($$tag_ref =~ s{ ^ = \s* }{}x) {
            $val = $self->parse_variable($tag_ref);
        } else {
            $val = undef;
        }
        $$tag_ref =~ s{ ^ ; \s*}{}x;
        push @SET, [$set, $val];
    }
    return \@SET;
}

sub play_SET {
    my ($self, $set) = @_;
    foreach (@$set) {
        my ($set, $val) = @$_;
        $val = defined($val) ? $self->vivify_variable($val) : '';
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
    ### $node->[5] is thrown away

    my $default;
    while ($node = $node->[6]) { # CASES
        my $var = $node->[3];
        if (! $var) {
            $default = $node->[5];
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

            my $body_ref = $node->[5] ||= [];
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

    my $body_ref = $node->[5];
    eval { $self->execute_tree($body_ref, $template_ref, $out_ref) };

    return if ! $@;
    my $err = $@;

    if (! $node->[6]) {
        $$out_ref = ''; # hack;
        die $self->exception('parse.missing', "Missing CATCH block", $node);
    }

    my $catch_body_ref;
    my $last_found;
    my $type = $err->type;
    while ($node = $node->[6]) { # CATCH
        my $name = $self->vivify_variable($node->[3]);
        $name = '' if ! defined $name;
        if ($type =~ / ^ \Q$name\E \b /x
            && (! defined($last_found) || length($last_found) < length($name))) { # more specific wins
            $catch_body_ref = $node->[5] || [];
            $last_found     = $name;
        }
    }

    die $err if ! $catch_body_ref;

    local $self->{'_swap'}->{'error'} = $err;
    $self->execute_tree($catch_body_ref, $template_ref, $out_ref);
    return;
}

sub parse_WHILE {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $var;
    my $var2;
    if ($copy =~ s{ ^ \( \s* }{}x
        && ($var = $self->parse_variable(\$copy))
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

    my $prev_val = defined($var) ? $self->vivify_variable($var) : undef;

    my $sub_tree = $node->[5];

    ### iterate use the iterator object
    my $max = $WHILE_MAX;
    while (--$max > 0) {

        my $value = $self->vivify_variable($var2);
        last if ! $value;

        ### update vars as needed
        if (defined $var) {
            $self->vivify_variable($var, {
                set_var => 1,
                var_val => $value,
            });
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

    $self->vivify_variable($var, {
        set_var => 1,
        var_val => $prev_val,
    }) if defined $var;

    return undef;
}

sub parse_WRAPPER { $DIRECTIVES->{'INCLUDE'}->{'parse'}->(@_) }

sub play_WRAPPER {
    my ($self, $var, $node, $template_ref, $out_ref) = @_;
    my $sub_tree = $node->[5] || return;

    my $out = '';
    $self->execute_tree($sub_tree, $template_ref, \$out);

    local $self->{'_swap'}->{'content'} = $out;
    return $DIRECTIVES->{'INCLUDE'}->{'play'}->(@_)
}

###----------------------------------------------------------------###

sub stash {
    my $self = shift;
    return $self->{'stash'} ||= {};
}

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
    my $stash = $self->stash;
    my @keys  = keys %$stash;
    local @$stash{@keys} = values %$stash;
    local @$stash{keys %$swap}  = values %$swap;

    ### do the swap
    $content = $self->swap($content, $stash);

    ### remove items added to stash
    my %keys = map {$_ => 1} @keys;
    delete @$stash{grep {!$keys{$_}} keys %$stash};

    ### put it back out
    if (ref $out) {
        if (UNIVERSAL::isa($out, 'SCALAR')) { # reference to a string
            $$out = $content;
        } elsif (UNIVERSAL::isa($out, 'CODE')) {
            $out->($content);
        } elsif (UNIVERSAL::can($out, 'print')) {
            $out->print($content);
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

    return 1;
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
    my $msg  = $self->type .' - '. $self->msg;
    if (my $node = $self->node) {
        $msg .= " (In tag $node->[0] starting at char ".($node->[1] + $self->offset).")";
    }
    return "$msg\n";
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

1;

__END__

=head1 NAME

CGI::Ex::Template - Beginning interface to Templating systems - for they are many

=head1 SYNOPSIS

  None yet.

=head1 DESCRIPTION

=head1 TODO

    Finish USE
    Finish MACRO
    Finish META
    Argument parsing

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
    "one"              [ \"one", 0 ]
    2.34               [ \2.34,  0 ]
    "one"|length       [ \"one", 0, '|', 'length', 0 ]
    "one $a two"       [ \ [ 'concat', [\ 'one ', 0], ['a', 0], [\ ' two', 0 ] ], 0 ]
    [0,1,2]            [ \ [ 'arrayref', [\0, 0], [\1, 0], [\2, 0] ], 0 ]
    [0,1,2].size       [ \ [ 'arrayref', [\0, 0], [\1, 0], [\2, 0] ], 0, '.', 'size', 0 ]
    ['a', a, $a ]      [ \ [ 'arrayref', [\ 'a', 0], ['a', 0], [['a', 0], 0] ], 0]
    {a  => 'b'}        [ \ [ 'hashref',  [\ 'a', 0], [\ 'b', 0] ], 0 ]
    {a  => 'b'}.size   [ \ [ 'hashref',  [\ 'a', 0], [\ 'b', 0] ], 0, '.', 'size', 0 ]
    {$a => b}          [ \ [ 'hashref',  ['a', 0], ['b', 0] ], 0 ]
    a + b              [ \ [ '+', ['a', 0], ['b', 0] ], 0 ]
    a * (b + c)        [ \ [ '*', ['a', 0], [ \ ['+', ['b', 0], ['c', 0]], 0 ]], 0 ]
    (a + b)            [ \ [ '+', ['a', 0], ['b', 0] ]], 0 ]
    (a + b) * c        [ \ [ '*', [ \ [ '+', ['a', 0], ['b', 0] ], 0 ], ['c', 0] ], 0 ]

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut

