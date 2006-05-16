package CGI::Ex::Var;

=head1 NAME

CGI::Ex::Var - Variable and expression parsing and execution for CGI::Ex::Template (and other takers)

=cut

use strict;

use vars qw(
            $SCALAR_OPS
            $FILTER_OPS
            $LIST_OPS
            $HASH_OPS
            $FILTERS

            $OPERATORS
            $OP_UNARY
            $OP_BINARY
            $OP_TRINARY

            $QR_OP
            $QR_OP_UNARY
            $QR_OP_PARENED
            $QR_COMMENTS
            $QR_AQ_NOTDOT

            $RT_NAMESPACE
            $RT_FILTERS
            $RT_GET_CONTEXT
            $RT_DEBUG_UNDEF
            $RT_UNDEFINED_SUB
            $RT_OPERATOR_PRECEDENCE

            $TT_FILTERS
            );
use constant trace => 0;

BEGIN {
    $SCALAR_OPS = {
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

    $FILTER_OPS = { # generally - non-dynamic filters belong in scalar ops
        eval     => [\&filter_eval, 1],
        evaltt   => [\&filter_eval, 1],
        file     => [\&filter_redirect, 1],
        redirect => [\&filter_redirect, 1],
    };

    $LIST_OPS = {
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

    $HASH_OPS = {
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

    ### Runtime set variables that control lookups of various pieces of info
    $RT_NAMESPACE   = {};
    $RT_FILTERS     = {};
    $RT_GET_CONTEXT = sub { {} };
    $RT_DEBUG_UNDEF = 0;
    $RT_OPERATOR_PRECEDENCE = 0;

    ### setup the operator parsing
    $OPERATORS ||= [
        # name => # order, precedence, symbols, only_in_parens, sub to create
        [2, 96, ['**', '^', 'pow'],  0, sub {bless(shift(), 'CGI::Ex::_pow')}    ],
        [1, 93, ['!'],               0, sub {bless(shift(), 'CGI::Ex::_not')}    ],
        [1, 93, ['-'],               0, sub {bless(shift(), 'CGI::Ex::_negate')} ],
        [2, 90, ['*'],               0, sub {bless(shift(), 'CGI::Ex::_mult')}   ],
        [2, 90, ['/'],               0, sub {bless(shift(), 'CGI::Ex::_div')}    ],
        [2, 90, ['div', 'DIV'],      0, sub {bless(shift(), 'CGI::Ex::_intdiv')} ],
        [2, 90, ['%', 'mod', 'MOD'], 0, sub {bless(shift(), 'CGI::Ex::_mod')}    ],
        [2, 85, ['+'],               0, sub {bless(shift(), 'CGI::Ex::_plus')}   ],
        [2, 85, ['-'],               0, sub {bless(shift(), 'CGI::Ex::_subtr')}  ],
        [2, 85, ['_', '~'],          0, \&_concat                                ],
        [2, 80, ['<'],               0, sub {bless(shift(), 'CGI::Ex::_num_lt')} ],
        [2, 80, ['>'],               0, sub {bless(shift(), 'CGI::Ex::_num_gt')} ],
        [2, 80, ['<='],              0, sub {bless(shift(), 'CGI::Ex::_num_le')} ],
        [2, 80, ['>='],              0, sub {bless(shift(), 'CGI::Ex::_num_ge')} ],
        [2, 80, ['lt'],              0, sub {bless(shift(), 'CGI::Ex::_str_lt')} ],
        [2, 80, ['gt'],              0, sub {bless(shift(), 'CGI::Ex::_str_gt')} ],
        [2, 80, ['le'],              0, sub {bless(shift(), 'CGI::Ex::_str_le')} ],
        [2, 80, ['ge'],              0, sub {bless(shift(), 'CGI::Ex::_str_ge')} ],
        [2, 75, ['==', 'eq'],        0, sub {bless(shift(), 'CGI::Ex::_eq')}     ],
        [2, 75, ['!=', 'ne'],        0, sub {bless(shift(), 'CGI::Ex::_ne')}     ],
        [2, 70, ['&&'],              0, sub {bless(shift(), 'CGI::Ex::_and')}    ],
        [2, 65, ['||'],              0, sub {bless(shift(), 'CGI::Ex::_or')}     ],
        [2, 60, ['..'],              0, sub {bless(shift(), 'CGI::Ex::_range')}  ],
        [3, 55, ['?', ':'],          0, sub {bless(shift(), 'CGI::Ex::_ifelse')} ],
        [2, 52, ['='],               1, sub {bless(shift(), 'CGI::Ex::_set')}    ],
        [1, 50, ['not', 'NOT'],      0, sub {bless(shift(), 'CGI::Ex::_not')}    ],
        [2, 45, ['and', 'AND'],      0, sub {bless(shift(), 'CGI::Ex::_and')}    ],
        [2, 40, ['or', 'OR'],        0, sub {bless(shift(), 'CGI::Ex::_or')}     ],
    ];

    $OP_UNARY   ||= {map {my $ref = $_; map {$_ => $ref} @{$ref->[2]}} grep {$_->[0] == 1} @$OPERATORS};
    $OP_BINARY  ||= {map {my $ref = $_; map {$_ => $ref} @{$ref->[2]}} grep {$_->[0] == 2} @$OPERATORS};
    $OP_TRINARY ||= {map {my $ref = $_; map {$_ => $ref} @{$ref->[2]}} grep {$_->[0] == 3} @$OPERATORS};
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

    $QR_COMMENTS   = '(?-s: \# .* \s*)*';
    $QR_AQ_NOTDOT  = "(?! \\s* $QR_COMMENTS \\.)";
};

###----------------------------------------------------------------###

sub _var     { return bless(shift(), __PACKAGE__        ) }
sub _literal { return bless(shift(), 'CGI::Ex::_literal') }
sub _autobox { return bless(shift(), 'CGI::Ex::_autobox') }
sub _hash    { return bless(shift(), 'CGI::Ex::_hash'   ) }
sub _array   { return bless(shift(), 'CGI::Ex::_array'  ) }
sub _concat  { return bless(shift(), 'CGI::Ex::_concat' ) }

sub throw {
    require CGI::Ex::Template;
    CGI::Ex::Template->throw(@_);
}

###----------------------------------------------------------------###

sub parse_exp {
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
            return parse_exp(\$name);
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
    my $is_construct;
    my $is_namespace;

    ### allow for numbers
    if ($copy =~ s{ ^ ( (?:\d*\.\d+ | \d+) ) \s* $QR_COMMENTS }{}ox) {
        my $number = $1;
        push @var, _literal(\ $number);
        $is_literal = 1;

    ### looks like a normal variable start
    } elsif ($copy =~ s{ ^ (\w+) \s* $QR_COMMENTS }{}ox) {
        push @var, $1;
        $is_namespace = 1 if $RT_NAMESPACE->{$1};

    ### allow for literal strings
    } elsif ($copy =~ s{ ^ ([\"\']) (|.*?[^\\]) \1 \s* $QR_COMMENTS }{}sox) {
        if ($1 eq "'") { # no interpolation on single quoted strings
            my $str = $2;
            $str =~ s{ \\\' }{\'}xg;
            push @var, _literal(\ $str);
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
                $piece = parse_exp(\$name);
            }
            @pieces = grep {defined && length} @pieces;
            if (@pieces == 1 && ! ref $pieces[0]) {
                push @var, _literal(\ $pieces[0]);
                $is_literal = 1;
            } elsif (! @pieces) {
                my $str = '';
                push @var, _literal(\ $str);
                $is_literal = 1;
            } else {
                push @var, _concat(\@pieces);
                $is_construct = 1;
            }
        }
        if ($ARGS->{'auto_quote'}){
            $$str_ref = $copy;
            return ${ $var[0] } if $is_literal;
            return _var([@var, 0]);
        }

    ### allow for leading $foo or ${foo.bar} type constructs
    } elsif ($copy =~ s{ ^ \$ (\w+) \b \s* $QR_COMMENTS }{}ox
        || $copy =~ s{ ^ \$\{ \s* ([^\}]+) \} \s* $QR_COMMENTS }{}ox) {
        my $name = $1;
        push @var, parse_exp(\$name);

    ### looks like an array constructor
    } elsif ($copy =~ s{ ^ \[ \s* $QR_COMMENTS }{}ox) {
        local $RT_OPERATOR_PRECEDENCE = 0; # reset presedence
        my $arrayref = [];
        while (defined(my $var = parse_exp(\$copy))) {
            push @$arrayref, $var;
            $copy =~ s{ ^ , \s* $QR_COMMENTS }{}ox;
        }
        $copy =~ s{ ^ \] \s* $QR_COMMENTS }{}ox
            || throw('parse.missing.square', "Missing close \]", undef, length($$str_ref) - length($copy));
        push @var, _array($arrayref);
        $is_construct = 1;

    ### looks like a hash constructor
    } elsif ($copy =~ s{ ^ \{ \s* $QR_COMMENTS }{}ox) {
        local $RT_OPERATOR_PRECEDENCE = 0; # reset precedence
        my $hashref = [];
        while (defined(my $key = parse_exp(\$copy, {auto_quote => qr{ ^ (\w+) $QR_AQ_NOTDOT }xo}))) {
            $copy =~ s{ ^ = >? \s* $QR_COMMENTS }{}ox;
            my $val = parse_exp(\$copy);
            push @$hashref, $key, $val;
            $copy =~ s{ ^ , \s* $QR_COMMENTS }{}ox;
        }
        $copy =~ s{ ^ \} \s* $QR_COMMENTS }{}ox
            || throw('parse.missing.curly', "Missing close \} ($copy)", undef, length($$str_ref) - length($copy));
        push @var, _hash($hashref);
        $is_construct = 1;

    ### looks like a paren grouper
    } elsif ($copy =~ s{ ^ \( \s* $QR_COMMENTS }{}ox) {
        local $RT_OPERATOR_PRECEDENCE = 0; # reset precedence
        my $var = parse_exp(\$copy, {allow_parened_ops => 1});
        $copy =~ s{ ^ \) \s* $QR_COMMENTS }{}ox
            || throw('parse.missing.paren', "Missing close \)", undef, length($$str_ref) - length($copy));
        push @var, $var;
        $is_construct = 1;

    ### nothing to find - return failure
    } else {
        return;
    }

    return if $ARGS->{'auto_quote'}; # auto_quoted thing was too complicated

    ### looks for args for the initial
    if ($copy =~ s{ ^ \( \s* $QR_COMMENTS }{}ox) {
        local $RT_OPERATOR_PRECEDENCE = 0; # reset precedence
        my $args = parse_args(\$copy);
        $copy =~ s{ ^ \) \s* $QR_COMMENTS }{}ox
            || throw('parse.missing.paren', "Missing close \)", undef, length($$str_ref) - length($copy));
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
            my $var = parse_exp(\$name);
            push @var, $var;
        } elsif ($copy =~ s{ ^ (\w+) \s* $QR_COMMENTS }{}ox) {
            push @var, $1;
        } else {
            throw('parse', "Not sure how to continue parsing on \"$copy\" ($$str_ref)");
        }

        ### looks for args for the nested item
        if ($copy =~ s{ ^ \( \s* $QR_COMMENTS }{}ox) {
            local $RT_OPERATOR_PRECEDENCE = 0; # reset precedence
            my $args = parse_args(\$copy);
            $copy =~ s{ ^ \) \s* $QR_COMMENTS }{}ox
                || throw('parse.missing.paren', "Missing close \)", undef, length($$str_ref) - length($copy));
            push @var, $args;
        } else {
            push @var, 0;
        }

    }

    ### flatten literals and constants as much as possible
    my $var;
    if (@var == 2) {
        if ($is_literal) {
            $var = ${ $var[0] };
        } elsif ($is_construct) {
            $var = $var[0];
        } else {
            $var = _var(\@var);
        }
    } elsif ($is_namespace) {
        $var = _var(\@var)->call({}, {is_namespace_during_compile => 1});
    } else {
        $var[0] = _autobox([$var[0]]) if $is_literal || $is_construct;
        $var = _var(\@var);
    }

    ### allow for all "operators"
    if (! $RT_OPERATOR_PRECEDENCE) {
        my $tree;
        my $found;
        while ($copy =~ s{ ^ ($QR_OP) \s* $QR_COMMENTS }{}ox ## look for operators - then move along
               || ($ARGS->{'allow_parened_ops'}
                   && $copy =~ s{ ^ ($QR_OP_PARENED) \s* $QR_COMMENTS }{}ox) ) {
            local $RT_OPERATOR_PRECEDENCE = 1;
            my $op   = $1;
            my $var2 = parse_exp(\$copy);
            ### allow for unary operator precedence
            if ($has_unary && (($OP_BINARY->{$op} || $OP_TRINARY->{$op}) < $OP_UNARY->{$has_unary})) {
                if ($tree) {
                    if (@$tree == 2) { # only one operator - keep simple things fast
                        $var = $OP_BINARY->{$tree->[0]}->[4]->([$var, $tree->[1]]);
                    } else {
                        unshift @$tree, $var;
                        $var = apply_precedence($tree, $found);
                    }
                    undef $tree;
                    undef $found;
                }
                $var = $OP_UNARY->{$has_unary}->[4]->([$var]);
                undef $has_unary;
            }

            ### add the operator to the tree
            push (@{ $tree ||= [] }, $op, $var2);
            $found->{$op} = $OP_BINARY->{$op}->[1] || $OP_TRINARY->{$op}->[1] || die "Couldn't find op precedence for $op";
        }

        ### if we found operators - tree the nodes by operator precedence
        if ($tree) {
            if (@$tree == 2) { # only one operator - keep simple things fast
                $var = $OP_BINARY->{$tree->[0]}->[4]->([$var, $tree->[1]]);
            } else {
                unshift @$tree, $var;
                $var = apply_precedence($tree, $found);
            }
        }
    }

    ### allow for unary on non-chained variables
    if ($has_unary) {
        $var = $OP_UNARY->{$has_unary}->[4]->([$var]);
    }

    $$str_ref = $copy; # commit the changes
    return $var;
}

### this is used to put the parsed variables into the correct operations tree
sub apply_precedence {
    my ($tree, $found) = @_;

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
            next if $tree->[$i] ne $op && (! exists $OP_TRINARY->{$op} || $OP_TRINARY->{$op} ne $tree->[$i]);
            push @trees, [splice @$tree, 0, $i, ()]; # everything up to the operator
            push @trinary, $tree->[0] if exists $OP_TRINARY->{$op};
            shift @$tree; # pull off the operator
            $i = -1;
        }
        next if ! @trees; # this iteration didn't have the current operator
        push @trees, $tree if scalar @$tree; # elements after last operator

        ### now - for this level split on remaining operators, or add the variable to the tree
        for (@trees) {
            if (@$_ == 1) {
                $_ = $_->[0]; # single item - its not a tree
            } elsif (@$_ == 3) {
                $_ = $OP_BINARY->{$_->[1]}->[4]->([$_->[0], $_->[2]]); # single operator - put it straight on
            } else {
                $_ = apply_precedence($_, $found); # more complicated - recurse
            }
        }

        ### all done if we are looking at binary operations
        my $ref = $OP_BINARY->{$op} || $OP_TRINARY->{$op};
        if ($#trinary <= 1) {
            my $val = $trees[0];
            $val = $ref->[4]->([$val, $trees[$_]]) for 1 .. $#trees;
            return $val;
        }

        ### reorder complex trinary - rare case
        while ($#trinary >= 1) {
            ### if we look starting from the back - the first lead trinary op will always be next to its matching op
            for (my $i = $#trinary; $i >= 0; $i --) {
                next if ! exists $OP_TRINARY->{$trinary[$i]};
                my ($op, $op2) = splice @trinary, $i, 2, (); # remove the pair of operators
                my $node = [ \ [$op, @trees[$i .. $i + 2] ], 0 ];
                splice @trees, $i, 3, $node;
            }
        }
        return $trees[0]; # at this point the trinary has been reduced to a single operator

    }

    throw('parse', "Couldn't apply precedence");
}

### look for arguments - both positional and named
sub parse_args {
    my $str_ref = shift;
    my $ARGS    = shift || {};
    my $copy    = $$str_ref;

    my @args;
    my @named;
    while (length $$str_ref) {
        my $copy = $$str_ref;
        if (defined(my $name = parse_exp(\$copy, {auto_quote => qr{ ^ (\w+) $QR_AQ_NOTDOT }xo}))
            && $copy =~ s{ ^ = >? \s* $QR_COMMENTS }{}ox) {
            throw('parse', 'Named arguments not allowed') if $ARGS->{'positional_only'};
            my $val = parse_exp(\$copy);
            $copy =~ s{ ^ , \s* $QR_COMMENTS }{}ox;
            push @named, $name, $val;
            $$str_ref = $copy;
        } elsif (defined(my $arg = parse_exp($str_ref))) {
            push @args, $arg;
            $$str_ref =~ s{ ^ , \s* $QR_COMMENTS }{}ox;
        } else {
            last;
        }
    }

    ### allow for named arguments to be added also
    push @args, [\ ['hashref', @named], 0] if scalar @named;

    return \@args;
}

sub get_exp { ref($_[0]) ? $_[0]->call($_[1]) : $_[0] }

sub set_exp {
    my $var = shift;
    $var = _var([$var, 0]) if ! ref $var; # allow for the parse tree to store literals - the literal is used as a name (like [% 'a' = 'A' %])
    return $var->set($_[0], $_[1]);
}


sub dump_parse {
    my $str = shift;
    require Data::Dumper;
    return Data::Dumper::Dumper(parse_exp(\$str));
}

sub dump_get {
    my ($str, $hash) = @_;
    require Data::Dumper;
    return Data::Dumper::Dumper(get_exp(parse_exp(\$str), $hash));
}

sub dump_set {
    my ($str, $val, $hash) = @_;
    $hash ||= {};
    require Data::Dumper;
    set_exp(parse_exp(\$str), $val, $hash);
    return Data::Dumper::Dumper($hash);
}

sub vivify_args {
    my $vars = shift;
    my $hash = shift;
    return [map {get_exp($_, $hash)} @$vars];
}

###----------------------------------------------------------------###

sub new {
    my $class = shift;
    return bless $_[0], $class;
}

sub is_nested { @{ $_[0] } >= 3 }

sub call {
    my $self = shift;
    my $hash = shift || {};
    my $ARGS = shift || {};
    my $i    = 0;
    my $generated_list;

    ### determine the top level of this particular variable access
    my $ref  = $self->[$i++];
    my $args = $self->[$i++];
    warn "CGI::Ex::Var::call: begin \"$ref\"\n" if trace;

    if (ref $ref) {
        if ($ref->isa('CGI::Ex::_autobox')) {
            $ref = $ref->call($hash);
        } else {
            $ref = $ref->call($hash);
            return if $ref =~ /^[_.]/; # don't allow vars that begin with _
            $ref = $hash->{$ref};
        }
    } else {
        if ($ARGS->{'is_namespace_during_compile'}) {
            $ref = $RT_NAMESPACE->{$ref};
        } else {
            return if $ref =~ /^[_.]/; # don't allow vars that begin with _
            $ref = $hash->{$ref};
        }
    }

    my %seen_filters;
    while (defined $ref) {

        ### check at each point if the returned thing was a code
        if (UNIVERSAL::isa($ref, 'CODE')) {
            my @results = $ref->($args ? (map {get_exp($_, $hash)} @$args) : ());
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
        last if $i >= $#$self;
        my $was_dot_call = $ARGS->{'no_dots'} ? 1 : $self->[$i++] eq '.';
        my $name         = $self->[$i++];
        my $args         = $self->[$i++];
        warn "CGI::Ex::Var::get_exp: nested \"$name\"\n" if trace;

        ### allow for named portions of a variable name (foo.$name.bar)
        if (ref $name) {
            $name = $name->call($hash);
            if (! defined $name) {
                $ref = undef;
                last;
            }
        }

        if ($name =~ /^_/) { # don't allow vars that begin with _
            $ref = undef;
            last;
        }

        ### allow for scalar and filter access (this happens for every non virtual method call)
        if (! ref $ref) {
            if ($SCALAR_OPS->{$name}) {                        # normal scalar op
                $ref = $SCALAR_OPS->{$name}->($ref, $args ? (map {get_exp($_, $hash)} @$args) : ());

            } elsif ($LIST_OPS->{$name}) {                     # auto-promote to list and use list op
                $ref = $LIST_OPS->{$name}->([$ref], $args ? (map {get_exp($_, $hash)} @$args) : ());

            } elsif (my $filter = $RT_FILTERS->{$name}           # filter configured in Template args
                     || $FILTER_OPS->{$name}                     # predefined filters in CET
                     || (UNIVERSAL::isa($name, 'CODE') && $name) # looks like a filter sub passed in the stash
                     || list_filters()->{$name}) {               # filter defined in Template::Filters

                if (UNIVERSAL::isa($filter, 'CODE')) {
                    $ref = eval { $filter->($ref) }; # non-dynamic filter - no args
                    if (my $err = $@) {
                        throw('filter', $err) if ref($err) !~ /Template::Exception$/;
                        die $err;
                    }
                } elsif (! UNIVERSAL::isa($filter, 'ARRAY')) {
                    throw('filter', "invalid FILTER entry for '$name' (not a CODE ref)");

                } elsif (@$filter == 2 && UNIVERSAL::isa($filter->[0], 'CODE')) { # these are the TT style filters
                    eval {
                        my $sub = $filter->[0];
                        if ($filter->[1]) { # it is a "dynamic filter" that will return a sub
                            ($sub, my $err) = $sub->($RT_GET_CONTEXT->(), $args ? (map {get_exp($_, $hash)} @$args) : ());
                            if (! $sub && $err) {
                                throw('filter', $err) if ref($err) !~ /Template::Exception$/;
                                die $err;
                            } elsif (! UNIVERSAL::isa($sub, 'CODE')) {
                                throw('filter', "invalid FILTER for '$name' (not a CODE ref)")
                                    if ref($sub) !~ /Template::Exception$/;
                                die $sub;
                            }
                        }
                        $ref = $sub->($ref);
                    };
                    if (my $err = $@) {
                        throw('filter', $err) if ref($err) !~ /Template::Exception$/;
                        die $err;
                    }
                } else { # this looks like our vmethods turned into "filters" (a filter stored under a name)
                    throw('filter', 'Recursive filter alias \"$name\"') if $seen_filters{$name} ++;
                    $self = [$name, 0, '|', @$filter, @{$self}[$i..$#$self]]; # splice the filter into our current tree
                    $i = 2;
                }
                if (scalar keys %seen_filters
                    && $seen_filters{$self->[$i - 5] || ''}) {
                    throw('filter', "invalid FILTER entry for '".$self->[$i - 5]."' (not a CODE ref)");
                }
            } else {
                $ref = undef;
            }

        } else {

            ### method calls on objects
            if (UNIVERSAL::can($ref, 'can')) {
                my @args = $args ? (map {get_exp($_, $hash)} @$args) : ();
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
                    $ref = $HASH_OPS->{$name}->($ref, $args ? (map {get_exp($_, $hash)} @$args) : ());
                } elsif ($ARGS->{'is_namespace_during_compile'}) {
                    return $self; # abort - can't fold namespace variable
                } else {
                    $ref = undef;
                }

            ### array access
            } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
                if ($name =~ /^\d+$/) {
                    $ref = ($name > $#$ref) ? undef : $ref->[$name];
                } else {
                    $ref = (! $LIST_OPS->{$name}) ? undef : $LIST_OPS->{$name}->($ref, $args ? (map {get_exp($_, $hash)} @$args) : ());
                }
            }
        }

    } # end of while

    ### allow for undefinedness
    if (! defined $ref) {
        if ($RT_DEBUG_UNDEF) {
            my $chunk = $self->[$i - 2];
            $chunk = $chunk->call($hash) if ref $chunk;
            die "$chunk is undefined\n";
        } else {
            $ref = $self->undefined($self);
        }
    }

    ### allow for special behavior for the '..' operator
    if ($generated_list && $ARGS->{'list_context'} && ref($ref) eq 'ARRAY') {
        return @$ref;
    }

    return $ref;
}

sub undefined { $RT_UNDEFINED_SUB ? $RT_UNDEFINED_SUB->(@_) : '' }

sub set {
    my ($var, $val, $hash) = @_;
    my $i = 0;

    ### determine the top level of this particular variable access
    my $ref  = $var->[$i++];
    my $args = $var->[$i++];

    if (ref $ref) {
        $ref = $ref->call($hash);
        return if ! defined $ref;
    }

    return if $ref =~ /^[_.]/; # don't allow vars that begin with _

    if ($#$var <= $i) {
        $hash->{$ref} = $val;
        return;
    } else {
        $ref = $hash->{$ref} ||= {};
    }

    ### let the top level thing be a code block
    return if UNIVERSAL::isa($ref, 'CODE');

    ### vivify the chained levels
    while (defined $ref && $#$var > $i) {
        my $was_dot_call = $var->[$i++];
        my $name         = $var->[$i++];
        my $args         = $var->[$i++];

        ### allow for named portions of a variable name (foo.$name.bar)
        if (ref $name) {
            $name = $name->call($hash);
            if (! defined $name) {
                $ref = undef;
                last;
            }
        }

        if ($name =~ /^_/) { # don't allow vars that begin with _
            return;
        }

        ### method calls on objects
        if (UNIVERSAL::can($ref, 'can')) {
            my $lvalueish;
            my @args = $args ? (map {get_exp($_, $hash)} @$args) : ();
            if ($i >= $#$var) {
                $lvalueish = 1;
                push @args, $val;
            }
            my @results = eval { $ref->$name(@args) };
            if ($@) {
                die $@ if ref $@ || $@ !~ /Can\'t locate object method/;
            } elsif (defined $results[0]) {
                $ref = ($#results > 0) ? \@results : $results[0];
            } elsif (defined $results[1]) {
                die $results[1]; # TT behavior - why not just throw ?
            } else {
                $ref = undef;
            }
            return if $lvalueish;
            next;
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
            my @results = $ref->($args ? (map {get_exp($_, $hash)} @$args) : ());
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

###----------------------------------------------------------------###
### filters and vmethod definition

sub list_filters {
    return $TT_FILTERS ||= eval { require Template::Filters; $Template::Filters::FILTERS } || {};
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
### "here be dragons"

package CGI::Ex::_literal;
sub call { ${ $_[0] } }
sub set {}

package CGI::Ex::_autobox;
sub call { $_[0]->[0]->call($_[1]) }
sub set {}

package CGI::Ex::_not;
sub call { ! (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0]) || '' }
sub set {}

package CGI::Ex::_concat;
sub call { join "", grep {defined} map {ref($_) ? $_->call($_[1]) : $_} @{ $_[0] } }
sub set {}

package CGI::Ex::_and;
sub call { (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  &&  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_or;
sub call { ((ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  ||  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1])) || '' }
sub set {}

package CGI::Ex::_ifelse;

package CGI::Ex::_set;
sub call {
    my ($var, $val) = @{ $_[0] };
    $val = CGI::Ex::Var::get_exp($val, $_[1]);
    CGI::Ex::Var::set_exp($var, $val, $_[1]);
    return $val;
}
sub set {}

package CGI::Ex::_hash;
sub call { return {map {ref($_) ? $_->call($_[1]) : $_} @{ $_[0] }} }
sub set {}

package CGI::Ex::_array;
sub call { return [map {ref($_) ? $_->call($_[1]) : $_} @{ $_[0] }] }
sub set {}

package CGI::Ex::_str_lt;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  lt  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_str_gt;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  gt  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_str_le;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  le  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_str_ge;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  ge  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_eq;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  eq  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_ne;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  ne  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_negate;
sub call { local $^W; 0 - (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0]) }
sub set {}

package CGI::Ex::_pow;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  **  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_mult;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  *   (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_div;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  /   (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_intdiv;
sub call { local $^W; int( (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  /   (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) ) }
sub set {}

package CGI::Ex::_mod;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  %   (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_plus;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  +   (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_subtr;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  -   (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_num_lt;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  <   (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_num_gt;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  >   (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_num_le;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  <=  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_num_ge;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0])  >=  (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) }
sub set {}

package CGI::Ex::_range;
sub call { local $^W; (ref($_[0]->[0]) ? $_[0]->[0]->call($_[1]) : $_[0]->[0]) || 0 .. (ref($_[0]->[1]) ? $_[0]->[1]->call($_[1]) : $_[0]->[1]) || 0 }
sub set {}

###----------------------------------------------------------------###

1;
