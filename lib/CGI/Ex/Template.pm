package CGI::Ex::Template;

use CGI::Ex::Dump qw(debug);
use strict;
use vars qw(@INCLUDE_PATH
            $START_TAG
            $END_TAG
            $SCALAR_OPS $HASH_OPS $LIST_OPS
            $DIRECTIVES
            $QR_FILENAME
            $MAX_RECURSE
            $SPS
            $SPS_QR
            );

BEGIN {
    $START_TAG  ||= '[%';
    $END_TAG    ||= '%]';
    $SPS = chr(186);
    $SPS_QR = qr/$SPS(\d+)$SPS/;

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
            parse => \&parse_BLOCK,
            exect => \&exect_BLOCK,
            end   => 1,
        },
        CALL    => {
            parse => \&parse_CALL,
            exect => \&exect_CALL,
        },
        DEFAULT => {
            parse => \&parse_DEFAULT,
            exect => \&exect_DEFAULT,
        },
        DUMP    => {
            parse => \&parse_DUMP,
            exect => \&exect_DUMP,
        },
        END     => 1, # builtin that should never be called
        GET     => {
            parse => \&parse_GET,
            exect => \&exect_GET,
        },
        IF      => {
            parse => \&parse_IF,
            exect => \&exect_IF,
            end   => 1,
        },
        INCLUDE => {
            parse => \&parse_INCLUDE,
            exect => \&exect_INCLUDE,
        },
        INSERT  => {
            parse => \&parse_INSERT,
            exect => \&exect_INSERT,
        },
        SET     => {
            parse => \&parse_SET,
            exect => \&exect_SET,
        },
        PROCESS => {
            parse => \&parse_PROCESS,
            exect => \&exect_PROCESS,
        },
    };

    $QR_FILENAME = qr{(?i: [a-z]:/|/)? [\w\-\.]+ (?:/[\w\-\.]+)* }x;
    $MAX_RECURSE = 50;
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

    local $self->{'_state'} = {};
    local $self->{'_swap'}  = $_[1] || {};

    my $START = $self->{'START_TAG'} || $START_TAG;
    my $END   = $self->{'END_TAG'}   || $END_TAG;
    my $len_s = length $START;
    my $len_e = length $END;

    my @tree;
    my @tstate;
    my $last = 0;
    while (1) {
        ### look through the string using index
        my $i = index($_[0], $START, $last);
        last if $i == -1;
        push @tree, ['text', $last, $i] if $last != $i;
        my $begin = substr($_[0], $last, $i - $last),
        my $j = index($_[0], $END, $i + $len_s);
        $last = $j + $len_e;
        if ($j == -1) { # missing closing tag
            $last = length($_[0]);
            last;
        }
        my $tag = substr($_[0], $i + $len_s, $j - ($i + $len_s));
        my $level = [undef , $i + $len_s, $j, 0, 0];

        ### take care of whitespace
        if ($tag =~ s/^-// || $self->{'PRE_CHOMP'}) {
            $level->[3] = 1;
        }
        if ($tag =~ s/-$// || $self->{'POST_CHOMP'}) {
            $level->[4] = 1;
        }
        $tag =~ s/^\s+//;

        ### look for functions or variables
        if ($tag =~ /^(\w+) (?: $|\s)/x
            && ($DIRECTIVES->{(my $func = $1)})) {
            $tag =~ s/^\w+\s*//;
            $level->[0] = 'directive';
            $level->[5] = $func;
            $level->[6] = $tag;
            push @tree, $level;
            if ($func eq 'END') {
                if ($#tstate == -1) {
                    die "Found an unmatched END tag";
                } else {
                    my $s = pop @tstate;
                    $s->[7] = $i + $len_s;
                }

            } elsif ($DIRECTIVES->{$func}->{'end'}) {
                $level->[7] = -1;
                push @tstate, $level;
            }

        } elsif (my $var = $self->parse_variable(\$tag)) {
            die "Found trailing info during variable access \"$tag" if $tag;
            $level->[0] = 'variable';
            $level->[5] = $var;
            push @tree, $level;

        } else {
            my $all  = substr($_[0], $i + $len_s, $j - ($i + $len_s));
            $all =~ s/^\s+//;
            $all =~ s/\s+$//;
            die "Not sure how to handle tag $all";
        }
    }

    #die "Missing END tag while parsing $state[0][1] tag" if $#state != -1;
    return $_[0] if $#tree == -1;
    push @tree, ['text', $last, length($_[0])] if $last != length($_[0]);
#    debug \@tree;
    my $new2 = $self->execute_tree(\@tree, \$_[0]);

    return $new2;
}

sub execute_tree {
    my ($self, $tree, $template_ref) = @_;
    my $str = '';
    my $post_chomp;
    while (my $node = shift @$tree) {
        if ($node->[0] eq 'text') {
            my $pre_chomp = $tree->[0] && $tree->[0]->[3];

            my $txt = substr($$template_ref, $node->[1], $node->[2] - $node->[1]);

            $txt =~ s/ (?:\n|^) [^\S\n]* \z //xm if $pre_chomp; # remove any leading whitespace on the same line
            $txt =~ m/ ( [^\S\n]* (?:\n?$|\n) ) /xg if $post_chomp;

            $str .= $txt;
        }
        $post_chomp = $node->[4];
        if ($node->[0] eq 'directive') {
            my $func = $node->[5];
            my $tag  = $node->[6];
            my $val;
            if ($node->[7]) {
                my @subtree; # optimize
                push @subtree, shift(@$tree) while $#$tree != -1 && $node->[7] != ($tree->[0]->[1] || 0);
                shift @$tree;
                $val = $self->get_function($func)->($self, \$tag, $func, \@subtree, $template_ref);
            } else {
                $val = $self->get_function($func)->($self, \$tag, $func, $template_ref);
            }
            $val = '' if ! defined $val;
            $str .= $val;
        } elsif ($node->[0] eq 'variable') {
            my $var = UNIVERSAL::isa($node->[5], 'SCALAR') ? ${ $node->[5] } : $self->vivify_var($node->[5]);
            $str .= $var;
        }
    }
    return $str;
}

###----------------------------------------------------------------###

sub parse_variable {
    my $self    = shift;
    my $str_ref = shift;
    my $args    = shift || {};

    ### allow for custom auto_quoting (such as hash constructors)
    if (my $quote_qr = $args->{'auto_quote'}) {
        if ($$str_ref =~ s{ ^ ($quote_qr) \s* (?! [\.\|]) }{}x) { # auto-quoted - not followed by a chained operator
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
        } elsif ($#pieces == 0) {
            push @var, ref($pieces[0]) ? $pieces[0] : \ $pieces[0];
        } else {
            push @var, [['concat', map {ref($_) ? $_ : [\$_, 0]} @pieces]];
        }

    ### looks like an array constructor
    } elsif ($copy =~ s{ ^ \[ \s* }{}x) {
        my $arrayref = ['arrayref'];
        while (my $var = $self->parse_variable(\$copy)) {
            push @$arrayref, $var;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \] \s* }{}x || die "Missing close \] on \"$copy\"";
        push @var, [$arrayref];

    ### looks like a hash constructor
    } elsif ($copy =~ s{ ^ \{ \s* }{}x) {
        my $hashref = ['hashref'];
        while (my $key = $self->parse_variable(\$copy, {auto_quote => qr/\w+/})) {
            $copy =~ s{ ^ => \s* }{}x;
            my $val = $self->parse_variable(\$copy);
            push @$hashref, $key, $val;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \} \s* }{}x || die "Missing close \} on \"$copy\"";
        push @var, [$hashref];

    ### looks like a paren grouper
    } elsif ($copy =~ s{ ^ \( \s* }{}x) {
        my $var = $self->parse_variable(\$copy);
        $copy =~ s{ ^ \) \s* }{}x || die "Missing close \) on \"$copy\"";
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
        my @args;
        while (my $var = $self->parse_variable(\$copy)) {
            push @args, $var;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \) \s* }{}x || die "Missing close \) on \"$copy\"";
        push @var, \@args;
    } else {
        push @var, 0;
    }

    ### allow for nested items
    while ($copy =~ s{ ^ ([\.\|]) \s* }{}x) {
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
            die "Not sure how to continue parsing on \"$copy\"";
        }

        ### looks for args for the nested item
        if ($copy =~ s{ ^ \( \s* }{}x) {
            my @args;
            while (my $var = $self->parse_variable(\$copy)) {
                push @args, $var;
                $copy =~ s{ ^ , \s* }{}x;
            }
            $copy =~ s{ ^ \) \s* }{}x || die "Missing close \) on \"$copy\"";
            push @var, \@args;
        } else {
            push @var, 0;
        }

    }

    ### allow for all "operators"
    if ($copy =~ s{ ^ ( &&  | \|\| | \*\* |
                        and | or   | pow  |
                        >=  | >    | <=   | <  | == | != |
                        ge  | gt   | le   | lt | eq | ne |
                        concat | mod | not | arrayref | hashref |
                        [_~+\-*%\^!] ) \s* }{}x) {
        my $op   = $1;
        my $var1 = [@var];
        my $var2 = $self->parse_variable(\$copy);
        @var = ([[$op, $var1, $var2]], 0);
    }

    #debug \@var, $copy;
    $$str_ref = $copy; # commit the changes
    return \@var;
}

sub vivify_args {
    my ($self, $args) = @_;
    return [map {$self->vivify_var($_)} @$args];
}

sub vivify_var {
    my ($self, $var) = @_;
    my $ref;
    if (! ref $var) {
        return $var; # looks like a num or a quoted string
    } elsif (! ref $var->[0]) {
        $ref = $self->{'_swap'};
    } else {
        shift @$var; # remove args if the first thing was a data structure
    }
    while ($#$var != -1) {
        my $name = shift @$var;
        my $args = shift @$var;
        if (! defined $ref) {
            next;
        } elsif (UNIVERSAL::can($ref, $name)) {
            my @results = $ref->$name(@{ $self->vivify_args($args) });
            $ref = ($#results > 0) ? \@results : $results[0];
        } elsif (UNIVERSAL::isa($ref, 'HASH')) {
            if (exists $ref->{$name}) {
                $ref = $ref->{$name};
            } elsif (my $code = $self->hash_op($name)) {
                $ref = $code->($ref, @{ $self->vivify_args($args) });
            } else {
                $ref = undef;
            }
        } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
            if ($name =~ /^\d+$/ && $name <= $#$ref) {
                $ref = $ref->[$name];
            } elsif (my $code = $self->list_op($name)) {
                $ref = $code->($ref, @{ $self->vivify_args($args) });
            } else {
                $ref = undef;
            }
        } elsif (! ref($ref) && defined($ref)) {
            if (my $code = $self->scalar_op($name)) {
                $ref = $code->($ref, @{ $self->vivify_args($args) });
            } else {
                $ref = undef;
            }
        }

        if (UNIVERSAL::isa($ref, 'CODE')) {
#            $TRACE .= "In code\n" if trace;
            my @results = $ref->(@{ $self->vivify_args($args) });
            $ref = ($#results > 0) ? \@results : $results[0];
        }
    }

    return $ref;
}

###----------------------------------------------------------------###

sub get_variable_ref {
    my $self    = shift;
    my $str_ref = shift;
    my $args    = shift || {};

    ### allow hash various items to auto quote (such as hash constructors)
    if ($args->{'auto_quote'}) {
        my $quote_qr = $args->{'quote_qr'} || qr/\w+/;
        if ($$str_ref =~ s{
            ^(  \$\w+                    # $foo
              | \$\{\s* [^\}]+ \s*\}     # ${foo.bar}
              | $quote_qr                # whatever
              )  (?: \s+ | \s* (?!\.) )  # end of string or not a dot
          }{}x) {
            my $str = $1;
            $self->interpolate(\$str);
            return \ $str;
        }
    }


    ### determine our data source start point
    my ($ref, $root, $set_ref, $set_name);
    my $copy = $$str_ref; # retain for rollback

    ### looks like an unquoted number
    if ($copy =~ s/^ (-?(?:\d*\.\d+|\d+))//x) {
        my $num = $1;
        $ref = \ $num;
        $copy =~ s/^\.(?!\.)//; # remove trailing . on a "string".length type construct - but allow 0..1

    ### looks like an literal string
    } elsif ($copy =~ s/^([\"\']) (|.*?[^\\]) \1 //xs) {
        my $str = $2;
        $self->interpolate(\$str) if $1 eq '"'; # ' doesn't interpolate
        $ref = \ $str;
        $copy =~ s/^\.(?!\.)//; # remove trailing . on a "string".length type construct - but allow 'a'..'z'

    ### looks like an array constructor
    } elsif ($copy =~ s/^\[\s*//) {
        my @array;
        my ($range, $_ref);
        while (($range = $copy =~ s/^\.\.\s*//) || ($_ref = $self->get_variable_ref(\$copy))) {
            push @array, $range ? "\0..\0" : UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : $_ref;
            next if $copy =~ s/^,\s*//;
        }
        $copy =~ s/^\]\.?// || die "Unterminated array constructor in $$str_ref";
        for (my $i = 0; $i <= $#array; $i++) {
            next if $array[$i] ne "\0..\0";
            my $range = do { local $^W; $i == 0 ? [] : [$array[$i - 1] .. $array[$i + 1]] };
            splice @array, ($i == 0 ? 0 : $i - 1), ($i == 0 ? 2 : 3), @$range;
            $i += ($#$range + 1) - ($i == 0 ? 2 : 3)
        }
        $ref = \@array;

    ### looks like a hash constructor
    } elsif ($copy =~ s/^\{\s*//) {
        my %hash;
        while (my $_ref = $self->get_variable_ref(\$copy, {auto_quote => 1})) {
            my $key = UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : "$_ref";
            $copy =~ s/^=>\s*// || die "Missing => in hash constructor in $$str_ref";
            $_ref = $self->get_variable_ref(\$copy);
            my $val = defined($_ref) ? UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : $_ref : undef;
            $hash{$key} = $val;
            next if $copy =~ s/^,\s*//;
        }
        $copy =~ s/^\}\.?// || die "Unterminated hash constructor in $$str_ref";
        $ref = \%hash;

    ### looks like a paren grouper
    } elsif ($copy =~ s/^\(\s*//) {
        local $self->{'_state'}->{'parse_math'}; # let grouping parens have their own math
        $ref = $self->get_variable_ref(\$copy);
        $copy =~ s/^\s*\)\.?//;

    ### just use the swap
    } else {
        $ref = $root = $self->{'_swap'};
    }


    ### look for normal name element types
    while (defined $ref) {

        ### parse for arguments - one.two(arg1, arg2)
        my @args;
        if ($copy =~ s/^\(\s*//) {
            local $self->{'_state'}->{'parse_math'}; # arguments can have their own math calculations
            while (my $_ref = $self->get_variable_ref(\$copy)) {
                push @args, UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : $_ref;
                next if $copy =~ s/^,\s*//;
            }
            $copy =~ s/^\)\.?// || die "Unterminated arg list in $$str_ref";
        }

        ### if the previously found ref was a code block - run it with any parsed args
        if (UNIVERSAL::isa($ref, 'CODE')) {
            my @results = $ref->(@args);
            if ($#results > 0) {
                $ref = \@results;
            } elsif (defined(my $result = $results[0])) {
                $ref = ref($result) ? $result : \ $result;
            } else {
                my $str = '';
                $ref = \ $str;
            }
            next;
        }

        ### allow for interpolated variables in the middle - one.$foo.two or one.${foo.bar}.two
        if ($copy =~ s/^\$(\w+)(\.?)//
            || $copy =~ s/^\$\{\s* ([^\}]+) \s*\}(\.?)//x) {
            my ($var, $dot) = ($1, $2);
            my $_ref = $self->get_variable_ref(\$var);
            if (! $_ref || ! UNIVERSAL::isa($_ref, 'SCALAR')) {
                die "Unknown variable \"$var\" in $$str_ref"; # TODO - allow
            } elsif ($$_ref =~ /^\w+$/) {
                $copy = $$_ref . $dot . $copy;
            } else {
                $ref = \ undef;
                next;
            }
        }

        ### walk down the normal line this.that.foo(blah).0.them
        if ($copy =~ s/^(\w+)\.?//) {
            my $name = $1;

            ### current level looks like an object method calll
            if (UNIVERSAL::can($ref, $name)) {
                my $obj = $ref;
                $ref = sub {
                    my @results = $obj->$name(@_);
                    if ($#results > 0) {
                        return \@results;
                    } else {
                        my $result = $results[0];
                        return ref($result) ? $result : \ $result;
                    }
                };
                next;

            ### current level looks like a hashref
            } elsif (UNIVERSAL::isa($ref, 'HASH')) {
                if (exists $ref->{$name}) {
                    if ($args->{'return_set_ref'}) {
                        $set_ref  = $ref;
                        $set_name = $name;
                    }
                    $ref = ref($ref->{$name}) ? $ref->{$name} : \ $ref->{$name};
                    next;
                } elsif (my $code = $self->hash_op($name)) {
                    my $hash = $ref;
                    $ref = sub { $code->($hash, @_) };
                    next;
                } elsif ($args->{'return_set_ref'}) {
                    $set_ref  = $ref;
                    $set_name = $name;
                    $ref = $ref->{$name} = {}; # AUTOVIVIFY
                    next;
                } else { # undef - no such key or virtual method
                    $ref = \ undef;
                    next;
                }

            ### current level looks like an arrayref
            } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
                if ($name =~ /^\d+/) { # index lookup
                    if ($name > $#$ref) { # allow for out of bounds
                        if ($args->{'return_set_ref'}) {
                            $ref->[$name] = {}; # AUTOVIVIFY
                        } else {
                            $ref = \ undef;
                            next;
                        }
                    }
                    if ($args->{'return_set_ref'}) {
                        $set_ref  = $ref;
                        $set_name = $name;
                    }
                    $ref = ref($ref->[$name]) ? $ref->[$name] : \ $ref->[$name];
                    next;
                } elsif (my $code = $self->list_op($name)) { # virtual method
                    my $array = $ref;
                    $ref = sub { $code->($array, @_) };
                    next;
                } elsif ($args->{'return_set_ref'}) {
                    die "Refusing to translate array to hash during set operation in $$str_ref";
                } else { # undef - no such lookup
                    $ref = \ undef;
                    next;
                }

            ### looks like a normal variable
            } elsif (UNIVERSAL::isa($ref, 'SCALAR')) {
                if (! defined $$ref) {
                    ### undefs gobble up the rest
                    next;
                } elsif (my $code = $self->scalar_op($name)) {
                    my $scalar = $$ref;
                    $ref = sub { $code->($scalar, @_) };
                    next;
                } elsif ($args->{'return_set_ref'}) {
                    if (UNIVERSAL::isa($set_ref, 'HASH')) {
                        $set_ref = $set_ref->{$set_name} = {};
                        $ref     = $set_ref->{$name}     = {};
                    } elsif (UNIVERSAL::isa($set_ref, 'ARRAY')) {
                        $set_ref = $set_ref->[$set_name] = {};
                        $ref     = $set_ref->{$name}     = {};
                    }
                    $set_name = $name;
                    next;
                } else {
                    $ref = \ undef;
                    next;
                    #die "SCALAR virtual method not implemented ($$str_ref)";
                }
            }
        }

        ### if we hit here - we were unable to parse more - so stop
        last;
    } # end of while


    ### allow for math operators
    if ($copy =~ s/^\s*(\*\*)\s*//
        || $copy =~ s|^\s*([%*/+-])\s*||) {
        my $op = $1;
        my $is_top = ! $self->{'_state'}->{'parse_math'};
        local $self->{'_state'}->{'parse_math'} = 1;

        my $val  = UNIVERSAL::isa($ref, 'SCALAR')  ? do { local $^W; $$ref + 0 } : 0;
        my $ref2 = $self->get_variable_ref(\$copy); # parsed with parse_math
        my $val2 = $$ref2;

        $val = "$val$op$val2";
        if (! $is_top) {
            $ref = \ $val; # return our built up string
        } else {
            $root = undef;
            $val = ($val =~ m|^([\d\.%*/+-]+)$|) ? eval $1 : 0; # TODO - maybe not eval
            $ref = \ $val;
        }

    ### if we are parsing math operations - turn non numbers into numbers
    } elsif ($self->{'_state'}->{'parse_math'}) {
        my $val = UNIVERSAL::isa($ref, 'SCALAR') ? do { local $^W; $$ref + 0 } : 0;
        $ref = \ $val;

    ### allow for boolean operators
    } elsif ($copy =~ s/^\s*(&&|\|\|)\s*//) {
        my $op       = $1;
        my $bool = UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : $ref;
        if ($op eq '&&') {
            if ($bool) {
                $ref = $self->get_variable_ref(\$copy);
            } else {
                ### $ref stays as is
                $self->get_variable_ref(\$copy); # TODO - we want to short circuit - not call things that don't need to be
            }
        } else {
            if ($bool) {
                ### $ref stays as is
                $self->get_variable_ref(\$copy); # TODO - we want to short circuit - not call things that don't need to be
            } else {
                $ref = $self->get_variable_ref(\$copy);
            }
        }
    }


    ### we didn't find a variable - return nothing
    return if $root && $ref == $root;

    ### finalize the changes
    $copy =~ s/^\s+//;
    $$str_ref = $copy;

    ### allow for custom undefined
    if (UNIVERSAL::isa($ref, 'SCALAR') && ! defined $$ref) {
        my $str = $self->undefined($$str_ref);
        $str = '' if ! defined $str;
        $ref = \ $str;

    ### if we are setting - return the accessor info
    } elsif ($args->{'return_set_ref'}) {
        return [$set_ref, $set_name];

    ### normal variable access
    } else {
        return $ref;
    }
}

###----------------------------------------------------------------###

sub interpolate {
    my ($self, $str_ref) = @_;
    my $copy;
    $$str_ref =~ s/\$(\w+)/$self->_get_interp_value($1)/egs;
    $$str_ref =~ s/\$\{\s* ([^\}]+) \s*\}/$self->_get_interp_value($1)/egsx;
}

sub _get_interp_value {
    my ($self, $name) = @_;
    my $ref = $self->get_variable_ref(\$name);
    die "Couldn't find interpolation value in $name" if ! $ref;
    return UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : "$ref";
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

sub get_function { $DIRECTIVES->{$_[1]}->{'parse'} }


###----------------------------------------------------------------###

sub parse_BLOCK {
    my ($self, $tag_ref, $func, $body_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref, {auto_quote => 1, quote_qr => qr/\w+/o});
    die "Couldn't find a filename on INSERT/INCLUDE/PROCESS on $copy"
        if ! $ref || ! UNIVERSAL::isa($ref, 'SCALAR') || ! length($$ref);

    my $block_name = $$ref;
    if (UNIVERSAL::isa($body_ref, 'SCALAR')) {
        $self->{'BLOCKS'}->{$block_name} =  $$body_ref;
    } else {
        $self->{'_blocks'}->{$block_name} = $body_ref;
    }
    return '';
}


sub parse_CALL { &parse_GET; '' }

sub parse_DEFAULT {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $str = $self->parse_GET($tag_ref);
    if (! $str) {
        $self->parse_SET(\$copy);
    }
    return '';
}

sub parse_DUMP {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref);
    require Data::Dumper;
    my $str = Data::Dumper::Dumper(UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : $ref);
    $str =~ s/\$VAR1/$copy/g;
    return $str;
}

sub parse_IF {
    my ($self, $tag_ref, $func, $body_ref, $template_ref) = @_;
    my $ref = $self->get_variable_ref($tag_ref);
    if (UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : $ref) {
        return UNIVERSAL::isa($body_ref, 'SCALAR') ? $$body_ref : $self->execute_tree($body_ref, $template_ref);
    } else {
        return '';
    }
}

sub parse_INCLUDE {
    my ($self, $tag_ref, $func, $template_ref) = @_;

    ### localize the swap
    my $swap = $self->{'_swap'};
    my @keys  = keys %$swap;
    local @$swap{@keys} = values %$swap; # note that we are only "cloning" one level deep

    my $str = $self->parse_PROCESS($tag_ref, $func, $template_ref);

    ### kill added keys
    my %keys = map {$_ => 1} @keys;
    delete @$swap{grep {!$keys{$_}} keys %$swap};

    return $str;
}

sub parse_INSERT {
    my ($self, $tag_ref, $func, $template_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref, {auto_quote => 1, quote_qr => qr/$QR_FILENAME|\w+/});
    die "Couldn't find a filename on INSERT/INCLUDE/PROCESS on $copy"
        if ! $ref || ! UNIVERSAL::isa($ref, 'SCALAR') || ! length($$ref);

    if ($$ref =~ /^\w+$/) {
        if (! $template_ref) {
            my $body_ref = $self->{'BLOCKS'}->{$$ref};
            return defined($body_ref) ? $body_ref : '';
        } else {
            my $body_ref = $self->{'_blocks'}->{$$ref};
            my $s = $self->execute_tree($body_ref, $template_ref);
            return $s;
        }
    } else {
        return $self->include_file($$ref);
    }
}

sub parse_GET {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref);
    die "Couldn't find variable on GET on $copy" if ! $ref;
    return UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : "$ref";
}

sub parse_PROCESS {
    my ($self, $tag_ref, $func, $template_ref) = @_;

    $self->{'state'}->{'recurse'} ||= 0;
    $self->{'state'}->{'recurse'} ++;
    die "MAX_RECURSE $MAX_RECURSE reached during INCLUDE/PROCESS on $$tag_ref"
        if $self->{'state'}->{'recurse'} >= $MAX_RECURSE;

    my $str = $self->parse_INSERT($tag_ref, $func, $template_ref);

    $str = $self->swap($str, $self->{'_swap'}); # restart the swap - passing it our current stash

    $self->{'state'}->{'recurse'} --;
    return $str;
}

sub parse_SET {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    while (length $$tag_ref) {
        my $set = $self->get_variable_ref($tag_ref, {return_set_ref => 1});
        die "Couldn't find variable on SET on $copy" if ! $set;
        my $val;
        if ($$tag_ref =~ s/^=\s*//) {
            my $val_ref = $self->get_variable_ref($tag_ref);
            $val = UNIVERSAL::isa($val_ref, 'SCALAR') ? $$val_ref : $val_ref;
        } else {
            $val = '';
        }
        $$tag_ref =~ s/^;\s*//;

        my ($ref, $name) = @$set;
        if (UNIVERSAL::isa($ref, 'HASH')) {
            $ref->{$name} = $val;
        } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
            $ref->[$name] = $val;
        }
    }
    return '';
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
        die "ABSOLUTE paths disabled" if ! $self->{'ABSOLUTE'};
        return $file if -e $file;
    } elsif ($file =~ m|^\./|) {
        die "RELATIVE paths disabled" if ! $self->{'RELATIVE'};
        return $file if -e $file;
    } else {
        my $paths = $self->include_path;
        $paths = [$paths] if ! ref $paths;
        foreach my $path (@$paths) {
            return "$path/$file" if -e "$path/$file";
        }
    }
    die "Couldn't find \"$file\" in INCLUDE_PATH";
}

sub include_file {
    my ($self, $file) = @_;
    my $full = $self->include_filename($file);
    open(my $fh, "<$full") || die "Couldn't open $file for reading: $!";
    read $fh, my $txt, -s $full;
    return $txt;
}

sub process {
    my ($self, $in, $swap, $out) = @_;

    ### get the content
    my $content;
    if (ref $in) {
        if (UNIVERSAL::isa($in, 'SCALAR')) { # reference to a string
            $content = $$in;
        } elsif (UNIVERSAL::isa($in, 'CODE')) {
            $content = $in->();
        } else { # should be a file handle
            local $/ = undef;
            $content = <$in>;
        }
    } else {
        $content = $self->include_file($in);
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
            die "ABSOLUTE paths disabled" if ! $self->{'ABSOLUTE'};
            $file = $out;
        } elsif ($out =~ m|^\./|) {
            die "RELATIVE paths disabled" if ! $self->{'RELATIVE'};
            $file = $out;
        } else {
            die "OUTPUT_PATH not set" if ! $self->{'OUTPUT_PATH'};
            $file = $self->{'OUTPUT_PATH'} . '/' . $out;
        }
        open(my $fh, ">$file") || die "Couldn't open \"$out\" for writing: $!";
        print $fh $content;
    } else {
        print $content;
    }

    return 1;
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex::Template - Beginning interface to Templating systems - for they are many

=head1 SYNOPSIS

  None yet.

=head1 DESCRIPTION

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
    "one $a two"       [ [[ 'concat', [\ 'one ', 0], ['a', 0], [\ ' two', 0 ] ]], 0 ]
    [0,1,2]            [ [[ 'arrayref', [\0, 0], [\1, 0], [\2, 0] ]], 0 ]
    [0,1,2].size       [ [[ 'arrayref', [\0, 0], [\1, 0], [\2, 0] ]], 0, '.', 'size', 0 ]
    ['a', a, $a ]      [ [[ 'arrayref', [\ 'a', 0], ['a', 0], [['a', 0], 0] ]], 0]
    {a  => 'b'}        [ [[ 'hashref',  [\ 'a', 0], [\ 'b', 0] ]], 0 ]
    {a  => 'b'}.size   [ [[ 'hashref',  [\ 'a', 0], [\ 'b', 0] ]], 0, '.', 'size', 0 ]
    {$a => b}          [ [[ 'hashref',  ['a', 0], ['b', 0] ]], 0 ]
    a + b              [ [[ '+', ['a', 0], ['b', 0] ]], 0 ]
    a * (b + c)        [ [[ '*', ['a', 0], [ [['+', ['b', 0], ['c', 0]]], 0 ]]], 0 ]
    (a + b)            [ [[ '+', ['a', 0], ['b', 0] ]], 0 ]
    (a + b) * c        [ [[ '*', [ [[ '+', ['a', 0], ['b', 0] ]], 0 ], ['c', 0] ]], 0 ]

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut

