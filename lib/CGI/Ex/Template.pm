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
            $PRECEDENCE
            );

BEGIN {
    $START_TAG  ||= '[%';
    $END_TAG    ||= '%]';

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
            parse   => \&parse_BLOCK,
            play    => \&play_BLOCK,
            end     => 1,
        },
        CALL    => {
            parse => \&parse_CALL,
            play  => \&play_CALL,
        },
        DEFAULT => {
            parse => \&parse_DEFAULT,
            play  => \&play_DEFAULT,
        },
        DUMP    => {
            parse => \&parse_DUMP,
            play  => \&play_DUMP,
        },
        END     => 1, # builtin that should never be called
        GET     => {
            parse => \&parse_GET,
            play  => \&play_GET,
        },
        IF      => {
            parse => \&parse_IF,
            play  => \&play_IF,
            end   => 1,
        },
        INCLUDE => {
            parse => \&parse_INCLUDE,
            play  => \&play_INCLUDE,
        },
        INSERT  => {
            parse => \&parse_INSERT,
            play  => \&play_INSERT,
        },
        SET     => {
            parse => \&parse_SET,
            play  => \&play_SET,
        },
        PROCESS => {
            parse => \&parse_PROCESS,
            play  => \&play_PROCESS,
        },
    };

    $PRECEDENCE = {qw(**  99   ^  99   pow 99
                      !   95
                      *   90   /  90   div 90   %  90   mod    90
                      +   85   -  85   _   85   ~  85   concat 85
                      <   80   >  80   <=  80   >= 80
                      lt  80   gt 80   le  80   ge 80
                      ==  75   != 75   eq  75   ne 75
                      &&  70
                      ||  65
                      ..  60
                      not 55
                      and 50
                      or  45
                      hashref 1 arrayref 1
                      )};

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

    my $tree = $self->parse_tree(\$_[0]);
    return defined($tree) ? $self->execute_tree($tree, \$_[0]) : $_[0];
}

sub parse_tree {
    my $self    = shift;
    my $str_ref = shift;

    my $START = $self->{'START_TAG'} || $START_TAG;
    my $END   = $self->{'END_TAG'}   || $END_TAG;
    my $len_s = length $START;
    my $len_e = length $END;

    my @tree;
    my @tstate;
    my $last = 0;
    while (1) {
        ### look through the string using index
        my $i = index($$str_ref, $START, $last);
        last if $i == -1;
        push @tree, ['TEXT', $last, $i] if $last != $i;
        my $begin = substr($$str_ref, $last, $i - $last),
        my $j = index($$str_ref, $END, $i + $len_s);
        $last = $j + $len_e;
        if ($j == -1) { # missing closing tag
            $last = length($$str_ref);
            last;
        }
        my $tag = substr($$str_ref, $i + $len_s, $j - ($i + $len_s));
        my $level = [undef , $i + $len_s, $j, 0, 0];

        ### take care of whitespace
        if ($tag =~ s/^-// || $self->{'PRE_CHOMP'}) {
            $level->[3] = 1;
            $level->[1] ++;
        }
        if ($tag =~ s/-$// || $self->{'POST_CHOMP'}) {
            $level->[4] = 1;
            $level->[2] --;
        }
        $tag =~ s/^\s+//;

        ### look for functions or variables
        if ($tag =~ /^(\w+) (?: $|\s)/x && $DIRECTIVES->{$1}) {
            my $func = $level->[0] = $1;
            $tag =~ s/^\w+\s*//;
            push @tree, $level;
            if ($func eq 'END') {
                if ($#tstate == -1) {
                    die "Found an unmatched END tag";
                } else {
                    ### store any child nodes into the parent node
                    my $parent_level = pop @tstate;
                    my $start_index = $parent_level->[6] = $i + $len_s;
                    my $j = $#tree;
                    for ( ; $j >= 0; $j--) {
                        last if $tree[$j]->[6] == $start_index;
                    }
                    my @sub_tree = splice @tree, $j + 1, $#tree - ($j + 1), (); # remove from main tree - but store
                    my $storage = $parent_level->[7] ||= [];
                    @$storage = @sub_tree;
                }
                next;
            }
            if ($DIRECTIVES->{$func}->{'end'}) {
                $level->[6] = -1;
                $level->[7] = [];
                push @tstate, $level;
            }
            $level->[5] = eval { $DIRECTIVES->{$func}->{'parse'}->($self, \$tag, $func, $level) };
            die $@ if $@ && $@ !~ /missing/i;

        } elsif (my $var = $self->parse_variable(\$tag)) {
            die "Found trailing info during variable access \"$tag" if $tag;
            $level->[0] = 'GET';
            $level->[5] = $var;
            push @tree, $level;

        } else {
            my $all  = substr($$str_ref, $i + $len_s, $j - ($i + $len_s));
            $all =~ s/^\s+//;
            $all =~ s/\s+$//;
            die "Not sure how to handle tag $all";
        }
    }

    return undef if $#tree == -1;

    push @tree, ['TEXT', $last, length($$str_ref)] if $last != length($$str_ref);

    return \@tree;
}

sub execute_tree {
    my ($self, $tree, $template_ref) = @_;
    my $str = '';
    my $post_chomp;
    # node contains (0: DIRECTIVE,
    #                1: start_index,
    #                2: end_index,
    #                3: pre_chomp,
    #                4: post_chomp,
    #                5: parsed tag,
    #                6: end block location
    while (my $node = shift @$tree) {
        my $val;
        if ($node->[0] eq 'TEXT') {
            my $pre_chomp = $tree->[0] && $tree->[0]->[3];

            $val = substr($$template_ref, $node->[1], $node->[2] - $node->[1]);

            $val =~ s{ (?:\n|^) [^\S\n]* \z }{}xm   if $pre_chomp; # remove any leading whitespace on the same line
            $val =~ s{ \G [^\S\n]* (?:\n?$|\n) }{}x if $post_chomp;

        } elsif ($node->[0] eq 'END') {
            $post_chomp = $node->[4];
            next;

        ### normal directive
        } else {
            $post_chomp = $node->[4];
            $val = $DIRECTIVES->{$node->[0]}->{'play'}->($self, $node->[5], $node, $template_ref);

        }

        $str .= $val if defined $val;
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
        my $arrayref = ['arrayref'];
        while (my $var = $self->parse_variable(\$copy)) {
            push @$arrayref, $var;
            $copy =~ s{ ^ , \s* }{}x;
        }
        $copy =~ s{ ^ \] \s* }{}x || die "Missing close \] on \"$copy\" $$str_ref";
        push @var, \ $arrayref;

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
        push @var, \ $hashref;

    ### looks like a paren grouper
    } elsif ($copy =~ s{ ^ \( \s* }{}x) {
        local $self->{'_state'}->{'operator_precedence'} = 0; # allow parens to have their own precedence
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
    if (! $self->{'_state'}->{'operator_precedence'}) {
        my $last_op;
        my $last_var;
        while ($copy =~ s{ ^ ( &&  | \|\| | \*\* | /   | \.\. |
                               and | or   | pow  | div |
                               >=  | >    | <=   | <   | == | != |
                               ge  | gt   | le   | lt  | eq | ne |
                               concat | mod | not | arrayref | hashref | _\b |
                               [~+\-*%\^!] ) \s* }{}x) {
            my $op   = $1;
            local $self->{'_state'}->{'operator_precedence'} = 1;
            my $var1 = [@var];
            my $var2 = $self->parse_variable(\$copy);
            if ($last_op && $self->operator_precedence($last_op, $op)) {
                my @var1 = @$last_var;
                @$last_var = (\ [$op, \@var1, $var2], 0);
            } else {
                @var = (\ [$op, $var1, $var2], 0);
            }
            $last_var = $var2;
            $last_op  = $op;
        }
    }

    #debug \@var, $copy;
    $$str_ref = $copy; # commit the changes
    return \@var;
}

sub operator_precedence {
    my ($self, $op1, $op2) = @_;
    my $val1 = $PRECEDENCE->{$op1} || 0;
    my $val2 = $PRECEDENCE->{$op2} || 0;
    return $val2 > $val1;
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
            $ref = $self->play_operator($$ref);
            $generated_list = 1;
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

    ### let the top level thing be a code block
    if (UNIVERSAL::isa($ref, 'CODE')) {
        return if $ARGS->{'set_var'} && $#$var <= $i;
        my @results = $ref->($args ? $self->vivify_args($args) : ());
        $ref = ($#results > 0) ? \@results : $results[0];
    }

    ### vivify the chained levels
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

    my $op = shift @$tree;
    if ($op eq 'concat') {
        return join "", grep {defined} $self->vivify_args($tree);
    } elsif ($op eq 'arrayref') {
        my @vals = $self->vivify_args($tree, {list_context => 1});
        return [@vals];
    } elsif ($op eq 'hashref') {
        my @args = $self->vivify_args($tree);
        push @args, undef if ! ($#args % 2);
        return {@args};
    } elsif ($op eq '||' || $op eq 'or') {
        for my $node (@$tree) {
            my $var = $self->vivify_variable($node);
            return $var if $var;
        }
        return '';
    } else{
        my @args = $self->vivify_args($tree);
        if ($op eq '..') {
            return [$args[0] .. $args[1]];
        } elsif ($op eq '+') {
            return $args[0] + $args[1];
        } elsif ($op eq '-') {
            return $args[0] - $args[1];
        } elsif ($op eq '*') {
            return $args[0] * $args[1];
        } elsif ($op eq '/' || $op eq 'div') {
            return $args[0] / $args[1];
        } elsif ($op eq '**' || $op eq 'pow') {
            return $args[0] ** $args[1];
        } elsif ($op eq '&&' || $op eq 'and') {
            for (@args) {
                return 0 if ! $_;
            }
            return $args[-1];
        }
    }
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
    my ($self, $tag_ref, $func, $tree_level) = @_;
    my $block = $tree_level->[7] ||= []; # create a location that can be occupied with the parsed tree

    if ($$tag_ref =~ s{ ^ (\w+) \s* (?! [\.\|]) }{}x) {
        $self->{'BLOCKS'}->{$1} = $block; # store an named reference here
    }

    return '';
}

sub play_BLOCK { '' }

sub parse_CALL { &parse_GET }

sub play_CALL { &play_GET; '' }

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
    return '';
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

sub parse_IF {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref);
    return $ref;
}

sub play_IF {
    my ($self, $var, $node, $template_ref) = @_;
    my $val = $self->vivify_variable($var);
    if ($val) {
        my $body_ref = $node->[7] ||= [];
        return $self->execute_tree($body_ref, $template_ref);
    } else {
        return '';
    }
}

sub parse_INCLUDE { &parse_PROCESS }

sub play_INCLUDE {
    my ($self, $tag_ref, $node, $template_ref) = @_;

    ### localize the swap
    my $swap = $self->{'_swap'};
    my @keys  = keys %$swap;
    local @$swap{@keys} = values %$swap; # note that we are only "cloning" one level deep

    my $str = $DIRECTIVES->{'PROCESS'}->{'play'}->($self, $tag_ref, $node, $template_ref);

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

sub parse_GET {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->parse_variable($tag_ref);
    die "Couldn't find variable on GET on $copy" if ! $ref;
    return $ref;
}

sub play_GET {
    my ($self, $ref) = @_;
    return $self->vivify_variable($ref);
}

sub parse_PROCESS {
    my ($self, $tag_ref) = @_;
    my $ref = $self->parse_variable($tag_ref, {auto_quote => qr/$QR_FILENAME|\w+/});
    return $ref;
}

sub play_PROCESS {
    my ($self, $var, $node, $template_ref) = @_;

    return '' if ! $var;
    my $filename = $self->vivify_variable($var);

    $self->{'state'}->{'recurse'} ||= 0;
    $self->{'state'}->{'recurse'} ++;
    if ($self->{'state'}->{'recurse'} >= $MAX_RECURSE) {
        my $func = $node->[0];
        die "MAX_RECURSE $MAX_RECURSE reached during $func on $filename";
    }

    ### see if the filename is an existing block name
    if (my $body_ref = $self->{'BLOCKS'}->{$filename}) {
        return $self->execute_tree($body_ref, $template_ref);
    }

    my $str = eval { $self->include_file($filename) };
    die $@ if $@ && $filename !~ /^\w+$/;

    $str = $self->swap($str, $self->{'_swap'}); # restart the swap - passing it our current stash

    $self->{'state'}->{'recurse'} --;
    return $str;
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

