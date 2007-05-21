package CGI::Ex::Template::HTE;

=head1 NAME

CGI::Ex::Template::HTE - provide HTML::Template and HTML::Template::Expr support

=head1 DESCRIPTION

Provides for extra or extended features that may not be as commonly used.
This module should not normally be used by itself.

See the CGI::Ex::Template documentation for configuration and other parameters.

=head1 AUTHOR

Paul Seamons <paul at seamons dot com>

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut

use strict;
use warnings;

our $VERSION = '2.12';

sub parse_tree_hte {
    my $self    = shift;
    my $str_ref = shift;
    if (! $str_ref || ! defined $$str_ref) {
        $self->throw('parse.no_string', "No string or undefined during parse");
    }

    my $START = qr{<(|!--\s*)(/?)([+=~-]?)[Tt][Mm][Pp][Ll]_(\w+)\b};
    local $self->{'_end_tag'}; # changes over time

    #local @{ $self }{@CONFIG_COMPILETIME} = @{ $self }{@CONFIG_COMPILETIME};

    my @tree;             # the parsed tree
    my $pointer = \@tree; # pointer to current tree to handle nested blocks
    my @state;            # maintain block levels
    local $self->{'_state'} = \@state; # allow for items to introspect (usually BLOCKS)
    local $self->{'_in_perl'};         # no interpolation in perl
    my @in_view;          # let us know if we are in a view
    my @move_to_front;    # items that need to be declared first (usually BLOCKS)
    my @meta;             # place to store any found meta information (to go into META)
    my $post_chomp = 0;   # previous post_chomp setting
    my $continue   = 0;   # flag for multiple directives in the same tag
    my $post_op    = 0;   # found a post-operative DIRECTIVE
    my $capture;          # flag to start capture
    my $func;
    my $node;
    my ($comment, $is_close);
    local pos $$str_ref = 0;
    my $allow_expr = ! defined($self->{'EXPR'}) || $self->{'EXPR'}; # default is on

    while (1) {
        ### allow for TMPL_SET foo = PROCESS foo
        if ($capture) {
            $func = $$str_ref =~ m{ \G \s* (\w+)\b }gcx
                ? uc $1 : $self->throw('parse', "Error looking for block in capture DIRECTIVE", undef, pos($$str_ref));
            if ($func ne 'VAR' && ! $CGI::Ex::Template::DIRECTIVES->{$func}) {
                $self->throw('parse', "Found unknow DIRECTIVE ($func)", undef, pos($$str_ref) - length($func));
            }

            $node = [$func, pos($$str_ref) - length($func), undef];

            push @{ $capture->[4] }, $node;
            undef $capture;

        ### handle all other TMPL tags
        } else {
            ### find the next opening tag
            $$str_ref =~ m{ \G (.*?) $START }gcxs
                || last;
            (my $text, $comment, $is_close, my $pre_chomp, $func) = ($1, $2, $3, $4, uc $5);

            ### found a text portion - chomp it, interpolate it and store it
            if (length $text) {
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

            ### make sure we know this directive
            if ($func ne 'VAR' && ! $CGI::Ex::Template::DIRECTIVES->{$func}) {
                $self->throw('parse', "Found unknow DIRECTIVE ($func)", undef, pos($$str_ref) - length($func));
            }
            $node = [$func, pos($$str_ref) - length($func) - length($pre_chomp) - 5, undef];

            ### take care of chomping - yes HT now get CHOMP SUPPORT
            $pre_chomp ||= $self->{'PRE_CHOMP'};
            $pre_chomp  =~ y/-=~+/1230/ if $pre_chomp;
            if ($pre_chomp && $pointer->[-1] && ! ref $pointer->[-1]) {
                if    ($pre_chomp == 1) { $pointer->[-1] =~ s{ (?:\n|^) [^\S\n]* \z }{}x  }
                elsif ($pre_chomp == 2) { $pointer->[-1] =~ s{             (\s+) \z }{ }x }
                elsif ($pre_chomp == 3) { $pointer->[-1] =~ s{             (\s+) \z }{}x  }
                splice(@$pointer, -1, 1, ()) if ! length $pointer->[-1]; # remove the node if it is zero length
            }

            push @$pointer, $node;
        }

        $$str_ref =~ m{ \G \s+ }gcx;

        ### parse remaining tag details
        if (! $is_close) {
            ### handle HT style nodes
            if ($func =~ /^(IF|ELSIF|UNLESS|LOOP|VAR|INCLUDE)$/) {
                $func = $node->[0] = 'GET' if $func eq 'VAR';

                ### handle EXPR attribute
                if ($$str_ref =~ m{ \G [Ee][Xx][Pp][Rr] \s*=\s* ([\"\']?) \s* }gcx) {
                    if (! $allow_expr) {
                        $self->throw('parse', 'EXPR are not allowed without hte mode', undef, pos($$str_ref));
                    }
                    my $quote = $1;
                    $self->{'_end_tag'} = $comment ? qr{$quote\s*([+=~-]?)-->} : qr{$quote\s*([+=~-]?)>};
                    $node->[3] = $self->parse_expr($str_ref)
                        || $self->throw('parse', 'Error while looking for EXPR', undef, pos($$str_ref));

                ### handle "normal" NAME attributes
                } else {

                    ### store what we'll find at the end of the tag
                    $self->{'_end_tag'} = $comment ? qr{([+=~-]?)-->} : qr{([+=~-]?)>};

                    my ($name, $escape, $default);
                    while (1) {
                        if ($$str_ref =~ m{ \G (\w+) \s*=\s* }gcx) {
                            my $key = lc $1;
                            my $val = $$str_ref =~ m{ \G ([\"\']) (.*?) (?<!\\) \1 \s* }gcx ? $2
                                    : $$str_ref =~ m{ \G ([\w./+_]+) \s* }gcx               ? $1
                                    : $self->throw('parse', "Error while looking for value of \"$key\" attribute", undef, pos($$str_ref));
                            if ($key eq 'name') {
                                $name ||= $val;
                            } else {
                                $self->throw('parse', uc($key)." not allowed in TMPL_$func tag") if $func ne 'GET';
                                if    ($key eq 'escape')  { $escape  ||= lc $val }
                                elsif ($key eq 'default') { $default ||= $val    }
                                else  { $self->throw('parse', uc($key)." not allowed in TMPL_$func tag") }
                            }
                        } elsif ($$str_ref =~ m{ \G ([\w./+_]+) \s* }gcx) {
                            $name ||= $1;
                        } else {
                            last;
                        }
                    }

                    $self->throw('parse', 'Error while looking for NAME', undef, pos($$str_ref)) if ! $name;
                    $node->[3] = $func eq 'INCLUDE' ? $name : [($self->{'CASE_SENSITIVE'} ? $name : lc $name), 0]; # set the variable
                    $node->[3] = [[undef, '||', $node->[3], $default], 0] if $default;
                    $node->[2] = pos $$str_ref;

                    ### dress up node before finishing
                    $escape = lc $self->{'DEFAULT_ESCAPE'} if ! $escape && $self->{'DEFAULT_ESCAPE'};
                    if ($escape) {
                        $self->throw('parse', "ESCAPE not allowed in TMPL_$func tag") if $func ne 'GET';
                        if ($escape eq 'html' || $escape eq '1') {
                            push @{ $node->[3] }, '|', 'html', 0;
                        } elsif ($escape eq 'url') {
                            push @{ $node->[3] }, '|', 'url', 0;
                        } elsif ($escape eq 'js') {
                            push @{ $node->[3] }, '|', 'js', 0;
                        }
                    }
                }

                ### fixup DIRECTIVE storage
                if ($func eq 'INCLUDE') {
                    $node->[3] = [[[undef, '{}'],0], $node->[3]];
                } elsif ($func eq 'UNLESS') {
                    $node->[0] = 'IF';
                    $node->[3] = [[undef, '!', $node->[3]], 0];
                }

            ### handle TT Directive extensions
            } else {
                $self->throw('parse', "Found a TT tag $func with NO_TT enabled", undef, pos($$str_ref)) if $self->{'NO_TT'};
                $self->{'_end_tag'} = $comment ? qr{\s*([+=~-]?)-->} : qr{\s*([+=~-]?)>};
                $node->[3] = eval { $CGI::Ex::Template::DIRECTIVES->{$func}->[0]->($self, $str_ref, $node) };
                if (my $err = $@) {
                    $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                    die $err;
                }
                $node->[2] = pos $$str_ref;
            }
        }

        ### handle ending tags - or continuation blocks
        if ($is_close || $CGI::Ex::Template::DIRECTIVES->{$func}->[4]) {
            if (! @state) {
                $self->throw('parse', "Found an $func tag while not in a block", $node, pos($$str_ref));
            }
            my $parent_node = pop @state;

            ### TODO - check for matching loop close name
            $func = $node->[0] = 'END' if $is_close;

            ### handle continuation blocks such as elsif, else, catch etc
            if ($CGI::Ex::Template::DIRECTIVES->{$func}->[4]) {
                pop @$pointer; # we will store the node in the parent instead
                $parent_node->[5] = $node;
                my $parent_type = $parent_node->[0];
                if (! $CGI::Ex::Template::DIRECTIVES->{$func}->[4]->{$parent_type}) {
                    $self->throw('parse', "Found unmatched nested block", $node, pos($$str_ref));
                }
            }

            ### restore the pointer up one level (because we hit the end of a block)
            $pointer = (! @state) ? \@tree : $state[-1]->[4];

            ### normal end block
            if (! $CGI::Ex::Template::DIRECTIVES->{$func}->[4]) {
                if ($CGI::Ex::Template::DIRECTIVES->{$parent_node->[0]}->[5]) { # move things like BLOCKS to front
                    if ($parent_node->[0] eq 'BLOCK'
                        && defined($parent_node->[3])
                        && @in_view) {
                        push @{ $in_view[-1] }, $parent_node;
                    } else {
                        push @move_to_front, $parent_node;
                    }
                    if ($pointer->[-1] && ! $pointer->[-1]->[6]) { # capturing doesn't remove the var
                        splice(@$pointer, -1, 1, ());
                    }
                } elsif ($parent_node->[0] =~ /PERL$/) {
                    delete $self->{'_in_perl'};
                } elsif ($parent_node->[0] eq 'VIEW') {
                    my $ref = { map {($_->[3] => $_->[4])} @{ pop @in_view }};
                    unshift @{ $parent_node->[3] }, $ref;
                }


            ### continuation block - such as an elsif
            } else {
                push @state, $node;
                $pointer = $node->[4] ||= [];
            }

        } else {

            ### handle block directives
            if ($CGI::Ex::Template::DIRECTIVES->{$func}->[2]) {
                push @state, $node;
                $pointer = $node->[4] ||= []; # allow future parsed nodes before END tag to end up in current node
                push @in_view, [] if $func eq 'VIEW';
            } elsif ($func eq 'META') {
                unshift @meta, %{ $node->[3] }; # first defined win
                $node->[3] = undef;             # only let these be defined once - at the front of the tree
            }
        }


        ### look for the closing tag
        if ($$str_ref =~ m{ \G $self->{'_end_tag'} }gcxs) {
            $post_chomp = $1 || $self->{'POST_CHOMP'};
            $post_chomp =~ y/-=~+/1230/ if $post_chomp;
            $continue = 0;
            $post_op  = 0;
            next;

        ### setup capturing
        } elsif ($node->[6]) {
            $capture = $node;
            next;

        ### no closing tag
        } else {
            $self->throw('parse', "Not sure how to handle tag", $node, pos($$str_ref));
        }
    }

    ### cleanup the tree
    if (@move_to_front) {
        unshift @tree, @move_to_front;
    }
    if (@meta) {
        unshift @tree, ['META', 0, 0, {@meta}];
    }
    if ($#state > -1) {
        $self->throw('parse', "Missing END tag", $state[-1], 0);
    }

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

###----------------------------------------------------------------###
### a few HTML::Template and HTML::Template::Expr routines

sub param {
    my $self = shift;
    my $args;
    if (@_ == 1) {
        my $key = shift;
        if (ref($key) ne 'HASH') {
            $key = lc $key if $self->{'CASE_SENSITIVE'};
            return $self->{'param'}->{$key};
        }
        $args = [%$key];
    } else {
        $self->throw('param', "Odd number of parameters") if @_ % 2;
        $args = \@_;
    }
    while (@$args) {
        my $key = shift @$args;
        $key = lc $key if $self->{'CASE_SENSITIVE'};
        $self->{'param'}->{$key} = shift @$args;
    }
    return;
}

sub output {
    my $self = shift;
    my $args = ref($_[0]) eq 'HASH' ? shift : {@_};
    my $type = $self->{'TYPE'} || '';

    my $content;
    if ($type eq 'filehandle' || $self->{'FILEHANDLE'}) {
        my $in = $self->{'FILEHANDLE'} || $self->{'SOURCE'} || $self->throw('output', 'Missing source for type filehandle');
        local $/ = undef;
        $content = <$in>;
        $content = \$content;
    } elsif ($type eq 'arrayref' || $self->{'ARRAYREF'}) {
        my $in = $self->{'ARRAYREF'} || $self->{'SOURCE'} || $self->throw('output', 'Missing source for type arrayref');
        $content = join "", @$in;
        $content = \$content;
    } elsif ($type eq 'filename' || $self->{'FILENAME'}) {
        $content = $self->{'FILENAME'} || $self->{'SOURCE'} || $self->throw('output', 'Missing source for type filename');
    } elsif ($type eq 'scalarref' || $self->{'SCALARREF'}) {
        $content = $self->{'SCALARREF'} || $self->{'SOURCE'} || $self->throw('output', 'Missing source for type scalarref');
    } else {
        $self->throw('output', "Unknown input type");
    }


    my $param = $self->{'param'} || {};
    if (my $ref = $self->{'ASSOCIATE'}) {
        foreach my $obj (ref($ref) eq 'ARRAY' ? $ref : @$ref) {
            foreach my $key ($obj->param) {
                $self->{'param'}->{$key} = $obj->param($key);
            }
        }
    }


    ### override some TT defaults
    local $self->{'FILE_CACHE'} = $self->{'DOUBLE_FILE_CACHE'} ? 1 : $self->{'FILE_CACHE'};
    my $cache_size  = ($self->{'CACHE'})         ? undef : 0;
    my $compile_dir = (! $self->{'FILE_CACHE'})  ? undef : $self->{'FILE_CACHE_DIR'} || $self->throw('output', 'Missing file_cache_dir');
    my $stat_ttl    = (! $self->{'BLIND_CACHE'}) ? undef : 60; # not sure how high to set the blind cache
    $cache_size = undef if $self->{'DOUBLE_FILE_CACHE'};

    local $self->{'SYNTAX'}       = $self->{'SYNTAX'} || 'hte';
    local $self->{'NO_TT'}        = $self->{'NO_TT'} || ($self->{'SYNTAX'} eq 'hte' ? 0 : 1);
    local $self->{'CACHE_SIZE'}   = $cache_size;
    local $self->{'STAT_TTL'}     = $stat_ttl;
    local $self->{'COMPILE_DIR'}  = $compile_dir;
    local $self->{'ABSOLUTE'}     = 1;
    local $self->{'RELATIVE'}     = 1;
    local $self->{'INCLUDE_PATH'} = $self->{'PATH'} || './';
    local $CGI::Ex::Template::QR_PRIVATE = undef;

    if ($args->{'print_to'}) {
        $self->process_simple($content, $param, $args->{'print_to'}) || die $self->error;
        return undef;
    } else {
        my $out = '';
        $self->process_simple($content, $param, \$out) || die $self->error;
        return $out;
    }
}

###----------------------------------------------------------------###

1;
