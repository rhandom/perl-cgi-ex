package CGI::Ex::Template::TT;

=head1 NAME

CGI::Ex::Template::TT - provide Template::Toolkit support

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
use CGI::Ex::Template qw($ALIASES $DIRECTIVES $TAGS @CONFIG_COMPILETIME $QR_COMMENTS $QR_DIRECTIVE $QR_OP_ASSIGN);

our $VERSION = '2.13';

###----------------------------------------------------------------###

sub parse_tree_tt3 {
    my $self    = shift;
    my $str_ref = shift;
    if (! $str_ref || ! defined $$str_ref) {
        $self->throw('parse.no_string', "No string or undefined during parse");
    }

    my $STYLE = $self->{'TAG_STYLE'} || 'default';
    local $self->{'_end_tag'}   = $self->{'END_TAG'}   || $TAGS->{$STYLE}->[1];
    local $self->{'START_TAG'}  = $self->{'START_TAG'} || $TAGS->{$STYLE}->[0];
    local $self->{'_start_tag'} = (! $self->{'INTERPOLATE'}) ? $self->{'START_TAG'} : qr{(?: $self->{'START_TAG'} | (\$))}sx;

    local @{ $self }{@CONFIG_COMPILETIME} = @{ $self }{@CONFIG_COMPILETIME};
    local @{ $DIRECTIVES }{ keys %$ALIASES } = values %$ALIASES; # temporarily add to the table

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
            $$str_ref =~ m{ \G (.*?) $self->{'_start_tag'} }gcxs
                || last;
            my ($text, $dollar) = ($1, $2); # dollar is set only on an interpolated var

            ### found a text portion - chomp it and store it
            if (length $text) {
                if (! $post_chomp) { }
                elsif ($post_chomp == 1) { $text =~ s{ ^ [^\S\n]* \n }{}x  }
                elsif ($post_chomp == 2) { $text =~ s{ ^ \s+         }{ }x }
                elsif ($post_chomp == 3) { $text =~ s{ ^ \s+         }{}x  }
                push @$pointer, $text if length $text;
            }

            ### handle variable interpolation ($2 eq $)
            if ($dollar) {
                ### inspect previous text chunk for escape slashes
                my $n = ($text =~ m{ (\\+) $ }x) ? length($1) : 0;
                if ($self->{'_no_interp'} || $n % 2) { # were there odd escapes
                    my $prev_text;
                    $prev_text = \$pointer->[-1] if defined($pointer->[-1]) && ! ref($pointer->[-1]);
                    chop($$prev_text) if $n % 2;
                    if ($prev_text) { $$prev_text .= $dollar } else { push @$pointer, $dollar }
                    next;
                }

                my $func = ($$str_ref =~ m{ \G ! }gcx) ? 'CALL' : 'GET';
                my $mark = pos($$str_ref);
                my $ref;
                if ($$str_ref =~ m{ \G \{ }gcx) {
                    local $self->{'_operator_precedence'} = 0; # allow operators
                    $ref = $self->parse_expr($str_ref);
                    $$str_ref =~ m{ \G \s* $QR_COMMENTS \} }gcxo
                        || $self->throw('parse', 'Missing close }', undef, pos($$str_ref));
                } else {
                    local $self->{'_operator_precedence'} = 1; # no operators
                    $ref = $self->parse_expr($str_ref);
                }
                $self->throw('parse', "Error while parsing for interpolated string", undef, pos($$str_ref))
                    if ! defined $ref;
                push @$pointer, [$func, $mark, pos($$str_ref), $ref];
                $post_chomp = 0; # no chomping after dollar vars
                next;
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
                $$str_ref =~ m{ \G (.*?) ([+~=-]?) ($self->{'_end_tag'}) }gcxs # brute force - can't comment tags with nested %]
                    || $self->throw('parse', "Missing closing tag", undef, pos($$str_ref));
                $node->[0] = '#';
                $node->[2] = pos($$str_ref) - length($3) - length($2);
                push @$pointer, $node;

                $post_chomp = $2;
                $post_chomp ||= $self->{'POST_CHOMP'};
                $post_chomp =~ y/-=~+/1230/ if $post_chomp;
                next;
            }
            #$$str_ref =~ m{ \G \s* $QR_COMMENTS }gcxo;
        }

        ### look for DIRECTIVES
        if ($$str_ref =~ m{ \G \s* $QR_COMMENTS $QR_DIRECTIVE }gcxo   # find a word
            && ($func = $self->{'ANYCASE'} ? uc($1) : $1)
            && ($DIRECTIVES->{$func}
                || ((pos($$str_ref) -= length $1) && 0))
            ) {                       # is it a directive
            $$str_ref =~ m{ \G \s* $QR_COMMENTS }gcx;

            $func = $ALIASES->{$func} if $ALIASES->{$func};
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
                ($self->{'_start_tag'}, $self->{'_end_tag'}, my $old_end) = (@{ $node->[3] }[0,1], $self->{'_end_tag'});

                ### allow for one more closing tag of the old style
                if ($$str_ref =~ m{ \G \s* $QR_COMMENTS ([+~=-]?) $old_end }gcxs) {
                    $post_chomp = $1 || $self->{'POST_CHOMP'};
                    $post_chomp =~ y/-=~+/1230/ if $post_chomp;
                    $continue = 0;
                    $post_op  = 0;
                    next;
                }

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
            if ($$str_ref =~ m{ \G \s* $QR_COMMENTS ($QR_OP_ASSIGN) >? (?! [+=~-]? $self->{'_end_tag'}) \s* $QR_COMMENTS }gcx) {
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
        if ($$str_ref =~ m{ \G \s* $QR_COMMENTS (?: ; \s* $QR_COMMENTS)? ([+=~-]?) $self->{'_end_tag'} }gcxs) {
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
        if (! $post_chomp) { }
        elsif ($post_chomp == 1) { $text =~ s{ ^ [^\S\n]* \n }{}x  }
        elsif ($post_chomp == 2) { $text =~ s{ ^ \s+         }{ }x }
        elsif ($post_chomp == 3) { $text =~ s{ ^ \s+         }{}x  }
        push @$pointer, $text if length $text;
    }

    return \@tree;
}

###----------------------------------------------------------------###

1;
