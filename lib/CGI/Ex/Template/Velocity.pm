package CGI::Ex::Template::Velocity;

=head1 NAME

CGI::Ex::Template::Tmpl - provide Text::Tmpl support

=head1 DESCRIPTION

Provides for extra or extended features that may not be as commonly used.
This module should not normally be used by itself.

See the CGI::Ex::Template documentation for configuration and other parameters.

http://velocity.apache.org/engine/devel/vtl-reference-guide.html
http://www.javaworld.com/javaworld/jw-12-2001/jw-1228-velocity.html?page=4

=head1 AUTHOR

Paul Seamons <paul at seamons dot com>

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut

use strict;
use warnings;
use CGI::Ex::Dump qw(debug);

our $VERSION = '2.13';
our $error;

sub parse_tree_velocity {
    my $self    = shift;
    my $str_ref = shift;
    if (! $str_ref || ! defined $$str_ref) {
        $self->throw('parse.no_string', "No string or undefined during parse");
    }

    local $self->{'V2EQUALS'}    = $self->{'V2EQUALS'} || 0;
    local $self->{'INTERPOLATE'} = defined($self->{'INTERPOLATE'}) ? $self->{'INTERPOLATE'} : 1;
    local $self->{'V1DOLLAR'}    = defined($self->{'V1DOLLAR'})    ? $self->{'V1DOLLAR'}    : 1;
    local $self->{'ANYCASE'}     = defined($self->{'ANYCASE'})     ? $self->{'ANYCASE'}     : 1;
    local $self->{'AUTO_EVAL'}   = defined($self->{'AUTO_EVAL'})   ? $self->{'AUTO_EVAL'}   : 1;
    local $self->{'SHOW_UNDEFINED_INTERP'} = defined($self->{'SHOW_UNDEFINED_INTERP'}) ? $self->{'SHOW_UNDEFINED_INTERP'} : 1;

    local $self->{'START_TAG'}  = qr{\#};
    local $self->{'_start_tag'} = (! $self->{'INTERPOLATE'}) ? $self->{'START_TAG'} : qr{(?: $self->{'START_TAG'} | (\$))}sx;
    local $self->{'_end_tag'}; # changes over time

    local @{ $CGI::Ex::Template::Parse::ALIASES }{qw(PARSE   INCLUDE _INCLUDE ELSEIF)}
                                                = qw(PROCESS INSERT  INCLUDE  ELSIF);
    my $dirs    = $CGI::Ex::Template::Parse::DIRECTIVES;
    my $aliases = $CGI::Ex::Template::Parse::ALIASES;
    local @{ $dirs }{ keys %$aliases } = values %$aliases; # temporarily add to the table
    local @{ $self }{@CGI::Ex::Template::CONFIG_COMPILETIME} = @{ $self }{@CGI::Ex::Template::CONFIG_COMPILETIME};

    my @tree;             # the parsed tree
    my $pointer = \@tree; # pointer to current tree to handle nested blocks
    my @state;            # maintain block levels
    local $self->{'_state'} = \@state; # allow for items to introspect (usually BLOCKS)
    local $self->{'_no_interp'} = 0;   # no interpolation in perl
    my @in_view;          # let us know if we are in a view
    my @blocks;           # storage for defined blocks
    my @meta;             # place to store any found meta information (to go into META)
    my $post_chomp = 0;   # previous post_chomp setting
    my $continue   = 0;   # flag for multiple directives in the same tag
    my $post_op    = 0;   # found a post-operative DIRECTIVE
    my $capture;          # flag to start capture
    my $func;
    my $pre_chomp;
    my $node;
    local pos $$str_ref = 0;

    while (1) {
        ### allow for #set(foo = PROCESS foo)
        if ($capture) {
            $$str_ref =~ m{ \G \s* (\w+)\b }gcx
                || $self->throw('parse', "Error looking for block in capture DIRECTIVE", undef, pos($$str_ref));
            $func = $self->{'ANYCASE'} ? uc($1) : $1;

            $func = $aliases->{$func} if $aliases->{$func};
            $self->throw('parse', "Found unknown DIRECTIVE ($func)", undef, pos($$str_ref) - length($func))
                if ! $dirs->{$func};

            $node = [$func, pos($$str_ref) - length($func), undef];

            push @{ $capture->[4] }, $node;
            undef $capture;

        ### handle all other
        } else {
            ### find the next opening tag
            $$str_ref =~ m{ \G (.*?) $self->{'_start_tag'} }gcxs
                || last;
            my ($text, $dollar) = ($1, $2);

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

                my $not  = $$str_ref =~ m{ \G ! }gcx;
                my $mark = pos($$str_ref);
                my $ref;
                if ($$str_ref =~ m{ \G \{ }gcx) {
                    local $self->{'_operator_precedence'} = 0; # allow operators
                    local $self->{'_end_tag'} = qr{\}};
                    $ref = $self->parse_expr($str_ref);
                    $$str_ref =~ m{ \G \s* $CGI::Ex::Template::Parse::QR_COMMENTS \} }gcxo
                        || $self->throw('parse', 'Missing close }', undef, pos($$str_ref));
                } else {
                    local $self->{'_operator_precedence'} = 1; # no operators
                    local $CGI::Ex::Template::Parse::QR_COMMENTS = qr{};
                    $ref = $self->parse_expr($str_ref);
                }
                $self->throw('parse', "Error while parsing for interpolated string", undef, pos($$str_ref))
                    if ! defined $ref;
                if (! $not && $self->{'SHOW_UNDEFINED_INTERP'}) {
                    $ref = [[undef, '//', $ref, '$'.substr($$str_ref, $mark, pos($$str_ref)-$mark)], 0];
                }
                push @$pointer, ['GET', $mark, pos($$str_ref), $ref];
                $post_chomp = 0; # no chomping after dollar vars
                next;
            }

            ### allow for escaped #
            my $n = ($text =~ m{ (\\+) $ }x) ? length($1) : 0;
            if ($n % 2) { # were there odd escapes
                my $prev_text;
                $prev_text = \$pointer->[-1] if defined($pointer->[-1]) && ! ref($pointer->[-1]);
                chop($$prev_text) if $n % 2;
                if ($prev_text) { $$prev_text .= '#' } else { push @$pointer, '#' }
                next;
            }

            $$str_ref =~ m{ \G (\w+) }gcx
                || $$str_ref =~ m{ \G \{ (\w+) \} }gcx
                || $self->throw('parse', 'Missing directive name', undef, pos($$str_ref));
            $func = $self->{'ANYCASE'} ? uc($1) : $1;

            ### make sure we know this directive
            $func = $aliases->{$func} if $aliases->{$func};
            $self->throw('parse', "Found unknow DIRECTIVE ($func)", undef, pos($$str_ref) - length($func))
                if ! $dirs->{$func};
            $node = [$func, pos($$str_ref), undef];

            if ($$str_ref =~ m{ \G \( ([+=~-]?) }gcx) {
                $self->{'_end_tag'} = qr{\s*([+=~-]?)\)};
                $pre_chomp = $1;
            } else {
                $self->{'_end_tag'} = qr{};
                $pre_chomp = '';
            }

            ### take care of chomping (this is an extention to velocity
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
        if ($func ne 'END') {
            $node->[3] = eval { $dirs->{$func}->[0]->($self, $str_ref, $node) };
            if (my $err = $@) {
                $err->node($node) if UNIVERSAL::can($err, 'node') && ! $err->node;
                die $err;
            }
            $node->[2] = pos $$str_ref;
        }

        ### handle ending tags - or continuation blocks
        if ($func eq 'END' || $dirs->{$func}->[4]) {
            if (! @state) {
                print Data::Dumper::Dumper(\@tree);
                $self->throw('parse', "Found an $func tag while not in a block", $node, pos($$str_ref));
            }
            my $parent_node = pop @state;

            ### handle continuation blocks such as elsif, else, catch etc
            if ($dirs->{$func}->[4]) {
                pop @$pointer; # we will store the node in the parent instead
                $parent_node->[5] = $node;
                my $parent_type = $parent_node->[0];
                if (! $dirs->{$func}->[4]->{$parent_type}) {
                    $self->throw('parse', "Found unmatched nested block", $node, pos($$str_ref));
                }
            }

            ### restore the pointer up one level (because we hit the end of a block)
            $pointer = (! @state) ? \@tree : $state[-1]->[4];

            ### normal end block
            if (! $dirs->{$func}->[4]) {
                if ($parent_node->[0] eq 'BLOCK') { # move BLOCKS to front
                    if (defined($parent_node->[3]) && @in_view) {
                        push @{ $in_view[-1] }, $parent_node;
                    } else {
                        push @blocks, $parent_node;
                    }
                    if ($pointer->[-1] && ! $pointer->[-1]->[6]) { # capturing doesn't remove the var
                        splice(@$pointer, -1, 1, ());
                    }
                } elsif ($parent_node->[0] eq 'VIEW') {
                    my $ref = { map {($_->[3] => $_->[4])} @{ pop @in_view }};
                    unshift @{ $parent_node->[3] }, $ref;
                } elsif ($dirs->{$parent_node->[0]}->[5]) { # allow no_interp to turn on and off
                    $self->{'_no_interp'}--;
                }


            ### continuation block - such as an elsif
            } else {
                push @state, $node;
                $pointer = $node->[4] ||= [];
            }
            $node->[2] = pos $$str_ref;

        ### handle block directives
        } elsif ($dirs->{$func}->[2]) {
            push @state, $node;
            $pointer = $node->[4] ||= []; # allow future parsed nodes before END tag to end up in current node
            push @in_view, [] if $func eq 'VIEW';
            $self->{'_no_interp'}++ if $dirs->{$node->[0]}->[5] # allow no_interp to turn on and off

        } elsif ($func eq 'META') {
            unshift @meta, %{ $node->[3] }; # first defined win
            $node->[3] = undef;             # only let these be defined once - at the front of the tree
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
    unshift(@tree, @blocks) if @blocks;
    unshift(@tree, ['META', 1, 1, {@meta}]) if @meta;
    $self->throw('parse', "Missing end tag", $state[-1], pos($$str_ref)) if @state > 0;

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

sub merge {
    my ($self, $in, $swap, $out) = @_;
    local $self->{'SYNTAX'} = $self->{'SYNTAX'} || 'velocity';
    return $self->process_simple($in, $swap, $out);
}

###----------------------------------------------------------------###

1;
