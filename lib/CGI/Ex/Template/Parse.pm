package CGI::Ex::Template::Parse;

=head1 NAME

CGI::Ex::Template::Parse - Common parsing routines for creating AST from templates

=head1 DESCRIPTION

=head1 AUTHOR

Paul Seamons <paul at seamons dot com>

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut

use strict;
use warnings;
use base qw(Exporter);

our $VERSION   = '2.13';
our @EXPORT_OK = qw($ALIASES $DIRECTIVES $TAGS $QR_DIRECTIVE $QR_COMMENTS);

use CGI::Ex::Template qw(@CONFIG_COMPILETIME @CONFIG_RUNTIME $QR_OP_ASSIGN);




###----------------------------------------------------------------###

sub parse_BLOCK {
    my ($self, $str_ref, $node) = @_;

    my $end = $self->{'_end_tag'} || '(?!)';
    my $block_name = $self->parse_expr($str_ref, {auto_quote => "
              ($QR_FILENAME               # file name
              | $QR_BLOCK                 # or block
                (?= [+=~-]? $end          # an end tag
                  | \\s*[+,;]             # followed by explicit + , or ;
                  | \\s+ (?! [\\s=])      # or space not before an =
                )  \\s* $QR_COMMENTS"});

    return '' if ! defined $block_name;

    my $prepend = join "/", map {$_->[3]} grep {ref($_) && $_->[0] eq 'BLOCK'} @{ $self->{'_state'} || {} };
    return $prepend ? "$prepend/$block_name" : $block_name;
}

sub parse_CALL { $DIRECTIVES->{'GET'}->[0]->(@_) }

sub parse_CASE {
    my ($self, $str_ref) = @_;
    return if $$str_ref =~ m{ \G DEFAULT \s* }gcx;
    return $self->parse_expr($str_ref);
}

sub parse_CATCH {
    my ($self, $str_ref) = @_;
    return $self->parse_expr($str_ref, {auto_quote => "(\\w+\\b (?: \\.\\w+\\b)*) $QR_AQ_SPACE \\s* $QR_COMMENTS"});
}

sub parse_CONFIG {
    my ($self, $str_ref) = @_;

    my %ctime = map {$_ => 1} @CONFIG_COMPILETIME;
    my %rtime = map {$_ => 1} @CONFIG_RUNTIME;

    my $mark   = pos($$str_ref);
    my $config = $self->parse_args($str_ref, {named_at_front => 1, is_parened => 1});
    my $ref = $config->[0]->[0];
    for (my $i = 2; $i < @$ref; $i += 2) {
        my $key = $ref->[$i] = uc $ref->[$i];
        my $val = $ref->[$i + 1];
        if ($ctime{$key}) {
            $self->{$key} = $self->play_expr($val);
            if ($key eq 'INTERPOLATE') {
                $self->{'_start_tag'} = (! $self->{'INTERPOLATE'}) ? $self->{'START_TAG'} : qr{(?: $self->{'START_TAG'} | (\$))}sx;
            }
        } elsif (! $rtime{$key}) {
            $self->throw('parse', "Unknown CONFIG option \"$key\"", undef, pos($$str_ref));
        }
    }
    for (my $i = 1; $i < @$config; $i++) {
        my $key = $config->[$i] = uc $config->[$i]->[0];
        if ($ctime{$key}) {
            $config->[$i] = "CONFIG $key = ".(defined($self->{$key}) ? $self->{$key} : 'undef');
        } elsif (! $rtime{$key}) {
            $self->throw('parse', "Unknown CONFIG option \"$key\"", undef, pos($$str_ref));
        }
    }
    return $config;
}

sub parse_DEBUG {
    my ($self, $str_ref) = @_;
    $$str_ref =~ m{ \G ([Oo][Nn] | [Oo][Ff][Ff] | [Ff][Oo][Rr][Mm][Aa][Tt]) \s* }gcx
        || $self->throw('parse', "Unknown DEBUG option", undef, pos($$str_ref));
    my $ret = [lc($1)];
    if ($ret->[0] eq 'format') {
        $$str_ref =~ m{ \G ([\"\']) (|.*?[^\\]) \1 \s* }gcxs
            || $self->throw('parse', "Missing format string", undef, pos($$str_ref));
        $ret->[1] = $2;
    }
    return $ret;
}

sub parse_DEFAULT { $DIRECTIVES->{'SET'}->[0]->(@_) }

sub parse_DUMP {
    my ($self, $str_ref) = @_;
    return $self->parse_args($str_ref, {named_at_front => 1});
}

sub parse_FILTER {
    my ($self, $str_ref) = @_;
    my $name = '';
    if ($$str_ref =~ m{ \G ([^\W\d]\w*) \s* = \s* }gcx) {
        $name = $1;
    }

    my $filter = $self->parse_expr($str_ref);
    $filter = '' if ! defined $filter;

    return [$name, $filter];
}

sub parse_FOREACH {
    my ($self, $str_ref) = @_;
    my $items = $self->parse_expr($str_ref);
    my $var;
    if ($$str_ref =~ m{ \G \s* $QR_COMMENTS (= | [Ii][Nn]\b) \s* }gcxo) {
        $var = [@$items];
        $items = $self->parse_expr($str_ref);
    }
    return [$var, $items];
}

sub parse_GET {
    my ($self, $str_ref) = @_;
    my $ref = $self->parse_expr($str_ref);
    $self->throw('parse', "Missing variable name", undef, pos($$str_ref)) if ! defined $ref;
    return $ref;
}

sub parse_IF {
    my ($self, $str_ref) = @_;
    return $self->parse_expr($str_ref);
}

sub parse_INCLUDE { $DIRECTIVES->{'PROCESS'}->[0]->(@_) }

sub parse_INSERT { $DIRECTIVES->{'PROCESS'}->[0]->(@_) }

sub parse_LOOP {
    my ($self, $str_ref, $node) = @_;
    return $self->parse_expr($str_ref)
        || $self->throw('parse', 'Missing variable on LOOP directive', undef, pos($$str_ref));
}

sub parse_MACRO {
    my ($self, $str_ref, $node) = @_;

    my $name = $self->parse_expr($str_ref, {auto_quote => "(\\w+\\b) (?! \\.) \\s* $CGI::Ex::Template::QR_COMMENTS"});
    $self->throw('parse', "Missing macro name", undef, pos($$str_ref)) if ! defined $name;
    if (! ref $name) {
        $name = [ $name, 0 ];
    }

    my $args;
    if ($$str_ref =~ m{ \G \( \s* }gcx) {
        $args = $self->parse_args($str_ref, {positional_only => 1});
        $$str_ref =~ m{ \G \) \s* }gcx || $self->throw('parse.missing', "Missing close ')'", undef, pos($$str_ref));
    }

    $node->[6] = 1;           # set a flag to keep parsing
    return [$name, $args];
}

sub parse_META {
    my ($self, $str_ref) = @_;
    my $args = $self->parse_args($str_ref, {named_at_front => 1});
    my $hash;
    return $hash if ($hash = $self->play_expr($args->[0])) && UNIVERSAL::isa($hash, 'HASH');
    return undef;
}

sub parse_PROCESS {
    my ($self, $str_ref) = @_;

    return $self->parse_args($str_ref, {
        named_at_front       => 1,
        allow_bare_filenames => 1,
        require_arg          => 1,
    });
}

sub parse_SET {
    my ($self, $str_ref, $node, $initial_op, $initial_var) = @_;
    my @SET;
    my $func;

    if ($initial_op) {
        if ($$str_ref =~ m{ \G \s* $QR_COMMENTS $QR_DIRECTIVE }gcx # find a word
            && ((pos($$str_ref) -= length($1)) || 1)             # always revert
            && $DIRECTIVES->{$self->{'ANYCASE'} ? uc $1 : $1}) { # make sure its a directive - if so set up capturing
            $node->[6] = 1;                                      # set a flag to keep parsing
            my $val = $node->[4] ||= [];                         # setup storage
            return [[$initial_op, $initial_var, $val]];
        } else { # get a normal variable
            return [[$initial_op, $initial_var, $self->parse_expr($str_ref)]];
        }
    }

    while (1) {
        my $set = $self->parse_expr($str_ref);
        last if ! defined $set;

        if ($$str_ref =~ m{ \G \s* $QR_COMMENTS ($QR_OP_ASSIGN) >? }gcx) {
            my $op = $1;
            if ($$str_ref =~ m{ \G \s* $QR_COMMENTS $QR_DIRECTIVE }gcx # find a word
                && ((pos($$str_ref) -= length($1)) || 1)             # always revert
                && $DIRECTIVES->{$self->{'ANYCASE'} ? uc $1 : $1}) { # make sure its a directive - if so set up capturing
                $node->[6] = 1;                                      # set a flag to keep parsing
                my $val = $node->[4] ||= [];                         # setup storage
                push @SET, [$op, $set, $val];
                last;
            } else { # get a normal variable
                push @SET, [$op, $set, $self->parse_expr($str_ref)];
            }
        } else {
            push @SET, ['=', $set, undef];
        }
    }

    return \@SET;
}

sub parse_SWITCH { $DIRECTIVES->{'GET'}->[0]->(@_) }

sub parse_TAGS {
    my ($self, $str_ref, $node) = @_;

    my ($start, $end);
    if ($$str_ref =~ m{ \G (\w+) }gcxs) {
        my $ref = $CGI::Ex::Template::TAGS->{lc $1} || $self->throw('parse', "Invalid TAGS name \"$1\"", undef, pos($$str_ref));
        ($start, $end) = @$ref;

    } else {
        local $self->{'_operator_precedence'} = 1; # prevent operator matching
        $start = $$str_ref =~ m{ \G (?= \s* $CGI::Ex::Template::QR_COMMENTS [\'\"\/]) }gcx
            ? $self->parse_expr($str_ref)
            : $self->parse_expr($str_ref, {auto_quote => "(\\S+) \\s+ $CGI::Ex::Template::QR_COMMENTS"})
            || $self->throw('parse', "Invalid opening tag in TAGS", undef, pos($$str_ref));
        $end   = $$str_ref =~ m{ \G (?= \s* $CGI::Ex::Template::QR_COMMENTS [\'\"\/]) }gcx
            ? $self->parse_expr($str_ref)
            : $self->parse_expr($str_ref, {auto_quote => "(\\S+) \\s* $CGI::Ex::Template::QR_COMMENTS"})
            || $self->throw('parse', "Invalid closing tag in TAGS", undef, pos($$str_ref));
        for my $tag ($start, $end) {
            $tag = $self->play_expr($tag);
            $tag = quotemeta($tag) if ! ref $tag;
        }
    }
    return [$start, $end];
}

sub parse_THROW {
    my ($self, $str_ref, $node) = @_;
    my $name = $self->parse_expr($str_ref, {auto_quote => "(\\w+\\b (?: \\.\\w+\\b)*) $QR_AQ_SPACE \\s* $QR_COMMENTS"});
    $self->throw('parse.missing', "Missing name in THROW", $node, pos($$str_ref)) if ! $name;
    my $args = $self->parse_args($str_ref, {named_at_front => 1});
    return [$name, $args];
}

sub parse_UNLESS {
    my $ref = $DIRECTIVES->{'IF'}->[0]->(@_);
    return [[undef, '!', $ref], 0];
}

sub parse_USE {
    my ($self, $str_ref) = @_;

    my $QR_COMMENTS = $CGI::Ex::Template::QR_COMMENTS;

    my $var;
    my $mark = pos $$str_ref;
    if (defined(my $_var = $self->parse_expr($str_ref, {auto_quote => "(\\w+\\b) (?! \\.) \\s* $QR_COMMENTS"}))
        && ($$str_ref =~ m{ \G = >? \s* $QR_COMMENTS }gcxo # make sure there is assignment
            || ((pos($$str_ref) = $mark) && 0))               # otherwise we need to rollback
        ) {
        $var = $_var;
    }

    my $module = $self->parse_expr($str_ref, {auto_quote => "(\\w+\\b (?: (?:\\.|::) \\w+\\b)*) (?! \\.) \\s* $QR_COMMENTS"});
    $self->throw('parse', "Missing plugin name while parsing $$str_ref", undef, pos($$str_ref)) if ! defined $module;
    $module =~ s/\./::/g;

    my $args;
    my $open = $$str_ref =~ m{ \G \( \s* $QR_COMMENTS }gcxo;
    $args = $self->parse_args($str_ref, {is_parened => $open, named_at_front => 1});

    if ($open) {
        $$str_ref =~ m{ \G \) \s* $QR_COMMENTS }gcxo || $self->throw('parse.missing', "Missing close ')'", undef, pos($$str_ref));
    }

    return [$var, $module, $args];
}

sub parse_VIEW {
    my ($self, $str_ref) = @_;

    my $ref = $self->parse_args($str_ref, {
        named_at_front       => 1,
        require_arg          => 1,
    });

    return $ref;
}

sub parse_WHILE { $DIRECTIVES->{'IF'}->[0]->(@_) }

sub parse_WRAPPER { $DIRECTIVES->{'PROCESS'}->[0]->(@_) }

###----------------------------------------------------------------###

1;
