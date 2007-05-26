package CGI::Ex::Template::Play;

=head1 NAME

CGI::Ex::Template::Play - Take the AST and play it in perl

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

###----------------------------------------------------------------###

sub play_tree {
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

        $DIRECTIVES->{$node->[0]}->[1]->($self, $node->[3], $node, $out_ref);
    }
}

###----------------------------------------------------------------###

sub play_BLOCK {
    my ($self, $block_name, $node, $out_ref) = @_;

    ### store a named reference - but do nothing until something processes it
    $self->{'BLOCKS'}->{$block_name} = {
        _tree => $node->[4],
        name  => $self->{'_component'}->{'name'} .'/'. $block_name,
    };

    return;
}

sub play_CALL {
    my ($self, $ident, $node) = @_;
    my $var = $self->play_expr($ident);
    $var = $self->undefined_get($ident, $node) if ! defined $var;
    return;
}

sub play_control {
    my ($self, $undef, $node) = @_;
    $self->throw(lc($node->[0]), 'Control exception', $node);
}

sub play_CLEAR {
    my ($self, $undef, $node, $out_ref) = @_;
    $$out_ref = '';
    return;
}

sub play_CONFIG {
    my ($self, $config, $node, $out_ref) = @_;

    my %rtime = map {$_ => 1} @CGI::Ex::Template::CONFIG_RUNTIME;

    ### do runtime config - not many options get these
    my ($named, @the_rest) = @$config;
    $named = $self->play_expr($named);
    @{ $self }{keys %$named} = @{ $named }{keys %$named};

    ### show what current values are
    $$out_ref .= join("\n", map { $rtime{$_} ? ("CONFIG $_ = ".(defined($self->{$_}) ? $self->{$_} : 'undef')) : $_ } @the_rest);
    return;
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
    return;
}

sub play_DEFAULT {
    my ($self, $set) = @_;
    foreach (@$set) {
        my ($op, $set, $default) = @$_;
        next if ! defined $set;
        my $val = $self->play_expr($set);
        if (! $val) {
            $default = defined($default) ? $self->play_expr($default) : '';
            $self->set_variable($set, $default);
        }
    }
    return;
}

sub play_DUMP {
    my ($self, $dump, $node, $out_ref) = @_;

    my $conf = $self->{'DUMP'};
    return if ! $conf && defined $conf; # DUMP => 0
    $conf = {} if ref $conf ne 'HASH';

    ### allow for handler override
    my $handler = $conf->{'handler'};
    if (! $handler) {
        require Data::Dumper;
        my $obj = Data::Dumper->new([]);
        my $meth;
        foreach my $prop (keys %$conf) { $obj->$prop($conf->{$prop}) if $prop =~ /^\w+$/ && ($meth = $obj->can($prop)) }
        my $sort = defined($conf->{'Sortkeys'}) ? $obj->Sortkeys : 1;
        $obj->Sortkeys(sub { my $h = shift; [grep {$_ !~ $CGI::Ex::Template::QR_PRIVATE} ($sort ? sort keys %$h : keys %$h)] });
        $handler = sub { $obj->Values([@_]); $obj->Dump }
    }

    my ($named, @dump) = @$dump;
    push @dump, $named if ! $self->is_empty_named_args($named); # add named args back on at end - if there are some
    $_ = $self->play_expr($_) foreach @dump;

    ### look for the text describing what to dump
    my $info = $self->node_info($node);
    my $out;
    if (@dump) {
        $out = $handler->(@dump && @dump == 1 ? $dump[0] : \@dump);
        my $name = $info->{'text'};
        $name =~ s/^[+=~-]?\s*DUMP\s+//;
        $name =~ s/\s*[+=~-]?$//;
        $out =~ s/\$VAR1/$name/;
    } elsif (defined($conf->{'EntireStash'}) && ! $conf->{'EntireStash'}) {
        $out = '';
    } else {
        $out = $handler->($self->{'_vars'});
        $out =~ s/\$VAR1/EntireStash/g;
    }

    if ($conf->{'html'} || (! defined($conf->{'html'}) && $ENV{'REQUEST_METHOD'})) {
        $out = $CGI::Ex::Template::SCALAR_OPS->{'html'}->($out);
        $out = "<pre>$out</pre>";
        $out = "<b>DUMP: File \"$info->{file}\" line $info->{line}</b>$out" if $conf->{'header'} || ! defined $conf->{'header'};
    } else {
        $out = "DUMP: File \"$info->{file}\" line $info->{line}\n    $out" if $conf->{'header'} || ! defined $conf->{'header'};
    }

    $$out_ref .= $out;
    return;
}

sub play_FILTER {
    my ($self, $ref, $node, $out_ref) = @_;
    my ($name, $filter) = @$ref;

    return '' if ! @$filter;

    $self->{'FILTERS'}->{$name} = $filter if length $name;

    my $sub_tree = $node->[4];

    ### play the block
    my $out = '';
    eval { $self->play_tree($sub_tree, \$out) };
    die $@ if $@ && ref($@) !~ /Template::Exception$/;

    my $var = [[undef, '~', $out], 0, '|', @$filter]; # make a temporary var out of it

    return $CGI::Ex::Template::DIRECTIVES->{'GET'}->[1]->($self, $var, $node, $out_ref);
}

sub play_FOREACH {
    my ($self, $ref, $node, $out_ref) = @_;

    ### get the items - make sure it is an arrayref
    my ($var, $items) = @$ref;

    $items = $self->play_expr($items);
    return '' if ! defined $items;

    if (ref($items) !~ /Iterator$/) {
        $items = $self->iterator($items);
    }

    my $sub_tree = $node->[4];

    local $self->{'_vars'}->{'loop'} = $items;

    ### if the FOREACH tag sets a var - then nothing but the loop var gets localized
    if (defined $var) {
        my ($item, $error) = $items->get_first;
        while (! $error) {

            $self->set_variable($var, $item);

            ### execute the sub tree
            eval { $self->play_tree($sub_tree, $out_ref) };
            if (my $err = $@) {
                if (UNIVERSAL::can($err, 'type')) {
                    if ($err->type eq 'next') {
                        ($item, $error) = $items->get_next;
                        next;
                    }
                    last if $err->type =~ /last|break/;
                }
                die $err;
            }

            ($item, $error) = $items->get_next;
        }
        die $error if $error && $error != 3; # Template::Constants::STATUS_DONE;
    ### if the FOREACH tag doesn't set a var - then everything gets localized
    } else {

        ### localize variable access for the foreach
        my $swap = $self->{'_vars'};
        local $self->{'_vars'} = my $copy = {%$swap};

        ### iterate use the iterator object
        #foreach (my $i = $items->index; $i <= $#$vals; $items->index(++ $i)) {
        my ($item, $error) = $items->get_first;
        while (! $error) {

            if (ref($item) eq 'HASH') {
                @$copy{keys %$item} = values %$item;
            }

            ### execute the sub tree
            eval { $self->play_tree($sub_tree, $out_ref) };
            if (my $err = $@) {
                if (UNIVERSAL::can($err, 'type')) {
                    if ($err->type eq 'next') {
                        ($item, $error) = $items->get_next;
                        next;
                    }
                    last if $err->type =~ /last|break/;
                }
                die $err;
            }

            ($item, $error) = $items->get_next;
        }
        die $error if $error && $error != 3; # Template::Constants::STATUS_DONE;
    }

    return;
}

sub play_GET {
    my ($self, $ident, $node, $out_ref) = @_;
    my $var = $self->play_expr($ident);
    if (defined $var) {
        $$out_ref .= $var;
    } else {
        $var = $self->undefined_get($ident, $node);
        $$out_ref .= $var if defined $var;
    }
    return;
}

sub play_IF {
    my ($self, $var, $node, $out_ref) = @_;

    my $val = $self->play_expr($var);
    if ($val) {
        my $body_ref = $node->[4] ||= [];
        $self->play_tree($body_ref, $out_ref);
        return;
    }

    while ($node = $node->[5]) { # ELSE, ELSIF's
        if ($node->[0] eq 'ELSE') {
            my $body_ref = $node->[4] ||= [];
            $self->play_tree($body_ref, $out_ref);
            return;
        }
        my $var = $node->[3];
        my $val = $self->play_expr($var);
        if ($val) {
            my $body_ref = $node->[4] ||= [];
            $self->play_tree($body_ref, $out_ref);
            return;
        }
    }
    return;
}

sub play_INCLUDE {
    my ($self, $str_ref, $node, $out_ref) = @_;

    ### localize the swap
    my $swap = $self->{'_vars'} || {};
    local $self->{'_vars'} = {%$swap};

    ### localize the blocks
    my $blocks = $self->{'BLOCKS'} || {};
    local $self->{'BLOCKS'} = {%$blocks};

    return $DIRECTIVES->{'PROCESS'}->[1]->($self, $str_ref, $node, $out_ref);
}

sub play_INSERT {
    my ($self, $args, $node, $out_ref) = @_;
    if ($self->{'NO_INCLUDES'}) {
        $self->throw('file', "NO_INCLUDES was set during a $node->[0] directive");
    }

    my ($named, @files) = @$args;

    foreach my $name (@files) {
        my $filename = $self->play_expr($name);
        $$out_ref .= $self->_insert($filename);
    }

    return;
}

sub play_LOOP {
    my ($self, $ref, $node, $out_ref) = @_;

    my $var = $self->play_expr(ref($ref) ? $ref : [$ref,0]); # allow for "string" identified loops
    my $sub_tree = $node->[4];

    my $global = ! $self->{'SYNTAX'} || $self->{'SYNTAX'} ne 'ht' || $self->{'GLOBAL_VARS'};

    my $items = ref($var) eq 'ARRAY' ? $var : ref($var) eq 'HASH' ? [$var] : [];

    my $i = 0;
    for my $ref (@$items) {
        ### setup the loop
        $self->throw('loop', 'Scalar value used in LOOP') if $ref && ref($ref) ne 'HASH';
        local $self->{'_vars'} = (! $global) ? ($ref || {}) : (ref($ref) eq 'HASH') ? {%{ $self->{'_vars'} }, %$ref} : $self->{'_vars'};
        if ($self->{'LOOP_CONTEXT_VARS'} && ! $CGI::Ex::Template::QR_PRIVATE) {
            $self->{'_vars'}->{'__counter__'} = ++$i;
            $self->{'_vars'}->{'__first__'} = $i == 1 ? 1 : 0;
            $self->{'_vars'}->{'__last__'}  = $i == @$items ? 1 : 0;
            $self->{'_vars'}->{'__inner__'} = $i == 1 || $i == @$items ? 0 : 1;
            $self->{'_vars'}->{'__odd__'}   = ($i % 2) ? 1 : 0;
        }

        ### execute the sub tree
        $self->play_tree($sub_tree, $out_ref);
    }

    return;
}

sub play_MACRO {
    my ($self, $ref, $node, $out_ref) = @_;
    my ($name, $args) = @$ref;

    ### get the sub tree
    my $sub_tree = $node->[4];
    if (! $sub_tree || ! $sub_tree->[0]) {
        $self->set_variable($name, undef);
        return;
    } elsif ($sub_tree->[0]->[0] eq 'BLOCK') {
        $sub_tree = $sub_tree->[0]->[4];
    }

    my $self_copy = $self;
    eval {require Scalar::Util; Scalar::Util::weaken($self_copy)};

    ### install a closure in the stash that will handle the macro
    $self->set_variable($name, sub {
        ### macros localize
        my $copy = $self_copy->{'_vars'};
        local $self_copy->{'_vars'}= {%$copy};

        ### prevent recursion
        local $self_copy->{'_macro_recurse'} = $self_copy->{'_macro_recurse'} || 0;
        my $max = $self_copy->{'MAX_MACRO_RECURSE'} || $CGI::Ex::Template::MAX_MACRO_RECURSE;
        $self_copy->throw('macro_recurse', "MAX_MACRO_RECURSE $max reached")
            if ++$self_copy->{'_macro_recurse'} > $max;

        ### set arguments
        my $named = pop(@_) if $_[-1] && UNIVERSAL::isa($_[-1],'HASH') && $#_ > $#$args;
        my @positional = @_;
        foreach my $var (@$args) {
            $self_copy->set_variable($var, shift(@positional));
        }
        foreach my $name (sort keys %$named) {
            $self_copy->set_variable([$name, 0], $named->{$name});
        }

        ### finally - run the sub tree
        my $out = '';
        $self_copy->play_tree($sub_tree, \$out);
        return $out;
    });

    return;
}

sub play_META {
    my ($self, $hash) = @_;
    return if ! $hash;
    my @keys = keys %$hash;

    my $ref;
    if ($self->{'_top_level'}) {
        $ref = $self->{'_template'} ||= {};
    } else {
        $ref = $self->{'_component'} ||= {};
    }

    @{ $ref }{ @keys } = @{ $hash }{ @keys };
    return;
}

sub play_PERL {
    my ($self, $info, $node, $out_ref) = @_;
    $self->throw('perl', 'EVAL_PERL not set') if ! $self->{'EVAL_PERL'};

    ### fill in any variables
    my $perl = $node->[4] || return;
    my $out  = '';
    $self->play_tree($perl, \$out);
    $out = $1 if $out =~ /^(.+)$/s; # blatant untaint - shouldn't use perl anyway

    ### try the code
    my $err;
    eval {
        package CGI::Ex::Template::Perl;

        my $context = $self->context;
        my $stash   = $context->stash;

        ### setup a fake handle
        local *PERLOUT;
        tie *PERLOUT, 'CGI::Ex::Template::EvalPerlHandle', $out_ref;
        my $old_fh = select PERLOUT;

        eval $out;
        $err = $@;

        ### put the handle back
        select $old_fh;

    };
    $err ||= $@;


    if ($err) {
        $self->throw('undef', $err) if ref($err) !~ /Template::Exception$/;
        die $err;
    }

    return;
}

sub play_PROCESS {
    my ($self, $info, $node, $out_ref) = @_;
    if ($self->{'NO_INCLUDES'}) {
        $self->throw('file', "NO_INCLUDES was set during a $node->[0] directive");
    }

    my ($args, @files) = @$info;

    ### set passed args
    # [[undef, '{}', 'key1', 'val1', 'key2', 'val2'], 0]
    $args = $args->[0];
    foreach (my $i = 2; $i < @$args; $i+=2) {
        my $key = $args->[$i];
        my $val = $self->play_expr($args->[$i+1]);
        if (ref($key) && @$key == 2 && $key->[0] eq 'import' && UNIVERSAL::isa($val, 'HASH')) { # import ?! - whatever
            foreach my $key (keys %$val) {
                $self->set_variable([$key,0], $val->{$key});
            }
            next;
        }
        $self->set_variable($key, $val);
    }

    ### iterate on any passed block or filename
    foreach my $ref (@files) {
        next if ! defined $ref;
        my $filename = $self->play_expr($ref);
        my $out = ''; # have temp item to allow clear to correctly clear

        ### normal blocks or filenames
        if (! ref $filename) {
            eval { $self->_process($filename, $self->{'_vars'}, \$out) }; # restart the swap - passing it our current stash

        ### allow for $template which is used in some odd instances
        } else {
            my $doc;
            if ($ref->[0] eq 'template') {
                $doc = $filename;
            } else {
                $doc = $self->play_expr($ref);
                if (ref($doc) ne 'HASH' || ! $doc->{'_tree'}) {
                    $self->throw('process', "Passed item doesn't appear to be a valid document");
                }
            }
            $self->throw('process', "Recursion detected in $node->[0] \$template") if $self->{'_process_dollar_template'};
            local $self->{'_process_dollar_template'} = 1;
            local $self->{'_component'} = $filename;
            return if ! $doc->{'_tree'};

            ### execute and trim
            eval { $self->play_tree($doc->{'_tree'}, \$out) };
            if ($self->{'TRIM'}) {
                $out =~ s{ \s+ $ }{}x;
                $out =~ s{ ^ \s+ }{}x;
            }

            ### handle exceptions
            if (my $err = $@) {
                $err = $self->exception('undef', $err) if ref($err) !~ /Template::Exception$/;
                $err->doc($doc) if $doc && $err->can('doc') && ! $err->doc;
            }

        }

        ### append any output
        $$out_ref .= $out;
        if (my $err = $@) {
            die $err if ref($err) !~ /Template::Exception$/ || $err->type !~ /return/;
        }
    }

    return;
}

sub play_RAWPERL {
    my ($self, $info, $node, $out_ref) = @_;
    $self->throw('perl', 'EVAL_PERL not set') if ! $self->{'EVAL_PERL'};

    ### fill in any variables
    my $tree = $node->[4] || return;
    my $perl  = '';
    $self->play_tree($tree, \$perl);
    $perl = $1 if $perl =~ /^(.+)$/s; # blatant untaint - shouldn't use perl anyway

    ### try the code
    my $err;
    my $output = '';
    eval {
        package CGI::Ex::Template::Perl;

        my $context = $self->context;
        my $stash   = $context->stash;

        eval $perl;
        $err = $@;
    };
    $err ||= $@;

    $$out_ref .= $output;

    if ($err) {
        $self->throw('undef', $err) if ref($err) !~ /Template::Exception$/;
        die $err;
    }

    return;
}

sub play_SET {
    my ($self, $set, $node) = @_;
    foreach (@$set) {
        my ($op, $set, $val) = @$_;
        if (! defined $val) { # not defined
            # do nothing - allow for setting to undef
        } elsif ($node->[4] && $val == $node->[4]) { # a captured directive
            my $sub_tree = $node->[4];
            $sub_tree = $sub_tree->[0]->[4] if $sub_tree->[0] && $sub_tree->[0]->[0] eq 'BLOCK';
            $val = '';
            $self->play_tree($sub_tree, \$val);
        } else { # normal var
            $val = $self->play_expr($val);
        }

        if ($OP_DISPATCH->{$op}) {
            local $^W;
            $val = $OP_DISPATCH->{$op}->($self->play_expr($set), $val);
        }

        $self->set_variable($set, $val);
    }
    return;
}

sub play_SWITCH {
    my ($self, $var, $node, $out_ref) = @_;

    my $val = $self->play_expr($var);
    $val = '' if ! defined $val;
    ### $node->[4] is thrown away

    my $default;
    while ($node = $node->[5]) { # CASES
        my $var = $node->[3];
        if (! defined $var) {
            $default = $node->[4];
            next;
        }

        my $val2 = $self->play_expr($var);
        $val2 = [$val2] if ! UNIVERSAL::isa($val2, 'ARRAY');
        for my $test (@$val2) { # find matching values
            next if ! defined $val && defined $test;
            next if defined $val && ! defined $test;
            if ($val ne $test) { # check string-wise first - then numerical
                next if $val  !~ m{ ^ -? $QR_NUM $ }ox;
                next if $test !~ m{ ^ -? $QR_NUM $ }ox;
                next if $val != $test;
            }

            my $body_ref = $node->[4] ||= [];
            $self->play_tree($body_ref, $out_ref);
            return;
        }
    }

    if ($default) {
        $self->play_tree($default, $out_ref);
    }

    return;
}

sub play_THROW {
    my ($self, $ref, $node) = @_;
    my ($name, $args) = @$ref;

    $name = $self->play_expr($name);

    my ($named, @args) = @$args;
    push @args, $named if ! $self->is_empty_named_args($named); # add named args back on at end - if there are some

    @args = map { $self->play_expr($_) } @args;
    $self->throw($name, \@args, $node); # dies
    return; # but return just in case
}

sub play_TRY {
    my ($self, $foo, $node, $out_ref) = @_;
    my $out = '';

    my $body_ref = $node->[4];
    eval { $self->play_tree($body_ref, \$out) };
    my $err = $@;

    if (! $node->[5]) { # no catch or final
        if (! $err) { # no final block and no error
            $$out_ref .= $out;
            return;
        }
        $self->throw('parse.missing', "Missing CATCH block", $node);
    }
    if ($err) {
        $err = $self->exception('undef', $err) if ref($err) !~ /Template::Exception$/;
        if ($err->type =~ /stop|return/) {
            $$out_ref .= $out;
            die $err;
        }
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
        my $name = $self->play_expr($node->[3]);
        $name = '' if ! defined $name || lc($name) eq 'default';
        if ($type =~ / ^ \Q$name\E \b /x
            && (! defined($last_found) || length($last_found) < length($name))) { # more specific wins
            $catch_body_ref = $node->[4] || [];
            $last_found     = $name;
        }
    }

    ### play the best catch block
    if ($err) {
        if (! $catch_body_ref) {
            $$out_ref .= $out;
            die $err;
        }
        local $self->{'_vars'}->{'error'} = $err;
        local $self->{'_vars'}->{'e'}     = $err;
        eval { $self->play_tree($catch_body_ref, \$out) };
        if (my $err = $@) {
            $$out_ref .= $out;
            die $err;
        }
    }

    ### the final block
    $self->play_tree($final, \$out) if $final;

    $$out_ref .= $out;

    return;
}

sub play_UNLESS { return $DIRECTIVES->{'IF'}->[1]->(@_) }

sub play_USE {
    my ($self, $ref, $node, $out_ref) = @_;
    my ($var, $module, $args) = @$ref;

    ### get the stash storage location - default to the module
    $var = $module if ! defined $var;
    my @var = map {($_, 0, '.')} split /(?:\.|::)/, $var;
    pop @var; # remove the trailing '.'

    my ($named, @args) = @$args;
    push @args, $named if ! $self->is_empty_named_args($named); # add named args back on at end - if there are some

    ### look for a plugin_base
    my $BASE = $self->{'PLUGIN_BASE'} || 'Template::Plugin'; # I'm not maintaining plugins - leave that to TT
    my $obj;

    foreach my $base (ref($BASE) eq 'ARRAY' ? @$BASE : $BASE) {
        my $package = $self->{'PLUGINS'}->{$module} ? $self->{'PLUGINS'}->{$module}
        : $self->{'PLUGIN_FACTORY'}->{$module} ? $self->{'PLUGIN_FACTORY'}->{$module}
        : "${base}::${module}";
        my $require = "$package.pm";
        $require =~ s|::|/|g;

        ### try and load the module - fall back to bare module if allowed
        if ($self->{'PLUGIN_FACTORY'}->{$module} || eval {require $require}) {
            my $shape   = $package->load;
            my $context = $self->context;
            $obj = $shape->new($context, map { $self->play_expr($_) } @args);
        } elsif (lc($module) eq 'iterator') { # use our iterator if none found (TT's works just fine)
            $obj = $self->iterator($args[0]);
        } elsif (my @packages = grep {lc($package) eq lc($_)} @{ $self->list_plugins({base => $base}) }) {
            foreach my $package (@packages) {
                my $require = "$package.pm";
                $require =~ s|::|/|g;
                eval {require $require} || next;
                my $shape   = $package->load;
                my $context = $self->context;
                $obj = $shape->new($context, map { $self->play_expr($_) } @args);
            }
        } elsif ($self->{'LOAD_PERL'}) {
            my $require = "$module.pm";
            $require =~ s|::|/|g;
            if (eval {require $require}) {
                $obj = $module->new(map { $self->play_expr($_) } @args);
            }
        }
    }
    if (! defined $obj) {
        my $err = "$module: plugin not found";
        $self->throw('plugin', $err);
    }

    ### all good
    $self->set_variable(\@var, $obj);

    return;
}

sub play_VIEW {
    my ($self, $ref, $node, $out_ref) = @_;

    my ($blocks, $args, $name) = @$ref;

    ### get args ready
    # [[undef, '{}', 'key1', 'val1', 'key2', 'val2'], 0]
    $args = $args->[0];
    my $hash = {};
    foreach (my $i = 2; $i < @$args; $i+=2) {
        my $key = $args->[$i];
        my $val = $self->play_expr($args->[$i+1]);
        if (ref $key) {
            if (@$key == 2 && ! ref($key->[0]) && ! $key->[1]) {
                $key = $key->[0];
            } else {
                $self->set_variable($key, $val);
                next; # what TT does
            }
        }
        $hash->{$key} = $val;
    }

    ### prepare the blocks
    my $prefix = $hash->{'prefix'} || (ref($name) && @$name == 2 && ! $name->[1] && ! ref($name->[0])) ? "$name->[0]/" : '';
    foreach my $key (keys %$blocks) {
        $blocks->{$key} = {name => "${prefix}${key}", _tree => $blocks->{$key}};
    }
    $hash->{'blocks'} = $blocks;

    ### get the view
    if (! eval { require Template::View }) {
        $self->throw('view', 'Could not load Template::View library');
    }
    my $view = Template::View->new($self->context, $hash)
        || $self->throw('view', $Template::View::ERROR);

    ### 'play it'
    my $old_view = $self->play_expr(['view', 0]);
    $self->set_variable($name, $view);
    $self->set_variable(['view', 0], $view);

    if ($node->[4]) {
        my $out = '';
        $self->play_tree($node->[4], \$out);
        # throw away $out
    }

    $self->set_variable(['view', 0], $old_view);
    $view->seal;

    return;
}

sub play_WHILE {
    my ($self, $var, $node, $out_ref) = @_;
    return if ! defined $var;

    my $sub_tree = $node->[4];

    ### iterate use the iterator object
    my $count = $WHILE_MAX;
    while (--$count > 0) {

        $self->play_expr($var) || last;

        ### execute the sub tree
        eval { $self->play_tree($sub_tree, $out_ref) };
        if (my $err = $@) {
            if (UNIVERSAL::can($err, 'type')) {
                next if $err->type =~ /next/;
                last if $err->type =~ /last|break/;
            }
            die $err;
        }
    }
    die "WHILE loop terminated (> $WHILE_MAX iterations)\n" if ! $count;

    return;
}

sub play_WRAPPER {
    my ($self, $args, $node, $out_ref) = @_;
    my $sub_tree = $node->[4] || return;

    my ($named, @files) = @$args;

    my $out = '';
    $self->play_tree($sub_tree, \$out);

    foreach my $name (reverse @files) {
        local $self->{'_vars'}->{'content'} = $out;
        $out = '';
        $DIRECTIVES->{'INCLUDE'}->[1]->($self, [$named, $name], $node, \$out);
    }

    $$out_ref .= $out;
    return;
}

###----------------------------------------------------------------###

1;
