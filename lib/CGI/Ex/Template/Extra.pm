package CGI::Ex::Template::Extra;

=head1 NAME

CGI::Ex::Template::Extra - load extra and advanced features that aren't as commonly used

=cut

use strict;
use warnings;

sub parse_CONFIG {
    my ($self, $str_ref) = @_;

    my %ctime = map {$_ => 1} @CGI::Ex::Template::CONFIG_COMPILETIME;
    my %rtime = map {$_ => 1} @CGI::Ex::Template::CONFIG_RUNTIME;

    my $config = $self->parse_args($str_ref, {named_at_front => 1, is_parened => 1});
    my $ref = $config->[0]->[0];
    for (my $i = 2; $i < @$ref; $i += 2) {
        my $key = $ref->[$i] = uc $ref->[$i];
        my $val = $ref->[$i + 1];
        if ($ctime{$key}) {
            $self->{$key} = $self->play_expr($val);
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

sub play_CONFIG {
    my ($self, $config) = @_;

    my %rtime = map {$_ => 1} @CGI::Ex::Template::CONFIG_RUNTIME;

    ### do runtime config - not many options get these
    my ($named, @the_rest) = @$config;
    $named = $self->play_expr($named);
    @{ $self }{keys %$named} = @{ $named }{keys %$named};

    ### show what current values are
    return join("\n", map { $rtime{$_} ? ("CONFIG $_ = ".(defined($self->{$_}) ? $self->{$_} : 'undef')) : $_ } @the_rest);
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

sub play_DEBUG {
    my ($self, $ref) = @_;
    if ($ref->[0] eq 'on') {
        delete $self->{'_debug_off'};
    } elsif ($ref->[0] eq 'off') {
        $self->{'_debug_off'} = 1;
    } elsif ($ref->[0] eq 'format') {
        $self->{'_debug_format'} = $ref->[1];
    }
}

sub play_DUMP {
    my ($self, $dump, $node) = @_;

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

    return $out;
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

sub play_FILTER {
    my ($self, $ref, $node, $out_ref) = @_;
    my ($name, $filter) = @$ref;

    return '' if ! @$filter;

    $self->{'FILTERS'}->{$name} = $filter if length $name;

    my $sub_tree = $node->[4];

    ### play the block
    my $out = '';
    eval { $self->execute_tree($sub_tree, \$out) };
    die $@ if $@ && ref($@) !~ /Template::Exception$/;

    my $var = [[undef, '~', $out], 0, '|', @$filter]; # make a temporary var out of it


    return $CGI::Ex::Template::DIRECTIVES->{'GET'}->[1]->($self, $var, $node, $out_ref);
}

sub parse_LOOP {
    my ($self, $str_ref, $node) = @_;
    return $self->parse_expr($str_ref)
        || $self->throw('parse', 'Missing variable on LOOP directive', undef, pos($$str_ref));
}

sub play_LOOP {
    my ($self, $ref, $node, $out_ref) = @_;

    my $var = $self->play_expr($ref);
    my $sub_tree = $node->[4];

    my $global = ! $self->{'SYNTAX'} || $self->{'SYNTAX'} ne 'ht' || $self->{'GLOBAL_VARS'};

    my $items = ref($var) eq 'ARRAY' ? $var : ! defined($var) ? [] : [$var];

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
        eval { $self->execute_tree($sub_tree, $out_ref) };
        if (my $err = $@) {
            if (UNIVERSAL::isa($err, $CGI::Ex::Template::PACKAGE_EXCEPTION)) {
                next if $err->type eq 'next';
                last if $err->type =~ /last|break/;
            }
            die $err;
        }
    }

    return undef;
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
        $self_copy->execute_tree($sub_tree, \$out);
        return $out;
    });

    return;
}

sub play_PERL {
    my ($self, $info, $node, $out_ref) = @_;
    $self->throw('perl', 'EVAL_PERL not set') if ! $self->{'EVAL_PERL'};

    ### fill in any variables
    my $perl = $node->[4] || return;
    my $out  = '';
    $self->execute_tree($perl, \$out);
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

sub play_RAWPERL {
    my ($self, $info, $node, $out_ref) = @_;
    $self->throw('perl', 'EVAL_PERL not set') if ! $self->{'EVAL_PERL'};

    ### fill in any variables
    my $tree = $node->[4] || return;
    my $perl  = '';
    $self->execute_tree($tree, \$perl);
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

sub parse_VIEW {
    my ($self, $str_ref) = @_;

    my $ref = $self->parse_args($str_ref, {
        named_at_front       => 1,
        require_arg          => 1,
    });

    return $ref;
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
        $self->execute_tree($node->[4], \$out);
        # throw away $out
    }

    $self->set_variable(['view', 0], $old_view);
    $view->seal;

    return '';
}

###----------------------------------------------------------------###

sub list_plugins {
    my $self = shift;
    my $args = shift || {};
    my $base = $args->{'base'} || '';

    return $self->{'_plugins'}->{$base} ||= do {
        my @plugins;

        $base =~ s|::|/|g;
        my @dirs = grep {-d $_} map {"$_/$base"} @INC;

        foreach my $dir (@dirs) {
            require File::Find;
            File::Find::find(sub {
                my $mod = $base .'/'. ($File::Find::name =~ m|^ $dir / (.*\w) \.pm $|x ? $1 : return);
                $mod =~ s|/|::|g;
                push @plugins, $mod;
            }, $dir);
        }

        \@plugins; # return of the do
    };
}

###----------------------------------------------------------------###

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
    local pos $$str_ref = 0;
    my $allow_expr = ! defined($self->{'EXPR'}) || $self->{'EXPR'}; # default is on

    while (1) {
        ### find the next opening tag
        $$str_ref =~ m{ \G (.*?) $START }gcxs
            || last;
        my ($text, $comment, $is_close, $pre_chomp, $func) = ($1, $2, $3, $4, uc $5);

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

        $$str_ref =~ m{ \G \s+ }gcx;

        ### parse remaining tag details
        if (! $is_close) {
            ### handle HT style nodes
            if ($func =~ /^(IF|ELSIF|UNLESS|LOOP|VAR|INCLUDE)$/) {
                $func = $node->[0] = 'GET' if $func eq 'VAR';

                ### allow for variable escaping (we'll add on a vmethod)
                my $escape = ($$str_ref =~ m{
                    \G [Ee][Ss][Cc][Aa][Pp][Ee] \s*=\s* ([\"\']?)
                        ([Nn][Oo][Nn][Ee] | [Hh][Tt][Mm][Ll] | [Uu][Rr][Ll] | [Jj][Ss] | [01])
                        \1 \s* }gcx) ? lc($2) : '';

                my $is_expr;
                my $quote = '';
                if ($$str_ref =~ m{ \G [Ee][Xx][Pp][Rr] \s*=\s* ([\"\']?) \s* }gcx) {
                    $is_expr = 1;
                    $quote = $1;
                    if (! $allow_expr) {
                        $self->throw('parse', 'EXPR are not allowed without hte mode', undef, pos($$str_ref));
                    }
                } elsif ($$str_ref =~ m{ \G [Nn][Aa][Mm][Ee] \s*=\s* ([\"\']?) \s* }gcx) {
                    $quote = $1;
                }

                ### store what we'll find at the end of the tag
                $self->{'_end_tag'} = $comment ? qr{$quote\s*([+=~-]?)-->} : qr{$quote\s*([+=~-]?)>};

                ### allow for TT style expressions
                if ($is_expr) {
                    $node->[3] = $self->parse_expr($str_ref)
                        || $self->throw('parse', 'Error while looking for EXPR', undef, pos($$str_ref));
                ### or normal HT style Named variables
                } else {
                    $$str_ref =~ m{ \G ([\w./+_]*) }gcx
                        || $self->throw('parse', 'Error while looking for NAME', undef, pos($$str_ref));
                    $node->[3] = $func eq 'INCLUDE' ? $1 : [($self->{'CASE_SENSITIVE'} ? $1 : lc $1), 0]; # set the variable
                    $node->[2] = pos $$str_ref;
                }

                ### dress up node before finishing
                if ($escape) {
                    $self->throw('parse', "ESCAPE not allowed in TMPL_$func tag") if $func ne 'GET';
                    if ($escape eq 'html' || $escape eq '1') {
                        push @{ $node->[3] }, '|', 'html', 0;
                    } elsif ($escape eq 'url') {
                        push @{ $node->[3] }, '|', 'url', 0;
                    } elsif ($escape eq 'js') {
                        push @{ $node->[3] }, '|', 'js', 0;
                    }
                } elsif ($func eq 'INCLUDE') {
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
#            } elsif ($capture) {
#                push @{ $capture->[4] }, $node;
#                undef $capture;
#                # normal nodes

        ### look for the closing tag
        if ($$str_ref =~ m{ \G $self->{'_end_tag'} }gcxs) {
            $post_chomp = $1 || $self->{'POST_CHOMP'};
            $post_chomp =~ y/-=~+/1230/ if $post_chomp;
            $continue = 0;
            $post_op  = 0;
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
    local $self->{'INCLUDE_PATH'} = $self->{'PATH'};
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

package CGI::Ex::Template::Context;

use vars qw($AUTOLOAD);

sub new {
    my $class = shift;
    my $self  = shift || {};
    die "Missing _template" if ! $self->{'_template'};
    return bless $self, $class;
}

sub _template { shift->{'_template'} || die "Missing _template" }

sub template {
    my ($self, $name) = @_;
    return $self->_template->{'BLOCKS'}->{$name} || $self->_template->load_parsed_tree($name);
}

sub config { shift->_template }

sub stash {
    my $self = shift;
    return $self->{'stash'} ||= bless {_template => $self->_template}, 'CGI::Ex::Template::_Stash';
}

sub insert { shift->_template->_insert(@_) }

sub eval_perl { shift->_template->{'EVAL_PERL'} }

sub process {
    my $self = shift;
    my $ref  = shift;
    my $args = shift || {};

    $self->_template->set_variable($_, $args->{$_}) for keys %$args;

    my $out  = '';
    $self->_template->_process($ref, $self->_template->_vars, \$out);
    return $out;
}

sub include {
    my $self = shift;
    my $ref  = shift;
    my $args = shift || {};

    my $t = $self->_template;

    my $swap = $t->{'_vars'};
    local $t->{'_vars'} = {%$swap};

    $t->set_variable($_, $args->{$_}) for keys %$args;

    my $out = ''; # have temp item to allow clear to correctly clear
    eval { $t->_process($ref, $t->_vars, \$out) };
    if (my $err = $@) {
        die $err if ref($err) !~ /Template::Exception$/ || $err->type !~ /return/;
    }

    return $out;
}

sub define_filter {
    my ($self, $name, $filter, $is_dynamic) = @_;
    $filter = [ $filter, 1 ] if $is_dynamic;
    $self->define_vmethod('filter', $name, $filter);
}

sub filter {
    my ($self, $name, $args, $alias) = @_;
    my $t = $self->_template;

    my $filter;
    if (! ref $name) {
        $filter = $t->{'FILTERS'}->{$name} || $CGI::Ex::Template::FILTER_OPS->{$name} || $CGI::Ex::Template::SCALAR_OPS->{$name};
        $t->throw('filter', $name) if ! $filter;
    } elsif (UNIVERSAL::isa($name, 'CODE') || UNIVERSAL::isa($name, 'ARRAY')) {
        $filter = $name;
    } elsif (UNIVERSAL::can($name, 'factory')) {
        $filter = $name->factory || $t->throw($name->error);
    } else {
        $t->throw('undef', "$name: filter not found");
    }

    if (UNIVERSAL::isa($filter, 'ARRAY')) {
        $filter = ($filter->[1]) ? $filter->[0]->($t->context, @$args) : $filter->[0];
    } elsif ($args && @$args) {
        my $sub = $filter;
        $filter = sub { $sub->(shift, @$args) };
    }

    $t->{'FILTERS'}->{$alias} = $filter if $alias;

    return $filter;
}

sub define_vmethod { shift->_template->define_vmethod(@_) }

sub throw {
    my ($self, $type, $info) = @_;

    if (UNIVERSAL::isa($type, $CGI::Ex::Template::PACKAGE_EXCEPTION)) {
	die $type;
    } elsif (defined $info) {
	$self->_template->throw($type, $info);
    } else {
	$self->_template->throw('undef', $type);
    }
}

sub AUTOLOAD { shift->_template->throw('not_implemented', "The method $AUTOLOAD has not been implemented") }

sub DESTROY {}

###----------------------------------------------------------------###

package CGI::Ex::Template::_Stash;

use vars qw($AUTOLOAD);

sub _template { shift->{'_template'} || die "Missing _template" }

sub get {
    my ($self, $var) = @_;
    if (! ref $var) {
        if ($var =~ /^\w+$/) {  $var = [$var, 0] }
        else {                  $var = $self->_template->parse_expr(\$var, {no_dots => 1}) }
    }
    return $self->_template->play_expr($var, {no_dots => 1});
}

sub set {
    my ($self, $var, $val) = @_;
    if (! ref $var) {
        if ($var =~ /^\w+$/) {  $var = [$var, 0] }
        else {                  $var = $self->_template->parse_expr(\$var, {no_dots => 1}) }
    }
    $self->_template->set_variable($var, $val, {no_dots => 1});
    return $val;
}

sub AUTOLOAD { shift->_template->throw('not_implemented', "The method $AUTOLOAD has not been implemented") }

sub DESTROY {}

###----------------------------------------------------------------###

package CGI::Ex::Template::EvalPerlHandle;

sub TIEHANDLE {
    my ($class, $out_ref) = @_;
    return bless [$out_ref], $class;
}

sub PRINT {
    my $self = shift;
    ${ $self->[0] } .= $_ for grep {defined && length} @_;
    return 1;
}

###----------------------------------------------------------------###

1;
