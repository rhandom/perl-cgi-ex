package CGI::Ex::Template::Extra;

=head1 NAME

CGI::Ex::Template::Extra - load extra and advanced features that aren't as commonly used

=cut

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
            splice @$ref, $i, 2, (); # remove the options
            $self->{$key} = $self->play_expr($val);
            $i -= 2;
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
        $self->execute_tree($node->[4], \$out);
        # throw away $out
    }

    $self->set_variable(['view', 0], $old_view);
    $view->seal;

    return '';
}


1;
