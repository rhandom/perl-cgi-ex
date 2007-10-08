package CGI::Ex::App;

###----------------------------------------------------------------###
#  See the perldoc in CGI/Ex/App.pod
#  Copyright 2007 - Paul Seamons
#  Distributed under the Perl Artistic License without warranty
###----------------------------------------------------------------###

use strict;
use Carp qw(croak);
BEGIN {
    eval { use Time::HiRes qw(time) };
    eval { use Scalar::Util };
}

our $VERSION = '2.19';

sub new {
    my $class = shift || croak "Usage: ".__PACKAGE__."->new";
    my $self  = ref($_[0]) ? shift() : (@_ % 2) ? {} : {@_};
    bless $self, $class;

    $self->init;
    $self->init_from_conf;

    return $self;
}

sub init_from_conf {
    my $self = shift;
    return if ! $self->load_conf;
    my $conf = $self->conf;
    @{ $self }{ keys %$conf } = values %$conf;
    return;
}

###---------------------###

sub navigate {
    my ($self, $args) = @_;
    $self = $self->new($args) if ! ref $self;

    $self->{'_time'} = time;
    eval {
        return $self if ! $self->{'_no_pre_navigate'} && $self->pre_navigate;

        eval {
            local $self->{'_morph_lineage_start_index'} = $#{$self->{'_morph_lineage'} || []};
            $self->nav_loop;
        };
        croak $@ if $@ && $@ ne "Long Jump\n";

        $self->post_navigate if ! $self->{'_no_post_navigate'};
    };
    $self->handle_error($@) if $@ && $@ ne "Long Jump\n"; # catch any errors

    $self->destroy;

    return $self;
}

sub nav_loop {
    my $self = shift;

    ### keep from an infinate nesting
    local $self->{'_recurse'} = $self->{'_recurse'} || 0;
    if ($self->{'_recurse'}++ >= $self->recurse_limit) {
        my $err = "recurse_limit (".$self->recurse_limit.") reached";
        $err .= " number of jumps (".$self->{'jumps'}.")" if ($self->{'jumps'} || 0) > 1;
        croak $err;
    }

    my $path = $self->path;

    ### allow for an early return
    return if $self->pre_loop($path); # a true value means to abort the navigate

    ### iterate on each step of the path
    foreach ($self->{'path_i'} ||= 0;
             $self->{'path_i'} <= $#$path;
             $self->{'path_i'} ++) {
        my $step = $path->[$self->{'path_i'}];
        if ($step !~ /^([^\W0-9]\w*)$/) { # don't process the step if it contains odd characters
            $self->stash->{'forbidden_step'} = $step;
            $self->replace_path($self->forbidden_step);
            next;
        }
        $step = $1; # untaint

        ### allow for per-step authentication
        if (! $self->is_authed) {
            my $req = $self->run_hook('require_auth', $step, 1);
            if (ref($req) ? $req->{$step} : $req) { # in the hash - or true
                return if ! $self->run_hook('get_valid_auth', $step);
            }
        }

        ### allow for becoming another package (allows for some steps in external files)
        $self->morph($step);

        ### allow for mapping path_info pieces to form elements
        if (my $info = $self->path_info) {
            my $maps = $self->run_hook('path_info_map', $step) || [];
            croak 'Usage: sub path_info_map { [] }' if ! UNIVERSAL::isa($maps, 'ARRAY');
            foreach my $map (@$maps) {
                croak 'Usage: sub path_info_map { [[qr{/path_info/(\w+)}, "keyname"]] }' if ! UNIVERSAL::isa($map, 'ARRAY');
                my @match = $info =~ $map->[0];
                next if ! @match;
                $self->form->{$map->[$_]} = $match[$_ - 1] foreach grep {! defined $self->form->{$map->[$_]}} 1 .. $#$map;
                last;
            }
        }

        ### run the guts of the step
        my $handled = $self->run_hook('run_step', $step);

        ### Allow for the run_step to intercept.
        ### A true status means the run_step took over navigation.
        if ($handled) {
            $self->unmorph($step);
            return;
        }

        ### if there are no future steps - allow for this step to designate one to follow
        my $is_at_end = $self->{'path_i'} >= $#$path ? 1 : 0;
        $self->run_hook('refine_path', $step, $is_at_end);

        $self->unmorph($step);
    }

    ### allow for one exit point after the loop
    return if $self->post_loop($path); # a true value means to abort the navigate

    ### run the default step as a last resort
    $self->insert_path($self->default_step);
    $self->nav_loop; # go recursive

    return;
}

sub path {
    my $self = shift;
    if (! $self->{'path'}) {
        my $path = $self->{'path'} = []; # empty path

        ### add initial items to the form hash from path_info5B
        if (my $info = $self->path_info) {
            my $maps = $self->path_info_map_base || [];
            croak 'Usage: sub path_info_map_base { [] }' if ! UNIVERSAL::isa($maps, 'ARRAY');
            foreach my $map (@$maps) {
                croak 'Usage: sub path_info_map_base { [[qr{/path_info/(\w+)}, "keyname"]] }' if ! UNIVERSAL::isa($map, 'ARRAY');
                my @match = $info =~ $map->[0];
                next if ! @match;
                $self->form->{$map->[$_]} = $match[$_ - 1] foreach grep {! defined $self->form->{$map->[$_]}} 1 .. $#$map;
                last;
            }
        }

        ### make sure the step is valid
        my $step = $self->form->{$self->step_key};
        if (defined $step) {
            if ($step =~ /^_/) {         # can't begin with _
                $self->stash->{'forbidden_step'} = $step;
                push @$path, $self->forbidden_step;
            } elsif ($self->valid_steps  # must be in valid_steps if defined
                && ! $self->valid_steps->{$step}
                && $step ne $self->default_step
                && $step ne $self->js_step) {
                $self->stash->{'forbidden_step'} = $step;
                push @$path, $self->forbidden_step;
            } else {
                push @$path, $step;
            }
        }
    }

    return $self->{'path'};
}

sub run_hook {
    my $self = shift;
    my $hook = shift;
    my $step = shift;
    my ($code, $found) = @{ $self->find_hook($hook, $step) };
    croak "Could not find a method named ${step}_${hook} or ${hook}" if ! $code;
    croak "Value for $hook ($found) is not a code ref ($code)" if ! UNIVERSAL::isa($code, 'CODE');

    my $hist;
    if (! $self->{'no_history'}) {
        $hist = {
            step  => $step,
            meth  => $hook,
            found => $found,
            time  => time,
        };
        push @{ $self->history }, $hist;
        $hist->{'level'} = $self->{'_level'};
        $hist->{'elapsed'}  = time - $hist->{'time'};
    }
    local $self->{'_level'} = 1 + ($self->{'_level'} || 0);

    my $resp = $self->$code($step, @_);

    if (! $self->{'no_history'}) {
        $hist->{'elapsed'} = time - $hist->{'time'};
        $hist->{'response'} = $resp;
    }

    return $resp;
}

sub run_step {
    my $self = shift;
    my $step = shift;

    ### if the pre_step exists and returns true, exit the nav_loop
    return 1 if $self->run_hook('pre_step', $step);

    ### allow for skipping this step (but stay in the nav_loop)
    return 0 if $self->run_hook('skip', $step);

    ### see if we have complete valid information for this step
    ### if so, do the next step
    ### if not, get necessary info and print it out
    if (   ! $self->run_hook('prepare', $step)
        || ! $self->run_hook('info_complete', $step)
        || ! $self->run_hook('finalize', $step)) {

        ### show the page requesting the information
        $self->run_hook('prepared_print', $step);

        ### a hook after the printing process
        $self->run_hook('post_print', $step);

        return 1;
    }

    ### a hook before end of loop
    ### if the post_step exists and returns true, exit the nav_loop
    return 1 if $self->run_hook('post_step', $step);

    ### let the nav_loop continue searching the path
    return 0;
}

sub prepared_print {
    my $self = shift;
    my $step = shift;

    my $hash_base = $self->run_hook('hash_base',   $step) || {};
    my $hash_comm = $self->run_hook('hash_common', $step) || {};
    my $hash_form = $self->run_hook('hash_form',   $step) || {};
    my $hash_fill = $self->run_hook('hash_fill',   $step) || {};
    my $hash_swap = $self->run_hook('hash_swap',   $step) || {};
    my $hash_errs = $self->run_hook('hash_errors', $step) || {};

    $hash_errs->{$_} = $self->format_error($hash_errs->{$_}) foreach keys %$hash_errs;
    $hash_errs->{'has_errors'} = 1 if scalar keys %$hash_errs;

    my $fill = {%$hash_form, %$hash_base, %$hash_comm, %$hash_fill};
    my $swap = {%$hash_form, %$hash_base, %$hash_comm, %$hash_swap, %$hash_errs};

    $self->run_hook('print', $step, $swap, $fill);
}

sub print {
    my ($self, $step, $swap, $fill) = @_;
    my $file = $self->run_hook('file_print', $step); # get a filename relative to template_path
    my $out  = $self->run_hook('swap_template', $step, $file, $swap);
    $self->run_hook('fill_template', $step, \$out, $fill);
    $self->run_hook('print_out',     $step, \$out);
}

sub handle_error {
    my ($self, $err) = @_;

    die $err if $self->{'_handling_error'};
    local $self->{'_handling_error'} = 1;
    local $self->{'_recurse'} = 0; # allow for this next step - even if we hit a recurse error

    $self->stash->{'error_step'} = $self->current_step;
    $self->stash->{'error'}      = $err;
    $self->replace_path($self->error_step);

    eval { $self->jump };
    die $@ if $@ && $@ ne "Long Jump\n";
}

###---------------------###
# read only accessors

sub allow_morph          { $_[0]->{'allow_morph'} }
sub allow_nested_morph   { $_[0]->{'allow_nested_morph'} }
sub auth_args            { $_[0]->{'auth_args'} }
sub charset              { $_[0]->{'charset'}        ||  '' }
sub conf_args            { $_[0]->{'conf_args'} }
sub conf_die_on_fail     { $_[0]->{'conf_die_on_fail'} || ! defined $_[0]->{'conf_die_on_fail'} }
sub conf_path            { $_[0]->{'conf_path'}      ||  $_[0]->base_dir_abs }
sub conf_validation      { $_[0]->{'conf_validation'} }
sub default_step         { $_[0]->{'default_step'}   || 'main'        }
sub error_step           { $_[0]->{'error_step'}     || '__error'     }
sub fill_args            { $_[0]->{'fill_args'} }
sub forbidden_step       { $_[0]->{'forbidden_step'} || '__forbidden' }
sub form_name            { $_[0]->{'form_name'}      || 'theform'     }
sub history              { $_[0]->{'history'}        ||= []           }
sub js_step              { $_[0]->{'js_step'}        || 'js'          }
sub login_step           { $_[0]->{'login_step'}     || '__login'     }
sub mimetype             { $_[0]->{'mimetype'}       ||  'text/html'  }
sub path_info            { $_[0]->{'path_info'}      ||  $ENV{'PATH_INFO'}   || '' }
sub path_info_map_base   { $_[0]->{'path_info_map_base'} ||[[qr{/(\w+)}, $_[0]->step_key]] }
sub recurse_limit        { $_[0]->{'recurse_limit'}  ||  15                   }
sub script_name          { $_[0]->{'script_name'}    ||  $ENV{'SCRIPT_NAME'} || $0 }
sub stash                { $_[0]->{'stash'}          ||= {}    }
sub step_key             { $_[0]->{'step_key'}       || 'step' }
sub template_args        { $_[0]->{'template_args'} }
sub template_path        { $_[0]->{'template_path'}  ||  $_[0]->base_dir_abs  }
sub val_args             { $_[0]->{'val_args'} }
sub val_path             { $_[0]->{'val_path'}       ||  $_[0]->template_path }

sub conf_obj {
    my $self = shift;
    return $self->{'conf_obj'} || do {
        my $args = $self->conf_args || {};
        $args->{'paths'}     ||= $self->conf_path;
        $args->{'directive'} ||= 'MERGE';
        require CGI::Ex::Conf;
        CGI::Ex::Conf->new($args);
    };
}

sub template_obj {
    my ($self, $args) = @_;
    return $self->{'template_obj'} || do {
        require Template::Alloy;
        Template::Alloy->new($args);
    };
}

sub auth_obj {
    my ($self, $args) = @_;
    return $self->{'auth_obj'} || do {
        require CGI::Ex::Auth;
        CGI::Ex::Auth->new($args);
    };
}

sub val_obj {
    my $self = shift;
    return $self->{'val_obj'} || do {
        my $args = $self->val_args || {};
        $args->{'cgix'} ||= $self->cgix;
        require CGI::Ex::Validate;
        CGI::Ex::Validate->new($args);
    };
}

###---------------------###
# read/write accessors

sub auth_data    { (@_ == 2) ? $_[0]->{'auth_data'}    = pop : $_[0]->{'auth_data'}              }
sub base_dir_abs { (@_ == 2) ? $_[0]->{'base_dir_abs'} = pop : $_[0]->{'base_dir_abs'} || ['.']  }
sub base_dir_rel { (@_ == 2) ? $_[0]->{'base_dir_rel'} = pop : $_[0]->{'base_dir_rel'} || ''     }
sub cgix         { (@_ == 2) ? $_[0]->{'cgix'}         = pop : $_[0]->{'cgix'}         ||= do { require CGI::Ex; CGI::Ex->new } }
sub cookies      { (@_ == 2) ? $_[0]->{'cookies'}      = pop : $_[0]->{'cookies'}      ||= $_[0]->cgix->get_cookies }
sub ext_conf     { (@_ == 2) ? $_[0]->{'ext_conf'}     = pop : $_[0]->{'ext_conf'}     || 'pl'   }
sub ext_print    { (@_ == 2) ? $_[0]->{'ext_print'}    = pop : $_[0]->{'ext_print'}    || 'html' }
sub ext_val      { (@_ == 2) ? $_[0]->{'ext_val'}      = pop : $_[0]->{'ext_val'}      || 'val'  }
sub form         { (@_ == 2) ? $_[0]->{'form'}         = pop : $_[0]->{'form'}         ||= $_[0]->cgix->get_form    }
sub load_conf    { (@_ == 2) ? $_[0]->{'load_conf'}    = pop : $_[0]->{'load_conf'}              }

sub conf {
    my $self = shift;
    $self->{'conf'} = pop if @_ == 1;
    return $self->{'conf'} ||= do {
        my $conf = $self->conf_file;
        if (! ref $conf) {
            $conf = $self->conf_obj->read($conf, {no_warn_on_fail => 1}) || ($self->conf_die_on_fail ? croak $@ : {});
        }
        my $hash = $self->conf_validation;
        if ($hash && scalar keys %$hash) {
            my $err_obj = $self->val_obj->validate($conf, $hash);
            die $err_obj if $err_obj;
        }
        $conf;
    }
}

sub conf_file {
    my $self = shift;
    $self->{'conf_file'} = pop if @_ == 1;
    return $self->{'conf_file'} ||= do {
        my $module = $self->name_module || croak 'Missing name_module during conf_file call';
        $module .'.'. $self->ext_conf;
    };
}

###---------------------###
# general methods

sub add_to_base          { my $self = shift; $self->add_to_hash($self->hash_base,   @_) }
sub add_to_common        { my $self = shift; $self->add_to_hash($self->hash_common, @_) }
sub add_to_errors        { shift->add_errors(@_) }
sub add_to_fill          { my $self = shift; $self->add_to_hash($self->hash_fill,   @_) }
sub add_to_form          { my $self = shift; $self->add_to_hash($self->hash_form,   @_) }
sub add_to_path          { shift->append_path(@_) } # legacy
sub add_to_swap          { my $self = shift; $self->add_to_hash($self->hash_swap,   @_) }
sub cleanup_user         { my ($self, $user) = @_; $user }
sub current_step         { $_[0]->step_by_path_index($_[0]->{'path_i'} || 0) }
sub destroy              {}
sub first_step           { $_[0]->step_by_path_index(0) }
sub fixup_after_morph    {}
sub fixup_before_unmorph {}
sub format_error         { my ($self, $error) = @_; $error }
sub get_pass_by_user     { croak "get_pass_by_user is a virtual method and needs to be overridden for authentication to work" }
sub has_errors           { scalar keys %{ $_[0]->hash_errors } }
sub init                 {}
sub last_step            { $_[0]->step_by_path_index($#{ $_[0]->path }) }
sub path_info_map        {}
sub post_loop            { 0 } # true value means to abort the nav_loop - don't recurse
sub post_navigate        {}
sub pre_loop             { 0 } # true value means to abort the nav_loop routine
sub pre_navigate         { 0 } # true means to not enter nav_loop
sub previous_step        { $_[0]->step_by_path_index(($_[0]->{'path_i'} || 0) - 1) }
sub valid_steps          {}
sub verify_user          { 1 }

sub add_errors {
    my $self = shift;
    my $hash = $self->hash_errors;
    my $args = ref($_[0]) ? shift : {@_};
    foreach my $key (keys %$args) {
        my $_key = ($key =~ /error$/) ? $key : "${key}_error";
        if ($hash->{$_key}) {
            $hash->{$_key} .= '<br>' . $args->{$key};
        } else {
            $hash->{$_key} = $args->{$key};
        }
    }
    $hash->{'has_errors'} = 1;
}

sub add_to_hash {
    my $self = shift;
    my $old  = shift;
    my $new  = shift;
    $new = {$new, @_} if ! ref $new; # non-hashref
    $old->{$_} = $new->{$_} foreach keys %$new;
}

sub append_path { my $self = shift; push @{ $self->path }, @_ }

sub clear_app {
    my $self = shift;
    delete @{ $self }{qw(cgix cookies form hash_common hash_errors hash_fill hash_swap history
                         _morph_lineage _morph_lineage_start_index path path_i stash val_obj)};
    return $self;
}

sub dump_history {
    my ($self, $all) = @_;

    my $hist = $self->history;
    my $dump = [sprintf "Elapsed: %.5f", time - $self->{'_time'}];

    foreach my $row (@$hist) {
        if (! ref($row) || ref($row) ne 'HASH' || ! exists $row->{'elapsed'}) {
            push @$dump, $row;
            next;
        }
        my $note = ('    ' x ($row->{'level'} || 0))
            . join(' - ', $row->{'step'}, $row->{'meth'}, $row->{'found'}, sprintf('%.5f', $row->{'elapsed'}));
        my $resp = $row->{'response'};
        if (ref($resp) eq 'HASH' && ! scalar keys %$resp) {
            $note .= ' - {}';
        } elsif (ref($resp) eq 'ARRAY' && ! @$resp) {
            $note .= ' - []';
        } elsif (! defined $resp) {
            $note .= ' - undef';
        } elsif (! ref $resp || ! $all) {
            my $max = $self->{'history_max'} || 30;
            if (length($resp) > $max) {
                $resp = substr($resp, 0, $max);
                $resp =~ s/\n.+//s;
                $resp = "$resp ...";
            }
            $note .= " - $resp";
        } else {
            $note = [$note, $resp];
        }
        push @$dump, $note;
    }

    return $dump;
}

sub exit_nav_loop {
    my $self = shift;

    ### undo morphs
    if (my $ref = $self->{'_morph_lineage'}) {
        ### use the saved index - this allows for early "morphers" to only get rolled back so far
        my $index = $self->{'_morph_lineage_start_index'};
        $index = -1 if ! defined $index;
        $self->unmorph while $#$ref != $index;
    }

    die "Long Jump\n";
}

sub find_hook {
    my ($self, $hook, $step) = @_;
    croak "Missing hook name" if ! $hook;
    if ($step && (my $code = $self->can("${step}_${hook}"))) {
        return [$code, "${step}_${hook}"],
    } elsif ($code = $self->can($hook)) {
        return [$code, $hook];
    } else {
        return [];
    }
}

sub insert_path {
    my $self = shift;
    my $ref  = $self->path;
    my $i    = $self->{'path_i'} || 0;
    if ($i + 1 > $#$ref) {
        push @$ref, @_;
    } else {
        splice(@$ref, $i + 1, 0, @_); # insert a path at the current location
    }
}

sub jump {
    my $self   = shift;
    my $i      = @_ == 1 ? shift : 1;
    my $path   = $self->path;
    my $path_i = $self->{'path_i'};
    croak "Can't jump if nav_loop not started" if ! defined $path_i;

    if ($i =~ /^\w+$/) {
        if (   $i eq 'FIRST'   ) { $i = - $path_i - 1 }
        elsif ($i eq 'LAST'    ) { $i = $#$path - $path_i }
        elsif ($i eq 'NEXT'    ) { $i = 1  }
        elsif ($i eq 'CURRENT' ) { $i = 0  }
        elsif ($i eq 'PREVIOUS') { $i = -1 }
        else { # look for a step by that name
            for (my $j = $#$path; $j >= 0; $j --) {
                if ($path->[$j] eq $i) {
                    $i = $j - $path_i;
                    last;
                }
            }
        }
    }
    croak "Invalid jump index ($i)" if $i !~ /^-?\d+$/;

    ### manipulate the path to contain the new jump location
    my $cut_i   = $path_i + $i;
    my @replace = ($cut_i > $#$path) ? $self->default_step
                : ($cut_i < 0)       ? @$path
                :                      @$path[$cut_i .. $#$path];
    $self->replace_path(@replace);

    $self->{'jumps'} = ($self->{'jumps'} || 0) + 1;

    $self->{'path_i'}++; # move along now that the path is updated
    $self->nav_loop;     # recurse on the path
    $self->exit_nav_loop;
}

sub js_uri_path {
    my $self   = shift;
    my $script = $self->script_name;
    my $js_step = $self->js_step;
    return ($self->can('path') == \&CGI::Ex::App::path)
        ? $script .'/'. $js_step # try to use a cache friendly URI (if path is our own)
        : $script . '?'.$self->step_key.'='.$js_step.'&js='; # use one that works with more paths
}


sub morph {
    my $self  = shift;
    my $step  = shift || return;
    my $allow = $self->run_hook('allow_morph', $step) || return;
    my $lin   = $self->{'_morph_lineage'} ||= [];
    my $cur   = ref $self; # what are we currently
    push @$lin, $cur;     # store so subsequent unmorph calls can do the right thing

    my $hist = {step => $step, meth => 'morph', found => 'morph', time => time, elapsed => 0, response => 0};
    push @{ $self->history }, $hist if ! $self->{'no_history'};

    if (ref($allow) && ! $allow->{$step}) { # hash - but no step - record for unbless
        $hist->{'found'} .= " (not allowed to morph to that step)";
        return 0;
    }

    ### make sure we haven't already been reblessed
    if ($#$lin != 0                                       # is this the second morph call
        && (! ($allow = $self->allow_nested_morph($step)) # not true
            || (ref($allow) && ! $allow->{$step})         # hash - but no step
            )) {
        $hist->{'found'} .= $allow ? " (not allowed to nested_morph to that step)" : " (nested_morph disabled)";
        return 0; # just return - don't die so that we can morph early
    }

    ### if we are not already that package - bless us there
    my $new = $self->run_hook('morph_package', $step);
    if ($cur ne $new) {
        (my $file = "$new.pm") =~ s|::|/|g;
        if (UNIVERSAL::can($new, 'can')  # check if the package space exists
            || eval { require $file }) { # check for a file that holds this package
            bless $self, $new;           # become that package
            $hist->{'found'} .= " (changed $cur to $new)";
            $self->fixup_after_morph($step);
        } elsif ($@) {
            if ($@ =~ /^\s*(Can\'t locate \S+ in \@INC)/) { # let us know what happened
                $hist->{'found'} .= " (failed from $cur to $new: $1)";
            } else {
                $hist->{'found'} .= " (failed from $cur to $new: $@)";
                my $err = "Trouble while morphing to $file: $@";
                warn $err;
            }
        }
    }

    $hist->{'response'} = 1;
    return 1;
}

sub replace_path {
    my $self = shift;
    my $ref  = $self->path;
    my $i    = $self->{'path_i'} || 0;
    if ($i + 1 > $#$ref) {
        push @$ref, @_;
    } else {
        splice(@$ref, $i + 1, $#$ref - $i, @_); # replace remaining entries
    }
}

sub set_path {
    my $self = shift;
    my $path = $self->{'path'} ||= [];
    croak "Cannot call set_path after the navigation loop has begun" if $self->{'path_i'};
    splice @$path, 0, $#$path + 1, @_; # change entries in the ref (which updates other copies of the ref)
}

sub step_by_path_index {
    my $self = shift;
    my $i    = shift || 0;
    my $ref  = $self->path;
    return '' if $i < 0;
#    return $self->default_step if $i > $#$ref;
    return $ref->[$i];
}

sub unmorph {
    my $self = shift;
    my $step = shift || '_no_step';
    my $lin  = $self->{'_morph_lineage'} || return;
    my $cur  = ref $self;

    my $prev = pop(@$lin) || croak "unmorph called more times than morph - current ($cur)";
    delete $self->{'_morph_lineage'} if ! @$lin;

    my $hist = {step => $step, meth => 'unmorph', found => 'unmorph', time => time, elapsed => 0, response => 0};
    push @{ $self->history }, $hist if ! $self->{'no_history'};

    if ($cur ne $prev) {
        $self->fixup_before_unmorph($step);
        bless $self, $prev;
        $hist->{'found'} .= " (changed from $cur to $prev)";
    } else {
        $hist->{'found'} .= " (already isa $cur)";
    }

    $hist->{'response'} = 1;
    return $self;
}

###---------------------###
# hooks

sub file_print {
    my ($self, $step) = @_;

    my $base_dir = $self->base_dir_rel;
    my $module   = $self->run_hook('name_module', $step);
    my $_step    = $self->run_hook('name_step', $step) || croak "Missing name_step";
    $_step .= '.'. $self->ext_print if $_step !~ /\.\w+$/;

    foreach ($base_dir, $module) { $_ .= '/' if length($_) && ! m|/$| }

    return $base_dir . $module . $_step;
}

sub file_val {
    my ($self, $step) = @_;

    ### determine the path to begin looking for files - allow for an arrayref
    my $abs = $self->val_path || [];
    $abs = $abs->() if UNIVERSAL::isa($abs, 'CODE');
    $abs = [$abs] if ! UNIVERSAL::isa($abs, 'ARRAY');
    return {} if @$abs == 0;

    my $base_dir = $self->base_dir_rel;
    my $module   = $self->run_hook('name_module', $step);
    my $_step    = $self->run_hook('name_step', $step) || croak "Missing name_step";
    $_step =~ s/\.\w+$//;
    $_step .= '.'. $self->ext_val;

    foreach (@$abs, $base_dir, $module) { $_ .= '/' if length($_) && ! m|/$| }

    if (@$abs > 1) {
        foreach my $_abs (@$abs) {
            my $path = $_abs . $base_dir . $module . $_step;
            return $path if -e $path;
        }
    }

    return $abs->[0] . $base_dir . $module . $_step;
}

sub fill_template {
    my ($self, $step, $outref, $fill) = @_;
    return if ! $fill || ! scalar keys %$fill;

    my $args = $self->run_hook('fill_args', $step) || {};
    local $args->{'text'} = $outref;
    local $args->{'form'} = $fill;

    require CGI::Ex::Fill;
    CGI::Ex::Fill::fill($args);
}

sub finalize  { 1 } # failure means show step

sub hash_base {
    my ($self, $step) = @_;

    return $self->{'hash_base'} ||= do {
        ### create a weak copy of self to use in closures
        my $copy = $self;
        eval {require Scalar::Util; Scalar::Util::weaken($copy)};
        my $hash = {
            script_name     => $self->script_name,
            path_info       => $self->path_info,
            js_validation   => sub { $copy->run_hook('js_validation', $step, shift) },
            form_name       => $self->run_hook('form_name', $step),
            $self->step_key => $step,
        }; # return of the do
    };
}

sub hash_common { $_[0]->{'hash_common'} ||= {} }
sub hash_errors { $_[0]->{'hash_errors'} ||= {} }
sub hash_fill   { $_[0]->{'hash_fill'}   ||= {} }
sub hash_form   { $_[0]->form }
sub hash_swap   { $_[0]->{'hash_swap'}   ||= {} }

sub hash_validation {
  my ($self, $step) = @_;
  return $self->{'hash_validation'}->{$step} ||= do {
      my $file = $self->run_hook('file_val', $step);
      my $hash = $file ? $self->val_obj->get_validation($file) : {}; # if the file is not found, errors will be in the webserver logs (all else dies)
      $hash; # return of the do
  };
}

sub info_complete {
    my ($self, $step) = @_;
    return 0 if ! $self->run_hook('ready_validate', $step);
    return 0 if ! $self->run_hook('validate', $step, $self->form);
    return 1;
}

sub js_validation {
    my ($self, $step) = @_;
    return '' if $self->ext_val =~ /^html?$/; # let htm validation do it itself

    my $form_name = $_[2] || $self->run_hook('form_name', $step);
    my $hash_val  = $_[3] || $self->run_hook('hash_validation', $step);
    my $js_uri    = $self->js_uri_path;
    return '' if ! $form_name || ! ref($hash_val) || ! scalar keys %$hash_val;

    return $self->val_obj->generate_js($hash_val, $form_name, $js_uri);
}

sub morph_package {
    my ($self, $step) = @_;
    my $cur = ref $self; # default to using self as the base for morphed modules
    my $new = $cur .'::'. ($step || croak "Missing step");
    $new =~ s/(\b|_+)(\w)/\u$2/g; # turn Foo::my_step_name into Foo::MyStepName
    return $new;
}

sub name_module {
    my ($self, $step) = @_;
    return $self->{'name_module'} ||= ($self->script_name =~ m/ (\w+) (?:\.\w+)? $/x)
        ? $1 # allow for cgi-bin/foo or cgi-bin/foo.pl to resolve to "foo"
        : die "Couldn't determine module name from \"name_module\" lookup (".($step||'').")";
}

sub name_step  { my ($self, $step) = @_; $step }
sub next_step  { $_[0]->step_by_path_index(($_[0]->{'path_i'} || 0) + 1) }
sub post_print { 0 }
sub post_step  { 0 } # success indicates we handled step (don't continue step or loop)
sub pre_step   { 0 } # success indicates we handled step (don't continue step or loop)
sub prepare    { 1 } # failure means show step

sub print_out {
    my ($self, $step, $out) = @_;

    $self->cgix->print_content_type($self->mimetype($step), $self->charset($step));
    print ref($out) eq 'SCALAR' ? $$out : $out;
}

sub ready_validate { ($ENV{'REQUEST_METHOD'} && $ENV{'REQUEST_METHOD'} eq 'POST') ? 1 : 0 }

sub refine_path {
    my ($self, $step, $is_at_end) = @_;
    return 0 if ! $is_at_end; # if we aren't at the end of the path, don't do anything

    my $next_step = $self->run_hook('next_step', $step) || return 0;
    $self->run_hook('set_ready_validate', $step, 0);
    $self->append_path($next_step);
    return 1;
}

sub set_ready_validate { # hook and method
    my $self = shift;
    my ($step, $is_ready) = (@_ == 2) ? @_ : (undef, shift);
    $ENV{'REQUEST_METHOD'} = ($is_ready) ? 'POST' : 'GET';
    return $is_ready;
}

sub skip { 0 } # success indicates to skip the step (and continue loop)

sub swap_template {
    my ($self, $step, $file, $swap) = @_;

    my $args = $self->run_hook('template_args', $step) || {};
    $args->{'INCLUDE_PATH'} ||= $args->{'include_path'} || $self->template_path;

    my $t = $self->template_obj($args);
    my $out = '';
    $t->process($file, $swap, \$out) || die $t->error;
    return $out;
}

sub validate {
    my ($self, $step, $form) = @_;

    my $hash = $self->run_hook('hash_validation', $step);
    my $what_was_validated = [];

    return 1 if ! ref($hash) || ! scalar keys %$hash;
    my $err_obj = eval { $self->val_obj->validate($form, $hash, $what_was_validated) };
    die "Step $step: $@" if $@ && ! $err_obj;

    ### had an error - store the errors and return false
    if ($err_obj) {
        $self->add_errors($err_obj->as_hash({
            as_hash_join   => "<br>\n",
            as_hash_suffix => '_error',
        }));
        return 0;
    }

    ### allow for the validation to give us some redirection
    foreach my $ref (@$what_was_validated) {
        foreach my $method (qw(append_path replace_path insert_path)) {
            next if ! (my $val = $ref->{$method});
            $self->$method(ref $val ? @$val : $val);
        }
    }

    return 1;
}

###---------------------###
# authentication

sub navigate_authenticated {
    my ($self, $args) = @_;
    $self = $self->new($args) if ! ref $self;

    croak "The default navigate_authenticated method was called but the default require_auth method has been overwritten - aborting"
        if $self->can('require_auth') != \&CGI::Ex::App::require_auth;

    $self->require_auth(1);

    return $self->navigate;
}

sub require_auth {
    my $self = shift;
    $self->{'require_auth'} = shift if @_ == 1 && (! defined($_[0]) || ref($_[0]) || $_[0] =~ /^[01]$/);
    return $self->{'require_auth'} || 0;
}

sub is_authed {
    my $data = shift->auth_data;
    return $data && ! $data->{'error'};
}

sub check_valid_auth {
    return shift->_do_auth({
        login_print     => sub {}, # check only - don't login if not
        location_bounce => sub {}, # call get_valid_auth - but don't bounce to other locations
    });
}

sub get_valid_auth {
    my $self = shift;

    return $self->_do_auth({
        login_print => sub { # use CGI::Ex::Auth - but use our formatting and printing
            my ($auth, $template, $hash) = @_;
            my $step = $self->login_step;
            my $hash_base = $self->run_hook('hash_base',   $step) || {};
            my $hash_comm = $self->run_hook('hash_common', $step) || {};
            my $hash_swap = $self->run_hook('hash_swap',   $step) || {};
            my $swap = {%$hash_base, %$hash_comm, %$hash_swap, %$hash};

            my $out = $self->run_hook('swap_template', $step, $template, $swap);
            $self->run_hook('fill_template', $step, \$out, $hash);
            $self->run_hook('print_out', $step, \$out);
        }
    });
}

sub _do_auth {
    my ($self, $extra) = @_;
    return $self->auth_data if $self->is_authed;

    my $args = { %{ $self->auth_args || {} }, %{ $extra || {} } };
    $args->{'script_name'}      ||= $self->script_name;
    $args->{'path_info'}        ||= $self->path_info;
    $args->{'cgix'}             ||= $self->cgix;
    $args->{'form'}             ||= $self->form;
    $args->{'cookies'}          ||= $self->cookies;
    $args->{'js_uri_path'}      ||= $self->js_uri_path;
    $args->{'get_pass_by_user'} ||= sub { my ($auth, $user) = @_; $self->get_pass_by_user($user, $auth) };
    $args->{'verify_user'}      ||= sub { my ($auth, $user) = @_; $self->verify_user(     $user, $auth) };
    $args->{'cleanup_user'}     ||= sub { my ($auth, $user) = @_; $self->cleanup_user(    $user, $auth) };

    my $obj  = $self->auth_obj($args);
    my $resp = $obj->get_valid_auth;

    my $data = $obj->last_auth_data;
    delete $data->{'real_pass'} if defined $data; # data may be defined but false
    $self->auth_data($data); # failed authentication may still have auth_data

    return ($resp && $data) ? $data : undef;
}

###---------------------###
# default steps

### A simple step that allows for printing javascript libraries that are stored in perls @INC.
### Which ever step is in js_step should do something similar for js validation to work.
sub js_run_step {
    my $self = shift;

    ### make sure path info looks like /js/CGI/Ex/foo.js
    my $file = $self->form->{'js'} || $self->path_info;
    $file = ($file =~  m!^(?:/js/|/)?(\w+(?:/\w+)*\.js)$!) ? $1 : '';

    $self->cgix->print_js($file);
    $self->{'_no_post_navigate'} = 1;
    return 1;
}

### A step that will be used the path method determines it is forbidden
sub __forbidden_info_complete { 0 }
sub __forbidden_hash_swap  { shift->stash }
sub __forbidden_file_print { \ "<h1>Denied</h1>You do not have access to the step <b>\"[% forbidden_step %]\"</b>" }

### A step that is used by the default handle_error
sub __error_info_complete { 0 }
sub __error_hash_swap  { shift->stash }
sub __error_file_print { \ "<h1>A fatal error occurred</h1>Step: <b>\"[% error_step %]\"</b><br>[% TRY; CONFIG DUMP => {header => 0}; DUMP error; END %]" }

1;

### See the perldoc in CGI/Ex/App.pod
