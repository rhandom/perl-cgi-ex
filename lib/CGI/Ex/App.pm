package CGI::Ex::App;

### CGI Extended Application

###----------------------------------------------------------------###
#  Copyright 2004 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom


use strict;
use vars qw($VERSION
            $EXT_PRINT $EXT_VAL $BASE_DIR_REL $BASE_DIR_ABS $BASE_NAME_MODULE
            $RECURSE_LIMIT
            %CLEANUP_EXCLUDE);

$VERSION = '1.14';

BEGIN {
  ### Default file locations
  ### these are used for the provided stub functions - if you are not
  ### using the stub functions - then you won't need to worry about these
  $EXT_PRINT ||= 'html';
  $EXT_VAL   ||= 'val';
  $BASE_DIR_REL ||= ''; # relative path - stub methods will look in $BASE_DIR_REL/dir/of/content.html
  $BASE_DIR_ABS ||= ''; # content should be found at "$BASE_DIR_ABS/$BASE_DIR_REL/dir/of/content.html"
  $BASE_NAME_MODULE ||= ''; # the cgi name

  ### list of modules to exclude during cleanup
  ### this takes care of situations such as
  ### template toolkits rules area which contains
  ### a nested structure of rules and sub references.
  $CLEANUP_EXCLUDE{'Template::Parser'} = 1;
}


###----------------------------------------------------------------###

sub new {
  my $class = shift || die "Usage: Package->new";
  my $self  = ref($_[0]) ? shift : {@_};
  bless $self, $class;
  $self->init;
  return $self;
}

sub init {}

###----------------------------------------------------------------###

sub navigate {
  my $self = shift;
  my $args = ref($_[0]) ? shift : {@_};
  $self = $self->new($args) if ! ref $self;

  eval {

    ### a chance to do things at the very beginning
    return $self if $self->pre_navigate;

    ### run the step loop
    eval {
      local $self->{'__morph_lineage_start_index'} = $#{$self->{'__morph_lineage'} || []};
      $self->nav_loop;
    };
    if ($@) {
      ### rethrow the error unless we long jumped out of recursive nav_loop calls
      die $@ if $@ ne "Long Jump\n";
    }

    ### one chance to do things at the very end
    $self->post_navigate;

  };

  ### catch errors - if any
  if ($@) {
    $self->handle_error($@);
  }

  return $self;
}

sub nav_loop {
  my $self = shift;

  ### keep from an infinate nesting
  local $self->{recurse} = $self->{recurse} || 0;
  if ($self->{recurse} ++ >= $self->recurse_limit) {
    my $err = "recurse_limit reached (".$self->recurse_limit.")";
    $err .= " number of jumps (".$self->{jumps}.")" if ($self->{jumps} || 0) > 1;
    die $err;
  }

  ### get the path (simple arrayref based thing)
  my $path = $self->path;

  ### allow for an early return
  return if $self->pre_loop($path); # a true value means to abort the navigate

  ### get a hash of valid paths (if any)
  my $valid_steps = $self->valid_steps;

  ### iterate on each step of the path
  foreach ($self->{path_i} ||= 0;
           $self->{path_i} <= $#$path;
           $self->{path_i} ++) {
    my $step = $path->[$self->{path_i}];
    next if $step !~ /^([a-zA-Z_]\w*)$/; # don't process the step if it contains odd characters
    $step = $1; # untaint

    ### check if this is an allowed step
    if ($valid_steps) {
      if (! $valid_steps->{$step}
          && $step ne 'forbidden'
          && $step ne $self->default_step
          && $step ne $self->js_step) {
        $self->stash->{'forbidden_step'} = $step;
        $self->replace_path('forbidden');
        next;
      }
    }

    ### allow for becoming another package (allows for some steps in external files)
    $self->morph($step);

    ### run the guts of the step
    my $status = $self->run_hook('run_step', $step);

    $self->unmorph($step);

    ### Allow for the run_step to intercept.
    ### A true status means the run_step took over navigation.
    return if $status;
  }

  ### allow for one exit point after the loop
  return if $self->post_loop($path); # a true value means to abort the navigate

  ### run the default step as a last resort
  $self->insert_path($self->default_step);
  $self->nav_loop; # go recursive

  return;
}

sub pre_navigate {}

sub post_navigate {}

sub recurse_limit { shift->{'recurse_limit'} || $RECURSE_LIMIT || 15 }

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

    return 2;
  }

  ### a hook before end of loop
  ### if the post_step exists and returns true, exit the nav_loop
  return 1 if $self->run_hook('post_step', $step);

  ### let the nav_loop continue searching the path
  return 0;
}

### standard functions for printing - gather information
sub prepared_print {
  my $self = shift;
  my $step = shift;

  my $hash_base = $self->run_hook('hash_base',   $step);
  my $hash_comm = $self->run_hook('hash_common', $step);
  my $hash_form = $self->run_hook('hash_form',   $step);
  my $hash_fill = $self->run_hook('hash_fill',   $step);
  my $hash_swap = $self->run_hook('hash_swap',   $step);
  my $hash_errs = $self->run_hook('hash_errors', $step);
  $_ ||= {} foreach $hash_base, $hash_comm, $hash_form, $hash_fill, $hash_swap, $hash_errs;

  ### fix up errors
  $hash_errs->{$_} = $self->format_error($hash_errs->{$_})
    foreach keys %$hash_errs;
  $hash_errs->{has_errors} = 1 if scalar keys %$hash_errs;

  ### layer hashes together
  my $fill = {%$hash_form, %$hash_base, %$hash_comm, %$hash_fill};
  my $swap = {%$hash_form, %$hash_base, %$hash_comm, %$hash_swap, %$hash_errs};
  $fill = {} if $self->no_fill($step);

  ### run the print hook - passing it the form and fill info
  $self->run_hook('print', $step, undef,
                  $swap, $fill);
}

sub no_fill { shift->{'no_fill'} }

sub exit_nav_loop {
  my $self = shift;

  ### undo morphs
  if (my $ref = $self->{'__morph_lineage'}) {
    ### use the saved index - this allows for early "morphers" to only get rolled back so far
    my $index = $self->{'__morph_lineage_start_index'};
    $index = -1 if ! defined $index;
    $self->unmorph while $#$ref != $index;
  }

  ### long jump back
  die "Long Jump\n";
}

sub jump {
  my $self   = shift;
  my $i      = ($#_ == -1) ? 1 : shift;
  my $path   = $self->path;
  my $path_i = $self->{path_i};
  die "Can't jump if nav_loop not started" if ! defined $path_i;

  ### validate where we are jumping to
  if ($i =~ /^\w+$/) {
    if ($i eq 'FIRST') {
      $i = - $path_i - 1;
    } elsif ($i eq 'LAST') {
      $i = $#$path - $path_i;
    } elsif ($i eq 'NEXT') {
      $i = 1;
    } elsif ($i eq 'CURRENT') {
      $i = 0;
    } elsif ($i eq 'PREVIOUS') {
      $i = -1;
    } else { # look for a step by that name
      for (my $j = $#$path; $j >= 0; $j --) {
        if ($path->[$j] eq $i) {
          $i = $j - $path_i;
          last;
        }
      }
    }
  }
  if ($i !~ /^-?\d+$/) {
    require Carp;
    Carp::croak("Invalid jump index ($i)");
  }

  ### manipulate the path to contain the new jump location
  my @replace;
  my $cut_i  = $path_i + $i;
  if ($cut_i > $#$path) {
    push @replace, $self->default_step;
  } elsif ($cut_i < 0) {
    push @replace, @$path;
  } else {
    push @replace, @$path[$cut_i .. $#$path];
  }
  $self->replace_path(@replace);

  ### record the number of jumps
  $self->{jumps} ||= 0;
  $self->{jumps} ++;

  ### run the newly fixed up path (recursively)
  $self->{path_i} ++; # move along now that the path is updated
  $self->nav_loop;
  $self->exit_nav_loop;
}

sub default_step { shift->{'default_step'} || 'main' }

sub js_step { shift->{'js_step'} || 'js' }

###----------------------------------------------------------------###

sub step_key {
  my $self = shift;
  return $self->{'step_key'} || 'step';
}

### determine the path to follow
sub path {
  my $self = shift;
  return $self->{path} ||= do {
    my @path     = (); # default to empty path
    my $step_key = $self->step_key;

    if (my $step = $self->form->{$step_key}) {
      push @path, $step;
    } elsif ($ENV{'PATH_INFO'} && $ENV{'PATH_INFO'} =~ m|^/(\w+)|) {
      push @path, lc($1);
    }

    \@path; # return of the do
  };
}

### really should only be used during initialization
sub set_path {
  my $self = shift;
  my $path = $self->{path} ||= [];
  die "Cannot call set_path after the navigation loop has begun" if $self->{path_i};
  splice @$path, 0, $#$path + 1, @_; # change entries in the ref
}

### legacy - same as append_path
sub add_to_path {
  my $self = shift;
  push @{ $self->path }, @_;
}

### append entries onto the end
sub append_path {
  my $self = shift;
  push @{ $self->path }, @_;
}

### replace all entries that are left
sub replace_path {
  my $self = shift;
  my $ref  = $self->path;
  my $i    = $self->{path_i} || 0;
  if ($i + 1 > $#$ref) {
    push @$ref, @_;
  } else {
    splice(@$ref, $i + 1, $#$ref - $i, @_); # replace remaining entries
  }
}

### insert more steps into the current path
sub insert_path {
  my $self = shift;
  my $ref  = $self->path;
  my $i    = $self->{path_i} || 0;
  if ($i + 1 > $#$ref) {
    push @$ref, @_;
  } else {
    splice(@$ref, $i + 1, 0, @_); # insert a path at the current location
  }
}

### a hash of paths that are allowed, default undef is all
sub valid_steps {}

###----------------------------------------------------------------###
### allow for checking where we are in the path

sub step_by_path_index {
  my $self = shift;
  my $i    = shift || 0;
  my $ref  = $self->path;
  return '' if $i < 0;
  return $self->default_step if $i > $#$ref;
  return $ref->[$i];
}

sub previous_step {
  my $self = shift;
  die "previous_step is readonly" if $#_ != -1;
  return $self->step_by_path_index( ($self->{path_i} || 0) - 1 );
}

sub current_step {
  my $self = shift;
  die "current_step is readonly" if $#_ != -1;
  return $self->step_by_path_index( ($self->{path_i} || 0) );
}

sub next_step {
  my $self = shift;
  die "next_step is readonly" if $#_ != -1;
  return $self->step_by_path_index( ($self->{path_i} || 0) + 1 );
}

sub last_step {
  my $self = shift;
  die "last_step is readonly" if $#_ != -1;
  return $self->step_by_path_index( $#{ $self->path } );
}

sub first_step {
  my $self = shift;
  die "first_step is readonly" if $#_ != -1;
  return $self->step_by_path_index( 0 );
}

###----------------------------------------------------------------###

sub pre_loop {}
sub post_loop {}

### return the appropriate hook to call
sub hook {
  my $self    = shift;
  my $hook    = shift || do { require Carp; Carp::confess("Missing hook name") };
  my $step    = shift || '';
  my $default = shift;
  my $hist    = $self->history;
  my $code;
  if ($step && ($code = $self->can("${step}_${hook}"))) {
    push @$hist, "$step - $hook - ${step}_${hook}";
    return $code;
  } elsif ($code = $self->can($hook)) {
    push @$hist, "$step - $hook - $hook";
    return $code;
  } elsif (UNIVERSAL::isa($default, 'CODE')) {
    push @$hist, "$step - $hook - DEFAULT CODE";
    return $default;
  } elsif ($default) {
    push @$hist, "$step - $hook - DEFAULT";
    return sub { return $default };
  } else {
    return sub {};
  }
}

### get and call the appropriate hook
sub run_hook {
  my $self    = shift;
  my $hook    = shift;
  my $step    = shift;
  my $default = shift;
  my $code = $self->hook($hook, $step, $default);
  return $self->$code($step, @_);
}

sub history {
  return shift->{'history'} ||= [];
}

### default die handler - show what happened and die (so its in the error logs)
sub handle_error {
  my $self = shift;
  my $err  = shift;

  die $err;
}

###----------------------------------------------------------------###
### utility modules for jeckyl/hyde on self

sub allow_morph {
  my $self = shift;
  return $self->{'allow_morph'} ? 1 : 0;
}

sub allow_nested_morph {
  my $self = shift;
  return $self->{'allow_nested_morph'} ? 1 : 0;
}

sub morph {
  my $self = shift;
  my $step = shift || return;
  return if ! (my $allow = $self->allow_morph); # not true

  ### place to store the lineage
  my $lin = $self->{'__morph_lineage'} ||= [];
  my $cur = ref $self; # what are we currently
  push @$lin, $cur;    # store so subsequent unmorph calls can do the right thing
  my $hist = $self->history;
  push @$hist, "$step - morph - morph";
  my $sref = \$hist->[-1]; # get ref so we can add more info in a moment

  if (ref($allow) && ! $allow->{$step}) { # hash - but no step - record for unbless
    $$sref .= " - not allowed to morph to that step";
    return;
  }

  ### make sure we haven't already been reblessed
  if ($#$lin != 0                                # is this the second morph call
      && (! ($allow = $self->allow_nested_morph) # not true
          || (ref($allow) && ! $allow->{$step})  # hash - but no step
          )) {
    $$sref .= $allow ? " - not allowed to nested_morph to that step" : " - nested_morph disabled";
    return; # just return - don't die so that we can morph early
  }

  ### if we are not already that package - bless us there
  my $new  = $self->run_hook('morph_package', $step);
  if ($cur ne $new) {
    my $file = $new .'.pm';
    $file =~ s|::|/|g;
    if (UNIVERSAL::can($new, 'can')  # check if the package space exists
        || eval { require $file }) { # check for a file that holds this package
      ### become that package
      bless $self, $new;
      $$sref .= " - changed $cur to $new";
      if (my $method = $self->can('fixup_after_morph')) {
        $self->$method($step);
      }
    } else {
      if ($@) {
        if ($@ =~ /^\s*(Can\'t locate \S+ in \@INC)/) { # let us know what happened
          $$sref .= " - failed from $cur to $new: $1";
        } else {
          $$sref .= " - failed from $cur to $new: $@";
          my $err = "Trouble while morphing to $file: $@";
          warn $err;
        }
      }
    }
  }

}

sub unmorph {
  my $self = shift;
  my $step = shift || '__no_step';
  my $lin  = $self->{'__morph_lineage'} || return;
  my $cur  = ref $self;
  my $prev = pop(@$lin) || die "unmorph called more times than morph - current ($cur)";

  ### if we are not already that package - bless us there
  my $hist = $self->history;
  if ($cur ne $prev) {
    if (my $method = $self->can('fixup_before_unmorph')) {
      $self->$method($step);
    }
    bless $self, $prev;
    push @$hist, "$step - unmorph - unmorph - changed from $cur to $prev";
  } else {
    push @$hist, "$step - unmorph - unmorph - already isa $cur";
  }

  return $self;
}

###----------------------------------------------------------------###
### allow for cleanup including deep nested objects

sub cleanup {
  my $self = shift;
  ref($self)->cleanup_cross_references($self);
}

sub cleanup_cross_references {
  my $class = shift;
  my $self  = shift;
  my $seen  = shift || {};
  return if $seen->{$self}; # prevent recursive checking
  $seen->{$self} = 1;
  return if $CLEANUP_EXCLUDE{ ref($self) };
  if (UNIVERSAL::isa($self, 'HASH')) {
    require Scalar::Util; # first self will always be hash
    foreach my $key (keys %$self) {
      next if ! $self->{$key};
      $class->cleanup_cross_references($self->{$key}, $seen);
      # weaken and remove blessed objects
      # this will clober objects in global caches that are referenced in the structure
      # so beware (that means weaken your cached references)
      if (Scalar::Util::blessed($self->{$key})
          && ! Scalar::Util::isweak($self->{$key})) {
        Scalar::Util::weaken($self->{$key});
        $self->{$key} = undef;
      } elsif (UNIVERSAL::isa($self->{$key}, 'CODE')) {
        $self->{$key} = undef;
      }
    }
  } elsif (UNIVERSAL::isa($self, 'ARRAY')) {
    for my $key (0 .. $#$self) {
      next if ! $self->[$key];
      $class->cleanup_cross_references($self->[$key], $seen);
      if (Scalar::Util::blessed($self->[$key])
          && ! Scalar::Util::isweak($self->[$key])) {
        Scalar::Util::weaken($self->[$key]);
        $self->[$key] = undef;
      } elsif (UNIVERSAL::isa($self->[$key], 'CODE')) {
        $self->[$key] = undef;
      }
    }
  }
}

###----------------------------------------------------------------###
### a few standard base accessors

sub form {
  my $self = shift;
  if ($#_ != -1) {
    $self->{form} = shift || die "Invalid form";
  }
  return $self->{form} ||= $self->cgix->get_form;
}

sub cookies {
  my $self = shift;
  if ($#_ != -1) {
    $self->{cookies} = shift || die "Invalid cookies";
  }
  return $self->{cookies} ||= $self->cgix->get_cookies;
}

sub cgix {
  my $self = shift;
  return $self->{cgix} ||= do {
    my $args = shift || {};
    require CGI::Ex;
    CGI::Ex->new($args); # return of the do
  };
}

sub set_cgix {
  my $self = shift;
  $self->{cgix} = shift;
}

sub vob {
  my $self = shift;
  return $self->{vob} ||= do {
    my $args = shift || {};
    $args->{cgix} ||= $self->cgix;
    require CGI::Ex::Validate;
    CGI::Ex::Validate->new($args); # return of the do
  };
}

sub set_vob {
  my $self = shift;
  $self->{vob} = shift;
}

sub auth {
  my $self = shift;
  return $self->{auth} ||= do {
    my $args = shift || {};
    $args->{cgix}    ||= $self->cgix,
    $args->{form}    ||= $self->form,
    $args->{cookies} ||= $self->cookies,
    require CGI::Ex::Auth;
    CGI::Ex::Auth->new($args); # return of the do
  };
}

sub set_auth {
  my $self = shift;
  $self->{auth} = shift;
}

### provide a place for placing variables
sub stash {
  my $self = shift;
  return $self->{'stash'} ||= {};
}

### allow for adding arbitrary values to self
sub add_property {
  my $self = shift;
  my $prop = shift;
  my $key  = '__prop_'. $prop;
  my $name = __PACKAGE__ ."::". $prop;
  no strict 'refs';
  *$name = sub : lvalue {
    my $self = shift;
    $self->{$key} = shift() if $#_ != -1;
    $self->{$key};
  } if ! defined &$name;
  $self->$prop(shift()) if $#_ != -1;
}

###----------------------------------------------------------------###
### js_validation items

### creates javascript suitable for validating the form
sub js_validation {
  my $self = shift;
  my $step = shift;
  return '' if $self->ext_val eq 'htm'; # let htm validation do it itself

  my $form_name = shift || $self->run_hook('form_name', $step);
  my $hash_val  = shift || $self->run_hook('hash_validation', $step, {});
  my $js_uri    = $self->js_uri_path;
  return '' if UNIVERSAL::isa($hash_val, 'HASH')  && ! scalar keys %$hash_val
            || UNIVERSAL::isa($hash_val, 'ARRAY') && $#$hash_val == -1;

  return $self->vob->generate_js($hash_val, $form_name, $js_uri);
}

### where to find the javascript files
### default to using this script as a handler
sub js_uri_path {
  my $self   = shift;
  my $script = $ENV{'SCRIPT_NAME'} || die "Missing SCRIPT_NAME";
  my $js_step = $self->js_step;
  return ($self->can('path') == \&CGI::Ex::App::path)
    ? $script .'/'. $js_step # try to use a cache friendly URI (if path is our own)
    : $script . '?'.$self->step_key.'='.$js_step.'&js='; # use one that works with more paths
}

### name to attach js validation to
sub form_name { 'theform' }

### provide some rudimentary javascript support
### if valid_steps is defined - it should include "js"
sub js_run_step {
  my $self = shift;

  ### make sure path info looks like /js/CGI/Ex/foo.js
  my $file = $self->form->{'js'} || $ENV{'PATH_INFO'} || '';
  $file = ($file =~  m!^(?:/js/|/)?(\w+(?:/\w+)*\.js)$!) ? $1 : '';

  $self->cgix->print_js($file);
  $self->exit_nav_loop;
}

###----------------------------------------------------------------###
### implementation specific subs

sub print {
  my ($self, $step, $swap, $fill) = @_;

  ### get a filename relative to base_dir_abs
  my $file = eval { $self->run_hook('file_print', $step) };
  if ($@ && $@ =~ /BASE_DIR_ABS not set/i) {
    die "Error while looking for template file on step \"$step\": $@";
  }

  my $out = $self->run_hook('swap_template', $step, undef, $file, $swap);

  $self->run_hook('fill_template', $step, undef, \$out, $fill);

  $self->run_hook('print_out', $step, undef, $out);
}

sub print_out {
  my ($self, $step, $out) = @_;

  ### now print
  $self->cgix->print_content_type();
  print $out;
}

sub swap_template {
  my ($self, $step, $file, $swap) = @_;

  require CGI::Ex::Template;
  my $t = CGI::Ex::Template->new($self->template_args($step));

  ### process the document
  my $out = '';
  my $status = $t->process($file, $swap, \$out) || die $Template::ERROR;

  return $out;
}

sub fill_template {
  my ($self, $step, $outref, $fill) = @_;

  return if ! $fill || $self->no_fill($step);

  ### fill in any forms
  $self->cgix->fill($outref, $fill);
}

sub template_args {
  my $self = shift;
  my $step = shift;
  return {
    INCLUDE_PATH => $self->base_dir_abs,
  };
}

sub base_dir_rel {
  my $self = shift;
  $self->{base_dir_rel} = shift if $#_ != -1;
  return $self->{base_dir_rel} ||= $BASE_DIR_REL;
}

sub base_dir_abs {
  my $self = shift;
  $self->{base_dir_abs} = shift if $#_ != -1;
  return $self->{base_dir_abs} || $BASE_DIR_ABS
    || die "\$BASE_DIR_ABS not set for use in stub functions";
}

sub ext_val {
  my $self = shift;
  $self->{ext_val} = shift if $#_ != -1;
  return $self->{ext_val} || $EXT_VAL || die "\$EXT_VAL not set for use in stub functions";
}

sub ext_print {
  my $self = shift;
  $self->{ext_print} = shift if $#_ != -1;
  return $self->{ext_print} || $EXT_PRINT || die "\$EXT_PRINT not set for use in stub functions";
}

sub has_errors {
  my $self = shift;
  return 1 if scalar keys %{ $self->hash_errors };
}

sub format_error {
  my $self  = shift;
  my $error = shift;
#  return $error if $error =~ /<span/i;
#  return "<span class=\"error\">$error</span>";
}

###----------------------------------------------------------------###
### default stub subs

sub pre_step   { 0 } # success indicates we handled step
sub skip       { 0 } # success indicates to skip the step
sub prepare    { 1 } # failure means show step
sub finalize   { 1 } # failure means show step
sub post_print {}
sub name_step {
  my $self = shift;
  my $step = shift;
  return $step;
}

### used for looking up a module to morph into
sub morph_package {
  my $self = shift;
  my $step = shift || '';
  my $cur = ref $self; # default to using self as the base for morphed modules
  my $new = $cur .'::'. $step;
  $new =~ s/(\b|_+)(\w)/\u$2/g; # turn Foo::my_step_name into Foo::MyStepName
  return $new;
}

sub base_name_module {
  my $self = shift;
  $self->{base_name_module} = shift if $#_ != -1;
  return $self->{base_name_module} ||= $BASE_NAME_MODULE;
}

### used for looking up template content
sub name_module {
  my $self = shift;
  my $step = shift || '';
  my $name;
  if ($name = $self->base_name_module) {
    return $name;
  } else {
    return ($0 =~ m/(\w+)(\.\w+)?$/) ? $1 # allow for cgi-bin/foo or cgi-bin/foo.pl
      : die "Couldn't determine module name from \"name_module\" lookup ($step)";
  }
}

### which file is used for templating
sub file_print {
  my $self = shift;
  my $step = shift;

  my $base_dir = $self->base_dir_rel;
  my $module   = $self->run_hook('name_module', $step);
  my $_step    = $self->run_hook('name_step', $step) || die "Missing name_step";
  $_step .= '.'. $self->ext_print if $_step !~ /\.\w+$/;

  foreach ($base_dir, $module) { $_ .= '/' if length($_) && ! m|/$| }

  return $base_dir . $module . $_step;
}

### which file is used for validation
sub file_val {
  my $self = shift;
  my $step = shift;

  my $base_dir = $self->base_dir_rel;
  my $module   = $self->run_hook('name_module', $step);
  my $_step    = $self->run_hook('name_step', $step, $step) || die "Missing name_step";
  $_step .= '.'. $self->ext_print if $_step !~ /\.\w+$/;
  my $abs      = $self->base_dir_abs;

  foreach ($base_dir, $module, $abs) { $_ .= '/' if length($_) && ! m|/$| }

  return $abs . $base_dir . $module . $_step;
}


sub info_complete {
  my $self = shift;
  my $step = shift;

  return 0 if ! $self->run_hook('ready_validate', $step);

  return $self->run_hook('validate', $step);
}

sub ready_validate {
  my $self = shift;
  my $step = shift;

  ### could do a slightly more complex test
  return 0 if ! $ENV{REQUEST_METHOD} || $ENV{REQUEST_METHOD} ne 'POST';
  return 1;
}

sub set_ready_validate {
  my $self = shift;
  my $ready = shift;
  $ENV{REQUEST_METHOD} = ($ready) ? 'POST' : 'GET';
}

sub validate {
  my $self = shift;
  my $step = shift;
  my $form = shift || $self->form;
  my $hash = $self->run_hook('hash_validation', $step, {});
  my $what_was_validated = [];

  my $eob = eval { $self->vob->validate($form, $hash, $what_was_validated) };
  if (! $eob && $@) {
    die "Step $step: $@";
  }

  ### had an error - store the errors and return false
  if ($eob) {
    $self->add_errors($eob->as_hash({
      as_hash_join   => "<br>\n",
      as_hash_suffix => '_error',
    }));
    return 0;
  }

  ### allow for the validation to give us some redirection
  my $val;
  OUTER: foreach my $ref (@$what_was_validated) {
    foreach my $method (qw(append_path replace_path insert_path)) {
      next if ! ($val = $ref->{$method});
      $self->$method(ref $val ? @$val : $val);
      last OUTER;
    }
  }

  return 1;
}

sub hash_validation {
  my $self = shift;
  my $step = shift;
  return $self->{hash_validation}->{$step} ||= do {
    my $hash;
    my $file = eval { $self->run_hook('file_val', $step) };
    if ($@ && $@ =~ /BASE_DIR_ABS not set/i) {
      die "Error while looking for validation on step \"$step\": $@";
    }

    ### allow for returning the validation hash in the filename
    ### a scalar ref means it is a yaml document to be read by get_validation
    if (ref($file) && ! UNIVERSAL::isa($file, 'SCALAR')) {
      $hash = $file;

    ### read the file - it it fails - errors should shown in the error logs
    } elsif ($file) {
      $hash = eval { $self->vob->get_validation($file) } || {};

    } else {
      $hash = {};
    }

    $hash; # return of the do
  };
}

sub hash_base {
  my ($self, $step) = @_;
  return $self->{hash_base} ||= {
    script_name   => $ENV{'SCRIPT_NAME'} || $0,
    path_info     => $ENV{'PATH_INFO'}   || '',
    js_validation => sub { $self->run_hook('js_validation', $step, shift) },
    form_name     => sub { $self->run_hook('form_name', $step) },
  };
}

sub hash_common { shift->{'hash_common'} ||= {} }
sub hash_form   { shift->form }
sub hash_fill   { shift->{'hash_fill'}   ||= {} }
sub hash_swap   { shift->{'hash_swap'}   ||= {} }
sub hash_errors { shift->{'hash_errors'} ||= {} }

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

sub add_to_errors { shift->add_errors(@_) }
sub add_to_swap   { my $self = shift; $self->add_to_hash($self->hash_swap,   @_) }
sub add_to_fill   { my $self = shift; $self->add_to_hash($self->hash_fill,   @_) }
sub add_to_form   { my $self = shift; $self->add_to_hash($self->hash_form,   @_) }
sub add_to_common { my $self = shift; $self->add_to_hash($self->hash_common, @_) }
sub add_to_base   { my $self = shift; $self->add_to_hash($self->hash_base,   @_) }

sub add_to_hash {
  my $self = shift;
  my $old  = shift;
  my $new  = shift;
  $new = {$new, @_} if ! ref $new; # non-hashref
  $old->{$_} = $new->{$_} foreach keys %$new;
}

###----------------------------------------------------------------###

sub forbidden_info_complete { 0 }

sub forbidden_file_print {
  my $self = shift;
  my $step = $self->stash->{'forbidden_step'};
  my $str = "You do not have access to \"$step\"";
  return \$str;
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex::App - Full featured (within reason) application builder.

=head1 DESCRIPTION

Fill in the blanks and get a ready made CGI.  This module is somewhat
similar in spirit to CGI::Application, CGI::Path, and CGI::Builder and any
other "CGI framework."  As with the others, CGI::Ex::App tries to do as
much as possible, in a simple manner, without getting in the
developer's way.  Your milage may vary.

=head1 SYNOPSIS (A LONG "SYNOPSIS")

More examples will come with time.  Here are the basics for now.
This example script would most likely be in the form of a cgi, accessible via
the path http://yourhost.com/cgi-bin/my_app (or however you do CGIs on
your system.  About the best way to get started is to pasted the following
code into a cgi script and try it out.  A detailed walk through follows in the
next section.

  #!/usr/bin/perl -w

  use strict;
  use base qw(CGI::Ex::App);
  use CGI::Ex::Dump qw(debug);

  __PACKAGE__->navigate;
   # OR you could do
   # my $obj = __PACKAGE__->new; $obj->navigate;
   # OR you could do the following which cleans
   # circular references - useful for a mod_perl situation
   # __PACKAGE__->navigate->cleanup;
  exit;

  ###------------------------------------------###

  sub valid_steps { return {success => 1} }
    # the default_step "main" and the built in step "js" are valid paths

  # base_dir_abs is only needed if default print is used
  # template toolkit needs an INCLUDE_PATH
  sub base_dir_abs { '/tmp' }

  sub post_print {
    debug shift->history;
  } # show what happened

  sub main_file_print {
    # reference to string means ref to content
    # non-reference means filename
    return \ "<h1>Main Step</h1>
      <form method=post name=[% form_name %]>
      <input type=text name=foo>
      <span style='color:red' id=foo_error>[% foo_error %]</span><br>
      <input type=submit>
      </form>
      [% js_validation %]
      <a href='[% script_name %]?step=foo'>Link to forbidden step</a>
    ";
  }

  sub main_file_val {
    # reference to string means ref to yaml document
    # non-reference means filename
    return \ "foo:
      required: 1
      min_len: 2
      max_len: 20
      match: 'm/^([a-z]\\d)+[a-z]?\$/'
      match_error: Characters must alternate letter digit letter.
      \n";
  }

  sub main_finalize {
    my $self = shift;

    debug $self->form, "Do something useful with form here";

    ### add success step
    $self->add_to_swap({success_msg => "We did something"});
    $self->append_path('success');
    $self->set_ready_validate(0);
    return 1;
  }

  sub success_file_print {
    \ "<h1>Success Step</h1> All done.<br>
       ([% success_msg %])<br>
       (foo = [% foo %])";
  }

  ### not necessary - this is the default hash_base
  sub hash_base { # used to include js_validation
    my ($self, $step) = @_;
    return $self->{hash_base} ||= {
      script_name   => $ENV{SCRIPT_NAME} || '',
      js_validation => sub { $self->run_hook('js_validation', $step) },
      form_name     => sub { $self->run_hook('form_name', $step) },
    };
  }

  __END__

Note: This example would be considerably shorter if the html file
(file_print) and the validation file (file_val) had been placed in
separate files.  Though CGI::Ex::App will work "out of the box" as
shown it is more probable that any platform using it will customize
the various hooks to their own tastes (for example, switching print to
use a system other than Template::Toolkit).

=head1 STEP BY STEP

This section goes step by step over the previous example.

Well - we start out with with the customary CGI introduction.

  #!/usr/bin/perl -w

  use strict;
  use base qw(CGI::Ex::App);
  use CGI::Ex::Dump qw(debug);

Note:  the "use base" is not normally used in the "main" portion of a script.
It does allow us to just do __PACKAGE__->navigate.

Now we need to invoke the process:

  __PACKAGE__->navigate;
   # OR you could do
   # my $obj = __PACKAGE__->new; $obj->navigate;
   # OR you could do the following which cleans
   # circular references - useful for a mod_perl situation
   # __PACKAGE__->navigate->cleanup;
  exit;

Note: the "exit" isn't necessary - but it is kind of nice to infer that
program flow doesn't go beyond the ->navigate call.

The navigate routine is now going to try and "run" through a series of
steps.  Navigate will call the ->path method which should return an arrayref
containing the valid steps.  By default, if path method has not been overridden,
the path method will default first to the step found in form key named ->step_name,
then it will fall to the contents of $ENV{'PATH_INFO'}.  If navigation runs out
of steps to run it will run the step found in ->default_step which defaults to 'main'.
So the URI '/cgi-bin/my_app' would run the step 'main' first by default.  The URI
'/cgi-bin/my_app?step=foo' would run the step 'foo' first.  The URI '/cgi-bin/my_app/bar'
would run the step 'bar' first.

CGI::Ex::App allows for running steps in a preset path.  The navigate method will go
through a step of the path at a time and see if it is completed (various methods determine
the definition of "completed").  This preset type of path can also be automated using the
CGI::Path module.  Rather than using a preset path, CGI::Ex::App also has methods that
allow for dynamic changing of the path, so that each step can determine which step to do next
(see the jump, append_path, insert_path, and replace_path methods).

Because the default ->path call allows for testing arbitrary step names, it would be
nice to restrict what they can use.  And you can with valid_steps.

  sub valid_steps { return {success => 1} }
    # the default_step (default "main") and the js_step (default "js") are valid paths

CGI::Ex::App is prewired to use Template::Toolkit, but should be easy to use any
other toolkit as well.  The following piece gives a default directory to get
the Template::Toolkit templates.  We aren't using a separate template in this
example so we default it to a non-existent dir

  # base_dir_abs is only needed if default print is used
  # template toolkit needs an INCLUDE_PATH
  sub base_dir_abs { '/non_existent_dir' }

During development it would be nice to see what happened during the course of
our navigation.  This is stored in the arrayref contained in ->history.  There
is a hook that runs after a step is printed its content called "post_print."  This
chunk will display history after we have printed the content.

  sub post_print {
    debug shift->history;
  } # show what happened

Ok.  Finally we are looking at the methods used by each step of the path.  The
hook mechanism of CGI::Ex::App will look first for a method ${step}_${hook_name}
called before falling back to the method named $hook_name.

  sub main_file_print {
    # reference to string means ref to content
    # non-reference means filename
    return \ "<h1>Main Step</h1>
      <form method=post name=[% form_name %]>
      <input type=text name=foo>
      <span style='color:red' id=foo_error>[% foo_error %]</span><br>
      <input type=submit>
      </form>
      [% js_validation %]
      <a href='[% script_name %]?step=foo'>Link to forbidden step</a>
    ";
  }

  sub main_file_val {
    # reference to string means ref to yaml document
    # non-reference means filename
    return \ "foo:
      required: 1
      min_len: 2
      max_len: 20
      match: 'm/^([a-z]\\d)+[a-z]?\$/'
      match_error: Characters must alternate letter digit letter.
      \n";
  }

  sub main_finalize {
    my $self = shift;

    debug $self->form, "Do something useful with form here";

    ### add success step
    $self->add_to_swap({success_msg => "We did something"});
    $self->append_path('success');
    $self->set_ready_validate(0);
    return 1;
  }

  sub success_file_print {
    \ "<h1>Success Step</h1> All done.<br>
       ([% success_msg %])<br>
       (foo = [% foo %])";
  }

  ### not necessary - this is the default hash_base
  sub hash_base { # used to include js_validation
    my ($self, $step) = @_;
    return $self->{hash_base} ||= {
      script_name   => $ENV{SCRIPT_NAME} || '',
      js_validation => sub { $self->run_hook('js_validation', $step) },
      form_name     => sub { $self->run_hook('form_name', $step) },
    };
  }

  __END__



=head1 AVAILABLE HOOKS / METHODS

CGI::Ex::App works on the principles of hooks which are essentially
glorified method lookups.  When a hook is called, CGI::Ex::App will
look for a corresponding method call for that hook for the current
step name.  See the discussion under the method named "hook" for more
details.  The methods listed below are normal method calls.
Hooks and methods are looked for in the following order:

=over 4

=item Method C<-E<gt>new>

Object creator.  Takes a hash or hashref.

=item Method C<-E<gt>init>

Called by the default new method.  Allows for any object
initilizations.

=item Method C<-E<gt>form>

Returns a hashref of the items passed to the CGI.  Returns
$self->{form}.  Defaults to CGI::Ex::get_form.

=item Method C<-E<gt>navigate>

Takes a class name or a CGI::Ex::App object as arguments.  If a class
name is given it will instantiate an object by that class.  All returns
from navigate will return the object.

The method navigate is essentially a safe wrapper around the ->nav_loop
method.  It will catch any dies and pass them to ->handle_error.

=item Method C<-E<gt>nav_loop>

This is the main loop runner.  It figures out the current path
and runs all of the appropriate hooks for each step of the path.  If
nav_loop runs out of steps to run (which happens if no path is set, or if
all other steps run successfully), it will insert the ->default_step into
the path and run nav_loop again (recursively).  This way a step is always
assured to run.  There is a method ->recurse_limit (default 15) that
will catch logic errors (such as inadvertently running the same
step over and over and over).

The basic outline of navigation is as follows (the default actions for hooks
are shown):

  navigate {
    eval {
      ->pre_navigate
      ->nav_loop
      ->post_navigate
    }
    # dying errors will run the ->handle_error method
  }


  nav_loop {
    ->path (get the path steps)
       # DEFAULT ACTION
       # look in $ENV{'PATH_INFO'}
       # look in ->form for ->step_key

    ->pre_loop
       # navigation stops if true

    ->valid_steps (get list of valid paths)

    foreach step of path {

      # check that path is valid

      ->morph
        # DEFAULT ACTION
        # check ->allow_morph
        # check ->allow_nested_morph
        # ->morph_package (hook - get the package to bless into)
        # ->fixup_after_morph if morph_package exists

      ->run_step (hook)

      ->unmorph
        # DEFAULT ACTION
        # ->fixup_before_unmorph if blessed to previous package

      # exit loop if ->run_step returned true (intercepted)

    } end of step foreach

    ->post_loop
      # navigation stops if true

    ->default_step (inserted into path at current location)
    ->nav_loop (called again recursively)

  } end of nav_loop


  run_step {
    ->pre_step (hook)
      # exits nav_loop if true

    ->skip (hook)
      # skips this step if true (stays in nav_loop)

    ->prepare (hook - defaults to true)

    ->info_complete (hook - ran if prepare was true)
      # DEFAULT ACTION
      # ->ready_validate (hook)
      # return false if ! ready_validate
      # ->validate (hook)
      #   ->hash_validation (hook)
      #     ->file_val (hook - uses base_dir_rel, name_module, name_step, ext_val)
      #   uses CGI::Ex::Validate to validate the hash
      # returns true if validate is true

    ->finalize (hook - defaults to true - ran if prepare and info_complete were true)

    if ! ->prepare || ! ->info_complete || ! ->finalize {
      ->prepared_print
        # DEFAULT ACTION
        # ->hash_base (hook)
        # ->hash_common (hook)
        # ->hash_form (hook)
        # ->hash_fill (hook)
        # ->hash_swap (hook)
        # ->hash_errors (hook)
        # merge form, base, common, and fill into merged fill
        # merge form, base, common, swap, and errors into merged swap
        # ->print (hook - passed current step, merged swap hash, and merged fill)
          # DEFAULT ACTION
          # ->file_print (hook - uses base_dir_rel, name_module, name_step, ext_print)
          # ->template_args
          # Processes the file with Template Toolkit
          # Fills the any forms with CGI::Ex::Fill
          # Prints headers and the content

      ->post_print (hook - used for anything after the print process)

      # return true to exit from nav_loop
    }

    ->post_step (hook)
      # exits nav_loop if true

  } end of run_step


=item Method C<-E<gt>pre_navigate>

Called from within navigate.  Called before the nav_loop method is started.
If a true value is returned then navigation is skipped (the nav_loop is never
started).

=item Method C<-E<gt>post_navigate>

Called from within navigate.  Called after the nav_loop has finished running.
Will only run if there were no errors which died during the nav_loop
process.

=item Method C<-E<gt>handle_error>

If anything dies during execution, handle_error will be called with
the error that had happened.  Default is to debug the error and path
history.

=item Method C<-E<gt>history>

Returns an arrayref of which hooks of which steps of the path were ran.
Useful for seeing what happened.  In general - each line of the history
will show the current step, the hook requested, and which hook was
actually called. (hooks that don't find a method don't add to history)

=item Method C<-E<gt>path>

Return an arrayref (modifyable) of the steps in the path.  For each
step the remaining hooks can be run.  Hook methods are looked up and
ran using the method "run_hook" which uses the method "hook" to lookup
the hook.  A history of ran hooks is stored in the array ref returned
by $self->history.  Default will be a single step path looked up in
$form->{path} or in $ENV{PATH_INFO}.  By default, path will look for
$ENV{'PATH_INFO'} or the value of the form by the key step_key.  For
the best functionality, the arrayref returned should be the same
reference returned for every call to path - this ensures that other
methods can add to the path (and will most likely break if the
arrayref is not the same).  If navigation runs out of steps to run,
the default step found in default_step will be run.

=item Method C<-E<gt>default_step>

Step to show if the path runs out of steps.  Default value is the
'default_step' property or the value 'main'.

=item Method C<-E<gt>step_key>

Used by default to determine which step to put in the path.  The
default path will only have one step within it

=item Method C<-E<gt>set_path>

Arguments are the steps to set.  Should be called before navigation
begins.  This will set the path arrayref to the passed steps.

=item Method C<-E<gt>append_path>

Arguments are the steps to append.  Can be called any time.  Adds more
steps to the end of the current path.

=item Method C<-E<gt>replace_path>

Arguments are the steps used to replace.  Can be called any time.
Replaces the remaining steps (if any) of the current path.

=item Method C<-E<gt>insert_path>

Arguments are the steps to insert.  Can be called any time.  Inserts
the new steps at the current path location.

=item Method C<-E<gt>jump>

This method should not normally be used.  It provides for moving to the
next step at any point during the nav_loop.  It effectively short circuits
the remaining hooks for the current step.  It does increment the recursion
counter (which has a limit of ->recurse_limit - default 15).  It is normally
better to allow the other hooks in the loop to carry on their normal functions
and avoid jumping.  (Essentially, this hook behaves like a goto method to
bypass everything else and continue at a different location in the path - there
are times when it is necessary or useful - but most of the time should be
avoided)

Jump takes a single argument which is the location in the path to jump
to.  This argument may be either a step name, the special words
"FIRST, LAST, CURRENT, PREVIOUS, OR NEXT" or the number of steps to
jump forward (or backward) in the path.  The default value, 1,
indicates that CGI::Ex::App should jump to the next step (the default action for
jump).  A value of 0 would repeat the current step (watch out for
recursion).  A value of -1 would jump to the previous step.  The
special value of "LAST" will jump to the last step.  The special value
of "FIRST" will jump back to the first step.  In each of these cases,
the path array retured by ->path is modified to allow for the jumping.

  ### goto previous step
  $self->jump($self->previous_step);
  $self->jump('PREVIOUS');
  $self->jump(-1);

  ### goto next step
  $self->jump($self->next_step);
  $self->jump('NEXT');
  $self->jump(1);
  $self->jump;

  ### goto current step (repeat)
  $self->jump($self->current_step);
  $self->jump('CURRENT');
  $self->jump(0);

  ### goto last step
  $self->jump($self->last_step);
  $self->jump('LAST');

  ### goto first step
  $self->jump($self->first_step);
  $self->jump('FIRST');

=item Method C<-E<gt>exit_nav_loop>

This method should not normally used.  It allows for a long jump to the
end of all nav_loops (even if they are recursively nested).  This
effectively short circuits all remaining hooks for the current and
remaining steps.  It is used to allow the ->jump functionality.  If the
application has morphed, it will be unmorphed before returning.

=item Method C<-E<gt>recurse_limit>

Default 15.  Maximum number of times to allow nav_loop to call itself.
If ->jump is used alot - the recurse_limit will be reached more quickly.
It is safe to raise this as high as is necessary - so long as it is intentional.

=item Method C<-E<gt>valid_steps>

Returns a hashref of path steps that are allowed.  If step found in
default method path is not in the hash, the method path will return a
single step "forbidden" and run its hooks.  If no hash or undef is
returned, all paths are allowed (default).  A key "forbidden_step"
containing the step that was not valid will be placed in the stash.
Often the valid_steps method does not need to be defined as arbitrary
method calls are not possible with CGI::Ex::App.

=item Method C<-E<gt>previous_step, -E<gt>current_step, -E<gt>next_step, -E<gt>last_step, -E<gt>first_step>

Return the previous, current, next, last, and first step name - useful for figuring
out where you are in the path.  Note that first_step may not be the same
thing as default_step if the path was overridden.

=item Method C<-E<gt>pre_loop>

Called right before the navigation loop is started.  At this point the
path is set (but could be modified).  The only argument is a reference
to the path array.  If it returns a true value - the navigation
routine is aborted.

=item Method C<-E<gt>run_hook>

Calls "hook" to get a code ref which it then calls and returns the
result.  Arguments are the same as that for "hook".

=item Method C<-E<gt>hook>

Arguments are a hook name, a pathstep name, and an optional code sub
or default value (default value will be turned to a sub) (code sub
will be called as method of $self).

  my $code = $self->hook('info_complete', 'main', sub {return 0});
  ### will look first for $self->main_info_complete;
  ### will then look  for $self->info_complete;
  ### will then run       $self->$default_passed_sub; # sub {return 0}

This system is used to allow for multiple steps to be in the same
file and still allow for moving some steps out to external sub classed
packages.  If the application has successfully morphed then it is not
necessary to add the step name to the beginning of the method name as
the morphed packages method will override the base package (it is still
OK to use the full method name "${step}_hookname").

If a hook is found (or a default value is found) then an entry is added
to the arrayref contained in ->history.

=item Method C<-E<gt>morph>

Allows for temporarily "becoming" another object type for the
execution of the current step.  This allows for separating some steps
out into their own packages.  Morph will only run if the method
allow_morph returns true.  Additionally if the allow_morph returns a hash
ref, morph will only run if the step being morphed to is in the hash.
The morph call occurs at the beginning of the step loop.  A
corresponding unmorph call occurs before the loop is exited.  An
object can morph several levels deep if allow_nested_morph returns
true. For example, an object running as Foo::Bar that is looping on
the step "my_step" that has allow_morph = 1, will do the following:
call the hook morph_package (which would default to returning
Foo::Bar::MyStep in this case), translate this to a package filename
(Foo/Bar/MyStep.pm) and try and require it, if the file can be
required, the object is blessed into that package.  If that package
has a "fixup_after_morph" method, it is called.  The navigate loop
then continues for the current step.  At any exit point of the loop,
the unmorph call is made which reblesses the object into the original
package.

It is possible to call morph earlier on in the program.  An example of
a useful early use of morph would be as in the following code:

  sub allow_morph { 1 }

  sub pre_navigate {
    my $self = shift;
    if ($ENV{'PATH_INFO'} && $ENV{'PATH_INFO'} =~ s|^/(\w+)||) {
      my $step = $1;
      $self->morph($step);
      $ENV{'PATH_INFO'} = "/$step";
      $self->stash->{'base_morphed'} = 1;
    }
    return 0;
  }

  sub post_navigate {
    my $self = shift;
    $self->unmorph if $self->stash->{'base_morphed'};
  }

If this code was in a module Base.pm and the cgi running was cgi/base
and called:

  Base->navigate;
  # OR - for mod_perl resident programs
  Base->navigate->cleanup;
  # OR
  sub post_navigate { shift->cleanup }

and you created a sub module that inherited Base.pm called
Base/Ball.pm -- you could then access it using cgi/base/ball.  You
would be able to pass it steps using either cgi/base/ball/step_name or
cgi/base/ball?step=step_name - Or Base/Ball.pm could implement its
own path.  It should be noted that if you do an early morph, it is
suggested to provide a call to unmorph. And if you want to let your
early morphed object morph again - you will need to provide

  sub allow_nested_morph { 1 }

With allow_nested_morph enabled you could create the file
Base/Ball/StepName.pm which inherits Base/Ball.pm.  The Base.pm, with
the custom init and default path method, would automatically morph us
first into a Base::Ball object (during init) and then into a
Base::Ball::StepName object (during the navigation loop).

=item Method C<-E<gt>unmorph>

Allows for returning an object back to its previous blessed state.
This only happens if the object was previously morphed into another
object type.  Before the object is reblessed the method
"fixup_before_unmorph" is called if it exists.

=item Method C<-E<gt>allow_morph>

Boolean value.  Specifies whether or not morphing is allowed.
Defaults to the property "allow_morph" if found, otherwise false.
For more granularity, if true value is a hash, the step being
morphed to must be in the hash.

=item Method C<-E<gt>allow_nested_morph>

Boolean value.  Specifies whether or not nested morphing is allowed.
Defaults to the property "allow_nested_morph" if found, otherwise
false.  For more granularity, if true value is a hash, the step being
morphed to must be in the hash.

=item Hook C<-E<gt>morph_package>

Used by morph.  Return the package name to morph into during a morph
call.  Defaults to using the current object type as a base.  For
example, if the current object running is a Foo::Bar object and the
step running is my_step, then morph_package will return
Foo::Bar::MyStep.

=item Hook C<-E<gt>run_step>

Runs all of the hooks specific to each step, beginning with pre_step
and ending with post_step.  Called after ->morph($step) has been
run.  If this returns true, the nav_loop is exited (meaning the
run_step hook displayed the information).  If it returns false,
the nav_loop continues on to run the next step.  This is essentially
the same thing as a method defined in CGI::Applications ->run_modes.

=item Hook C<-E<gt>pre_step>

Ran at the beginning of the loop before prepare, info_compelete, and
finalize are called.  If it returns true, execution of nav_loop is
returned and no more steps are processed.

=item Hook C<-E<gt>skip>

Ran at the beginning of the loop before prepare, info_compelete, and
finalize are called.  If it returns true, nav_loop moves on to the
next step (the current step is skipped).

=item Hook C<-E<gt>prepare>

Defaults to true.  A hook before checking if the info_complete is true.

=item Hook C<-E<gt>info_complete>

Checks to see if all the necessary form elements have been passed in.
Calls hooks ready_validate, and validate.  Will not be run unless
prepare returns true (default).

=item Hook C<-E<gt>finalize>

Defaults to true. Used to do whatever needs to be done with the data once
prepare has returned true and info_complete has returned true.  On failure
the print operations are ran.  On success navigation moves on to the next
step.

=item Hook C<-E<gt>ready_validate>

Should return true if enough information is present to run validate.
Default is to look if $ENV{'REQUEST_METHOD'} is 'POST'.  A common
usage is to pass a common flag in the form such as 'processing' => 1
and check for its presence - such as the following:

  sub ready_validate { shift->form->{'processing'} }

=item Method C<-E<gt>set_ready_validate>

Sets that the validation is ready to validate.  Should set the value
checked by the hook ready_validate.  The following would complement the
processing flag above:

  sub set_ready_validate {
    my $self = shift;
    if (shift) {
      $self->form->{'processing'} = 1;
    } else {
      delete $self->form->{'processing'};
    }
  }

Note thate for this example the form key "processing" was deleted.  This
is so that the call to fill in any html forms won't swap in a value of
zero for form elements named "processing."

=item Hook C<-E<gt>validate>

Runs validation on the information posted in $self->form.  Uses
CGI::Ex::Validate for the validation.  Calls the hook hash_validation
to load validation information.  Should return true if enough
information is present to run validate.  Errors are stored as a hash
in $self->{hash_errors} via method add_errors and can be checked for
at a later time with method has_errors (if the default validate was
used).

Upon success, it will look through all of the items which
were validated, if any of them contain the keys append_path, insert_path,
or replace_path, that method will be called with the value as arguments.
This allows for the validation to apply redirection to the path.  A
validation item of:

  {field => 'foo', required => 1, append_path => ['bar', 'baz']}

would append 'bar' and 'baz' to the path should all validation succeed.

=item Hook C<-E<gt>hash_validation>

Returns a hash of the validation information to check form against.
By default, will look for a filename using the hook file_val and will
pass it to CGI::Ex::Validate::get_validation.  If no file_val is
returned or if the get_validation fails, an empty hash will be returned.
Validation is implemented by ->vob which loads a CGI::Ex::Validate object.

=item Hook C<-E<gt>file_val>

Returns a filename containing the validation.  Adds method
base_dir_rel to hook name_module, and name_step and adds on the
default file extension found in $self->ext_val which defaults to the
global $EXT_VAL (the property $self->{ext_val} may also be set).  File
should be readible by CGI::Ex::Validate::get_validation.

=item Hook C<-E<gt>js_validation>

Requires YAML.pm.
Will return Javascript that is capable of validating the form.  This
is done using the capabilities of CGI::Ex::Validate.  This will call
the hook hash_validation which will then be encoded into yaml and
placed in a javascript string.  It will also call the hook form_name
to determine which html form to attach the validation to.  The method
js_uri_path is called to determine the path to the appropriate
yaml_load.js and validate.js files.  If the method ext_val is htm,
then js_validation will return an empty string as it assumes the htm
file will take care of the validation itself.  In order to make use
of js_validation, it must be added to either the hash_base, hash_common, hash_swap or
hash_form hook (see examples of hash_base used in this doc).

=item Hook C<-E<gt>form_name>

Return the name of the form to attach the js validation to.  Used by
js_validation.

=item Method C<-E<gt>js_uri_path>

Return the URI path where the CGI/Ex/yaml_load.js and
CGI/Ex/validate.js files can be found.  This will default to
"$ENV{SCRIPT_NAME}/js" if the path method has not been overridden,
otherwise it will default to "$ENV{SCRIPT_NAME}?step=js&js=" (the
latter is more friendly with overridden paths).  A default handler for
the "js" step has been provided in "js_run_step" (this handler will
nicely print out the javascript found in the js files which are
included with this distribution - if valid_steps is defined, it must
include the step "js" - js_run_step will work properly with the
default "path" handler.

=item Hook C<-E<gt>hash_swap>

Called in preparation for print after failed prepare, info_complete,
or finalize.  Should contain a hash of any items needed to be swapped
into the html during print.  Will be merged with hash_base, hash_common, hash_form,
and hash_errors.  Can be populated by passing a hash to ->add_to_swap.

=item Hook C<-E<gt>hash_form>

Called in preparation for print after failed prepare, info_complete,
or finalize.  Defaults to ->form.  Can be populated by passing a hash
to ->add_to_form.

=item Hook C<-E<gt>hash_fill>

Called in preparation for print after failed prepare, info_complete,
or finalize.  Should contain a hash of any items needed to be filled
into the html form during print.  Items from hash_form, hash_base, and hash_common
will be layered on top during a print cycle.  Can be populated by passing
a hash to ->add_to_fill.

By default - forms are sticky and data from previous requests will
try and populate the form.  There is a method called ->no_fill which
will turn off sticky forms.

=item Method C<-E<gt>no_fill>

Passed the current step.  Should return boolean value of whether or not
to fill in the form on the printed page. (prevents sticky forms)

=item Hook C<-E<gt>hash_errors>

Called in preparation for print after failed prepare, info_complete,
or finalize.  Should contain a hash of any errors that occured.  Will
be merged into hash_form before the pass to print.  Eash error that
occured will be passed to method format_error before being added to
the hash.  If an error has occurred, the default validate will
automatically add {has_errors =>1}.  To the error hash at the time of
validation.  has_errors will also be added during the merge incase the
default validate was not used.  Can be populated by passing a hash to
->add_to_errors or ->add_errors.

=item Hook C<-E<gt>hash_common>

Almost identical in function and purpose to hash_base.  It is
intended that hash_base be used for common items used in various
scripts inheriting from a common CGI::Ex::App type parent.  Hash_common
is more intended for step level populating of both swap and fill.

=item Hook C<-E<gt>hash_base>

A hash of base items to be merged with hash_form - such as pulldown
menues.  It will now also be merged with hash_fill, so it can contain
default fillins.  Can be populated by passing a hash to ->add_to_base.
By default the following sub is what is used for hash_common (or something
similiar).  Note the use of values that are code refs - so that the
js_validation and form_name hooks are only called if requested:

  sub hash_base {
    my ($self, $step) = @_;
    return $self->{hash_base} ||= {
      script_name   => $ENV{SCRIPT_NAME},
      js_validation => sub { $self->run_hook('js_validation', $step) },
      form_name     => sub { $self->run_hook('form_name', $step) },
    };
  }

=item Hook C<-E<gt>name_module>

Return the name (relative path) that should be prepended to name_step
during the default file_print and file_val lookups.  Defaults to
base_name_module.

=item Hook C<-E<gt>name_step>

Return the step (appended to name_module) that should used when
looking up the file in file_print and file_val lookups.  Defaults to
the current step.

=item Hook C<-E<gt>file_print>

Returns a filename of the content to be used in the default print
hook.  Adds method base_dir_rel to hook name_module, and name_step and
adds on the default file extension found in $self->ext_print which
defaults to the global $EXT_PRINT (the property $self->{ext_print} may
also be set).  Should be a file that can be handled by hook print.

=item Hook C<-E<gt>print>

Take the information generated by prepared_print, format it, and print it out.
Default incarnation uses Template::Toolkit.  Arguments are: step name, form hashref,
and fill hashref.

=item Hook C<-E<gt>prepared_print>

Called when any of prepare, info_complete, or finalize fail.  Prepares
a form hash and a fill hash to pass to print.  The form hash is primarily
intended for use by the templating system.  The fill hash is intended
to be used to fill in any html forms.

=item Hook C<-E<gt>post_print>

A hook which occurs after the printing has taken place.  Is only run
if the information was not complete.  Useful for printing rows of a
database query.

=item Hook C<-E<gt>post_step>

Ran at the end of the step's loop if prepare, info_complete, and
finalize all returned true.  Allows for cleanup.  If a true value is
returned, execution of navigate is returned and no more steps are
processed.

=item Method C<-E<gt>post_loop>

Ran after all of the steps in the loop have been processed (if
prepare, info_complete, and finalize were true for each of the steps).
If it returns a true value the navigation loop will be aborted.  If it
does not return true, navigation continues by then inserting the step
$self->default_step and running $self->nav_loop again (recurses) to
fall back to the default step.

=item Method C<-E<gt>stash>

Returns a hashref that can store arbitrary user space data without
clobering the internals of the application.

=item Method C<-E<gt>add_property>

Takes the property name as an argument.  Creates an accessor that can
be used to access a new property.  If there were additional arguments
they will call the new accessor. Calling the new accessor with an
argument will set the property.  Using the accessor in an assignment
will also set the property (it is an lvalue).  Calling the accessor in
any other way will return the value.

=item Method C<-E<gt>cleanup>

Can be used at the end of execution to tear down the structure.
Default method starts a cleanup_cross_references call.

=item Method C<-E<gt>cleanup_cross_references>

Used to destroy links in nested structures.  Will spider through the
data structure of the passed object and remove any blessed objects
that are no weakly referenced.  This means if you have a reference to
an object in a global cache, that object should have its reference
weakened in the global cache.  Requires Scalar::Util to function.  Use
of this function is highly recommended in mod_perl environments to
make sure that there are no dangling objects in memory.  There are
some global caches that can't be fixed (such as Template::Parser's
reference to Template::Grammar in the Template::Toolkit).  For these
situations there is a %CLEANUP_EXCLUDE hash that contains the names of
Object types to exclude from the cleanup process.  Add any such global
hashes (or objects with references to the global hashes) there.

=back

=head1 OTHER APPLICATION MODULES

The concepts used in CGI::Ex::App are not novel or unique.  However, they
are all commonly used and very useful.  All application builders were
built because somebody observed that there are common design patterns
in CGI building.  CGI::Ex::App differs in that it has found more common design
patterns of CGI's.

CGI::Ex::App is intended to be sub classed, and sub sub classed, and each step
can choose to be sub classed or not.  CGI::Ex::App tries to remain simple
while still providing "more than one way to do it."  It also tries to avoid
making any sub classes have to call ->SUPER::.

There are certainly other modules for building CGI applications.  The
following is a short list of other modules and how CGI::Ex::App is
different.

=over 4

=item C<CGI::Application>

Seemingly the most well know of application builders.
CGI::Ex::App is different in that it:

  * Uses Template::Toolkit by default
      CGI::Ex::App can easily use another toolkit by simply
      overriding the ->print method.
      CGI::Application uses HTML::Template.
  * Offers integrated data validation.
      CGI::Application has had custom addons created that
      add some of this functionality.  CGI::Ex::App has the benefit
      that once validation is created,
  * Allows the user to print at any time (so long as proper headers
      are sent.  CGI::Application requires data to be pipelined.
  * Offers hooks into the various phases of each step ("mode" in
      CGI::Application lingo).  CGI::Application essentially
      provides ->runmode
  * Support for easily jumping around in navigation steps.
  * Support for storing some steps in another package.

CGI::Ex::App and CGI::Application are similar in that they take care
of handling headers and they allow for calling other "runmodes" from
within any given runmode.  CGI::Ex::App's ->run_step is essentially
equivalent to a method call defined in CGI::Application's ->run_modes.
The ->run method of CGI::Application starts the application in the same
manner as CGI::Ex::App's ->navigate call.  Many of the hooks around
CGI::Ex::App's ->run_step call are similar in nature to those provided by
CGI::Application.

=item C<CGI::Prototype>

There are actually many simularities.  One of the nicest things about
CGI::Prototype is that it is extremely short (very very short).  The
->activate starts the application in the same manner as CGI::Ex::App's
=>navigate call.  Both use Template::Tookit as the default template system.
CGI::Ex::App is differrent in that it:

  * Offers integrated data validation.
      CGI::Application has had custom addons created that
      add some of this functionality.  CGI::Ex::App has the benefit
      that once validation is created,
  * Offers more hooks into the various phases of each step.
  * Support for easily jumping around in navigation steps.
  * Support for storing some steps in another package.

=item C<CGI::Path>

CGI::Path and CGI::Ex::App are fairly similar in may ways as they
were created under similar lines of thought.  The primary difference
in these two is that CGI::Ex::App:

  * Does not provide "automated path following" based on
      validated key information.  CGI::Path works well for
      wizard based applications.  CGI::Ex::App assumes that
      the application will chose it's own path (it works very
      well in non-linear paths - it also works fine in
      linear paths but it doesn't provide some of magic that
      CGI::Path provides).
  * Does not provide integrated session support.  CGI::Path
      requires it for path navigation.  CGI::Ex::App assumes that
      if session support or authentication is needed by an
      application, a custom Application layer that inherits
      from CGI::Ex::App will be written to provide this support.
  * Offers more granularity in the navigation phase.  CGI::Path
      has successfully been used as a sub class of CGI::Ex::App
      with limited modifications.

=back

=head1 BUGS

Uses CGI::Ex for header support by default - which means that support
for mod_perl 2 is limited at this point.

There are a lot of hooks.  Actually this is not a bug.  Some may
prefer not calling as many hooks - they just need to override
methods high in the chain and subsequent hooks will not be called.

=head1 THANKS

Bizhosting.com - giving a problem that fit basic design patterns.
Earl Cahill    - pushing the idea of more generic frameworks.
Adam Erickson  - design feedback, bugfixing, feature suggestions.
James Lance    - design feedback, bugfixing, feature suggestions.

=head1 AUTHOR

Paul Seamons

=cut
