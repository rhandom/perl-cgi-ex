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

$VERSION = '1.13';
use CGI::Ex::Dump qw(debug);

BEGIN {
  ### Default file locations
  ### these are used for the provided stub functions - if you are not
  ### using the stub functions - then you won't need to worry about these
  $EXT_PRINT ||= 'html';
  $EXT_VAL   ||= 'val';
  $BASE_DIR_REL ||= ''; # relative path - stub methods will look in $BASE_DIR_REL/dir/of/content.html
  $BASE_DIR_ABS ||= ''; # content should be found at "$BASE_DIR_ABS/$BASE_DIR_REL/dir/of/content.html"
  $BASE_NAME_MODULE ||= ''; # the cgi name

  ### the base stub functions use Template Toolkit and CGI::Ex::Validate
  ### If you are mod_perl and are using the stub functions - you may want
  ### to make sure that Template and CGI::Ex::Validate are loaded at server startup
  ### 
  #if ($ENV{MOD_PERL}) {
  #  require Template;
  #  require CGI::Ex::Validate;
  #}

  ### list of modules to exclude during cleanup
  ### this takes care of situations such as
  ### template toolkits rules area which contains
  ### a nested structure of rules - which are somehow
  ### referenced in other places
  $CLEANUP_EXCLUDE{'Template::Parser'} = 1;
}


###----------------------------------------------------------------###

sub new {
  my $class = shift || __PACKAGE__;
  my $self  = ref($_[0]) ? shift : {@_};
  bless $self, $class;
  $self->init();
  return $self;
}

sub init {}

###----------------------------------------------------------------###

sub navigate {
  my $self = shift;
  my $args = ref($_[0]) ? shift : {@_};
  $self = $self->new($args) if ! ref $self;

  ### run the step loop
  eval { $self->nav_loop };

  ### catch errors - if any
  if ($@) {
    if ($@ eq "Long Jump\n") {
      # do nothing - we had to long jump out of recursive navigate_loop calls
    } else {
      $self->handle_error($@);
    }
  }

  return $self;
}

sub nav_loop {
  my $self = shift;

  ### keep from an infinate nesting
  local $self->{recurse} = $self->{recurse} || 0;
  if ($self->{recurse} ++ >= $self->recurse_limit) {
    die "recurse_limit reached (".$self->recurse_limit.")";
  }

  ### get the path (simple array based thing)
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
    next if $step !~ /^\w+$/; # don't process the step if it contains odd characters

    ### check if this is an allowed step
    if ($valid_steps) {
      if (! $valid_steps->{$step}
          && $step ne $self->default_step
          && $step ne 'forbidden') {
        $self->stash->{'forbidden_step'} = $step;
        $self->replace_path('forbidden');
        next;
      }
    }

    ### allow for putting some steps in external files
    $self->morph($step);

    ### if the pre_step exists and returns true, return from navigate
    if ($self->run_hook($step, 'pre_step')) {
      $self->unmorph($step);
      return;
    }

    ### see if we have complete valid information for this step
    ### if so, do the next step
    ### if not, get necessary info and print it out
    if (   ! $self->run_hook($step, 'prepare', 1)
        || ! $self->run_hook($step, 'info_complete')
        || ! $self->run_hook($step, 'finalize', 1)) {
      my $hash_form = $self->run_hook($step, 'hash_form');
      my $hash_fill = $self->run_hook($step, 'hash_fill');
      my $hash_errs = $self->run_hook($step, 'hash_errors');
      my $hash_comm = $self->run_hook($step, 'hash_common');

      ### fix up errors
      $hash_errs->{$_} = $self->format_error($hash_errs->{$_})
        foreach keys %$hash_errs;
      $hash_errs->{has_errors} = 1 if scalar keys %$hash_errs;

      ### layer hashes together (micro-optimized)
      my $form = {%$hash_comm, %$hash_errs, %$hash_form};
      my $fill = {%$hash_comm, %$hash_form, %$hash_fill};

      ### run the print hook - passing it the form and fill info
      $self->run_hook($step, 'print', undef,
                      $form, $fill);

      ### a hook after the printing process
      $self->run_hook($step, 'post_print');

      $self->unmorph($step);

      return;
    }

    ### a hook before end of loop
    ### if the post_step exists and returns true, return from navigate
    if ($self->run_hook($step, 'post_step')) {
      $self->unmorph($step);
      return;
    }

    $self->unmorph($step);
  }

  ### allow for one exit point after the loop
  return if $self->post_loop($path); # a true value means to abort the navigate

  ### run the default step as a last resort
  $self->insert_path($self->default_step);
  $self->nav_loop; # go recursive

  return;
}

sub recurse_limit { shift->{'recurse_limit'} || $RECURSE_LIMIT || 10 }

sub exit_nav_loop {
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
  if ($i) {
    $i = - $path_i - 1     if $i =~ /first/i;
    $i = $#$path - $path_i if $i =~ /last/i;
  }
  die "Invalid jump index ($i)" if $i !~ /^-?\d+$/;

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

  ### run the newly fixed up path (recursively)
  $self->{path_i} ++; # move along now that the path is updated
  $self->nav_loop;

  $self->exit_nav_loop;
}

sub default_step {
  my $self = shift;
  return $self->{'default_step'} || 'main';
}

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
    } elsif ($ENV{PATH_INFO} && $ENV{PATH_INFO} =~ m|^/(\w+)|) {
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
  return $self->step_by_path_index( ($self->{path_i} || 0) - 1 );
}

sub current_step {
  my $self = shift;
  return $self->step_by_path_index( ($self->{path_i} || 0) );
}

sub next_step {
  my $self = shift;
  return $self->step_by_path_index( ($self->{path_i} || 0) + 1 );
}

###----------------------------------------------------------------###

sub pre_loop {}
sub post_loop {}

### return the appropriate hook to call
sub hook {
  my $self    = shift;
  my $step    = shift || '';
  my $hook    = shift || die "Missing hook name";
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
  my $step    = shift;
  my $hook    = shift;
  my $default = shift;
  my $code = $self->hook($step, $hook, $default);
  return $self->$code($step, @_);
}

sub history {
  return shift->{'history'} ||= [];
}

### default die handler - show what happened and die (so its in the error logs)
sub handle_error {
  my $self = shift;
  my $err  = shift;
  debug $err, $self->path, $self->history;
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
  return if ! $self->allow_morph;

  ### which package we are blessing into
  my $new = $self->run_hook($step, 'morph_package');
  my $cur = ref $self;

  ### store the lineage
  my $ref = $self->{'_morph_lineage'} ||= [];

  ### make sure we haven't already been reblessed
  if (! $self->allow_nested_morph && $#$ref != -1) {
    push @$ref, $cur; # needed so unmorph does the right thing
    return;
    # This is also possible if we want to die but it
    # is much nicer to allow us to morph early
    #my @old = @$ref;
    #my $err = "morph calls may not be nested: orig (@old), current ($cur), new ($new)";
    #debug $err;
    #die $err;
  }

  ### store our lineage
  push @$ref, $cur;

  ### if we are not already that package - bless us there
  if ($cur ne $new) {
    my $file = $new .'.pm';
    $file =~ s|::|/|g;
    if (eval { require $file }) { # check for the file that holds this package
      ### become that package
      bless $self, $new;
      if (my $method = $self->can('fixup_after_morph')) {
        $self->$method($step);
      }
    }
  }

}

sub unmorph {
  my $self = shift;
  my $step = shift;
  my $ref  = $self->{'_morph_lineage'} || return;
  my $cur  = ref $self;
  my $prev = pop(@$ref) || die "unmorph called more times than morph current ($cur)";

  ### if we are not already that package - bless us there
  if ($cur ne $prev) {
    if (my $method = $self->can('fixup_before_unmorph')) {
      $self->$method($step);
    }
    bless $self, delete $self->{'_parent_pckg'};
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

  my $form_name = $self->run_hook($step, 'form_name');
  my $hash_val  = $self->run_hook($step, 'hash_validation', {});
  my $js_uri    = $self->js_uri_path;
  return '' if ! scalar keys %$hash_val;

  return $self->vob->generate_js($hash_val, $form_name, $js_uri);
}

### where to find the javascript files
### default to using this script as a handler
sub js_uri_path {
  my $self   = shift;
  my $script = $ENV{'SCRIPT_NAME'} || die "Missing SCRIPT_NAME";
  return ($self->can('path') == \&CGI::Ex::App::path)
    ? $script . '/js' # try to use a cache friendly URI (if path is our own)
    : $script . '?'.$self->step_key.'=js&js='; # use one that works with more paths
}

### name to attach js validation to
sub form_name { 'theform' }

### provide some rudimentary javascript support
### if valid_steps is defined - it should include "js"
sub js_pre_step {
  my $self = shift;

  ### make sure path info looks like /js/CGI/Ex/foo.js
  my $file = $self->form->{'js'} || $ENV{'PATH_INFO'} || '';
  $file = ($file =~  m!^(?:/js/|/)?(\w+(?:/\w+)*\.js)$!) ? $1 : '';

  $self->cgix->print_js($file);
  return 1;
}

###----------------------------------------------------------------###
### implementation specific subs

sub template_args {
  my $self = shift;
  my $step = shift;
  return {
    INCLUDE_PATH => $self->base_dir_abs,
  };
}

sub print {
  my $self = shift;
  my $step = shift;
  my $form = shift;
  my $fill = shift;

  ### get a filename relative to base_dir_abs
  my $file = $self->run_hook($step, 'file_print');

  require Template;
  my $t = Template->new($self->template_args($step));

  ### process the document
  my $out = '';
  my $status = $t->process($file, $form, \$out) || die $Template::ERROR;

  ### fill in any forms
  $self->cgix->fill(\$out, $fill) if $fill && ! $self->{no_fill};

  ### now print
  $self->cgix->print_content_type();
  print $out;
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
  return 1 if $self->{hash_errors} && scalar keys %{ $self->{hash_errors} };
}

sub add_errors {
  my $self = shift;
  my $args = ref($_[0]) ? shift : {@_};
  $self->{hash_errors} ||= {};
  foreach my $key (keys %$args) {
    my $_key = ($key =~ /_error$/) ? $key : "${key}_error";
    if ($self->{hash_errors}->{$_key}) {
      $self->{hash_errors}->{$_key} .= '<br>' . $args->{$key};
    } else {
      $self->{hash_errors}->{$_key} = $args->{$key};
    }
  }
  $self->{hash_errors}->{has_errors} = 1;
}

sub format_error {
  my $self  = shift;
  my $error = shift;
#  return $error if $error =~ /<span/i;
#  return "<span class=\"error\">$error</span>";
}

###----------------------------------------------------------------###
### default stub subs

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

  my $base_dir_rel = $self->base_dir_rel;
  my $module       = $self->run_hook($step, 'name_module');
  my $_step        = $self->run_hook($step, 'name_step', $step);
  my $ext          = $self->ext_print;

  return "$base_dir_rel/$module/$_step.$ext";
}

### which file is used for validation
sub file_val {
  my $self = shift;
  my $step = shift;

  my $base_dir = $self->base_dir_rel;
  my $module   = $self->run_hook($step, 'name_module');
  my $_step    = $self->run_hook($step, 'name_step', $step);
  my $ext      = $self->ext_val;

  ### get absolute if necessary
  if ($base_dir !~ m|^/|) {
    $base_dir = $self->base_dir_abs . "/$base_dir";
  }

  return "$base_dir/$module/$_step.$ext";
}


sub info_complete {
  my $self = shift;
  my $step = shift;

  return 0 if ! $self->run_hook($step, 'ready_validate');

  return $self->run_hook($step, 'validate');
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
  my $form = $self->form;
  my $hash = $self->run_hook($step, 'hash_validation', {});
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
  foreach my $ref (@$what_was_validated) {
    foreach my $method (qw(append_path replace_path insert_path)) {
      next if ! ($val = $ref->{$method});
      $self->$method(ref $val ? @$val : $val);
      last;
    }
  }

  return 1;
}

### allow for using ConfUtil instead of yaml
sub hash_validation {
  my $self = shift;
  my $step = shift;
  return $self->{hash_validation}->{$step} ||= do {
    my $hash;
    my $file = $self->run_hook($step, 'file_val');

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

sub hash_common {
  my $self = shift;
  my $step = shift;
  return $self->{hash_common} ||= {
### don't force these to always be there
#    js_validation => $self->run_hook($step, 'js_validation'),
#    form_name     => $self->run_hook($step, 'form_name'),
  };
}

sub hash_errors {
  my $self = shift;
  return $self->{hash_errors} ||= {};
}

sub hash_form {
  my $self = shift;
  return $self->form;
}

sub hash_fill {
  my $self = shift;
  return $self->{hash_fill} ||= {};
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

=head1 SYNOPSIS

More examples will come with time.  Here are the basics for now.

  #!/usr/bin/perl -w

  MyApp->navigate;
   # OR you could do the following which cleans
   # circular references - useful for a mod_perl situation
   # MyApp->navigate->cleanup;
  exit;

  package MyApp;
  use strict;
  use base qw(CGI::Ex::App);
  use CGI::Ex::Dump qw(debug);

  sub valid_paths { return {success => 1, js => 1} }
    # default_step (main) is a valid path
    # note the inclusion of js step for js_validation

  # base_dir_abs is only needed if default print is used
  # template toolkit needs an INCLUDE_PATH
  sub base_dir_abs { '/tmp' }

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

  sub post_print { debug shift->history } # show what happened

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
    $self->append_path('success');
    $self->set_ready_validate(0);
    return 1;
  }

  sub success_file_print {
    \ "<h1>Success Step</h1> All done";
  }

  sub hash_common { # used to include js_validation
    my $self = shift;
    my $step = shift;
    return $self->{hash_common} ||= {
      script_name   => $ENV{SCRIPT_NAME},
      js_validation => $self->run_hook($step, 'js_validation'),
      form_name     => $self->run_hook($step, 'form_name'),
    };
  }

  __END__

Note: This example would be considerably shorter if the html file
(file_print) and the validation file (file_val) had been placed in
separate files.  Though CGI::Ex::App will work "out of the box" as
shown it is more probable that any platform using it will customize
the various hooks to their own tastes (for example, switching print to
use a system other than Template::Toolkit).

=head1 HOOKS / METHODS

Hooks are basically methods calls that look for a variety of method
names.  See the discussion under the method named "hook" for more
details.  Hooks and methods are looked for in the following order:

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
assured to run.  There is a method ->recurse_limit (default 10) that
will catch logic errors (such as inadvertently running the same
step over and over and over).

The basic outline of navigation is as follows (the default actions for hooks
are shown):

  navigate {
    eval { ->nav_loop }
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

      ->pre_step (hook)
        # skips this step if true

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
        ->hash_form (hook)
        ->hash_fill (hook)
        ->hash_errors (hook)
        ->hash_common (hook)

        # merge common, errors, and form into merged form
        # merge common, form, and fill into merged fill

        ->print (hook - passed current step, merged form hash, and merged fill)
          # DEFAULT ACTION
          # ->file_print (hook - uses base_dir_rel, name_module, name_step, ext_print)
          # ->template_args
          # Processes the file with Template Toolkit
          # Fills the any forms with CGI::Ex::Fill
          # Prints headers and the content

        ->post_print (hook - used for anything after the print process)

        # return from navigation
      }

      ->post_step (hook)

      ->unmorph (actually called any time the step exits the loop)
        # DEFAULT ACTION
        # ->fixup_before_unmorph if blessed to previous package

    } end of step foreach

    ->post_loop
      # navigation stops if true

    ->default_step (inserted into path at current location)
    ->nav_loop (called again recursively)

  } end of nav_loop

=item Method C<-E<gt>history>

Returns an arrayref of which hooks of which steps of the path were ran.
Useful for seeing what happened.

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
counter (which has a limit of ->recurse_limit - default 10).  It is normally
better to allow the other hooks in the loop to carry on their normal functions
and avoid jumping.

Jump takes a single argument which is the location in the path to jump to.
The default value, 1, indicates that we should jump to the next step (the
default action for jump).  A value of 0 would repeat the current step (watch
out for recursion).  A value of -1 would jump to the previous step.  The
special value of "last" will jump to the last step.  The special value of
"first" will jump back to the first step.  In each of these cases, the
path array retured by ->path is modified to allow for the jumping.

=item Method C<-E<gt>exit_nav_loop>

This method should not normally used.  It allows for a long jump to the
end of all nav_loops (even if they are recursively nested).  This
effectively short circuits all remaining hooks for the current and
remaining steps.  It is used to allow the ->jump functionality.

=item Method C<-E<gt>recurse_limit>

Default 10.  Maximum number of times to allow nav_loop to call itself.
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

=item Method C<-E<gt>previous_step, -E<gt>current_step, -E<gt>next_step>

Return the previous, current, and next step name - useful for figuring
out where you are in the path.

=item Method C<-E<gt>pre_loop>

Called right before the navigation loop is started.  At this point the
path is set (but could be modified).  The only argument is a reference
to the path array.  If it returns a true value - the navigation
routine is aborted.

=item Method C<-E<gt>run_hook>

Calls "hook" to get a code ref which it then calls and returns the
result.  Arguments are the same as that for "hook".

=item Method C<-E<gt>hook>

Arguments are a pathstep name, a hook name, and an optional code sub
or default value (default value will be turned to a sub) (code sub
will be called as method of $self).

  my $code = $self->hook('main', 'info_complete', sub {return 0});
  ### will look first for $self->main_info_complete;
  ### will then look  for $self->info_complete;
  ### will then run       $self->$default_passed_sub; # sub {return 0}

=item Method C<-E<gt>handle_error>

If anything dies during execution, handle_error will be called with
the error that had happened.  Default is to debug the error and path
history.

=item Method C<-E<gt>morph>

Allows for temporarily "becoming" another object type for the
execution of the current step.  This allows for separating some steps
out into their own packages.  Morph will only run if the method
allow_morph returns true.  The morph call occurs at the beginning of
the step loop.  A corresponding unmorph call occurs before the loop is
exited.  An object can morph several levels deep if allow_nested_morph
returns true. For example, an object running as Foo::Bar that is
looping on the step "my_step" that has allow_morph = 1, will do the
following: call the hook morph_package (which would default to
returning Foo::Bar::MyStep in this case), translate this to a package
filename (Foo/Bar/MyStep.pm) and try and require it, if the file can
be required, the object is blessed into that package.  If that package
has a "fixup_after_morph" method, it is called.  The navigate loop
then continues for the current step.  At any exit point of the loop,
the unmorph call is made which reblesses the object into the original
package.

It is possible to call morph earlier on in the program.  An example of
a useful early use of morph would be as in the following code:

  sub init {
    my $self = shift;
    $self->SUPER::init;

    return if ! $ENV{'PATH_INFO'};
    my $info = ($ENV{'PATH_INFO'} =~ s|^/(\w+)||) ? $1 : return;
    $self->morph($info);
  }

If this code was in a module Base.pm and the cgi running was cgi/base
and called:

  Base->navigate->unmorph;
  # OR - for mod_perl resident programs
  Base->navigate->unmorph->cleanup;

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

=item Method C<-E<gt>allow_nested_morph>

Boolean value.  Specifies whether or not nested morphing is allowed.
Defaults to the property "allow_nested_morph" if found, otherwise
false.

=item Hook C<-E<gt>morph_package>

Used by morph.  Return the package name to morph into during a morph
call.  Defaults to using the current object type as a base.  For
example, if the current object running is a Foo::Bar object and the
step running is my_step, then morph_package will return
Foo::Bar::MyStep.

=item Hook C<-E<gt>pre_step>

Ran at the beginning of the loop before info_compelete is called.  If
it returns true, execution of navigate is returned and no more steps
are processed.

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

  sub set_ready_validate { shift->form->{'processing'} = shift }

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

Will return Javascript that is capable of validating the form.  This
is done using the capabilities of CGI::Ex::Validate.  This will call
the hook hash_validation which will then be encoded into yaml and
placed in a javascript string.  It will also call the hook form_name
to determine which html form to attach the validation to.  The method
js_uri_path is called to determine the path to the appropriate
yaml_load.js and validate.js files.  If the method ext_val is htm,
then js_validation will return an empty string as it assumes the htm
file will take care of the validation itself.  In order to make use
of js_validation, it must be added to either the hash_common or
hash_form hook (see examples of hash_common used in this doc).

=item Hook C<-E<gt>form_name>

Return the name of the form to attach the js validation to.  Used by
js_validation.

=item Method C<-E<gt>js_uri_path>

Return the URI path where the CGI/Ex/yaml_load.js and
CGI/Ex/validate.js files can be found.  This will default to
"$ENV{SCRIPT_NAME}/js" if the path method has not been overridden,
otherwise it will default to "$ENV{SCRIPT_NAME}?step=js&js=" (the
latter is more friendly with overridden paths).  A default handler for
the "js" step has been provided in "js_pre_step" (this handler will
nicely print out the javascript found in the js files which are
included with this distribution - if valid_steps is defined, it must
include the step "js" - js_pre_step will work properly with the
default "path" handler.

=item Hook C<-E<gt>hash_form>

Called in preparation for print after failed prepare, info_complete,
or finalize.  Should contain a hash of any items needed to be swapped
into the html during print.

=item Hook C<-E<gt>hash_fill>

Called in preparation for print after failed prepare, info_complete,
or finalize.  Should contain a hash of any items needed to be filled
into the html form during print.  Items from hash_form will be layered
on top during a print cycle.

=item Hook C<-E<gt>hash_errors>

Called in preparation for print after failed prepare, info_complete,
or finalize.  Should contain a hash of any errors that occured.  Will
be merged into hash_form before the pass to print.  Eash error that
occured will be passed to method format_error before being added to
the hash.  If an error has occurred, the default validate will
automatically add {has_errors =>1}.  To the error hash at the time of
validation.  has_errors will also be added during the merge incase the
default validate was not used.

=item Hook C<-E<gt>hash_common>

A hash of common items to be merged with hash_form - such as pulldown
menues.  It will now also be merged with hash_fill, so it can contain
default fillins.  By default it is empty, but it would be wise to add the
following to allow for js_validation (as needed):

  sub hash_common {
    my $self = shift;
    my $step = shift;
    return $self->{hash_common} ||= {
      script_name   => $ENV{SCRIPT_NAME},
      js_validation => $self->run_hook($step, 'js_validation'),
      form_name     => $self->run_hook($step, 'form_name'),
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

Take the information and print it out.  Default incarnation uses
Template.  Arguments are: step name, form hashref, and fill hashref.

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
clobering the internals of the Application.

=item Method C<-E<gt>add_property>

Takes the property name as an argument.  Creates an accessor that can
be used to access a new property.  Calling the new accessor with an
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

=cut
