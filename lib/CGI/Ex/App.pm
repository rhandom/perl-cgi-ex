package CGI::Ex::App;

### CGI Extended Application

###----------------------------------------------------------------###
#  Copyright 2004 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom


use strict;
use vars qw($EXT_PRINT $EXT_VAL $BASE_DIR_REL $BASE_DIR_ABS);

use CGI::Ex::Dump qw(dex);

BEGIN {
  ### Default file locations
  ### these are used for the provided stub functions - if you are not
  ### using the stub functions - then you won't need to worry about these
  $EXT_PRINT ||= 'html';
  $EXT_VAL   ||= 'val';
  $BASE_DIR_REL ||= ''; # relative path - stub methods will look in $BASE_DIR_REL/dir/of/content.html
  $BASE_DIR_ABS ||= ''; # content should be found at "$BASE_DIR_ABS/$BASE_DIR_REL/dir/of/content.html"

  ### the base stub functions use Template Toolkit and CGI::Ex::Validate
  ### If you are mod_perl and are using the stub functions - you may want
  ### to make sure that Template and CGI::Ex::Validate are loaded at server startup
  ### 
  #if ($ENV{MOD_PERL}) {
  #  require Template;
  #  require CGI::Ex::Validate;
  #}
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

  local $self->{recurse} = ($self->{recurse} || 0) + 1;

  eval {

    ### keep from an infinate nesting
    die "Too much recursion ($self->{recurse})" if $self->{recurse} >= 5;

    ### get the path (simple array based thing)
    my $path = $args->{path} || $self->path;

    ### allow for a hook
    $self->pre_loop($path);

    ### iterate on each step of the path
    foreach my $step (@$path) {
      $self->morph($step);

      ### if the pre_step exists and returns true, return from navigate
      my $hook = $self->hook($step,'pre_step');
      if ($hook && $self->$hook($step)) {
        $self->unmorph($step);
        return;
      }

      ### see if we have complete valid information for this step
      ### if so, do the next step
      ### if not, get necessary info and print it out
      if (! $self->run_hook($step,'info_complete')) {
        my $formhash = $self->run_hook($step, 'hash_form',   \&form);
        my $fillhash = $self->run_hook($step, 'hash_fill',   {});
        my $errhash  = $self->run_hook($step, 'hash_errors', {});
        my $commhash = $self->run_hook($step, 'hash_common', {});

        ### layer common elements on top of form
        foreach my $key (keys %$commhash) {
          next if exists $formhash->{$key};
          $formhash->{$key} = $commhash->{$key};
        }

        ### layer errors on top of form
        $formhash->{has_errors} = 1 if scalar keys %$errhash;
        foreach my $key (keys %$errhash) {
          $formhash->{$key} = $self->format_error($errhash->{$key});
        }

        ### run the print hook - passing it the form and fill info
        $self->run_hook($step, 'print', undef,
                        $formhash, $fillhash);

        ### a hook after the printing process
        $self->run_hook($step, 'post_print');

        $self->unmorph($step);

        return;
      }

      ### a hook before end of loop
      ### if the post_step exists and returns true, return from navigate
      $hook = $self->hook($step,'post_step');
      if ($hook && $self->$hook($step)) {
        $self->unmorph($step);
        return;
      }

      $self->unmorph($step);
    }

    ### allow for one more hook after the loop
    $self->post_loop($path);

    ### run the main step as a last resort
    return $self->navigate({path => ['main']});

  }; # end of eval

  ### catch errors if any
  if ($@) {
    if ($self->{recurse} == 1) {
      if (my $meth = $self->can('handle_error')) {
        $self->$meth("$@");
        return;
      }
    }
    die $@;
  }

  return;
}

sub path_key {
  return 'step';
}

### determine the path to follow
sub path {
  my $self = shift;
  return $self->{path} ||= do {
    my $form = $self->form;
    my $key  = $self->path_key;
    my $step = $form->{$key};
    
    if (! $step && $ENV{PATH_INFO} && $ENV{PATH_INFO} =~ m|^/(\w+)|) {
      $step = lc($1);
    }

    ### check if this is an allowed path
    if ($step) {
      my $valid = $self->valid_paths;
      if ($valid) {
        if (UNIVERSAL::isa($valid, 'HASH')) {
          $step = 'forbidden' if ! $valid->{$step};
        } elsif (UNIVERSAL::isa($valid, 'CODE')) {
          $step = 'forbidden' if ! &{ $valid }($step);
        }
      }
    }
    
    ### if no step don't do anything
    $step ? [$step] : []; # return of the do
  };
}

### really should only be used during initialization
sub set_path {
  my $self = shift;
  my $path = $self->{path} ||= [];
  splice @$path, 0, $#$path + 1, @_; # change entries in the ref
}

sub add_to_path {
  my $self = shift;
  push @{ $self->path }, @_;
}

### a hash of paths that are allowed, default undef is all
sub valid_paths {}

sub pre_loop {}
sub post_loop {}

### return the appropriate hook to call
sub hook {
  my $self    = shift;
  my $step    = shift || '';
  my $hook    = shift || die "Missing hook name";
  my $default = shift;
  my $hist    = $self->{history} ||= [];
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

### default error - just show what happened
sub handle_error {
  my $self = shift;
  my $err  = shift;
  dex $err, $self->{history};
}

###----------------------------------------------------------------###
### utility modules for jeckyl/hyde on self

sub morph {}

sub unmorph {}

sub add_property {
  my $self = shift;
  my $prop = shift;
  no strict 'refs';
  my $name = __PACKAGE__ ."::". $prop;
  *$name = sub : lvalue {
    my $self = shift;
    $self->{$prop} = shift() if $#_ != -1;
    $self->{$prop};
  } if ! defined &$name;
  $self->$prop(shift()) if $#_ != -1;
}

###----------------------------------------------------------------###
### implementation specific subs

sub cgix {
  return shift()->{cgix} ||= do {
    require CGI::Ex;
    CGI::Ex->new(); # return of the do
  };
}

sub form {
  my $self = shift;
  if ($#_ != -1) { ### allow for setting of form
    return $self->{form} = shift;
  }
  return $self->{form} ||= $self->cgix->get_form;
}

sub vob {
  return shift()->{vob} ||= do {
    require CGI::Ex::Validate;
    CGI::Ex::Validate->new; # return of the do
  };
}

sub print {
  my $self = shift;
  my $step = shift;
  my $form = shift;
  my $fill = shift;

  my $file = $self->run_hook($step, 'file_print');

  require Template;
  return Template->print($file, $form, $fill);
}

sub base_dir_rel {
  my $self = shift;
  return $self->{base_dir_rel} ||= $BASE_DIR_REL;
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

sub name_module {
  my $self = shift;
  my $step = shift || '';
  return ($0 =~ m|(\w+)$|) ? $1
    : die "Couldn't determine module name from \"name_module\" lookup ($step)";
}

sub file_print {
  my $self = shift;
  my $step = shift;

  my $base_dir_rel = $self->base_dir_rel;
  my $module       = $self->run_hook($step, 'name_module');
  my $_step        = $self->run_hook($step, 'name_step', $step);
  my $ext          = $self->{file_print_ext} || $EXT_PRINT;

  return "$base_dir_rel/$module/$_step.$ext";
}

sub file_val {
  my $self = shift;
  my $step = shift;

  my $base_dir = $self->base_dir_rel;
  my $module   = $self->run_hook($step, 'name_module');
  my $_step    = $self->run_hook($step, 'name_step', $step);
  my $ext      = $self->{file_val_ext} || $EXT_VAL;

  ### get absolute if necessary
  $base_dir = do {
    $BASE_DIR_ABS ||= do {
      die "BASE_DIR_ABS not set for use in stub functions";
    };
    "$BASE_DIR_ABS/$base_dir";
  } if $base_dir !~ m|^/|;
  
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

  my $eob = eval { $self->vob->validate($form, $hash) };
  if (! $eob && $@) {
    die "Step $step: $@";
  }

  ### if no errors return true
  return 1 if ! $eob;

  ### store the errors and return false
  $self->add_errors($eob->as_hash({
    as_hash_join   => "<br>\n",
    as_hash_suffix => '_error',
  }));

  return 0;
}

sub hash_validation {
  my $self = shift;
  my $step = shift;
  my $file = $self->run_hook($step, 'file_val');
  require CGI::Ex::Validate;
  return CGI::Ex::Validate->new->get_validation($file);
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
  my $step = shift;
  return $self->run_hook($step, 'hash_form', {});
}

###----------------------------------------------------------------###

sub forbidden_info_complete {
  return 0;
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex::App - Simple CGI::Application type module

=head1 DESCRIPTION

Fill in the blanks and get a ready made CGI.  This module is
somewhat similar to CGI::Application and CGI::Path and CGI::Builder
and any other "CGI framework."  As with the others, CGI::App tries
to do as much as possible, in a simple manner, without getting in
the developer's way.  Your milage may vary.

=head1 HOOKS / METHODS

Hooks are basically methods calls that look for a variety of method
names.  See the discussion under the method named "hook" for more details.
Hooks and methods are looked for in the following order:

=over 4

=item Method C<-E<gt>new>

Object creator.  Takes a hash or hashref.

=item Method C<-E<gt>init>

Called by the default new method.  Allows for any object initilizations.

=item Method C<-E<gt>form>

Returns a hashref of the items passed to the CGI.  Returns $self->{form}.
Defaults to CGI::Ex::get_form.

=item Method C<-E<gt>navigate>

This is the main loop runner.  Figures out path and runs all
of the appropriate hooks.  Once all steps in the path run successfully,
it will call it self with a path set to ['main'] to allow for a default
main path to run.

=item Method C<-E<gt>path>

Return an arrayref (modifyable) of the steps in the path.
For each step the remaining hooks can be run.  Hook methods
are looked up and ran using the method "run_hook" which uses
the method "hook" to lookup the hook.  A history of ran hooks
is stored in $self->{history}.  Default will be a single step
path looked up in $form->{path} or in $ENV{PATH_INFO}.

=item Method C<-E<gt>valid_paths>

Returns a hashref of path steps that are allowed.  If step found in default
method path is not in the hash, the method path will return a single
step "forbidden" and run its hooks.  If no hash or undef is returned,
all paths are allowed (default).

=item Method C<-E<gt>pre_loop>

Called right before the navigation loop is started.  At this point the path
is set (but could be modified).  The only argument is a reference to the path
array.

=item Method C<-E<gt>run_hook>

Calls "hook" to get a code ref which it then calls and returns the
result.  Arguments are the same as that for "hook".

=item Method C<-E<gt>hook>

Arguments are a pathstep name, a hook name, and an optional code sub
or default value (default value will be turned to a sub) (code sub will
be called as method of $self).

  my $code = $self->hook('main', 'info_complete', sub {return 0});
  ### will look first for $self->main_info_complete();
  ### will then look  for $self->info_complete();
  ### will then run       $self->$default_passed_sub(); # sub {return 0}

=item Method C<-E<gt>handle_error>

If anything dies during execution, handle_error will be called with the error
that had happened.  Default is to debug the error and path history.

=item Hook C<-E<gt>pre_step>

Ran at the beginning of the loop before info_compelete is called.  If it
returns true, execution of navigate is returned and no more steps are processed.

=item Hook C<-E<gt>info_complete>

Checks to see if all the necessary form elements have been passed in.
Calls hooks ready_validate, and validate.

=item Hook C<-E<gt>ready_validate>

Should return true if enough information is present to run validate.

=item Hook C<-E<gt>validate>

Runs validation on the information posted in $self->form.  Uses CGI::Ex::Validate
for the validation.  Calls the hook hash_validation to load validation information.
Should return true if enough information is present to run validate.  Errors
are stored as a hash in $self->{hash_errors} via method add_errors and can be checked
for at a later time with method has_errors (if the default validate was used).

=item Hook C<-E<gt>hash_validation>

Returns a hash of the validation information to check form against.  By default,
will look for a filename using the hook file_val and will pass it to
CGI::Ex::Validate::get_validation.

=item Hook C<-E<gt>file_val>

Returns a filename containing the validation.  Adds method base_dir_rel to hook name_module,
and name_step and adds on the default file extension found in $EXT_VAL.  File
should be readible by CGI::Ex::Validate::get_validation.

=item Hook C<-E<gt>hash_form>

Called in preparation for print after failed info_complete.  Should contain a hash
of any items needed to be swapped into the html during print.

=item Hook C<-E<gt>hash_fill>

Called in preparation for print after failed info_complete.  Should contain a hash
of any items needed to be filled into the html form during print.

=item Hook C<-E<gt>hash_errors>

Called in preparation for print after failed info_complete.  Should contain a hash
of any errors that occured.  Will be merged into hash_form before the pass to print.
Eash error that occured will be passed to method format_error before being added to the
hash.  If an error has occurred, the default validate will automatically add {has_errors =>1}.
To the error hash at the time of validation.  has_errors will also be added during the
merge incase the default validate was not used.

=item Hook C<-E<gt>hash_common>

A hash of common items to be merged with hash_form - such as pulldown menues.

=item Hook C<-E<gt>file_print>

Returns a filename of the content to be used in the default print hook.  Adds method base_dir_rel to
hook name_module, and name_step and adds on the default file extension found in $EXT_PRINT.
Should be a file that can be handled by hook print.

=item Hook C<-E<gt>print>

Take the information and print it out.  Default incarnation uses Template.  Arguments
are: step name, form hashref, and fill hashref.

=item Hook C<-E<gt>post_hook>

A hook which occurs after the printing has taken place.  Is only run if the information was not complete.  Useful for printing rows of a database query.

=item Hook C<-E<gt>post_step>

Ran at the end of the step's loop if info_complete returned true.  Allows for cleanup.  If a true
value is returned, execution of navigate is returned and no more steps are processed.

=item Method C<-E<gt>post_loop>

Ran after all of the steps in the loop have been processed.  If this point is reached,
the post_loop hook will be called, and then the object will run $self->navigate({path => ['main']})
to fall back to the main method.

=back

=cut
