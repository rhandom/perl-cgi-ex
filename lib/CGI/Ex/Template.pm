package CGI::Ex::Template;

use strict;
use vars qw(@INCLUDE_PATH
            $START_TAG
            $END_TAG
            $SCALAR_OPS $HASH_OPS $LIST_OPS
            $FUNCTIONS
            );

BEGIN {
    $START_TAG  ||= qr/\[%/;
    $END_TAG    ||= qr/%\]/;
    $SCALAR_OPS = {
        hash    => sub { {value => $_[0]} },
        length  => sub { defined($_[0]) ? length($_[0]) : 0 },
        list    => sub { [ $_[0] ] },
        match   => sub {
            my ($str, $pat, $global) = @_;
            return [] if ! defined $str || ! defined $pat;
            return [$str =~ /$pat/g] if $global;
            return [$str =~ /$pat/ ];
        },
        replace => sub {
            my ($str, $pat, $replace) = @_;
            return undef if ! defined $str || ! defined $pat;
            $replace = '' if ! defined $replace;
            $str =~ s/$pat/$replace/g;
            return $str;
        },
        size    => sub { 1 },
    };

    $LIST_OPS = {
        grep    => sub { my ($ref, $pat) = @_; [grep {/$pat/} @$ref] },
        join    => sub { my ($ref, $join) = @_; $join = ' ' if ! defined $join; return join $join, @$ref },
        list    => sub { $_[0] },
        max     => sub { $#{ $_[0] } },
        nsort   => sub { [sort {$a->[1] <=> $b->[1]} @{ $_[0] } ] },
        pop     => sub { pop @{ $_[0] } },
        push    => sub { my $ref = shift; push @$ref, @_; return '' },
        reverse => sub { [ reverse @{ $_[0] } ] },
        shift   => sub { shift @{ $_[0] } },
        size    => sub { $#{ $_[0] } + 1 },
        sort    => sub { [map {$_->[0]} sort {$a->[1] cmp $b->[1]} map {[$_, lc $_]} @{ $_[0] } ] }, # case insensitive
        unshift => sub { my $ref = shift; unshift @$ref, @_; return '' },
    };

    $HASH_OPS = {
        defined => sub { return '' if ! defined $_[1]; defined $_[0]->{ $_[1] } },
        delete  => sub { return '' if ! defined $_[1]; delete  $_[0]->{ $_[1] } },
        each    => sub { [each %{ $_[0] }] },
        exists  => sub { return '' if ! defined $_[1]; exists $_[0]->{ $_[1] } },
        hash    => sub { $_[0] },
        keys    => sub { [keys %{ $_[0] }] },
        list    => sub { [map { {key => $_, value => $_[0]->{$_}} } keys %{ $_[0] } ] },
        nsort   => sub { my $ref = shift; [sort {$ref->{$a}    <=> $ref->{$b}   } keys %$ref] },
        size    => sub { scalar keys %{ $_[0] } },
        sort    => sub { my $ref = shift; [sort {lc $ref->{$a} cmp lc $ref->{$b}} keys %$ref] },
        values  => sub { [values %{ $_[0] }] },
    };

    $FUNCTIONS = {
        SET  => \&func_SET,
        GET  => \&func_GET,
        DUMP => \&func_DUMP,
    };
};

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $args  = ref($_[0]) ? shift : {@_};
  return bless $args, $class;
}

###----------------------------------------------------------------###

sub swap {
  my $self = shift;
  my $str  = shift;
  my $form = shift || {};

  return $str if ! $str;
  my $ref  = ref($str) ? $str : \$str;

  ### setup our start and end
  local $START_TAG = $self->{'START_TAG'} || $START_TAG;
  local $END_TAG   = $self->{'END_TAG'}   || $END_TAG;
  local $self->{'state'} = {};

  ### copy form to the stash
  my $stash = $self->stash;
  local @$stash{keys %$form} = values %$form;

  $self->swap_buddy($ref);

  return ref($str) ? 1 : $$ref;
}

sub swap_buddy {
    my $self = shift;
    my $ref  = shift;

    ### now do the swap
    $$ref =~ s{
        (\s*) (
               $START_TAG (-?) \s* # opening tag and prechomp info
               ( | \S.+?)          # nothing or something
               \s* (-?) $END_TAG   # the close tag and postchomp info
               ) (\s*)
    }{
        local $self->{'state'}->{'pos'} = pos;
        my ($ws_pre, $all, $pre_chomp, $tag, $post_chomp, $ws_post) = ($1, $2, $3, $4, $5, $6);
        my $val;

        ### look for functions or variables
        if ($tag =~ /^(\w+) (?: $|\s)/x
            && (my $code = $self->get_function(my $func = $1))) {
            $tag =~ s/^\w+\s*//;
            $val = $code->($self, \$tag, $func);
            $val = '' if ! defined $val;
        } elsif (my $ref = $self->get_variable_ref(\$tag)) {
            die "Found trailing info during variable access \"$tag" if $tag;
            $val = UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : "$ref";
        } else {
            die "Not sure how to handle tag $all";
        }

        ### return the val and any whitespace
        $ws_pre  = '' if $pre_chomp  || $self->{'PRE_CHOMP'};
        $ws_post = '' if $post_chomp || $self->{'POST_CHOMP'};
        "$ws_pre$val$ws_post"; # return of the swap
  }xeg;

}

###----------------------------------------------------------------###

sub get_variable_ref {
    my ($self, $str_ref, $args) = @_;
    $args ||= {};
    my $set_ref;
    my $set_name;

    ### looks like a quoted string - return early
    if ($$str_ref =~ s/^([\"\']) (|.*?[^\\]) \1 \s*//xs) {
        my $str = $2;
        $self->interpolate(\$str) if $1 eq '"'; # ' don't interpolate
        return \$str;

    ### looks like an unquoted num
    } elsif ($$str_ref =~ s/^(-?(?:\d*\.\d+|\d+))\s*//) {
        my $num = $1;
        return \$num;
    }

    my $stash    = $self->stash;
    my $ref      = $stash;
    my $copy     = $$str_ref;
    my $traverse = '';

    ### looks like an array constructor
    if ($copy =~ s/^\[\s*//) {
        my @array;
        while (my $_ref = $self->get_variable_ref(\$copy)) {
            push @array, UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : $_ref;
            next if $copy =~ s/^,\s*//;
        }
        $copy =~ s/^\]\.?// || die "Unterminated array constructor in $$str_ref";
        $ref = \@array;

    ### looks like a hash constructor
    } elsif ($copy =~ s/^\{\s*//) {
        my %hash;
        while (my $_ref = $self->get_variable_ref(\$copy)) {
            my $key = UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : "$_ref";
            $copy =~ s/^=>\s*// || die "Missing => in hash constructor in $$str_ref";
            $_ref = $self->get_variable_ref(\$copy);
            my $val = defined($_ref) ? UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : $_ref : undef;
            $hash{$key} = $val;
            next if $copy =~ s/^,\s*//;
        }
        $copy =~ s/^\}\.?// || die "Unterminated hash constructor in $$str_ref";
        $ref = \%hash;
    }

    ### look for normal name element types
    while (defined $ref) {

        ### parse for arguments (if we look like an arg list)
        my @args;
        if ($copy =~ s/^\(\s*//) {
            while (my $_ref = $self->get_variable_ref(\$copy)) {
                push @args, UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : $_ref;
                next if $copy =~ s/^,\s*//;
            }
            $copy =~ s/^\)\.?// || die "Unterminated arg list in $$str_ref";
        }

        ### if the previously found thing was a code block - run it with any parsed args
        if (UNIVERSAL::isa($ref, 'CODE')) {
            my @results = $ref->(@args);
            if ($#results > 0) {
                $ref = \@results;
            } elsif (defined(my $result = $results[0])) {
                $ref = ref($result) ? $result : \ $result;
            } else {
                my $str = '';
                $ref = \ $str;
            }
            next;
        }

        ### allow for interpolated variables in the middle
        if ($copy =~ s/^\$(\w+)\b(\.?)//
            || $copy =~ s/^\$\{\s* ([^\}]+) \s*\}(\.?)\s*//x) {
            my ($var, $dot) = ($1, $2);
            my $_ref = $self->get_variable_ref(\$var);
            if (! $_ref || ! UNIVERSAL::isa($_ref, 'SCALAR')) {
                die "Unknown variable \"$var\" in $$str_ref"; # TODO - allow
            } elsif ($$_ref =~ /^\w+$/) {
                $copy = $$_ref . $dot . $copy;
            } else {
                my $str = undef;
                $ref = \ $str;
                next;
            }
        }

        ### walk down the line this.that.foo(blah).equals george
        if ($copy =~ s/^(\w+)\b\.?//) {
            my $name = $1;

            ### current level looks like an object method calll
            if (UNIVERSAL::can($ref, $name)) {
                my $obj = $ref;
                $ref = sub {
                    my @results = $obj->$name(@_);
                    if ($#results > 0) {
                        return \@results;
                    } else {
                        my $result = $results[0];
                        return ref($result) ? $result : \ $result;
                    }
                };
                next;

            ### current level looks like a hashref
            } elsif (UNIVERSAL::isa($ref, 'HASH')) {
                if (exists $ref->{$name}) {
                    if ($args->{'return_set_ref'}) {
                        $set_ref  = $ref;
                        $set_name = $name;
                    }
                    $ref = ref($ref->{$name}) ? $ref->{$name} : \ $ref->{$name};
                    next;
                } elsif (my $code = $self->hash_op($name)) {
                    my $hash = $ref;
                    $ref = sub { $code->($hash, @_) };
                    next;
                } elsif ($args->{'return_set_ref'}) {
                    $set_ref  = $ref;
                    $set_name = $name;
                    $ref = $ref->{$name} = {}; # AUTOVIVIFY
                    next;
                } else { # undef
                    my $var = undef;
                    $ref = \ $var;
                    next;
                    #die "HASH virtual method \"$name\" not implemented ($$str_ref)";
                }

            ### current level looks like an arrayref
            } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
                if ($name =~ /^\d+/) { # index lookup
                    if ($name > $#$ref) { # allow for out of bounds
                        if ($args->{'return_set_ref'}) {
                            $ref->[$name] = {}; # AUTOVIVIFY
                        } else {
                            my $var = undef;
                            $ref = \ $var;
                            next;
                        }
                    }
                    if ($args->{'return_set_ref'}) {
                        $set_ref  = $ref;
                        $set_name = $name;
                    }
                    $ref = ref($ref->[$name]) ? $ref->[$name] : \ $ref->[$name];
                    next;
                } elsif (my $code = $self->list_op($name)) { # virtual method
                    my $array = $ref;
                    $ref = sub { $code->($array, @_) };
                    next;
                } elsif ($args->{'return_set_ref'}) {
                    die "Refusing to translate array to hash during set operation in $$str_ref";
                } else { # undef
                    my $var = undef;
                    $ref = \ $var;
                    next;
                    #die "ARRAY virtual method \"$name\" not implemented ($$str_ref)";
                }

            ### looks like a normal variable
            } elsif (UNIVERSAL::isa($ref, 'SCALAR')) {
                if (! defined $$ref) {
                    ### undefs gobble up the rest
                    next;
                } elsif (my $code = $self->scalar_op($name)) {
                    my $scalar = $$ref;
                    $ref = sub { $code->($scalar, @_) };
                    next;
                } elsif ($args->{'return_set_ref'}) {
                    if (UNIVERSAL::isa($set_ref, 'HASH')) {
                        $set_ref = $set_ref->{$set_name} = {};
                        $ref     = $set_ref->{$name}     = {};
                    } elsif (UNIVERSAL::isa($set_ref, 'ARRAY')) {
                        $set_ref = $set_ref->[$set_name] = {};
                        $ref     = $set_ref->{$name}     = {};
                    }
                    $set_name = $name;
                    next;
                } else {
                    my $var = undef;
                    $ref = \ $var;
                    next;
                    #die "SCALAR virtual method not implemented ($$str_ref)";
                }
            }
        }

        ### if we hit here - we were unable to parse more - so stop
        last;
    } # end of while


    ### we didn't find a variable - return nothing
    return if $ref == $stash;

    ### finalize the changes and return the var reference
    $copy =~ s/^\s+//;
    $$str_ref = $copy;

    ### undefined values
    if (UNIVERSAL::isa($ref, 'SCALAR') && ! defined $$ref) {
        my $str = $self->undefined($$str_ref);
        $str = '' if ! defined $str;
        $ref = \ $str;

    ### if we are setting
    } elsif ($args->{'return_set_ref'}) {
        return [$set_ref, $set_name];

    ### normal variable access
    } else {
        return $ref;
    }
}

sub undefined {''}

sub interpolate {
    my ($self, $str_ref) = @_;
    my $copy;
    $$str_ref =~ s/\$(\w+)/$self->_get_interp_value($1)/egs;
    $$str_ref =~ s/\$\{\s* ([^\}]+) \s*\}/$self->_get_interp_value($1)/egsx;
}

sub _get_interp_value {
    my ($self, $name) = @_;
    my $ref = $self->get_variable_ref(\$name);
    die "Couldn't find interpolation value in $name" if ! $ref;
    return UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : "$ref";
}

sub scalar_op {
    my ($self, $name) = @_;
    return $SCALAR_OPS->{$name};
}

sub list_op {
    my ($self, $name) = @_;
    return $LIST_OPS->{$name};
}

sub hash_op {
    my ($self, $name) = @_;
    return $HASH_OPS->{$name};
}

sub get_function {
    my ($self, $name) = @_;
    return $FUNCTIONS->{$name};
}

###----------------------------------------------------------------###

sub func_GET {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref);
    die "Couldn't find variable on GET on $copy" if ! $ref;
    return UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : "$ref";
}

sub func_SET {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    while (length $$tag_ref) {
        my $set = $self->get_variable_ref($tag_ref, {return_set_ref => 1});
        die "Couldn't find variable on SET on $copy" if ! $set;
        my $val;
        if ($$tag_ref =~ s/^=\s*//) {
            my $val_ref = $self->get_variable_ref($tag_ref);
            $val = UNIVERSAL::isa($val_ref, 'SCALAR') ? $$val_ref : $val_ref;
        } else {
            $val = '';
        }
        $$tag_ref =~ s/^;\s*//;

        my ($ref, $name) = @$set;
        if (UNIVERSAL::isa($ref, 'HASH')) {
            $ref->{$name} = $val;
        } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
            $ref->[$name] = $val;
        }
    }
    return '';
}

sub func_DUMP {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref);
    require Data::Dumper;
    my $str = Data::Dumper::Dumper(UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : $ref);
    $str =~ s/\$VAR1/$copy/g;
    return $str;
}

###----------------------------------------------------------------###

sub stash {
    my $self = shift;
    return $self->{'stash'} ||= {};
}

sub include_path {
    my $self = shift;
    return $self->{'INCLUDE_PATH'} ||= [@INCLUDE_PATH];
}

sub include_filename {
    my ($self, $file) = @_;
    if ($file =~ m|^/|) {
        die "ABSOLUTE paths disabled" if ! $self->{'ABSOLUTE'};
        return $file if -e $file;
    } elsif ($file =~ m|^\./|) {
        die "RELATIVE paths disabled" if ! $self->{'RELATIVE'};
        return $file if -e $file;
    } else {
        my $paths = $self->include_path;
        $paths = [$paths] if ! ref $paths;
        foreach my $path (@$paths) {
            return "$path/$file" if -e "$path/$file";
        }
    }
    die "Couldn't find \"$file\" in INCLUDE_PATH";
}

sub include_file {
    my ($self, $file) = @_;
    my $full = $self->include_filename($file);
    open(my $fh, "<$full") || die "Couldn't open $file for reading: $!";
    read $fh, my $txt, -s $full;
    return $txt;
}

sub process {
    my ($self, $in, $swap, $out) = @_;

    ### get the content
    my $content;
    if (ref $in) {
        if (UNIVERSAL::isa($in, 'SCALAR')) { # reference to a string
            $content = $$in;
        } elsif (UNIVERSAL::isa($in, 'CODE')) {
            $content = $in->();
        } else { # should be a file handle
            local $/ = undef;
            $content = <$in>;
        }
    } else {
        $content = $self->include_file($in);
    }

    ### do swap
    $self->swap(\$content, $swap);

    ### put it back out
    if (ref $out) {
        if (UNIVERSAL::isa($out, 'SCALAR')) { # reference to a string
            $$out = $content;
        } elsif (UNIVERSAL::isa($out, 'CODE')) {
            $out->($content);
        } elsif (UNIVERSAL::can($out, 'print')) {
            $out->print($content);
        } else { # should be a file handle
            print $out $content;
        }
    } elsif ($out) { # should be a filename
        my $file;
        if ($out =~ m|^/|) {
            die "ABSOLUTE paths disabled" if ! $self->{'ABSOLUTE'};
            $file = $out;
        } elsif ($out =~ m|^\./|) {
            die "RELATIVE paths disabled" if ! $self->{'RELATIVE'};
            $file = $out;
        } else {
            die "OUTPUT_PATH not set" if ! $self->{'OUTPUT_PATH'};
            $file = $self->{'OUTPUT_PATH'} . '/' . $out;
        }
        open(my $fh, ">$file") || die "Couldn't open \"$out\" for writing: $!";
        print $fh $content;
    } else {
        print $content;
    }

    return 1;
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex::Template - Beginning interface to Templating systems - for they are many

=head1 SYNOPSIS

  None yet.

=head1 DESCRIPTION

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut

