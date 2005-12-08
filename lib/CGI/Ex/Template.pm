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
        SET => \&func_SET,
        GET => \&func_GET,
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
  my $form = shift;
  $self->stash($form) if $form;

  return $str if ! $str;
  my $ref  = ref($str) ? $str : \$str;

  local $START_TAG = $self->{'START_TAG'} || $START_TAG;
  local $END_TAG   = $self->{'END_TAG'}   || $END_TAG;
  local $self->{'state'} = {};

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
    my ($self, $str_ref) = @_;

    ### looks like a quoted string - return early
    if ($$str_ref =~ s/^([\"\']) (|.*?[^\\]) \1 \s*//xs) {
        my $str = $2;
        $self->interpolate(\$str) if $1 eq '"'; # ' don't interpolate
        return \$str;

    ### looks like an unquoted num
    } elsif ($$str_ref =~ s/^(-?(?:\d*\.\d+|\d+))//) {
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
        $copy =~ s/^\]\.?\s*// || die "Unterminated array constructor in $$str_ref";
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
        $copy =~ s/^\}\.?\s*// || die "Unterminated hash constructor in $$str_ref";
        $ref = \%hash;
    }

    ### look for normal type names
    while (defined $ref) {

        ### parse for arguments (if we look like an arg list)
        my @args;
        if ($copy =~ s/^\(\s*//) {
            while (my $_ref = $self->get_variable_ref(\$copy)) {
                push @args, UNIVERSAL::isa($_ref, 'SCALAR') ? $$_ref : $_ref;
                next if $copy =~ s/^,\s*//;
            }
            $copy =~ s/^\)\.?\s*// || die "Unterminated arg list in $$str_ref";
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
        if ($copy =~ s/^\$(\w+)\b(\.?)\s*//
            || $copy =~ s/^\$\{\s* ([^\}]+) \s*\}(\.?)\s*//x) {
            my ($var, $dot) = ($1, $2);
            my $ref = $self->get_variable_ref(\$var);
            if (! $ref || ! UNIVERSAL::isa($ref, 'SCALAR')) {
                die "Unknown variable \"$var\" in $$str_ref"; # TODO - allow
            } else {
                $copy = $$ref . $dot . $copy;
            }
        }

        ### walk down the line this.that.foo(blah).equals george
        if ($copy =~ s/^(\w+)\b\.?\s*//) {
            my $name = $1;
            if (UNIVERSAL::can($ref, 'can')) {
                if (UNIVERSAL::can($ref, $name)) {
                    my $obj = $ref;
                    $ref = sub { [$ref->$name(@_)] };
                    next;
                } else {
                    die "Unknown method \"$name\" for object $ref";
                }
            } elsif (UNIVERSAL::isa($ref, 'HASH')) {
                if (exists $ref->{$name}) {
                    $ref = ref($ref->{$name}) ? $ref->{$name} : \ $ref->{$name};
                    next;
                } elsif (my $code = $self->hash_op($name)) {
                    my $hash = $ref;
                    $ref = sub { $code->($hash, @_) };
                    next;
                } elsif ($self->{'function'} && $self->{'function'} eq 'SET') { # setting
                    $ref = $ref->{$name} = {}; # AUTOVIVIFY
                    next;
                } else { # undef
                    my $var = undef;
                    $ref = \$var;
                    next;
                    #die "HASH virtual method \"$name\" not implemented ($$str_ref)";
                }
            } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
                if ($name =~ /^-?\d+/) {
                    if ($name >= 0 && $name > $#$ref) { # allow for out of bounds
                        if ($self->{'function'} && $self->{'function'} eq 'SET') {
                            $ref->[$name] = {}; # AUTOVIVIFY
                            next;
                        } else {
                            my $var = undef;
                            $ref = \ $var;
                            next;
                        }
                    } else { # return that index
                        $ref = ref($ref->[$name]) ? $ref->[$name] : \ $ref->[$name];
                    }
                    next;
                } elsif (my $code = $self->list_op($name)) { # virtual method
                    my $array = $ref;
                    $ref = sub { $code->($array, @_) };
                    next;
                } else { # undef
                    my $var = undef;
                    $ref = \$var;
                    next;
                    #die "ARRAY virtual method \"$name\" not implemented ($$str_ref)";
                }
            } elsif (UNIVERSAL::isa($ref, 'SCALAR')) {
                if (! defined $$ref) {
                    ### undefs gobble up the rest
                    next;
                } elsif (my $code = $self->scalar_op($name)) {
                    my $scalar = $$ref;
                    $ref = sub { $code->($scalar, @_) };
                    next;
                } else {
                    my $var = undef;
                    $ref = \$var;
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

    if (UNIVERSAL::isa($ref, 'SCALAR') && ! defined $$ref) {
        my $str = $self->undefined($$str_ref);
        $str = '' if ! defined $str;
        $ref = \ $str;
    }

    ### finalize the changes and return the var reference
    $$str_ref = $copy;
    return $ref;
}

sub undefined {''}

sub interpolate {
    my ($self, $str_ref) = @_;
    my $copy;
    $str_ref =~ s/\$(\w+)/$self->_get_interp_value($copy = $1)/gs;
    $str_ref =~ s/\$\{\s* ([^\}]+) \s*\}/$self->_get_interp_value($copy = $1)/gsx;
}

sub _get_interp_value {
    my ($self, $name) = @_;
    my $ref = $self->get_variable_ref(\$name);
    return $$ref; # TODO - allow for more return types
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

###----------------------------------------------------------------###

sub stash {
    my $self = shift;
    if (@_) {
        die "Stash must be a hashref" if ! UNIVERSAL::isa($_[0], 'HASH');
        $self->{'STASH'} = shift;
    }
    return $self->{'STASH'} ||= {};
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

