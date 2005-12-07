package CGI::Ex::Template;

use strict;
use vars qw(@INCLUDE_PATH
            $START_TAG
            $END_TAG
            );

BEGIN {
    $START_TAG  ||= qr/\[%/;
    $END_TAG    ||= qr/%\]/;
};

###----------------------------------------------------------------###

sub new {
  my $class = shift;
  my $args  = ref($_[0]) ? shift : {@_};
  return bless $args, $class;
}

###----------------------------------------------------------------###

### This is intended as a simple yet strong subroutine to swap
### in tags to a document.  It is intended to be very basic
### for those who may not want the full features of a Templating
### system such as Template::Toolkit (even though they should
### investigate them because they are pretty nice)
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
        if (my $ref = $self->get_variable_ref(\$tag)) {
            $val = UNIVERSAL::isa($ref, 'CODE')   ? $ref->()
                 : UNIVERSAL::isa($ref, 'SCALAR') ? $$ref
                 :                                  "$ref";
        } elsif ($tag =~ /(\w+) (?: $|\s)/x) {
            my $func = $1;
            die "Unknown function \"$func\" in tag $all" if ! $self->{'FUNCTIONS'}->{$func};
            $val = $self->{'FUNCTIONS'}->{$func}->($self, \$tag);
        } else {
            die "Not sure how to handle tag $all";
        }

        ### return the val and any whitespace
        $ws_pre  = '' if $pre_chomp  || $self->{'PRE_CHOMP'};
        $ws_post = '' if $post_chomp || $self->{'POST_CHOMP'};
        "$ws_pre$val$ws_post"; # return of the swap
  }xeg;

}

sub get_variable_ref {
    my ($self, $str_ref) = @_;

    if ($str_ref =~ /^([\"\']) (|.*?[^\\]) \1 \s*/xs) {
        my $str = $2;
        $self->interpolate(\$str);
        return \$str;
    }

    my $ref  = $self->stash;
    my $copy = $$str_ref;
    my $traverse = '';

    while (defined($ref)
           && length($copy)
           && $copy !~ /^\s+/) {
        if (UNIVERSAL::isa($ref, 'CODE')) {
            my $new_ref = $ref->();
            $ref = ref($new_ref) ? $new_ref : \ $new_ref;
            next;
        }
        if ($copy =~ s/^(\w+)\.?//) {
            my $name = $1;
            if (UNIVERSAL::can($ref, 'can')) {
                if (UNIVERSAL::can($ref, $name)) {
                    my $obj = $ref;
                    $ref = sub { $ref->$name(@_) };
                } else {
                    die "Unknown method \"$name\" for object $ref";
                }
            } elsif (UNIVERSAL::isa($ref, 'HASH')) {
                if (exists $ref->{$name}) {
                    $ref = ref($ref->{$name}) ? $ref->{$name} : \ $ref->{$name};
                    next;
                } elsif ($name eq 'keys') {
                    $ref = [keys %$ref];
                } elsif ($name eq 'sort') {
                    $ref = [sort {lc $ref->{$a} cmp lc $ref->{$b}} keys %$ref];
                } elsif ($name eq 'nsort') {
                    $ref = [sort {$ref->{$a} <=> $ref->{$b}} keys %$ref];
                } else {
                    die "HASH virtual method \"$name\" not implemented ($$str_ref)";
                }
            } elsif (UNIVERSAL::isa($ref, 'ARRAY')) {
                if ($name =~ /^-?\d+/) {
                    $ref = ref($ref->[$name]) ? $ref->[$name] : \ $ref->[$name];
                    next;
                } elsif ($name eq 'join') {
                    my $array_ref = $ref;
                    $ref = sub { my $join = shift; $join = ' ' if ! defined($join); return join $join, @$array_ref };
                } else {
                    die "ARRAY virtual method \"$name\" not implemented ($$str_ref)";
                }
            } elsif (UNIVERSAL::isa($ref, 'SCALAR')) {
                die "SCALAR virtual method not implemented ($$str_ref)";
            }
        }
    } # end of while

    return $ref;
}

sub interpolate {
    my ($self, $str_ref) = @_;
    my $copy;
    $str_ref =~ s/\$(\w+)/$self->_get_interp_value($copy = $1)/gs;
    $str_ref =~ s/\$\{ ([^\}]+) \}/$self->_get_interp_value($copy = $1)/gsx;
}

sub _get_interp_value {
    my ($self, $name) = @_;
    my $ref = $self->get_variable_ref(\$name);
    return $$ref; # TODO - allow for more return types
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

