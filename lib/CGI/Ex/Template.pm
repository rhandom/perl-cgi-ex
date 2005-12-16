package CGI::Ex::Template;

use strict;
use vars qw(@INCLUDE_PATH
            $START_TAG
            $END_TAG
            $SCALAR_OPS $HASH_OPS $LIST_OPS
            $FUNCTIONS
            $REQ_END
            $QR_FILENAME
            $MAX_RECURSE
            $SPS
            $SPS_QR
            );

BEGIN {
    $START_TAG  ||= '[%';
    $END_TAG    ||= '%]';
    $SPS = chr(186);
    $SPS_QR = qr/$SPS(\d+)$SPS/;

    ### list out the virtual methods
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
        BLOCK   => \&func_BLOCK,
        CALL    => \&func_CALL,
        DEFAULT => \&func_DEFAULT,
        DUMP    => \&func_DUMP,
        END     => sub { die "Shouldn't actually ever enter an end" },
        GET     => \&func_GET,
        IF      => \&func_IF,
        INCLUDE => \&func_INCLUDE,
        INSERT  => \&func_INSERT,
        SET     => \&func_SET,
        PROCESS => \&func_PROCESS,
    };

    $REQ_END = {
        BLOCK => 1,
        IF    => 1,
    };

    $QR_FILENAME = qr{(?i: [a-z]:/|/)? [\w\-\.]+ (?:/[\w\-\.]+)* }x;
    $MAX_RECURSE = 50;
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

    local $self->{'_swap'}  = $_[1] || {};
    my $START = quotemeta($self->{'START_TAG'} || $START_TAG);
    my $END   = quotemeta($self->{'END_TAG'}   || $END_TAG);
    my $new   = '';
    my $block = '';
    my $pos   = 0;
    my $ppos;

    my @args;
    my @var;
    my ($in_tag, $in_block, $in_var, $in_func,
        $post_chomp, $comment);
    use CGI::Ex::Dump;
    use constant trace => 1;
    my $trace = '';

    while (1) {
        $ppos = $pos;
        if (! $in_tag) {
            if ($_[0] =~ /\G (.*?) $START ([\-\#]*) \s*/gcxs) {
                $trace .= "Begin tag \"$&\"\n" if trace;
                my $pre_chomp = index($2, '-') != -1;
                $comment      = index($2, '#') != -1;
                $in_tag = 1;
                undef @args;
                if ($in_block) {
                    next;
                }
                my $begin = $1;
                if ($pre_chomp) {
                    $begin =~ s/ (?:\n|^) [^\S\n]* \z //xm; # remove any leading whitespace on the same line
                }
                $new .= $begin;
                next;
            } else {
                $trace .= "No tag found - ending parse\n" if trace;
                $new .= substr $_[0], pos($_[0]);
                last;
            }
        } elsif ($_[0] =~ /\G ([\-\#]*) $END /gcx) {
            $in_tag = 0;
            $trace .= "End of tag\n" if trace;
            $pos = pos $_[0];
            if ($in_var) {
                $in_var = 0;
                push @args, [@var];
                undef @var;
            }
            if ($in_block) {
                next;
            }
            if (my $func = $in_func) {
                $in_func = 0;
                my $val = $self->get_function($func)->($self->vivify_args(\@args));
                undef @args;
                $new .= $val;
                next;
            }
            if ($#args != -1) {
                dex \@args;
            }
        } elsif ($_[0] =~ /\G (\w+) \s*/gcx) {
            $trace .= "Found a word \"$1\"\n" if trace;
            if ($in_var) {
                push @var, $1;
            } elsif ($self->has_function($1)) {
                die "Already in function $in_func" if $in_func;
                $in_func = $1;
                next;
            } else {
                $in_var = 1;
                push @var, $1;
            }
        } else {
            dex $in_var, $in_tag, $in_func, \@var, \@args, $trace, pos $_[0];
            dex substr($_[0], pos($_[0]));;
            die "Don't know what to do with that yet";
        }
    }

    print $trace if trace;
#            $begin =~ s/ (?:\n|^) [^\S\n]* \z //xm; # remove any leading whitespace on the same line
#            $_[0] =~ m/\G [^\S\n]* (?:\n?$|\n) /xg; # "remove" postpended whitespace on the same line (by updating pos)
    return $new;
}

###----------------------------------------------------------------###

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

###----------------------------------------------------------------###

sub undefined {''}

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

sub has_function { $FUNCTIONS->{$_[1]} ? 1 : 0 }

sub get_function { $FUNCTIONS->{$_[1]} }


###----------------------------------------------------------------###

sub func_BLOCK {
    my ($self, $tag_ref, $func, $body_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref, {auto_quote => 1, quote_qr => qr/\w+/o});
    die "Couldn't find a filename on INSERT/INCLUDE/PROCESS on $copy"
        if ! $ref || ! UNIVERSAL::isa($ref, 'SCALAR') || ! length($$ref);

    my $block_name = $$ref;
    $self->{'BLOCKS'}->{$block_name} = $$body_ref;
    return '';
}


sub func_CALL { &func_GET; '' }

sub func_DEFAULT {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $str = $self->func_GET($tag_ref);
    if (! $str) {
        $self->func_SET(\$copy);
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

sub func_IF {
    my ($self, $tag_ref, $func, $body_ref) = @_;
    my $ref = $self->get_variable_ref($tag_ref);
    if (UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : $ref) {
        return $$body_ref;
    } else {
        return '';
    }
}

sub func_INCLUDE {
    my ($self, $tag_ref) = @_;

    ### localize the swap
    my $swap = $self->{'_swap'};
    my @keys  = keys %$swap;
    local @$swap{@keys} = values %$swap; # note that we are only "cloning" one level deep

    my $str = $self->func_PROCESS($tag_ref);

    ### kill added keys
    my %keys = map {$_ => 1} @keys;
    delete @$swap{grep {!$keys{$_}} keys %$swap};

    return $str;
}

sub func_INSERT {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref, {auto_quote => 1, quote_qr => qr/$QR_FILENAME|\w+/});
    die "Couldn't find a filename on INSERT/INCLUDE/PROCESS on $copy"
        if ! $ref || ! UNIVERSAL::isa($ref, 'SCALAR') || ! length($$ref);

    return $self->{'BLOCKS'}->{$$ref} if $$ref =~ /^\w+$/;
    return $self->include_file($$ref);
}

sub func_GET {
    my ($self, $tag_ref) = @_;
    my $copy = $$tag_ref;
    my $ref = $self->get_variable_ref($tag_ref);
    die "Couldn't find variable on GET on $copy" if ! $ref;
    return UNIVERSAL::isa($ref, 'SCALAR') ? $$ref : "$ref";
}

sub func_PROCESS {
    my ($self, $tag_ref) = @_;

    $self->{'state'}->{'recurse'} ||= 0;
    $self->{'state'}->{'recurse'} ++;
    die "MAX_RECURSE $MAX_RECURSE reached during INCLUDE/PROCESS on $$tag_ref"
        if $self->{'state'}->{'recurse'} >= $MAX_RECURSE;

    my $str = $self->func_INSERT($tag_ref);

    $str = $self->swap($str, $self->{'_swap'}); # restart the swap - passing it our current stash

    $self->{'state'}->{'recurse'} --;
    return $str;
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

    ### localize the stash
    my $stash = $self->stash;
    my @keys  = keys %$stash;
    local @$stash{@keys} = values %$stash;
    local @$stash{keys %$swap}  = values %$swap;

    ### do the swap
    $content = $self->swap($content, $stash);

    ### remove items added to stash
    my %keys = map {$_ => 1} @keys;
    delete @$stash{grep {!$keys{$_}} keys %$stash};


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

