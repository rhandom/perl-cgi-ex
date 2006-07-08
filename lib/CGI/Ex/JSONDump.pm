package CGI::Ex::JSONDump;

=head1 NAME

CGI::Ex::JSONDump - Simple data to JSON dump.

=cut

###----------------------------------------------------------------###
#  Copyright 2006 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

use vars qw($VERSION
            @EXPORT @EXPORT_OK);
use strict;
use base qw(Exporter);

BEGIN {
    $VERSION  = '2.03';

    @EXPORT = qw(JSONDump);
    @EXPORT_OK = @EXPORT;

};

sub JSONDump {
    my ($data, $args) = @_;
    return __PACKAGE__->new($args)->dump($data);
}

###----------------------------------------------------------------###

sub new {
    my $class = shift || __PACKAGE__;
    my $args  = shift || {};
    my $self  = bless {%$args}, $class;

    $self->{'handle_types'} ||= {};
    $self->{'handle_keys'}  ||= {};
    $self->{'skip_types'}   ||= {};
    $self->{'skip_keys'}    ||= {};

    $self->{'skip_types'} = {map {$_ => 1} ref($self->{'skip_types'}) eq 'ARRAY' ? @{ $self->{'skip_types'} } : $self->{'skip_types'}}
        if ref $self->{'skip_types'} ne 'HASH';
    $self->{'skip_keys'} = {map {$_ => 1} ref($self->{'skip_keys'}) eq 'ARRAY' ? @{ $self->{'skip_keys'} } : $self->{'skip_keys'}}
        if ref $self->{'skip_keys'} ne 'HASH';

    $self->{'pretty'} = 1 if ! exists $self->{'pretty'};

    return $self;
}

sub dump {
    my ($self, $data, $args) = @_;
    $self = $self->new($args) if ! ref $self;

    local $self->{'indent'}   = ! $self->{'pretty'} ? ''  : defined($self->{'indent'})   ? $self->{'indent'}   : '  ';
    local $self->{'hash_sep'} = ! $self->{'pretty'} ? ':' : defined($self->{'hash_sep'}) ? $self->{'hash_sep'} : ' : ';
    local $self->{'hash_nl'}  = ! $self->{'pretty'} ? ''  : defined($self->{'hash_nl'})  ? $self->{'hash_nl'}  : "\n";
    local $self->{'array_nl'} = ! $self->{'pretty'} ? ''  : defined($self->{'array_nl'}) ? $self->{'array_nl'} : "\n";
    local $self->{'str_nl'}   = ! $self->{'pretty'} ? ''  : defined($self->{'str_nl'})   ? $self->{'str_nl'}   : "\n";

    return $self->_dump($data, '');
}

sub _dump {
    my ($self, $data, $prefix) = @_;
    my $ref = ref $data;

    if ($self->{'handle_types'}->{$ref}) {
        return $self->{'handle_types'}->{$ref}->($self, $data, "${prefix}$self->{indent}");

    } elsif ($ref eq 'HASH') {
        my @keys = $self->{'no_sort'} ? (keys %$data) : (sort keys %$data);
        return "{$self->{hash_nl}${prefix}$self->{indent}"
            . join(",$self->{hash_nl}${prefix}$self->{indent}",
                   map  { $self->_encode($_, "${prefix}$self->{indent}")
                              . $self->{'hash_sep'}
                              . $self->_dump($data->{$_}, "${prefix}$self->{indent}") }
                   grep { ! $self->{'skip_types'}->{ref $data->{$_}} }
                   grep { ! $self->{'skip_keys'}->{$_} }
                   @keys)
            . "$self->{hash_nl}${prefix}}";

    } elsif ($ref eq 'ARRAY') {
        return "[$self->{array_nl}${prefix}$self->{indent}"
            . join(",$self->{array_nl}${prefix}$self->{indent}",
                   map { $self->_dump($_, "${prefix}$self->{indent}") }
                   @$data)
            . "$self->{array_nl}${prefix}]";

    } elsif ($ref) {
        ### don't do anything

    } else {
        return $self->_encode($data, "${prefix}$self->{indent}");
    }
}

sub _encode {
    my ($self, $str, $prefix) = @_;

    ### allow things that look like numbers to show up as numbers
    return $str if $str =~ /^ -? (?: \d{0,13} \. \d+ | \d{1,13}) $/x && $str !~ /0$/;

    $str =~ s/\\/\\\\/g;
    $str =~ s/\"/\\\"/g;
    $str =~ s{(</? (?: htm | scrip | bod | !-))}{$1"+"}gx; # escape <html> and </html> tags in the text
    if ($self->{'str_nl'}) {
        $str =~ s/\"\s*\+\"$// if $str =~ s/\n/\\n\"$self->{str_nl}${prefix}+\"/g;
    } else {
        $str =~ s/\n/\\n/g;
    }
    return '"' .$str. '"';
}

1;

__END__

=head1 DESCRIPTION

CGI::Ex::JSONDump is a very lightweight and fast data structure dumper.

=head1 AUTHORS

Paul Seamons <paul at seamons dot com>

=cut
