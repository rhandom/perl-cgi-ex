package CGI::Ex::JSONDump;

=head1 NAME

CGI::Ex::JSONDump - Simple data to JSON dump.

=cut

###----------------------------------------------------------------###
#  Copyright 2006 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

use vars qw($VERSION
            @EXPORT @EXPORT_OK
            %ESCAPE $QR_ESCAPE);
use strict;
use base qw(Exporter);

BEGIN {
    $VERSION  = '2.03';

    @EXPORT = qw(JSONDump);
    @EXPORT_OK = @EXPORT;

    %ESCAPE = (
        "\""   => '\"',
        "\t"   => '\t',
        "\r"   => '\r',
        "\\"   => '\\\\',
        "\x00" => '\u0030',
        "\x01" => '\u0031',
        "\x02" => '\u0032',
        "\x03" => '\u0033',
        "\x04" => '\u0034',
        "\x05" => '\u0035',
        "\x06" => '\u0036',
        "\x07" => '\u0037',
        "\x0B" => '\u0031',
        "\x0E" => '\u0031',
        "\x0F" => '\u0031',
        "\x10" => '\u0031',
        "\x11" => '\u0031',
        "\x12" => '\u0031',
        "\x13" => '\u0031',
        "\x14" => '\u0032',
        "\x15" => '\u0032',
        "\x16" => '\u0032',
        "\x17" => '\u0032',
        "\x18" => '\u0032',
        "\x19" => '\u0032',
        "\x1A" => '\u0032',
        "\x1B" => '\u0032',
        "\x1C" => '\u0032',
        "\x1D" => '\u0032',
        "\x1E" => '\u0033',
        "\x1F" => '\u0033',
               );
    $QR_ESCAPE = '['.join("", keys %ESCAPE).']';
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
    $self->{'skip_keys'}    ||= {};
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
        return "{$self->{hash_nl}${prefix}$self->{indent}"
            . join(",$self->{hash_nl}${prefix}$self->{indent}",
                   map  { $self->_encode($_, "${prefix}$self->{indent}")
                              . $self->{'hash_sep'}
                              . $self->_dump($data->{$_}, "${prefix}$self->{indent}") }
                   grep { my $r = ref $data->{$_}; ! $r || $self->{'handle_types'}->{$r} || $r eq 'HASH' || $r eq 'ARRAY' }
                   grep { ! $self->{'skip_keys'}->{$_} }
                   ($self->{'no_sort'} ? (keys %$data) : (sort keys %$data)))
            . "$self->{hash_nl}${prefix}}";

    } elsif ($ref eq 'ARRAY') {
        return "[$self->{array_nl}${prefix}$self->{indent}"
            . join(",$self->{array_nl}${prefix}$self->{indent}",
                   map { $self->_dump($_, "${prefix}$self->{indent}") }
                   @$data)
            . "$self->{array_nl}${prefix}]";

    } elsif ($ref) {
        return '"'.$data.'"'; ### don't do anything

    } else {
        return $self->_encode($data, "${prefix}$self->{indent}");
    }
}

sub _encode {
    my ($self, $str, $prefix) = @_;
    return 'null'  if ! defined $str;
    return 'true'  if $str =~ /^[Tt][Rr][Uu][Ee]$/;
    return 'false' if $str =~ /^[Ff][Aa][Ll][Ss][Ee]$/;

    ### allow things that look like numbers to show up as numbers (and those that aren't quite to not)
    return $str if $str =~ /^ -? (?: \d{0,13} \. \d+ | \d{1,13}) $/x && $str !~ /0$/;

    ### allow for really odd chars
    utf8::decode($str) if $self->{'utf8'} && &utf8::decode;

    $str =~ s/($QR_ESCAPE)/$ESCAPE{$1}/go;
    $str =~ s{(</? (?: htm | scrip | !-))}{$1"+"}gx; # escape <html> and </html> tags in the text

    ### add nice newlines (unless pretty is off)
    if ($self->{'str_nl'} && length($str) > 80) {
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
