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

    $self->{'skip_keys'} ||= {};
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

    if ($ref eq 'HASH') {
        return "{$self->{hash_nl}${prefix}$self->{indent}"
            . join(",$self->{hash_nl}${prefix}$self->{indent}",
                   map  { $self->js_escape($_, "${prefix}$self->{indent}")
                              . $self->{'hash_sep'}
                              . $self->_dump($data->{$_}, "${prefix}$self->{indent}") }
                   grep { my $r = ref $data->{$_}; ! $r || $self->{'handle_unknown_types'} || $r eq 'HASH' || $r eq 'ARRAY' }
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
        return $self->{'handle_unknown_types'}->($self, $data, $ref) if ref($self->{'handle_unknown_types'}) eq 'CODE';
        return '"'.$data.'"'; ### don't do anything

    } else {
        return $self->js_escape($data, "${prefix}$self->{indent}");
    }
}

sub js_escape {
    my ($self, $str, $prefix) = @_;
    return 'null'  if ! defined $str;

    ### allow things that look like numbers to show up as numbers (and those that aren't quite to not)
    return $str if $str =~ /^ -? (?: \d{0,13} \. \d* [1-9] | \d{1,13}) $/x;

    $str =~ s/\\/\\\\/g;
    $str =~ s/\"/\\\"/g;
    $str =~ s/\r/\\\r/g;
    $str =~ s/\t/\\\t/g;

    ### allow for really odd chars
    $str =~ s/([\x00-\x07\x0b\x0e-\x1f])/'\\u00' . unpack('H2',$1)/eg; # from JSON::Converter
    utf8::decode($str) if $self->{'utf8'} && &utf8::decode;

    ### escape <html> and </html> tags in the text
    $str =~ s{(</? (?: htm | scrip | !-))}{$1"+"}gx;

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

=head1 SYNOPSIS

    use CGI::Ex::JSONDump;

    my $js = JSONDump(\%complex_data, {pretty => 0});

    ### OR

    my $js = CGI::Ex::JSONDump->new({pretty => 0})->dump(\%complex_data);

=head1 DESCRIPTION

CGI::Ex::JSONDump is a very lightweight and fast data structure dumper.

=head1 METHODS

=over 4

=item new

=item dump

=item js_escape

=back

=head1 FUNCTIONS

=over 4

=item JSONDump

=back

=head1 AUTHORS

Paul Seamons <paul at seamons dot com>

=cut
