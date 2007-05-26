package CGI::Ex::Template::Exception;

=head1 NAME

CGI::Ex::Template::Exception - Handle exceptions

=cut

use overload
    '""' => \&as_string,
    bool => sub { defined shift },
    fallback => 1;

sub new {
    my ($class, $type, $info, $node, $pos, $doc) = @_;
    return bless [$type, $info, $node, $pos, $doc], $class;
}

sub type { shift->[0] }

sub info { shift->[1] }

sub node {
    my $self = shift;
    $self->[2] = shift if @_;
    $self->[2];
}

sub offset {
    my $self = shift;
    $self->[3] = shift if @_;
    $self->[3];
}

sub doc {
    my $self = shift;
    $self->[4] = shift if @_;
    $self->[4];
}

sub as_string {
    my $self = shift;
    if ($self->type =~ /^parse/) {
        if (my $doc = $self->doc) {
            my ($line, $char) = CGI::Ex::Template->get_line_number_by_index($doc, $self->offset, 'include_char');
            return $self->type ." error - $doc->{'name'} line $line char $char: ". $self->info;
        } else {
            return $self->type .' error - '. $self->info .' (At char '. $self->offset .')';
        }
    } else {
        return $self->type .' error - '. $self->info;
    }
}

###----------------------------------------------------------------###

1;
