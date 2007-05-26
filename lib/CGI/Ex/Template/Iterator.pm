package CGI::Ex::Template::Iterator;

=head1 NAME

CGI::Ex::Template::Iterator - Handle foreach iterations

=cut

sub new {
    my ($class, $items) = @_;
    $items = [] if ! defined $items;
    if (UNIVERSAL::isa($items, 'HASH')) {
	$items = [ map { {key => $_, value => $items->{ $_ }} } sort keys %$items ];
    } elsif (UNIVERSAL::can($items, 'as_list')) {
	$items = $items->as_list;
    } elsif (! UNIVERSAL::isa($items, 'ARRAY')) {
        $items = [$items];
    }
    return bless [$items, 0], $class;
}

sub get_first {
    my $self = shift;
    return (undef, 3) if ! @{ $self->[0] };
    return ($self->[0]->[$self->[1] = 0], undef);
}

sub get_next {
    my $self = shift;
    return (undef, 3) if ++ $self->[1] > $#{ $self->[0] };
    return ($self->items->[$self->[1]], undef);
}

sub items { shift->[0] }

sub index { shift->[1] }

sub max { $#{ shift->[0] } }

sub size { shift->max + 1 }

sub count { shift->index + 1 }

sub number { shift->index + 1 }

sub first { (shift->index == 0) || 0 }

sub last { my $self = shift; return ($self->index == $self->max) || 0 }

sub prev {
    my $self = shift;
    return undef if $self->index <= 0;
    return $self->items->[$self->index - 1];
}

sub next {
    my $self = shift;
    return undef if $self->index >= $self->max;
    return $self->items->[$self->index + 1];
}

1;
