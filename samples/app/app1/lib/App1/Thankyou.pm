package App1::Thankyou;

=head1 NAME

App1::Thankyou - show the final page of the application

=cut

use strict;
use warnings;
use base qw(App1);
use CGI::Ex::Dump qw(debug);

sub info_complete { 0 } # path officially ends here - don't try and run any other steps

sub hash_swap {
    my $self = shift;
    return {
        login_link => "some_sort_of_login_link",
    };
}

1;

