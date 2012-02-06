# -*- Mode: Perl; -*-

=head1 NAME

4_app_01_constants.t - Check for the importing of App constants

=head1 NOTE

These tests are extremely stripped down to test the basic path flow.  Normally
unit tests are useful for garnering information about a module.  For CGI::Ex::App
it is suggested to stick to live use cases or the CGI::Ex::App perldoc - though
we do try to put it through most paces.

=cut

use Test::More tests => 23;
use strict;
use warnings;
require CGI::Ex::App::Constants;
my $ok = sub { goto &ok };

{
    package Foo_1;
    use base qw(CGI::Ex::App);
    $ok->(__PACKAGE__->can('new'), "Can new");

    $ok->(!__PACKAGE__->can('App__info_complete__fail_and_show_page'), "CEAC - No methods imported yet");
    import CGI::Ex::App::Constants qw(App__info_complete__fail_and_show_page);
    $ok->(__PACKAGE__->can('App__info_complete__fail_and_show_page'), "CEAC - Now got the specific method");

    $ok->(!__PACKAGE__->can('App__info_complete__succeed_and_run_finalize'), "CEAC - No import of group level constants yet");
    import CGI::Ex::App::Constants qw(:App__info_complete);
    $ok->(__PACKAGE__->can('App__info_complete__succeed_and_run_finalize'), "CEAC - Now imported group level constants");

    $ok->(!__PACKAGE__->can('App__finalize__failed_and_show_page'), "CEAC - No import of all constants yet");
    import CGI::Ex::App::Constants qw(:App);
    $ok->(__PACKAGE__->can('App__finalize__failed_and_show_page'), "CEAC - Now imported group level constants using EXPORT_TAGS");
}

{
    package Foo_2;
    use base qw(CGI::Ex::App);

    $ok->(!__PACKAGE__->can('App__finalize__failed_and_show_page'), "CEAC - No import of all constants yet");
    import CGI::Ex::App::Constants qw();
    $ok->(__PACKAGE__->can('App__finalize__failed_and_show_page'), "CEAC - Now imported group level constants using default EXPORT");
}

{
    package Foo_3;
    use base qw(CGI::Ex::App);

    import CGI::Ex::App qw(2.0);
    $ok->(!__PACKAGE__->can('App__finalize__failed_and_show_page'), "CEA - No import of all constants yet");

    $ok->(!__PACKAGE__->can('App__info_complete__fail_and_show_page'), "CEA - No methods imported yet");
    import CGI::Ex::App qw(App__info_complete__fail_and_show_page);
    $ok->(__PACKAGE__->can('App__info_complete__fail_and_show_page'), "CEA - Now got the specific method");

    $ok->(!__PACKAGE__->can('App__info_complete__succeed_and_run_finalize'), "CEA - No import of group level constants yet");
    import CGI::Ex::App qw(:App__info_complete);
    $ok->(__PACKAGE__->can('App__info_complete__succeed_and_run_finalize'), "CEA - Now imported group level constants");

    $ok->(!__PACKAGE__->can('App__finalize__failed_and_show_page'), "CEA - No import of all constants yet");
    import CGI::Ex::App qw(:App);
    $ok->(__PACKAGE__->can('App__finalize__failed_and_show_page'), "CEA - Now imported group level constants using EXPORT_TAGS");
}

{
    package Foo_4;
    use base qw(CGI::Ex::App);
}
{
    package Foo_4_2;
    use base qw(Foo_4);

    __PACKAGE__->import(qw(2.0));
    $ok->(!__PACKAGE__->can('App__finalize__failed_and_show_page'), "Foo_4 - No import of all constants yet");

    $ok->(!__PACKAGE__->can('App__info_complete__fail_and_show_page'), "Foo_4 - No methods imported yet");
    __PACKAGE__->import(qw(App__info_complete__fail_and_show_page));
    $ok->(__PACKAGE__->can('App__info_complete__fail_and_show_page'), "Foo_4 - Now got the specific method");

    $ok->(!__PACKAGE__->can('App__info_complete__succeed_and_run_finalize'), "Foo_4 - No import of group level constants yet");
    __PACKAGE__->import(qw(:App__info_complete));
    $ok->(__PACKAGE__->can('App__info_complete__succeed_and_run_finalize'), "Foo_4 - Now imported group level constants");

    $ok->(!__PACKAGE__->can('App__finalize__failed_and_show_page'), "Foo_4 - No import of all constants yet");
    __PACKAGE__->import(qw(:App));
    $ok->(__PACKAGE__->can('App__finalize__failed_and_show_page'), "Foo_4 - Now imported group level constants using EXPORT_TAGS");
}

# functional - but rather not worry about portability
#{
#    package Foo_5;
#    $INC{'Foo_5.pm'} = 1;
#    use base qw(CGI::Ex::App);
#}
#{
#    package Foo_5_2;
#    eval "use Foo_5 qw(:App);";
#    $ok->(__PACKAGE__->can('App__info_complete__fail_and_show_page'), "Foo_5 - Now got the specific method");
#}
