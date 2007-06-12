package CGI::Ex::Template;

=head1 NAME

CGI::Ex::Template - Template::Alloy based TT2/TT3/HT/HTE/Tmpl/Velocity engine.

=cut

use strict;
use warnings;
use Template::Alloy qw(1.002);
use base qw(Template::Alloy);

our $VERSION = '2.13';

1;

__END__

=head1 SYNOPSIS

=head2 Template::Toolkit style usage

    my $t = Template::Alloy->new(
        INCLUDE_PATH => ['/path/to/templates'],
    );

    my $swap = {
        key1 => 'val1',
        key2 => 'val2',
        code => sub { 42 },
        hash => {a => 'b'},
    };

    # print to STDOUT
    $t->process('my/template.tt', $swap)
        || die $t->error;

    # process into a variable
    my $out = '';
    $t->process('my/template.tt', $swap, \$out);

    ### Alloy uses the same syntax and configuration as Template::Toolkit


=head2 HTML::Template::Expr style usage

    my $t = Template::Alloy->new(
        filename => 'my/template.ht',
        path     => ['/path/to/templates'],
    );

    my $swap = {
        key1 => 'val1',
        key2 => 'val2',
        code => sub { 42 },
        hash => {a => 'b'},
    };

    $t->param($swap);

    # print to STDOUT (errors die)
    $t->output(print_to => \*STDOUT);

    # process into a variable
    my $out = $t->output;

    ### Alloy can also use the same syntax and configuration as HTML::Template

=head2 Text::Tmpl style usage

    my $t = Template::Alloy->new;

    my $swap = {
        key1 => 'val1',
        key2 => 'val2',
        code => sub { 42 },
        hash => {a => 'b'},
    };

    $t->set_delimiters('#[', ']#');
    $t->set_strip(0);
    $t->set_values($swap);
    $t->set_dir('/path/to/templates');

    my $out = $t->parse_file('my/template.tmpl');

    my $str = "Foo #[echo $key1]# Bar";
    my $out = $t->parse_string($str);


    ### Alloy uses the same syntax and configuration as Text::Tmpl

=head2 Velocity (VTL) style usage

    my $t = Template::Alloy->new;

    my $swap = {
        key1 => 'val1',
        key2 => 'val2',
        code => sub { 42 },
        hash => {a => 'b'},
    };

    my $out = $t->merge('my/template.vtl', $swap);

    my $str = "#set($foo 1 + 3) ($foo) ($bar) ($!baz)";
    my $out = $t->merge(\$str, $swap);

=head1 DESCRIPTION

CGI::Ex::Template is the original base for the code that is now Template::Alloy.
Template::Alloy employed enough complexity and featureset to warrant moving it
out to a separate namespace.

CGI::Ex::Template is now a place holder subclass of Template::Alloy.  You
can use CGI::Ex::Template standalone - but it is suggested that you use
Template::Alloy directly instead.

=head1 AUTHOR

Paul Seamons <perl at seamons dot com>

=head1 LICENSE

This module may be distributed under the same terms as Perl itself.

=cut
