# -*- Mode: Perl; -*-

=head1 NAME

7_template_03_syntax.t - Test the ability to parse different syntaxes.

=cut

use vars qw($module);
BEGIN {
    $module = 'CGI::Ex::Template';
};

use strict;
use Test::More tests => 5;
use Data::Dumper qw(Dumper);
use constant test_taint => 0 && eval { require Taint::Runtime };

use_ok($module);

Taint::Runtime::taint_start() if test_taint;

### find a place to allow for testing
my $test_dir = $0 .'.test_dir';
END { rmdir $test_dir }
mkdir $test_dir, 0755;
ok(-d $test_dir, "Got a test dir up and running");


sub process_ok { # process the value and say if it was ok
    my $str  = shift;
    my $test = shift;
    my $vars = shift || {};
    my $conf = local $vars->{'tt_config'} = $vars->{'tt_config'} || [];
    my $obj  = shift || $module->new(@$conf, ABSOLUTE => 1, INCLUDE_PATH => $test_dir); # new object each time
    my $out  = '';
    my $line = (caller)[2];
    delete $vars->{'tt_config'};

    Taint::Runtime::taint(\$str) if test_taint;

    $obj->process(\$str, $vars, \$out);
    my $ok = ref($test) ? $out =~ $test : $out eq $test;
    if ($ok) {
        ok(1, "Line $line   \"$str\" => \"$out\"");
        return $obj;
    } else {
        ok(0, "Line $line   \"$str\"");
        warn "# Was:\n$out\n# Should've been:\n$test\n";
        print $obj->error if $obj->can('error');
        print Dumper $obj->parse_tree(\$str) if $obj->can('parse_tree');
        exit;
    }
}

### create some files to include
my $foo_template = "$test_dir/foo.tt";
END { unlink $foo_template };
open(my $fh, ">$foo_template") || die "Couldn't open $foo_template: $!";
print $fh "([% template.foo %][% INCLUDE bar.tt %])";
close $fh;

###----------------------------------------------------------------###
print "### BASIC SYNTAX ###########################################################\n";

process_ok("[%- BLOCK a %]b is [% b %][% END %][% PROCESS a b => 237 | repeat(2) %]" => "", {tt_config => [SYNTAX => 'garbage']});
process_ok("[%- BLOCK a %]b is [% b %][% END %][% PROCESS a b => 237 | repeat(2) %]" => "b is 237237");
process_ok("[%- BLOCK a %]b is [% b %][% END %][% PROCESS a b => 237 | repeat(2) %]" => "b is 237237", {tt_config => [SYNTAX => 'cet']});
process_ok("[%- BLOCK a %]b is [% b %][% END %][% PROCESS a b => 237 | repeat(2) %]" => "b is 237237", {tt_config => [SYNTAX => 'tt3']});
process_ok("[%- BLOCK a %]b is [% b %][% END %][% PROCESS a b => 237 | repeat(2) %]" => "b is 237b is 237", {tt_config => [SYNTAX => 'tt2']});
process_ok("[%- BLOCK a %]b is [% b %][% END %][% PROCESS a b => 237 | repeat(2) %]" => "b is 237b is 237", {tt_config => [SYNTAX => 'tt1']});
process_ok("[%- BLOCK a %]b is [% b %][% END %][% PROCESS a b => 237 | repeat(2) %]" => "b is 237b is 237", {tt_config => [SYNTAX => 'tt1']});


process_ok('[% a %]|[% $a %]|[% ${ a } %]|[% ${ "a" } %]' => 'A|bar|bar|A', {a => 'A', A => 'bar'});
process_ok('[% a %]|[% $a %]|[% ${ a } %]|[% ${ "a" } %]' => 'A|bar|bar|A', {a => 'A', A => 'bar', tt_config => [SYNTAX => 'tt2']});
process_ok('[% a %]|[% $a %]|[% ${ a } %]|[% ${ "a" } %]' => 'A|A|bar|A', {a => 'A', A => 'bar', tt_config => [SYNTAX => 'tt1']});

###----------------------------------------------------------------###
print "### DONE #############################################################\n";
