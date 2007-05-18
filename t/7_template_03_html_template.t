# -*- Mode: Perl; -*-

=head1 NAME

7_template_03_html_template.t - Test the ability to parse and play html template

=cut

use vars qw($module $is_ht $is_hte $is_cet);
BEGIN {
    $module = 'CGI::Ex::Template';
#    $module = 'HTML::Template';
#    $module = 'HTML::Template::Expr';
    $is_hte = $module eq 'HTML::Template::Expr';
    $is_ht  = $module eq 'HTML::Template';
    $is_cet = $module eq 'CGI::Ex::Template';
};

use strict;
use Test::More tests => ($is_cet) ? 45 : ($is_hte) ? 31 : 28;
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
    my $line = (caller)[2];
    delete $vars->{'tt_config'};

    Taint::Runtime::taint(\$str) if test_taint;

    my $obj;
    my $out;
    eval {
        $obj = shift || $module->new(scalarref => \$str, @$conf, die_on_bad_params => 0, path => $test_dir); # new object each time
        $obj->param($vars);
        $out = $obj->output;
    };
    my $err = $@;

    my $ok = ref($test) ? $out =~ $test : $out eq $test;
    if ($ok) {
        ok(1, "Line $line   \"$str\" => \"$out\"");
        return $obj;
    } else {
        ok(0, "Line $line   \"$str\"");
        warn "# Was:\n$out\n# Should've been:\n$test\n";
        print "$err\n";
        print Dumper $obj->parse_tree(\$str) if $obj && $obj->can('parse_tree');
        exit;
    }
}

### create some files to include
my $foo_template = "$test_dir/foo.ht";
END { unlink $foo_template };
open(my $fh, ">$foo_template") || die "Couldn't open $foo_template: $!";
print $fh "Good Day!";
close $fh;

###----------------------------------------------------------------###
print "### HTML::Template::Expr SYNTAX ######################################\n";

process_ok("Foo" => "Foo");

###----------------------------------------------------------------###
print "### VAR ##############################################################\n";

process_ok("<TMPL_VAR foo>" => "FOO", {foo => "FOO"});
process_ok("<TMPL_VAR name=foo>" => "FOO", {foo => "FOO"});
process_ok("<TMPL_VAR NAME=foo>" => "FOO", {foo => "FOO"});
process_ok("<TMPL_VAR NAME=\"foo\">" => "FOO", {foo => "FOO"});
process_ok("<TMPL_VAR NAME='foo'>" => "FOO", {foo => "FOO"});
process_ok("<TMPL_VAR NAME='foo' >" => "FOO", {foo => "FOO"});
process_ok("<TMPL_VAR foo >" => "FOO", {foo => "FOO"});

process_ok("<TMPL_VAR ESCAPE=html     foo>" => "&lt;&gt;", {foo => "<>"});
process_ok("<TMPL_VAR ESCAPE=HTML     foo>" => "&lt;&gt;", {foo => "<>"});
process_ok("<TMPL_VAR ESCAPE=\"HTML\" foo>" => "&lt;&gt;", {foo => "<>"});
process_ok("<TMPL_VAR ESCAPE='HTML'   foo>" => "&lt;&gt;", {foo => "<>"});
process_ok("<TMPL_VAR ESCAPE=1        foo>" => "&lt;&gt;", {foo => "<>"});
process_ok("<TMPL_VAR ESCAPE=0        foo>" => "<>", {foo => "<>"});
process_ok("<TMPL_VAR ESCAPE=NONE     foo>" => "<>", {foo => "<>"});
process_ok("<TMPL_VAR ESCAPE=URL      foo>" => "%3C%3E", {foo => "<>"});

###----------------------------------------------------------------###
print "### IF / ELSE / UNLESS ###############################################\n";

process_ok("<TMPL_IF foo>bar</TMPL_IF>" => "", {foo => ""});
process_ok("<TMPL_IF foo>bar</TMPL_IF>" => "bar", {foo => "1"});
process_ok("<TMPL_IF foo>bar<TMPL_ELSE>bing</TMPL_IF>" => "bing", {foo => ''});
process_ok("<TMPL_IF foo>bar<TMPL_ELSE>bing</TMPL_IF>" => "bar",  {foo => '1'});
process_ok("<TMPL_UNLESS foo>bar</TMPL_UNLESS>" => "bar", {foo => ""});
process_ok("<TMPL_UNLESS foo>bar</TMPL_UNLESS>" => "", {foo => "1"});

###----------------------------------------------------------------###
print "### TT3 DIRECTIVES ###################################################\n";

process_ok("<TMPL_GET foo>" => "FOO", {foo => "FOO"})    if $is_cet;
process_ok("<TMPL_GET 1+2+3+4>" => "10", {foo => "FOO"}) if $is_cet;

process_ok("<TMPL_IF foo>bar<TMPL_ELSIF wow>wee<TMPL_ELSE>bing</TMPL_IF>" => "bar", {foo => "1"}) if $is_cet;

process_ok("<TMPL_SET i = 'foo'>(<TMPL_VAR i>)" => "(foo)") if $is_cet;
process_ok("<TMPL_SET i = 'foo'>(<TMPL_GET i>)" => "(foo)") if $is_cet;
process_ok("<TMPL_FOR i IN [1..3]>(<TMPL_VAR i>)</TMPL_FOR>" => "(1)(2)(3)") if $is_cet;

process_ok("<TMPL_BLOCK foo>(<TMPL_VAR i>)</TMPL_BLOCK><TMPL_PROCESS foo i='bar'>" => "(bar)") if $is_cet;

process_ok("<TMPL_GET template.foo><TMPL_META foo = 'bar'>" => "bar") if $is_cet;

###----------------------------------------------------------------###
print "### EXPR #############################################################\n";

process_ok("<TMPL_VAR EXPR=\"sprintf('%d', foo)\">" => "777", {foo => "777"}) if ! $is_ht;
process_ok("<TMPL_VAR EXPR='sprintf(\"%d\", foo)'>" => "777", {foo => "777"}) if ! $is_ht && ! $is_hte; # odd that HTE can't parse this
process_ok("<TMPL_VAR EXPR=\"sprintf(\"%d\", foo)\">" => "777", {foo => "777"}) if ! $is_ht;
process_ok("<TMPL_VAR EXPR=sprintf(\"%d\", foo)>" => "777", {foo => "777"}) if ! $is_ht && ! $is_hte;
process_ok("<TMPL_VAR EXPR=\"sprintf('%s', foo)\">" => "<>", {foo => "<>"}) if ! $is_ht;
process_ok("<TMPL_VAR ESCAPE=HTML EXPR=\"sprintf('%s', foo)\">" => "&lt;&gt;", {foo => "<>"}) if ! $is_ht && ! $is_hte;

###----------------------------------------------------------------###
print "### INCLUDE ##########################################################\n";

process_ok("<TMPL_INCLUDE blah>" => "");
process_ok("<TMPL_INCLUDE foo.ht>" => "Good Day!");
process_ok("<TMPL_INCLUDE NAME=foo.ht>" => "Good Day!");
process_ok("<TMPL_INCLUDE NAME='foo.ht'>" => "Good Day!");
process_ok("<TMPL_INCLUDE EXPR=\"'foo.ht'\">" => "Good Day!") if $is_cet;
process_ok("<TMPL_INCLUDE EXPR=\"foo\">" => "Good Day!", {foo => 'foo.ht'}) if $is_cet;
process_ok("<TMPL_INCLUDE EXPR=\"sprintf('%s', 'foo.ht')\">" => "Good Day!") if $is_cet;

###----------------------------------------------------------------###
print "### DONE #############################################################\n";
