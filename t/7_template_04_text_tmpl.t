# -*- Mode: Perl; -*-

=head1 NAME

7_template_04_text_tmpl.t - Test the ability to parse and play Text::Tmpl

=cut

use vars qw($module $is_tt);
BEGIN {
    $module = 'CGI::Ex::Template';
    $module = 'Text::Tmpl';
    $is_tt = $module eq 'Text::Tmpl';
};

use strict;
use Test::More tests => (! $is_tt) ? 92 : 23;
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
    $obj = shift || $module->new; # new object each time
    $obj->set_delimiters('#[', ']#');
    $obj->set_strip(0);
    $obj->set_values($vars);
    $out = $obj->parse_string($str);
    my $err = eval($module."::strerror()");
    $out = '' if ! defined $out;

    my $ok = ref($test) ? $out =~ $test : $out eq $test;
    if ($ok) {
        ok(1, "Line $line   \"$str\" => \"$out\"");
        return $obj;
    } else {
        ok(0, "Line $line   \"$str\"");
        warn "# Was:\n$out\n# Should've been:\n$test\n";
        print "$err\n";
        if ($obj && $obj->can('parse_tree')) {
            local $obj->{'SYNTAX'} = 'hte';
            print Dumper $obj->parse_tree(\$str);
            print $err;
        }
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
print "### ECHO #############################################################\n";

process_ok("Foo" => "Foo");

process_ok("#[echo \$foo]#" => "FOO", {foo => "FOO"});
process_ok("#[echo \"hi\"]#" => "hi", {foo => "FOO"});
process_ok("#[echo 'hi']#" => "hi", {foo => "FOO"}) if ! $is_tt;
process_ok("#[echo foo]#" => "", {foo => "FOO"});

###----------------------------------------------------------------###
#print "### IF / ELSE / UNLESS ###############################################\n";
#
#process_ok("<TMPL_IF foo>bar</TMPL_IF>" => "", {foo => ""});
#process_ok("<TMPL_IF foo>bar</TMPL_IF>" => "bar", {foo => "1"});
#process_ok("<TMPL_IF foo>bar<TMPL_ELSE>bing</TMPL_IF>" => "bing", {foo => ''});
#process_ok("<TMPL_IF foo>bar<TMPL_ELSE>bing</TMPL_IF>" => "bar",  {foo => '1'});
#process_ok("<TMPL_UNLESS foo>bar</TMPL_UNLESS>" => "bar", {foo => ""});
#process_ok("<TMPL_UNLESS foo>bar</TMPL_UNLESS>" => "", {foo => "1"});
#
#process_ok("<TMPL_IF ESCAPE=HTML foo>bar</TMPL_IF>baz" => "", {foo => "1"});
#process_ok("<TMPL_IF DEFAULT=bar foo>bar</TMPL_IF>baz" => "", {foo => "1"});
#
####----------------------------------------------------------------###
#print "### INCLUDE ##########################################################\n";
#
#process_ok("<TMPL_INCLUDE blah>" => "");
#process_ok("<TMPL_INCLUDE foo.ht>" => "Good Day!");
#process_ok("<TMPL_INCLUDE $test_dir/foo.ht>" => "Good Day!", {tt_config => [path => '']});
#process_ok("<TMPL_INCLUDE NAME=foo.ht>" => "Good Day!");
#process_ok("<TMPL_INCLUDE NAME='foo.ht'>" => "Good Day!");
#process_ok("<TMPL_INCLUDE NAME='foo.ht'>" => "", {tt_config => [no_includes => 1]});
#
#process_ok("<TMPL_INCLUDE ESCAPE=HTML NAME='foo.ht'>" => "");
#process_ok("<TMPL_INCLUDE DEFAULT=bar NAME='foo.ht'>" => "");
#
#process_ok("<TMPL_INCLUDE EXPR=\"'foo.ht'\">" => "Good Day!")                if $is_cet;
#process_ok("<TMPL_INCLUDE EXPR=\"foo\">" => "Good Day!", {foo => 'foo.ht'})  if $is_cet;
#process_ok("<TMPL_INCLUDE EXPR=\"sprintf('%s', 'foo.ht')\">" => "Good Day!") if $is_cet;
#
####----------------------------------------------------------------###
#print "### EXPR #############################################################\n";
#
#process_ok("<TMPL_VAR EXPR=\"sprintf('%d', foo)\">" => "777", {foo => "777"}) if ! $is_ht;
#process_ok("<TMPL_VAR EXPR=\"sprintf('%d', foo)\">" => "777", {foo => "777"}) if ! $is_ht;
#process_ok("<TMPL_VAR EXPR='sprintf(\"%d\", foo)'>" => "777", {foo => "777"}) if ! $is_ht && ! $is_hte; # odd that HTE can't parse this
#process_ok("<TMPL_VAR EXPR=\"sprintf(\"%d\", foo)\">" => "777", {foo => "777"}) if ! $is_ht;
#process_ok("<TMPL_VAR EXPR=sprintf(\"%d\", foo)>" => "777", {foo => "777"}) if ! $is_ht && ! $is_hte;
#process_ok("<TMPL_VAR EXPR=\"sprintf('%s', foo)\">" => "<>", {foo => "<>"}) if ! $is_ht;
#process_ok("<TMPL_VAR ESCAPE=HTML EXPR=\"sprintf('%s', foo)\">" => "", {foo => "<>"});
#process_ok("<TMPL_VAR DEFAULT=bar EXPR=foo>" => "", {foo => "FOO", bar => "BAR"});
#
#process_ok("<!--TMPL_VAR EXPR=\"foo\"-->" => "FOO", {foo => "FOO"}) if ! $is_ht;;
#
####----------------------------------------------------------------###
#print "### LOOP #############################################################\n";
#
#process_ok("<TMPL_LOOP blah></TMPL_LOOP>foo" => "foo");
#process_ok("<TMPL_LOOP blah>Hi</TMPL_LOOP>foo" => "", {blah => 1});
#process_ok("<TMPL_LOOP blah>Hi</TMPL_LOOP>foo" => "Hifoo", {blah => {wow => 1}}) if $is_cet;
#process_ok("<TMPL_LOOP blah>Hi</TMPL_LOOP>foo" => "HiHifoo", {blah => [{}, {}]});
#process_ok("<TMPL_LOOP blah>(<TMPL_VAR i>)</TMPL_LOOP>foo" => "(1)(2)(3)foo", {blah => [{i=>1}, {i=>2}, {i=>3}]});
#process_ok("<TMPL_LOOP NAME=\"blah\">(<TMPL_VAR i>)</TMPL_LOOP>foo" => "(1)(2)(3)foo", {blah => [{i=>1}, {i=>2}, {i=>3}]});
#process_ok("<TMPL_LOOP EXPR=\"blah\">(<TMPL_VAR i>)</TMPL_LOOP>foo" => "(1)(2)(3)foo", {blah => [{i=>1}, {i=>2}, {i=>3}]}) if $is_cet;
#process_ok("<TMPL_LOOP blah>(<TMPL_VAR i>)(<TMPL_VAR blue>)</TMPL_LOOP>foo" => "(1)()(2)()(3)()foo", {blah => [{i=>1}, {i=>2}, {i=>3}], blue => 'B'}) if $is_ht;
#process_ok("<TMPL_LOOP blah>(<TMPL_VAR i>)(<TMPL_VAR blue>)</TMPL_LOOP>foo" => "(1)(B)(2)(B)(3)(B)foo", {blah => [{i=>1}, {i=>2}, {i=>3}], blue => 'B', tt_config => [GLOBAL_VARS => 1]});
#
#process_ok("<TMPL_LOOP blah>(<TMPL_VAR i>)(<TMPL_VAR blue>)</TMPL_LOOP>foo" => "(1)()(2)()(3)()foo", {blah => [{i=>1}, {i=>2}, {i=>3}], blue => 'B', tt_config => [SYNTAX => 'ht']}) if $is_cet;
#process_ok("<TMPL_LOOP blah>(<TMPL_VAR i>)(<TMPL_VAR blue>)</TMPL_LOOP>foo" => "(1)(B)(2)(B)(3)(B)foo", {blah => [{i=>1}, {i=>2}, {i=>3}], blue => 'B', tt_config => [GLOBAL_VARS => 1, SYNTAX => 'ht']}) if $is_cet;
#
#process_ok("<TMPL_LOOP blah>(<TMPL_VAR i>)</TMPL_LOOP>foo" => "(1)()(3)foo", {blah => [{i=>1}, undef, {i=>3}]});
#
#process_ok("<TMPL_LOOP blah>\n(<TMPL_VAR __first__>|<TMPL_VAR __last__>|<TMPL_VAR __odd__>|<TMPL_VAR __inner__>|<TMPL_VAR __counter__>)</TMPL_LOOP>foo" => "
#(||||)
#(||||)
#(||||)foo", {blah => [undef, undef, undef]});
#
#process_ok("<TMPL_LOOP blah>\n(<TMPL_VAR __first__>|<TMPL_VAR __last__>|<TMPL_VAR __odd__>|<TMPL_VAR __inner__>|<TMPL_VAR __counter__>)</TMPL_LOOP>foo" => "
#(1||1|0|1)
#(0|0||1|2)
#(0|1|1|0|3)foo", {blah => [undef, undef, undef], tt_config => [LOOP_CONTEXT_VARS => 1]}) if ! $is_cet;
#
#process_ok("<TMPL_LOOP blah>\n(<TMPL_VAR __first__>|<TMPL_VAR __last__>|<TMPL_VAR __odd__>|<TMPL_VAR __inner__>|<TMPL_VAR __counter__>)</TMPL_LOOP>foo" => "
#(1|0|1|0|1)
#(0|0|0|1|2)
#(0|1|1|0|3)foo", {blah => [undef, undef, undef], tt_config => [LOOP_CONTEXT_VARS => 1]}) if $is_cet;
#
#
#process_ok("<TMPL_LOOP NAME=\"blah\"><TMPL_IF EXPR='i==2'><TMPL_NEXT></TMPL_IF>(<TMPL_VAR i>)</TMPL_LOOP>foo" => "(1)(3)foo", {blah => [{i=>1}, {i=>2}, {i=>3}]}) if $is_cet;
#
####----------------------------------------------------------------###
#print "### TT3 DIRECTIVES ###################################################\n";
#
#process_ok("<TMPL_GET foo>" => "FOO", {foo => "FOO"})    if $is_cet;
#process_ok("<TMPL_GET foo>" => "", {foo => "FOO", tt_config => [NO_TT => 1]}) if $is_cet;
#process_ok("<TMPL_GET foo>" => "", {foo => "FOO", tt_config => [SYNTAX => 'ht']}) if $is_cet;
#process_ok("<TMPL_GET 1+2+3+4>" => "10", {foo => "FOO"}) if $is_cet;
#
#process_ok("<TMPL_IF foo>bar<TMPL_ELSIF wow>wee<TMPL_ELSE>bing</TMPL_IF>" => "bar", {foo => "1"}) if $is_cet;
#
#process_ok("<TMPL_SET i = 'foo'>(<TMPL_VAR i>)" => "(foo)") if $is_cet;
#process_ok("<TMPL_SET i = 'foo'>(<TMPL_GET i>)" => "(foo)") if $is_cet;
#process_ok("<TMPL_FOR i IN [1..3]>(<TMPL_VAR i>)</TMPL_FOR>" => "(1)(2)(3)") if $is_cet;
#
#process_ok("<TMPL_BLOCK foo>(<TMPL_VAR i>)</TMPL_BLOCK><TMPL_PROCESS foo i='bar'>" => "(bar)") if $is_cet;
#process_ok("<TMPL_BLOCK foo>(<TMPL_VAR i>)</TMPL_BLOCK><TMPL_SET wow = PROCESS foo i='bar'><TMPL_VAR wow>" => "(bar)") if $is_cet;
#
#process_ok("<TMPL_GET template.foo><TMPL_META foo = 'bar'>" => "bar") if $is_cet;
#
#process_ok('<TMPL_MACRO bar(n) BLOCK>You said <TMPL_VAR n></TMPL_MACRO><TMPL_GET bar("hello")>' => 'You said hello') if $is_cet;
#
####----------------------------------------------------------------###
#print "### TT3 CHOMPING #####################################################\n";
#
#process_ok("\n<TMPL_GET foo>" => "\nFOO", {foo => "FOO"}) if $is_cet;
#process_ok("<TMPL_GET foo->\n" => "FOO", {foo => "FOO"})  if $is_cet;
#process_ok("\n<-TMPL_GET foo>" => "FOO", {foo => "FOO"})  if $is_cet;
#
####----------------------------------------------------------------###
#print "### TT3 INTERPOLATE ##################################################\n";
#
#process_ok('$foo <TMPL_GET foo> ${ 1 + 2 }' => '$foo FOO ${ 1 + 2 }', {foo => "FOO"});
#process_ok('$foo <TMPL_GET foo> ${ 1 + 2 }' => 'FOO FOO 3', {foo => "FOO", tt_config => [INTERPOLATE => 1]}) if $is_cet;
#process_ok('<TMPL_CONFIG INTERPOLATE => 1>$foo <TMPL_GET foo> ${ 1 + 2 }' => 'FOO FOO 3', {foo => "FOO"}) if $is_cet;
#
####----------------------------------------------------------------###
#print "### DONE #############################################################\n";
