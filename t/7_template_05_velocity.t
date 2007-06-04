# -*- Mode: Perl; -*-

=head1 NAME

7_template_05_velocity.t - Test the ability to parse and play VTL (Velocity Template Language)

=cut

use vars qw($module $compile_perl);
BEGIN {
    $module = 'CGI::Ex::Template';
};

use strict;
use Test::More tests => 51;
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
    my $conf = local $vars->{'_config'} = $vars->{'_config'} || [];
    push @$conf, (COMPILE_PERL => $compile_perl) if $compile_perl;
    my $obj  = shift || $module->new(@$conf); # new object each time
    my $out  = '';
    my $line = (caller)[2];
    delete $vars->{'tt_config'};

    Taint::Runtime::taint(\$str) if test_taint;

    $obj->process_simple(\$str, $vars, \$out);
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
my $foo_template = "$test_dir/foo.tmpl";
END { unlink $foo_template };
open(my $fh, ">$foo_template") || die "Couldn't open $foo_template: $!";
print $fh "Good Day!";
close $fh;

### create some files to include
my $bar_template = "$test_dir/bar.tmpl";
END { unlink $bar_template };
open($fh, ">$bar_template") || die "Couldn't open $bar_template: $!";
print $fh "(#[echo \$bar]#)";
close $fh;

for $compile_perl (0, 1) {
    my $is_compile_perl = "compile perl ($compile_perl)";

###----------------------------------------------------------------###
print "### ECHO ############################################ $is_compile_perl\n";

process_ok("Foo" => "Foo");

process_ok('#[echo $foo]#bar' => "bar");
process_ok('#[echo $foo]#' => "FOO", {foo => "FOO"});
process_ok('#[echo $foo $foo]#' => "FOOFOO", {foo => "FOO"});
process_ok('#[echo $foo "bar" $foo]#' => "FOObarFOO", {foo => "FOO"});
process_ok('#[echo "hi"]#' => "hi", {foo => "FOO"});

###----------------------------------------------------------------###
print "### COMMENT ######################################### $is_compile_perl\n";

process_ok('#[comment]# Hi there #[endcomment]#bar' => "bar", {foo => "FOO"});

###----------------------------------------------------------------###
print "### IF / ELSIF / ELSE / IFN ######################### $is_compile_perl\n";

process_ok('#[if $foo]#bar#[endif]#bar' => "bar");
process_ok('#[if "1"]#bar#[endif]#' => "bar");
process_ok('#[if $foo]#bar#[endif]#' => "", {foo => ""});
process_ok('#[if $foo]#bar#[endif]#' => "bar", {foo => "1"});
process_ok('#[ifn $foo]#bar#[endifn]#' => "bar", {foo => ""});
process_ok('#[ifn $foo]#bar#[endifn]#' => "", {foo => "1"});

####----------------------------------------------------------------###
print "### INCLUDE ######################################### $is_compile_perl\n";

process_ok('#[include "foo.tmpl"]#' => "Good Day!");
process_ok("#[include \"$test_dir/foo.tmpl\"]#" => "Good Day!");

process_ok('#[include "bar.tmpl"]#' => "()");
process_ok('#[include "bar.tmpl"]#' => "(hi)", {bar => 'hi'});

###----------------------------------------------------------------###
print "### LOOP ############################################ $is_compile_perl\n";

process_ok('#[loop "loop1"]#Hi#[endloop]#foo' => "foo");
process_ok('#[loop "loop1"]#Hi#[endloop]#foo' => "Hifoo", {set_loop => [{}]});
process_ok('#[loop "loop1"]##[echo $bar]##[endloop]#foo' => "bingfoo", {set_loop => [{bar => 'bing'}]});
process_ok('#[loop "loop1"]##[echo $bar]##[endloop]#foo' => "bingbangfoo", {set_loop => [{bar => 'bing'}, {bar => 'bang'}]});
process_ok('#[loop "loop1"]##[echo $boop]##[endloop]#foo' => "bopfoo", {boop => 'bop', set_loop => [{bar => 'bing'}]});

###----------------------------------------------------------------###
print "### TT3 DIRECTIVES ################################## $is_compile_perl\n";







###----------------------------------------------------------------###
print "### TT3 CHOMPING #################################### $is_compile_perl\n";


###----------------------------------------------------------------###
print "### TT3 INTERPOLATE ################################# $is_compile_perl\n";


###----------------------------------------------------------------###
print "### DONE ############################################ $is_compile_perl\n";
} # end of for
