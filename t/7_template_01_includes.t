# -*- Mode: Perl; -*-

=head1 NAME

7_template_01_includes.t - Test the file include functionality of CGI::Ex::Template - including some edge cases

=cut

use vars qw($module $is_tt);
BEGIN {
    $module = 'CGI::Ex::Template';
#    $module = 'Template';
    $is_tt = $module eq 'Template';
};

use strict;
use Test::More tests => (! $is_tt) ? 97 : 90;
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

###
my $bar_template = "$test_dir/bar.tt";
END { unlink $bar_template };
open($fh, ">$bar_template") || die "Couldn't open $bar_template: $!";
print $fh "[% blue %]BAR";
close $fh;

my $baz_template = "$test_dir/baz.tt";
END { unlink $baz_template };
open($fh, ">$baz_template") || die "Couldn't open $baz_template: $!";
print $fh "[% SET baz = 42 %][% baz %][% bing %]";
close $fh;

###
my $wrap_template = "$test_dir/wrap.tt";
END { unlink $wrap_template };
open($fh, ">$wrap_template") || die "Couldn't open $wrap_template: $!";
print $fh "Hi[% baz; template.foo; baz = 'wrap' %][% content %]there";
close $fh;

###
my $meta_template = "$test_dir/meta.tt";
END { unlink $meta_template };
open($fh, ">$meta_template") || die "Couldn't open $meta_template: $!";
print $fh "[% META bar='meta.tt' %]Metafoo([% component.foo %]) Metabar([% component.bar %])";
close $fh;

###
my $catch_template = "$test_dir/catch.tt";
END { unlink $catch_template };
open($fh, ">$catch_template") || die "Couldn't open $catch_template: $!";
print $fh "Error ([% error.type %]) - ([% error.info %])";
close $fh;

###
my $catch2_template = "$test_dir/catch2.tt";
END { unlink $catch2_template };
open($fh, ">$catch2_template") || die "Couldn't open $catch2_template: $!";
print $fh "Error2 ([% error.type %]) - ([% error.info %])";
close $fh;

###
my $die_template = "$test_dir/die.tt";
END { unlink $die_template };
open($fh, ">$die_template") || die "Couldn't open $die_template: $!";
print $fh "[% THROW bing 'blang' %])";
close $fh;

###
my $config_template = "$test_dir/config.tt";
END { unlink $config_template };
open($fh, ">$config_template") || die "Couldn't open $config_template: $!";
print $fh "[% CONFIG DUMP => {html => 1} %][% DUMP foo %]";
close $fh;

###
my $template_template = "$test_dir/template.tt";
END { unlink $template_template };
open($fh, ">$template_template") || die "Couldn't open $template_template: $!";
print $fh "<<[% PROCESS \$template %][% content %]>>";
close $fh;

###----------------------------------------------------------------###
print "### INSERT ###########################################################\n";

process_ok("([% INSERT bar.tt %])" => '([% blue %]BAR)');
process_ok("([% SET file = 'bar.tt' %][% INSERT \$file %])"     => '([% blue %]BAR)');
process_ok("([% SET file = 'bar.tt' %][% INSERT \${file} %])"   => '([% blue %]BAR)') if ! $is_tt;
process_ok("([% SET file = 'bar.tt' %][% INSERT \"\$file\" %])" => '([% blue %]BAR)');
process_ok("([% SET file = 'bar' %][% INSERT \"\${file}.tt\" %])" => '([% blue %]BAR)');

###----------------------------------------------------------------###
print "### INCLUDE ##########################################################\n";

process_ok("([% INCLUDE bar.tt %])" => '(BAR)');
process_ok("[% PROCESS foo.tt %]" => '(BAR)');
process_ok("[% PROCESS meta.tt %]" => 'Metafoo() Metabar(meta.tt)');
process_ok("[% META foo = 'string'; PROCESS meta.tt %]" => 'Metafoo() Metabar(meta.tt)');
process_ok("[% PROCESS meta.tt %][% template.bar %]" => 'Metafoo() Metabar(meta.tt)');
process_ok("[% META foo = 'meta'; PROCESS foo.tt %]" => '(metaBAR)');
process_ok("([% SET file = 'bar.tt' %][% INCLUDE \$file %])" => '(BAR)');
process_ok("([% SET file = 'bar.tt' %][% INCLUDE \${file} %])" => '(BAR)') if ! $is_tt;
process_ok("([% SET file = 'bar.tt' %][% INCLUDE \"\$file\" %])" => '(BAR)');
process_ok("([% SET file = 'bar' %][% INCLUDE \"\${file}.tt\" %])" => '(BAR)');

process_ok("([% INCLUDE baz.tt %])" => '(42)');
process_ok("([% INCLUDE baz.tt %])[% baz %]" => '(42)');
process_ok("[% SET baz = 21 %]([% INCLUDE baz.tt %])[% baz %]" => '(42)21');

###----------------------------------------------------------------###
print "### PROCESS ##########################################################\n";

process_ok("([% PROCESS bar.tt %])" => '(BAR)');
process_ok("[% PROCESS foo.tt %]" => '(BAR)');
process_ok("[% PROCESS meta.tt %]" => 'Metafoo() Metabar(meta.tt)');
process_ok("[% META foo = 'string'; PROCESS meta.tt %]" => 'Metafoo() Metabar(meta.tt)');
process_ok("[% PROCESS meta.tt %][% template.bar %]" => 'Metafoo() Metabar(meta.tt)');
process_ok("[% META foo = 'meta'; PROCESS foo.tt %]" => '(metaBAR)');
process_ok("([% SET file = 'bar.tt' %][% PROCESS \$file %])" => '(BAR)');
process_ok("([% SET file = 'bar.tt' %][% PROCESS \${file} %])" => '(BAR)') if ! $is_tt;
process_ok("([% SET file = 'bar.tt' %][% PROCESS \"\$file\" %])" => '(BAR)');
process_ok("([% SET file = 'bar' %][% PROCESS \"\${file}.tt\" %])" => '(BAR)');

process_ok("([% PROCESS baz.tt %])" => '(42)');
process_ok("([% PROCESS baz.tt %])[% baz %]" => '(42)42');
process_ok("[% SET baz = 21 %]([% PROCESS baz.tt %])[% baz %]" => '(42)42');

###----------------------------------------------------------------###
print "### WRAPPER ##########################################################\n";

process_ok("([% WRAPPER wrap.tt %])" => '');
process_ok("([% WRAPPER wrap.tt %] one [% END %])" => '(Hi one there)');
process_ok("([% WRAPPER wrap.tt %] ([% baz %]) [% END %])" => '(Hi () there)');
process_ok("([% WRAPPER wrap.tt %] one [% END %])" => '(HiBAZ one there)', {baz => 'BAZ'});
process_ok("([% WRAPPER wrap.tt %] ([% baz; baz='-local' %]) [% END %][% baz %])" => '(Hi-local () there-local)');
process_ok("([% WRAPPER wrap.tt %][% META foo='BLAM' %] [% END %])" => '(HiBLAM there)');

###----------------------------------------------------------------###
print "### CONFIG PRE_PROCESS ###############################################\n";

process_ok("Foo" => "BARFoo",      {tt_config => [PRE_PROCESS => 'bar.tt']});
process_ok("Foo" => "BARFoo",      {tt_config => [PRE_PROCESS => ['bar.tt']]});
process_ok("Foo" => "(BAR)BARFoo", {tt_config => [PRE_PROCESS => ['foo.tt', 'bar.tt']]});
process_ok("Foo" => "BlueBARFoo",  {tt_config => [PRE_PROCESS => 'bar.tt'], blue => 'Blue'});
process_ok("Foo[% blue='Blue' %]" => "BARFoo", {tt_config => [PRE_PROCESS => 'bar.tt']});
process_ok("Foo[% META foo='meta' %]" => "(metaBAR)Foo", {tt_config => [PRE_PROCESS => 'foo.tt']});
process_ok("([% WRAPPER wrap.tt %] one [% END %])" => 'BAR(Hi one there)', {tt_config => [PRE_PROCESS => 'bar.tt']});

process_ok("Foo" => "<<Foo>>Foo",  {tt_config => [PRE_PROCESS => 'template.tt']});

###----------------------------------------------------------------###
print "### CONFIG POST_PROCESS ##############################################\n";

process_ok("Foo" => "FooBAR",      {tt_config => [POST_PROCESS => 'bar.tt']});
process_ok("Foo" => "FooBAR",      {tt_config => [POST_PROCESS => ['bar.tt']]});
process_ok("Foo" => "Foo(BAR)BAR", {tt_config => [POST_PROCESS => ['foo.tt', 'bar.tt']]});
process_ok("Foo" => "FooBlueBAR",  {tt_config => [POST_PROCESS => 'bar.tt'], blue => 'Blue'});
process_ok("Foo[% blue='Blue' %]" => "FooBlueBAR", {tt_config => [POST_PROCESS => 'bar.tt']});
process_ok("Foo[% META foo='meta' %]" => "Foo(metaBAR)", {tt_config => [POST_PROCESS => 'foo.tt']});
process_ok("([% WRAPPER wrap.tt %] one [% END %])" => '(Hi one there)BAR', {tt_config => [POST_PROCESS => 'bar.tt']});

process_ok("Foo" => "Foo<<Foo>>",  {tt_config => [POST_PROCESS => 'template.tt']});

###----------------------------------------------------------------###
print "### CONFIG PROCESS ###################################################\n";

process_ok("Foo" => "BAR",      {tt_config => [PROCESS => 'bar.tt']});
process_ok("Foo" => "BAR",      {tt_config => [PROCESS => ['bar.tt']]});
process_ok("Foo" => "(BAR)BAR", {tt_config => [PROCESS => ['foo.tt', 'bar.tt']]});
process_ok("Foo" => "BlueBAR",  {tt_config => [PROCESS => 'bar.tt'], blue => 'Blue'});
process_ok("Foo[% META foo='meta' %]" => "(metaBAR)", {tt_config => [PROCESS => 'foo.tt']});
process_ok("Foo[% META foo='meta' %]" => "BAR(metaBAR)", {tt_config => [PRE_PROCESS => 'bar.tt', PROCESS => 'foo.tt']});
process_ok("Foo[% META foo='meta' %]" => "(metaBAR)BAR", {tt_config => [POST_PROCESS => 'bar.tt', PROCESS => 'foo.tt']});

process_ok("Foo" => "<<Foo>>",  {tt_config => [PROCESS => 'template.tt']});

###----------------------------------------------------------------###
print "### CONFIG WRAPPER ###################################################\n";

process_ok(" one " => 'Hi one there', {tt_config => [WRAPPER => 'wrap.tt']});
process_ok(" one " => 'Hi one there', {tt_config => [WRAPPER => ['wrap.tt']]});
process_ok(" one " => 'HiwrapHi one therethere', {tt_config => [WRAPPER => ['wrap.tt', 'wrap.tt']]});
process_ok(" ([% baz %]) " => 'Hi () there', {tt_config => [WRAPPER => 'wrap.tt']});
process_ok(" one " => 'HiBAZ one there', {baz => 'BAZ', tt_config => [WRAPPER => 'wrap.tt']});;
process_ok(" ([% baz; baz='-local' %]) " => 'Hi-local () there', {tt_config => [WRAPPER => 'wrap.tt']});
process_ok("[% META foo='BLAM' %] " => 'HiBLAM there', {tt_config => [WRAPPER => 'wrap.tt']});

process_ok(" one " => 'BARHi one there', {tt_config => [WRAPPER => 'wrap.tt', PRE_PROCESS => 'bar.tt']});
process_ok(" one " => 'HiBARthere', {tt_config => [WRAPPER => 'wrap.tt', PROCESS => 'bar.tt']});
process_ok(" one " => 'Hi one thereBAR', {tt_config => [WRAPPER => 'wrap.tt', POST_PROCESS => 'bar.tt']});

process_ok("Foo" => "<<FooFoo>>",  {tt_config => [WRAPPER => 'template.tt']});

###----------------------------------------------------------------###
print "### CONFIG ERRORS ####################################################\n";

process_ok("[% THROW foo 'bar' %]" => 'Error (foo) - (bar)',  {tt_config => [ERROR  => 'catch.tt']});
process_ok("[% THROW foo 'bar' %]" => 'Error (foo) - (bar)',  {tt_config => [ERRORS => 'catch.tt']});
process_ok("[% THROW foo 'bar' %]" => 'Error (foo) - (bar)',  {tt_config => [ERROR  => {default => 'catch.tt'}]});
process_ok("[% THROW foo 'bar' %]" => 'Error (foo) - (bar)',  {tt_config => [ERRORS => {default => 'catch.tt'}]});
process_ok("[% THROW foo 'bar' %]" => 'Error2 (foo) - (bar)', {tt_config => [ERRORS => {foo => 'catch2.tt', default => 'catch.tt'}]});
process_ok("[% THROW foo.baz 'bar' %]" => 'Error2 (foo.baz) - (bar)', {tt_config => [ERRORS => {foo => 'catch2.tt', default => 'catch.tt'}]});
process_ok("[% THROW foo.baz 'bar' %]" => 'Error2 (foo.baz) - (bar)', {tt_config => [ERRORS => {'foo.baz' => 'catch2.tt', default => 'catch.tt'}]});
process_ok("[% THROW foo 'bar' %]" => 'Error (foo) - (bar)', {tt_config => [ERRORS => {'foo.baz' => 'catch2.tt', default => 'catch.tt'}]});
process_ok("[% THROW foo.baz 'bar' %]" => 'Error2 (foo.baz) - (bar)', {tt_config => [ERRORS => {foo => 'catch2.tt', default => 'catch.tt'}]});

process_ok("[% THROW foo 'bar' %]" => 'BARError (foo) - (bar)',  {tt_config => [ERROR  => 'catch.tt', PRE_PROCESS => 'bar.tt']});
process_ok("[% THROW foo 'bar' %]" => 'Error (bing) - (blang)',  {tt_config => [ERROR  => 'catch.tt', PROCESS => 'die.tt']});
process_ok("[% THROW foo 'bar' %]" => 'Error (bing) - (blang)',  {tt_config => [ERROR  => 'catch.tt', PROCESS => ['bar.tt', 'die.tt']]});
process_ok("[% THROW foo 'bar' %]" => 'Error (foo) - (bar)BAR',  {tt_config => [ERROR  => 'catch.tt', POST_PROCESS => 'bar.tt']});
process_ok("[% THROW foo 'bar' %]" => 'HiError (foo) - (bar)there', {tt_config => [ERROR  => 'catch.tt', WRAPPER => 'wrap.tt']});

process_ok("(outer)[% PROCESS 'die.tt' %]" => 'Error (bing) - (blang)',  {tt_config => [ERROR  => 'catch.tt']});
process_ok("(outer)[% TRY %][% PROCESS 'die.tt' %][% CATCH %] [% END %]" => '(outer) ',  {tt_config => [ERROR  => 'catch.tt']});

process_ok(" one " => '',  {tt_config => [ERROR  => 'catch.tt', PRE_PROCESS => 'die.tt']});
process_ok(" one " => '',  {tt_config => [ERROR  => 'catch.tt', POST_PROCESS => 'die.tt']});
process_ok(" one " => '',  {tt_config => [ERROR  => 'catch.tt', WRAPPER => 'die.tt']});

###----------------------------------------------------------------###
print "### CONFIG and DUMP ##################################################\n";

process_ok("[% CONFIG DUMP => {html => 0}; DUMP foo; PROCESS config.tt; DUMP foo %]" => qq{DUMP: File "input text" line 1
    foo = 'FOO';
<b>DUMP: File "config.tt" line 1</b><pre>foo = &apos;FOO&apos;;
</pre>DUMP: File "input text" line 1
    foo = 'FOO';
}, {foo => 'FOO'}) if ! $is_tt;

###----------------------------------------------------------------###
print "### NOT FOUND CACHE ##################################################\n";

process_ok("[% BLOCK foo; TRY; PROCESS blurty.tt; CATCH %]([% error.type %])([% error.info %])\n[% END; END; PROCESS foo; PROCESS foo %]" => "(file)(blurty.tt: not found)\n(file)(blurty.tt: not found (cached))\n", {tt_config => [NEGATIVE_STAT_TTL => 2]}) if ! $is_tt;
process_ok("[% BLOCK foo; TRY; PROCESS blurty.tt; CATCH %]([% error.type %])([% error.info %])\n[% END; END; PROCESS foo; PROCESS foo %]" => "(file)(blurty.tt: not found)\n(file)(blurty.tt: not found)\n", {tt_config => [NEGATIVE_STAT_TTL => -1]}) if ! $is_tt;
process_ok("[% BLOCK foo; TRY; PROCESS blurty.tt; CATCH %]([% error.type %])([% error.info %])\n[% END; END; PROCESS foo; PROCESS foo %]" => "(file)(blurty.tt: not found)\n(file)(blurty.tt: not found)\n", {tt_config => [STAT_TTL => -1]}) if ! $is_tt;

###----------------------------------------------------------------###
print "### DONE #############################################################\n";
