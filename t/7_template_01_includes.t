# -*- Mode: Perl; -*-

=head1 NAME

7_template_01_includes.t - Test the file include functionality of CGI::Ex::Template - including some edge cases

=cut

use vars qw($module $is_tt);
BEGIN {
    $module = 'CGI::Ex::Template';
    #$module = 'Template';
    $is_tt = $module eq 'Template';
};

use strict;
use Test::More tests => 25 - ($is_tt ? 6 : 0);
use Data::Dumper qw(Dumper);
use constant test_taint => 0 && eval { require Taint::Runtime };

use_ok($module);

Taint::Runtime::taint_start() if test_taint;

### find a place to allow for testing
my $test_dir = $0 .'.test_dir';
END { rmdir $test_dir }
mkdir $test_dir, 0755;
ok(-d $test_dir, "Got a test dir up and running");


sub process_ok { # process the value
    my $str  = shift;
    my $test = shift;
    my $args = shift;
    my $out  = '';

    Taint::Runtime::taint(\$str) if test_taint;

    my $obj = $module->new(ABSOLUTE => 1, INCLUDE_PATH => $test_dir);
    $obj->process(\$str, $args, \$out);
    my $ok = $out eq $test;
    ok($ok, "\"$str\" => \"$out\"" . ($ok ? '' : " - should've been \"$test\""));
    my $line = (caller)[2];
    warn "#   process_ok called at line $line.\n" if ! $ok;
}

### create some files to include
my $foo_template = "$test_dir/foo.tt";
END { unlink $foo_template };
open(my $fh, ">$foo_template") || die "Couldn't open $foo_template: $!";
print $fh "([% INCLUDE bar.tt %])";
close $fh;

###
my $bar_template = "$test_dir/bar.tt";
END { unlink $bar_template };
open($fh, ">$bar_template") || die "Couldn't open $bar_template: $!";
print $fh "BAR";
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
print $fh "Hi[% content %]there";
close $fh;

###----------------------------------------------------------------###
### INSERT

process_ok("([% INSERT bar.tt %])" => '(BAR)');
process_ok("([% SET file = 'bar.tt' %][% INSERT \$file %])" => '(BAR)');
process_ok("([% SET file = 'bar.tt' %][% INSERT \${file} %])" => '(BAR)') if ! $is_tt;
process_ok("([% SET file = 'bar.tt' %][% INSERT \"\$file\" %])" => '(BAR)');
process_ok("([% SET file = 'bar' %][% INSERT \"\$file.tt\" %])" => '(BAR)') if ! $is_tt;

###----------------------------------------------------------------###
### INCLUDE

process_ok("([% INCLUDE bar.tt %])" => '(BAR)');
process_ok("([% SET file = 'bar.tt' %][% INCLUDE \$file %])" => '(BAR)');
process_ok("([% SET file = 'bar.tt' %][% INCLUDE \${file} %])" => '(BAR)') if ! $is_tt;
process_ok("([% SET file = 'bar.tt' %][% INCLUDE \"\$file\" %])" => '(BAR)');
process_ok("([% SET file = 'bar' %][% INCLUDE \"\$file.tt\" %])" => '(BAR)') if ! $is_tt;

process_ok("([% INCLUDE baz.tt %])" => '(42)');
process_ok("([% INCLUDE baz.tt %])[% baz %]" => '(42)');
process_ok("[% SET baz = 21 %]([% INCLUDE baz.tt %])[% baz %]" => '(42)21');

###----------------------------------------------------------------###
### PROCESS

process_ok("([% PROCESS bar.tt %])" => '(BAR)');
process_ok("([% SET file = 'bar.tt' %][% PROCESS \$file %])" => '(BAR)');
process_ok("([% SET file = 'bar.tt' %][% PROCESS \${file} %])" => '(BAR)') if ! $is_tt;
process_ok("([% SET file = 'bar.tt' %][% PROCESS \"\$file\" %])" => '(BAR)');
process_ok("([% SET file = 'bar' %][% PROCESS \"\$file.tt\" %])" => '(BAR)') if ! $is_tt;

process_ok("([% PROCESS baz.tt %])" => '(42)');
process_ok("([% PROCESS baz.tt %])[% baz %]" => '(42)42');
process_ok("[% SET baz = 21 %]([% PROCESS baz.tt %])[% baz %]" => '(42)42');

###----------------------------------------------------------------###
### WRAPPER

process_ok("([% WRAPPER wrap.tt %])" => '');
process_ok("([% WRAPPER wrap.tt %] one [% END %])" => '(Hi one there)');
