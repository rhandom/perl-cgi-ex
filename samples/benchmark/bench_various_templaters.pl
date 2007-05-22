#!/usr/bin/perl -w

=head1 NAME

bench_various_templaters.pl - test the relative performance of several different types of template engines.

=cut

use strict;
use Benchmark qw(timethese cmpthese);

use Template;
use Template::Stash;
use Template::Stash::XS;
use Template::Parser::CET;
use Text::Template;
use HTML::Template;
use HTML::Template::Expr;
use HTML::Template::JIT;
use CGI::Ex::Dump qw(debug);
use CGI::Ex::Template;
use CGI::Ex::Template::XS;
use POSIX qw(tmpnam);
use File::Path qw(mkpath rmtree);

my $dir  = tmpnam;
my $dir2 = "$dir.cache";
mkpath($dir);
mkpath($dir2);
END {rmtree $dir; rmtree $dir2};
my @dirs = ($dir);

my $form = {
  foo => 'bar',
  pass_in_something => 'what ever you want',
};

###----------------------------------------------------------------###

my $stash_t = {
  shell_header => "This is a header",
  shell_footer => "This is a footer",
  shell_start  => "<html>",
  shell_end    => "<end>",
  a_stuff      => [qw(one two three four)],
};

my $stash_ht = {
  shell_header => "This is a header",
  shell_footer => "This is a footer",
  shell_start  => "<html>",
  shell_end    => "<end>",
  a_stuff      => [map {{name => $_}} qw(one two three four)],
};

$FOO::shell_header = $FOO::shell_footer = $FOO::shell_start = $FOO::shell_end = $FOO::a_stuff;
$FOO::shell_header = "This is a header";
$FOO::shell_footer = "This is a footer";
$FOO::shell_start  = "<html>";
$FOO::shell_end    = "<end>";
$FOO::a_stuff      = [qw(one two three four)];


###----------------------------------------------------------------###
### TT style template

my $content_tt = <<'DOC';
[% shell_header %]
[% shell_start %]

[% IF foo %]
This is some text.
[% END %]

[% FOREACH i IN a_stuff %][% i %][% END %]
[% pass_in_something %]

[% shell_end %]
[% shell_footer %]
DOC

if (open (my $fh, ">$dir/foo.tt")) {
    print $fh $content_tt;
    close $fh;
}

###----------------------------------------------------------------###
### HTML::Template style

my $content_ht = <<'DOC';
<TMPL_VAR NAME=shell_header>
<TMPL_VAR NAME=shell_start>

<TMPL_IF NAME=foo>
This is some text.
</TMPL_IF>

<TMPL_LOOP NAME=a_stuff><TMPL_VAR NAME=name></TMPL_LOOP>
<TMPL_VAR NAME=pass_in_something>

<TMPL_VAR NAME=shell_end>
<TMPL_VAR NAME=shell_footer>
DOC

if (open (my $fh, ">$dir/foo.ht")) {
    print $fh $content_ht;
    close $fh;
}

###----------------------------------------------------------------###
### Text::Template style template

my $content_p = <<'DOC';
{$shell_header}
{$shell_start}

{ if ($foo) {
    $OUT .= "
This is some text.
";
  }
}

{  $OUT .= $_ foreach @$a_stuff; }
{$pass_in_something}

{$shell_end}
{$shell_footer}
DOC

###----------------------------------------------------------------###
### The TT interface allows for a single object to be cached and reused.

my $tt  = Template->new(             INCLUDE_PATH => \@dirs, STASH => Template::Stash->new($stash_t));
my $ttx = Template->new(             INCLUDE_PATH => \@dirs, STASH => Template::Stash::XS->new($stash_t));
my $ct  = CGI::Ex::Template->new(    INCLUDE_PATH => \@dirs, VARIABLES => $stash_t);
my $ctx = CGI::Ex::Template::XS->new(INCLUDE_PATH => \@dirs, VARIABLES => $stash_t);

###----------------------------------------------------------------###

my $tests = {

    ###----------------------------------------------------------------###
    ### compile means item was compiled to optree or perlcode and stored on disk

    TT_compile => sub {
        my $tt = Template->new(INCLUDE_PATH => \@dirs, STASH => Template::Stash->new($stash_t), COMPILE_DIR => $dir2);
        my $out = ""; $tt->process('foo.tt', $form, \$out); $out;
    },
    TTX_compile => sub {
        my $tt = Template->new(INCLUDE_PATH => \@dirs, STASH => Template::Stash::XS->new($stash_t), COMPILE_DIR => $dir2);
        my $out = ""; $tt->process('foo.tt', $form, \$out); $out;
    },
    CET_compile => sub {
        my $t = CGI::Ex::Template->new(INCLUDE_PATH => \@dirs, VARIABLES => $stash_t, COMPILE_DIR  => $dir2);
        my $out = ''; $t->process('foo.tt', $form, \$out); $out;
    },
    CTX_compile => sub {
        my $t = CGI::Ex::Template::XS->new(INCLUDE_PATH => \@dirs, VARIABLES => $stash_t, COMPILE_DIR => $dir2);
        my $out = ''; $t->process('foo.tt', $form, \$out); $out;
    },

    CETH_compile => sub {
        my $ht = CGI::Ex::Template->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETHX_compile => sub {
        my $ht = CGI::Ex::Template::XS->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HT_compile => sub {
        my $ht = HTML::Template->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },

    ###----------------------------------------------------------------###
    ### str infers that we are pulling from a string reference

    TextTemplate_str => sub {
        my $pt = Text::Template->new(
            TYPE   => 'STRING',
            SOURCE => $content_p,
            HASH   => $form);
        my $out = $pt->fill_in(PACKAGE => 'FOO', HASH => $form);
    },

    TT_str => sub {
        my $t = Template->new(STASH => Template::Stash->new($stash_t));
        my $out = ""; $t->process(\$content_tt, $form, \$out); $out;
    },
    TTX_str => sub {
        my $t = Template->new(STASH => Template::Stash::XS->new($stash_t));
        my $out = ""; $t->process(\$content_tt, $form, \$out); $out;
    },
    TTXCET_str => sub {
        my $t = Template->new(STASH => Template::Stash::XS->new($stash_t), PARSER => Template::Parser::CET->new);
        my $out = ""; $t->process(\$content_tt, $form, \$out); $out;
    },
    CET_str => sub {
        my $t = CGI::Ex::Template->new(VARIABLES => $stash_t);
        my $out = ""; $t->process(\$content_tt, $form, \$out); $out;
    },
    CTX_str => sub {
        my $t = CGI::Ex::Template::XS->new(VARIABLES => $stash_t);
        my $out = ""; $t->process(\$content_tt, $form, \$out); $out;
    },

    CETH_str => sub {
        my $ht = CGI::Ex::Template->new(    type => 'scalarref', source => \$content_ht);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETHX_str => sub {
        my $ht = CGI::Ex::Template::XS->new(type => 'scalarref', source => \$content_ht);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HT_str => sub {
        my $ht = HTML::Template->new(       type => 'scalarref', source => \$content_ht);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTE_str => sub {
        my $ht = HTML::Template::Expr->new( type => 'scalarref', source => \$content_ht);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },

    ###----------------------------------------------------------------###
    ### mem indicates that the compiled form is stored in memory

    TT_mem   => sub { my $out = ""; $tt->process( 'foo.tt', $form, \$out); $out },
    TTX_mem  => sub { my $out = ""; $ttx->process('foo.tt', $form, \$out); $out },
    CET_mem  => sub { my $out = ""; $ct->process( 'foo.tt', $form, \$out); $out },
    CETX_mem => sub { my $out = ""; $ctx->process('foo.tt', $form, \$out); $out },

    CETH_mem => sub {
        my $ht = CGI::Ex::Template->new(    filename => "foo.ht", path => \@dirs, cache => 1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETHX_mem => sub {
        my $ht = CGI::Ex::Template::XS->new(filename => "foo.ht", path => \@dirs, cache => 1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HT_mem => sub {
        my $ht = HTML::Template->new(       filename => "foo.ht", path => \@dirs, cache => 1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTE_mem => sub {
        my $ht = HTML::Template::Expr->new( filename => "foo.ht", path => \@dirs, cache => 1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTJ_mem => sub { # this is interesting - it is compiled - but it is pulled into memory just once
        my $ht = HTML::Template::JIT->new(  filename => "foo.ht", path => \@dirs, jit_path => $dir2);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
};

my $test = $tests->{'TT_str'}->();
foreach my $name (sort keys %$tests) {
    if ($test ne $tests->{$name}->()) {
        die "$name did not match TT_str output\n";
    }
    print "$name OK\n";
}

###----------------------------------------------------------------###
### and now - the tests - grouped by common capability

my %mem_tests = map {($_ => $tests->{$_})} grep {/_mem$/} keys %$tests;
my %cpl_tests = map {($_ => $tests->{$_})} grep {/_compile$/} keys %$tests;
my %str_tests = map {($_ => $tests->{$_})} grep {/_str$/} keys %$tests;

print "------------------------------------------------------------------------\n";
print "From a string or scalarref tests\n";
cmpthese timethese (-2, \%str_tests);

print "------------------------------------------------------------------------\n";
print "Compiled and cached on the file system tests\n";
cmpthese timethese (-2, \%cpl_tests);

print "------------------------------------------------------------------------\n";
print "Cached in memory tests\n";
cmpthese timethese (-2, \%mem_tests);

#print "------------------------------------------------------------------------\n";
#print "All variants together\n";
#cmpthese timethese (-2, $tests);

###----------------------------------------------------------------###
