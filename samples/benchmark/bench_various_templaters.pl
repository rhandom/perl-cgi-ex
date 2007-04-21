#!/usr/bin/perl -w

=head1 NAME

bench_various_templaters.pl - test the relative performance of several different types of template engines.

=cut

use strict;
use Benchmark qw(timethese cmpthese);

my $file = $0;
$file =~ s|[^/]+$|WrapEx.pm|;
#require $file;

use Template;
use Template::Stash;
use Template::Stash::XS;
use Text::Template;
use HTML::Template;
use HTML::Template::Expr;
use HTML::Template::JIT;
use CGI::Ex::Dump qw(debug);
use CGI::Ex::Template;
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

my $content_tt = q{[% shell_header %]
[% shell_start %]

[% IF foo %]
This is some text.
[% END %]

[% FOREACH i IN a_stuff %][% i %][% END %]
[% pass_in_something %]

[% shell_end %]
[% shell_footer %]
};

if (open (my $fh, ">$dir/foo.tt")) {
    print $fh $content_tt;
    close $fh;
}

###----------------------------------------------------------------###
### HTML::Template style

my $content_ht = q{<TMPL_VAR NAME=shell_header>
<TMPL_VAR NAME=shell_start>

<TMPL_IF NAME=foo>
This is some text.
</TMPL_IF>

<TMPL_LOOP NAME=a_stuff><TMPL_VAR NAME=name></TMPL_LOOP>
<TMPL_VAR NAME=pass_in_something>

<TMPL_VAR NAME=shell_end>
<TMPL_VAR NAME=shell_footer>
};

if (open (my $fh, ">$dir/foo.ht")) {
    print $fh $content_ht;
    close $fh;
}

###----------------------------------------------------------------###
### Text::Template style template

my $content_p = q{{$shell_header}
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
};

###----------------------------------------------------------------###
### setup the objects

my $tt = Template->new({
  INCLUDE_PATH => \@dirs,
  STASH        => Template::Stash->new($stash_t),
});

my $ttx = Template->new({
  INCLUDE_PATH => \@dirs,
  STASH        => Template::Stash::XS->new($stash_t),
});

my $ct = CGI::Ex::Template->new({
  INCLUDE_PATH => \@dirs,
  VARIABLES    => $stash_t,
});

my $pt = Text::Template->new(TYPE => 'STRING', SOURCE => $content_p, HASH => $form);

my $ht = HTML::Template->new(type => 'scalarref', source => \$content_ht);
$ht->param($stash_ht);
$ht->param($form);

my $hte = HTML::Template::Expr->new(type => 'scalarref', source => \$content_ht);
$hte->param($stash_ht);
$hte->param($form);

my $ht_c = HTML::Template->new(type => 'filename', source => "foo.ht", cache => 1, path => \@dirs);
$ht_c->param($stash_ht);
$ht_c->param($form);

my $ht_j = HTML::Template::JIT->new(filename => "foo.ht", path => \@dirs, jit_path => $dir2);
$ht_j->param($stash_ht);
$ht_j->param($form);

###----------------------------------------------------------------###
### make sure everything is ok by trying it once

my $out_tt = "";
$tt->process(\$content_tt, $form, \$out_tt);

my $out_ttx = "";
$ttx->process(\$content_tt, $form, \$out_ttx);

my $out_ct = "";
$ct->process(\$content_tt, $form, \$out_ct);

my $out_c2 = "";
$ct->process('foo.tt', $form, \$out_c2);

my $out_c3 = '';
$ct->process_simple(\$content_tt, {%$stash_t, %$form}, \$out_c3);

my $out_pt = $pt->fill_in(PACKAGE => 'FOO', HASH => $form);

my $out_ht  = $ht->output;
my $out_hte = $hte->output;
my $out_htc = $ht_c->output;
my $out_htj = $ht_j->output;

if ($out_ct ne $out_tt) {
    debug $out_ct, $out_tt;
    die "CGI::Ex::Template didn't match tt";
}
if ($out_ttx ne $out_tt) {
    debug $out_ttx, $out_tt;
    die "Template::Stash::XS didn't match tt";
}
if ($out_c2 ne $out_tt) {
    debug $out_c2, $out_tt;
    die "CGI::Ex::Template from file didn't match tt";
}
if ($out_c3 ne $out_tt) {
    debug $out_c3, $out_tt;
    die "CGI::Ex::Template by swap didn't match tt";
}
if ($out_pt ne $out_tt) {
    debug $out_pt, $out_tt;
   die "Text Template didn't match tt";
}
if ($out_ht ne $out_tt) {
    debug $out_ht, $out_tt;
   die "HTML::Template didn't match tt";
}
if ($out_hte ne $out_tt) {
    debug $out_hte, $out_tt;
   die "HTML::Template::Expr didn't match tt";
}
if ($out_htc ne $out_tt) {
    debug $out_htc, $out_tt;
   die "HTML::Template::Expr didn't match tt";
}
if ($out_htj ne $out_tt) {
    debug $out_htj, $out_tt;
   die "HTML::Template::JIT didn't match tt";
}

###----------------------------------------------------------------###

my $tests = {
    TT_str => sub {
        my $tt = Template->new({
            INCLUDE_PATH => \@dirs,
            STASH        => Template::Stash->new($stash_t),
        });
        my $out = "";
        $tt->process(\$content_tt, $form, \$out);
    },
    TT_mem => sub {
        my $out = "";
        $tt->process('foo.tt', $form, \$out);
    },
    TT_compile => sub {
        my $tt = Template->new({
            INCLUDE_PATH => \@dirs,
            STASH        => Template::Stash->new($stash_t),
            COMPILE_DIR  => $dir2,
        });
        my $out = "";
        $tt->process('foo.tt', $form, \$out);
    },

    TTX_str => sub {
        my $tt = Template->new({
            INCLUDE_PATH => \@dirs,
            STASH        => Template::Stash::XS->new($stash_t),
        });
        my $out = "";
        $tt->process(\$content_tt, $form, \$out);
    },
    TTX_mem => sub {
        my $out = "";
        $ttx->process('foo.tt', $form, \$out);
    },
    TTX_compile => sub {
        my $tt = Template->new({
            INCLUDE_PATH => \@dirs,
            STASH        => Template::Stash::XS->new($stash_t),
            COMPILE_DIR  => $dir2,
        });
        my $out = "";
        $tt->process('foo.tt', $form, \$out);
    },

    CET_str => sub {
        my $ct = CGI::Ex::Template->new({
            INCLUDE_PATH => \@dirs,
            VARIABLES    => $stash_t,
        });
        my $out = "";
        $ct->process(\$content_tt, $form, \$out);
    },
    CET_mem => sub {
        my $out = "";
        $ct->process('foo.tt', $form, \$out);
    },
    CET_compile => sub {
        my $ct = CGI::Ex::Template->new({
            INCLUDE_PATH => \@dirs,
            VARIABLES    => $stash_t,
            COMPILE_DIR  => $dir2,
        });
        my $out = '';
        $ct->process('foo.tt', $form, \$out);
    },

    TextTemplate => sub {
        my $pt = Text::Template->new(
            TYPE   => 'STRING',
            SOURCE => $content_p,
            HASH   => $form);
        my $out = $pt->fill_in(PACKAGE => 'FOO', HASH => $form);
    },

    HT_str => sub {
        my $ht = HTML::Template->new(type => 'scalarref', source => \$content_ht);
        $ht->param($stash_ht);
        $ht->param($form);
        my $out = $ht->output;
    },
    HT_mem => sub {
        my $ht = HTML::Template->new(type => 'filename', source => "foo.ht", path => \@dirs, cache => 1);
        $ht->param($stash_ht);
        $ht->param($form);
        my $out = $ht->output;
    },
    HT_compile => sub {
        my $ht = HTML::Template->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2);
        $ht->param($stash_ht);
        $ht->param($form);
        my $out = $ht->output;
    },

    HTE_str => sub {
        my $ht = HTML::Template::Expr->new(type => 'scalarref', source => \$content_ht);
        $ht->param($stash_ht);
        $ht->param($form);
        my $out = $ht->output;
    },
    HTE_mem => sub {
        my $ht = HTML::Template::Expr->new(type => 'filename', source => "foo.ht", path => \@dirs, cache => 1);
        $ht->param($stash_ht);
        $ht->param($form);
        my $out = $ht->output;
    },

    HTJ_compile => sub {
        my $ht = HTML::Template::JIT->new(filename => "foo.ht", path => \@dirs, jit_path => $dir2);
        $ht->param($stash_ht);
        $ht->param($form);
        my $out = $ht->output;
    },
};


my %mem_tests = map {($_ => $tests->{$_})} qw(TT_mem TTX_mem CET_mem HT_mem HTE_mem);
my %cpl_tests = map {($_ => $tests->{$_})} qw(TT_compile TTX_compile CET_compile HT_compile HTJ_compile);
my %str_tests = map {($_ => $tests->{$_})} qw(TT_str TTX_str CET_str HT_str HTE_str TextTemplate);

print "------------------------------------------------------------------------\n";
print "From a string or scalarref tests\n";
cmpthese timethese (-2, \%str_tests);

print "------------------------------------------------------------------------\n";
print "Compiled and cached on the file system tests\n";
cmpthese timethese (-2, \%cpl_tests);

print "------------------------------------------------------------------------\n";
print "Cached in memory tests\n";
cmpthese timethese (-2, \%mem_tests);

print "------------------------------------------------------------------------\n";
print "All variants together\n";
cmpthese timethese (-2, $tests);

###----------------------------------------------------------------###
