#!/usr/bin/perl -w

use strict;
use Benchmark qw(timethese cmpthese);

my $file = $0;
$file =~ s|[^/]+$|WrapEx.pm|;
require $file;
#require Stash;
use Template;
use Template::Stash;
use Text::Template;
use CGI::Ex::Dump qw(debug);
use CGI::Ex::Template;
use POSIX qw(tmpnam);
use File::Path qw(mkpath rmtree);

my $dir = tmpnam;
mkpath($dir);
END {rmtree $dir};
my @dirs = ($dir);

my $form = {
  foo => 'bar',
  pass_in_something => 'what ever you want',
};

###----------------------------------------------------------------###

my $stash_w = {
  shell => {
    header => "This is a header",
    footer => "This is a footer",
    start  => "<html>",
    end    => "<end>",
    foo    => $form->{'foo'},
  },
  a => {
    stuff => [qw(one two three four)],
  },
};

my $stash_t = {
  shell_header => "This is a header",
  shell_footer => "This is a footer",
  shell_start  => "<html>",
  shell_end    => "<end>",
  a_stuff      => [qw(one two three four)],
};

$FOO::shell_header = $FOO::shell_footer = $FOO::shell_start = $FOO::shell_end = $FOO::a_stuff;
$FOO::shell_header = "This is a header";
$FOO::shell_footer = "This is a footer";
$FOO::shell_start  = "<html>";
$FOO::shell_end    = "<end>";
$FOO::a_stuff      = [qw(one two three four)];


###----------------------------------------------------------------###

my $content_w = q{[shell.header]
[shell.start]

[if shell.foo q{
This is some text.
}]

[loop i a.stuff.length q{[a.stuff]}]
[form.pass_in_something]

[shell.end]
[shell.footer]
};

my $content_t = q{[% shell_header %]
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
    print $fh $content_t;
    close $fh;
}

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

my $wrap = WrapEx->new({
  dirs => \@dirs,
  W    => $stash_w,
  form => [$form],
});

 my $tt = Template->new({
  INCLUDE_PATH => \@dirs,
  STASH => Template::Stash->new($stash_t),
});

my $ct = CGI::Ex::Template->new({
  INCLUDE_PATH => \@dirs,
  STASH => Template::Stash->new($stash_t),
});

my $pt = Text::Template->new(TYPE => 'STRING', SOURCE => $content_p, HASH => $form);

###----------------------------------------------------------------###
### make sure everything is ok

my $out_wr = $content_w;
$wrap->wrap(\$out_wr);

my $out_tt = "";
$tt->process(\$content_t, $form, \$out_tt);

my $out_ct = "";
$ct->process(\$content_t, $form, \$out_ct);

my $out_c2 = "";
$ct->process('foo.tt', $form, \$out_c2);

my $out_c3 = $ct->swap(\$content_t, {%$stash_t, %$form});

my $out_pt = $pt->fill_in(PACKAGE => 'FOO', HASH => $form);

if ($out_wr ne $out_tt) {
    debug $out_wr, $out_tt;
    die "Wrap didn't match tt";
}
if ($out_ct ne $out_tt) {
    debug $out_ct, $out_tt;
    die "CGI::Ex::Template didn't match tt";
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

###----------------------------------------------------------------###

cmpthese timethese (-2, {
    wrap => sub {
        my $out = $content_w;
        $wrap->wrap(\$out);
    },
    TemplateToolkit => sub {
        my $out = "";
        $tt->process(\$content_t, $form, \$out);
    },
    CET => sub {
        my $out = "";
        $ct->process(\$content_t, $form, \$out);
    },
    CET_mem => sub {
        my $out = "";
        $ct->process('foo.tt', $form, \$out);
    },
    CET_swap => sub {
        my $out = $ct->swap(\$content_t, {%$stash_t, %$form});
    },
    CET_cache => sub {
        my $ct = CGI::Ex::Template->new({
            INCLUDE_PATH => \@dirs,
            STASH => Template::Stash->new($stash_t),
            CACHE_DIR => $dir,
        });
        my $out = $ct->swap('foo.tt', {%$stash_t, %$form});
    },
    TextTemplate => sub {
        my $out = $pt->fill_in(PACKAGE => 'FOO', HASH => $form);
    },
    TextTemplate2 => sub {
        my $out = $pt->fill_in(PACKAGE => 'FOO', HASH => {%$stash_t, %$form});
    },
});

###----------------------------------------------------------------###
