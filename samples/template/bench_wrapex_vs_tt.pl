#!/usr/bin/perl -w

use strict;
use Benchmark qw(cmpthese);

require '../t/samples/template/WrapEx.pm';
#require Stash;
use Template;
use Template::Stash;
use Text::Template;

my @dirs = qw(.);

my $form = {
  foo => 'bar',
  pass_in_something => 'what ever you want',
};

my $stash_w = {
  shell => {
    header => "This is a header",
    footer => "This is a footer",
    start  => "<html>",
    end    => "<end>",
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


#$FOO::shell_header = "This is a header";
#$FOO::shell_footer = "This is a footer";
#$FOO::shell_start  = "<html>";
#$FOO::shell_end    = "<end>";
#$FOO::a_stuff      = [qw(one two three four)];
#
my $content_w = "[shell.header]
[shell.start]

[if a.foo q{
This is some text.
}]

[loop i a.stuff.length q{[a.stuff]}]
[form.pass_in_something]

[shell.end]
[shell.footer]
";

my $content_w2 = "[(( shell.header )
( shell.start ))]

[( if a.foo {
This is some text.
} )]

[( loop i a.stuff->size {[(a.stuff)]} )]
[( pass_in_something )]

[((shell.end)
 (shell.footer))]
";

my $content_l = "[% shell.header %]
[% shell.start %]

[% if a.foo %]
This is some text.
[% END %]

[% FOREACH i IN a_stuff %][% i %][% END %]
[% pass_in_something %]

[% shell.end %]
[% shell.footer %]
";

my $content_t = "[% shell_header %]
[% shell_start %]

[% IF \$foo %]
This is some text.
[% END %]

[% FOREACH i IN a_stuff %][% i %][% END %]
[% pass_in_something %]

[% shell_end %]
[% shell_footer %]
";

my $content_p = '{$shell_header}
{$shell_start}

{ if ($foo) {
    $OUT .= "This is some text.";
  }
}

{  $OUT .= $_ foreach @$a_stuff; }
{$pass_in_something}

{$shell_end}
{$shell_footer}
';

my $out;


#my $tt2 = Template->new({
#  INCLUDE_PATH => \@dirs,
#  STASH => Stash->new({
#    vars => [$stash_w],
#    dirs => \@dirs,
#  });
#});
#
#$out = "";
#$tt2->process(\$content_t, $form, \$out);
#print "----------------------\n";
#print $out;
#print "----------------------\n";
#
#exit;

my $wrap = WrapEx->new({
  dirs => \@dirs,
  W    => $stash_w,
  form => [$form],
});

for (1..200) {
$out = $content_w;
$wrap->wrap(\$out);
}
print "----------------------\n";
print $out;
print "----------------------\n";

exit;

my $tt = Template->new({
  INCLUDE_PATH => \@dirs,
  STASH => Template::Stash->new($stash_t),
});

my $pt = Text::Template->new(TYPE => 'STRING', SOURCE => $content_p);

$out = $content_w;
$wrap->wrap(\$out);
print "----------------------\n";
print $out;
print "----------------------\n";

$out = "";
$tt->process(\$content_t, $form, \$out);
print "----------------------\n";
print $out;
print "----------------------\n";

$out = $pt->fill_in(PACKAGE => 'FOO', HASH => $form);
print "----------------------\n";
print $out;
print "----------------------\n";

cmpthese (700, {
  wrap => sub {
    $out = $content_w;
    $wrap->wrap(\$out);
  },
  tt => sub {
    $out = "";
    $tt->process(\$content_t, $form, \$out);
  },
  pt => sub {
    $out = $pt->fill_in(PACKAGE => 'FOO', HASH => $form);
  },
});
