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
use Text::Tmpl;
use HTML::Template;
use HTML::Template::Compiled;
use HTML::Template::Expr;
use HTML::Template::JIT;
use CGI::Ex::Dump qw(debug);
use CGI::Ex::Template;
use CGI::Ex::Template::XS;
use POSIX qw(tmpnam);
use File::Path qw(mkpath rmtree);

###----------------------------------------------------------------###

my $names = {
  CET          => 'CGI::Ex::Template using TT interface',
  CETX         => 'CGI::Ex::Template::XS using TT interface',
  CETH         => 'CGI::Ex::Template using HTML::Template interface',
  CETXH        => 'CGI::Ex::Template::XS using HTML::Template interface',
  CETXHp       => 'CGI::Ex::Template::XS using HTML::Template interface - Perl code eval based',
  CETXTMPL     => 'CGI::Ex::Temmplate::XS using Text::Tmpl interface',
  HT           => 'HTML::Template',
  HTE          => 'HTML::Template::Expr',
  HTJ          => 'HTML::Template::JIT - Compiled to C template',
  HTC          => 'HTML::Template::Compiled',
  TextTemplate => 'Text::Template - Perl code eval based',
  TT           => 'Template::Toolkit',
  TTX          => 'Template::Toolkit with Stash::XS',
  TTXCET       => 'Template::Toolkit with Stash::XS and Template::Parser::CET',
  TMPL         => 'Text::Tmpl - Engine is C based',

  mem          => 'Compiled in memory',
  file         => 'Loaded from file',
  str          => 'From string ref',
};

###----------------------------------------------------------------###
### get cache and compile dirs ready

my $dir  = tmpnam;
my $dir2 = "$dir.cache";
mkpath($dir);
mkpath($dir2);
END {rmtree $dir; rmtree $dir2};
my @dirs = ($dir);

###----------------------------------------------------------------###

my $form = {
  foo => 'bar',
  pass_in_something => 'what ever you want',
};

my $filler = ((" foo" x 10)."\n") x 10;

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

my $content_tt = <<"DOC";
[% shell_header %]
[% shell_start %]
$filler

[% IF foo %]
This is some text.
[% END %]

[% FOREACH i IN a_stuff %][% i %][% END %]
[% pass_in_something %]

$filler
[% shell_end %]
[% shell_footer %]
DOC

if (open (my $fh, ">$dir/foo.tt")) {
    print $fh $content_tt;
    close $fh;
}

###----------------------------------------------------------------###
### HTML::Template style

my $content_ht = <<"DOC";
<TMPL_VAR NAME=shell_header>
<TMPL_VAR NAME=shell_start>
$filler

<TMPL_IF NAME=foo>
This is some text.
</TMPL_IF>

<TMPL_LOOP NAME=a_stuff><TMPL_VAR NAME=name></TMPL_LOOP>
<TMPL_VAR NAME=pass_in_something>

$filler
<TMPL_VAR NAME=shell_end>
<TMPL_VAR NAME=shell_footer>
DOC

if (open (my $fh, ">$dir/foo.ht")) {
    print $fh $content_ht;
    close $fh;
}

###----------------------------------------------------------------###
### Text::Template style template

my $content_p = <<"DOC";
{\$shell_header}
{\$shell_start}
$filler

{ if (\$foo) {
    \$OUT .= "
This is some text.
";
  }
}

{  \$OUT .= \$_ foreach \@\$a_stuff; }
{\$pass_in_something}

$filler
{\$shell_end}
{\$shell_footer}
DOC

###----------------------------------------------------------------###
### Tmpl style template

my $content_tmpl = <<"DOC";
<!--echo \$shell_header-->
<!--echo \$shell_start-->
$filler

<!-- if \$foo -->
This is some text.
<!-- endif -->

<!-- loop "a_stuff" --><!-- echo \$name --><!-- endloop -->
<!-- echo \$pass_in_something -->

$filler
<!-- echo \$shell_end -->
<!-- echo \$shell_footer -->
DOC

if (open (my $fh, ">$dir/foo.tmpl")) {
    print $fh $content_tmpl;
    close $fh;
}

###----------------------------------------------------------------###
### The TT interface allows for a single object to be cached and reused.

my $tt  = Template->new(             INCLUDE_PATH => \@dirs, STASH => Template::Stash->new($stash_t));
my $ttx = Template->new(             INCLUDE_PATH => \@dirs, STASH => Template::Stash::XS->new($stash_t));
my $ct  = CGI::Ex::Template->new(    INCLUDE_PATH => \@dirs, VARIABLES => $stash_t);
my $ctx = CGI::Ex::Template::XS->new(INCLUDE_PATH => \@dirs, VARIABLES => $stash_t);

###----------------------------------------------------------------###
my %CETH_DOCUMENTS;
my %CETXH_DOCUMENTS;
my %CETXHp_DOCUMENTS;


my $tests = {

    ###----------------------------------------------------------------###
    ### compile means item was compiled to optree or perlcode and stored on disk

    TT_file => sub {
        my $tt = Template->new(INCLUDE_PATH => \@dirs, STASH => Template::Stash->new($stash_t), COMPILE_DIR => $dir2);
        my $out = ""; $tt->process('foo.tt', $form, \$out); $out;
    },
    TTX_file => sub {
        my $tt = Template->new(INCLUDE_PATH => \@dirs, STASH => Template::Stash::XS->new($stash_t), COMPILE_DIR => $dir2);
        my $out = ""; $tt->process('foo.tt', $form, \$out); $out;
    },
#    CET_file => sub {
#        my $t = CGI::Ex::Template->new(INCLUDE_PATH => \@dirs, VARIABLES => $stash_t, COMPILE_DIR  => $dir2);
#        my $out = ''; $t->process('foo.tt', $form, \$out); $out;
#    },
#    CETX_file => sub {
#        my $t = CGI::Ex::Template::XS->new(INCLUDE_PATH => \@dirs, VARIABLES => $stash_t, COMPILE_DIR => $dir2);
#        my $out = ''; $t->process('foo.tt', $form, \$out); $out;
#    },

    CETH_file => sub {
        my $ht = CGI::Ex::Template->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2, CASE_SENSITVE=>1);
        $ht->{'_documents'} = \%CETH_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETXH_file => sub {
        my $ht = CGI::Ex::Template::XS->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2,
                                            CASE_SENSITVE=>1);
        $ht->{'_documents'} = \%CETXH_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETXHp_file => sub {
        my $ht = CGI::Ex::Template::XS->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2,
                                            CASE_SENSITVE=>1, compile_perl => 1, cache => 1);
        $ht->{'_documents'} = \%CETXHp_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HT_file => sub {
        my $ht = HTML::Template->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTC_file => sub {
        my $ht = HTML::Template::Compiled->new(type => 'filename', source => "foo.ht", file_cache => 1, path => \@dirs, file_cache_dir => $dir2, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    TMPL_file => sub {
        my $tt = Text::Tmpl->new;
        for my $ref (@{ $stash_ht->{'a_stuff'} }) {
            $tt->loop_iteration('a_stuff')->set_values($ref);
        }
        $tt->set_values($stash_ht);
        $tt->set_values($form);
        $tt->set_delimiters('<!--','-->');
        $tt->set_dir("$dir/");
        $tt->set_strip(0);
        my $out = $tt->parse_file("foo.tmpl");
    },
#    CETXTMPL_file => sub {
#        my $tt = CGI::Ex::Template::XS->new;
#        for my $ref (@{ $stash_ht->{'a_stuff'} }) {
#            $tt->loop_iteration('a_stuff')->set_values($ref);
#        }
#        $tt->set_values($stash_ht);
#        $tt->set_values($form);
#        $tt->set_delimiters('<!--','-->');
#        $tt->set_dir("$dir/");
#        $tt->set_strip(0);
#        my $out = $tt->parse_file("foo.tmpl");
#    },

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
#    CET_str => sub {
#        my $t = CGI::Ex::Template->new(VARIABLES => $stash_t);
#        my $out = ""; $t->process(\$content_tt, $form, \$out); $out;
#    },
#    CETX_str => sub {
#        my $t = CGI::Ex::Template::XS->new(VARIABLES => $stash_t);
#        my $out = ""; $t->process(\$content_tt, $form, \$out); $out;
#    },

    CETH_str => sub {
        my $ht = CGI::Ex::Template->new(    type => 'scalarref', source => \$content_ht, CASE_SENSITVE=>1);
        $ht->{'_documents'} = \%CETH_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETXH_str => sub {
        my $ht = CGI::Ex::Template::XS->new(type => 'scalarref', source => \$content_ht, CASE_SENSITVE=>1);
        $ht->{'_documents'} = \%CETXH_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETXHp_str => sub {
        my $ht = CGI::Ex::Template::XS->new(type => 'scalarref', source => \$content_ht, CASE_SENSITVE=>1, compile_perl => 1, cache => 1);
        $ht->{'_documents'} = \%CETXHp_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HT_str => sub {
        my $ht = HTML::Template->new(       type => 'scalarref', source => \$content_ht, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTE_str => sub {
        my $ht = HTML::Template::Expr->new( type => 'scalarref', source => \$content_ht, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTC_str => sub {
        my $ht = HTML::Template::Compiled->new(type => 'scalarref', source => \$content_ht, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    TMPL_str => sub {
        my $tt = Text::Tmpl->new;
        for my $ref (@{ $stash_ht->{'a_stuff'} }) {
            $tt->loop_iteration('a_stuff')->set_values($ref);
        }
        $tt->set_values($stash_ht);
        $tt->set_values($form);
        $tt->set_delimiters('<!--','-->');
        $tt->set_dir("$dir/");
        $tt->set_strip(0);
        my $out = $tt->parse_string($content_tmpl);
    },

    ###----------------------------------------------------------------###
    ### mem indicates that the compiled form is stored in memory

    TT_mem   => sub { my $out = ""; $tt->process( 'foo.tt', $form, \$out); $out },
    TTX_mem  => sub { my $out = ""; $ttx->process('foo.tt', $form, \$out); $out },
#    CET_mem  => sub { my $out = ""; $ct->process( 'foo.tt', $form, \$out); $out },
#    CETX_mem => sub { my $out = ""; $ctx->process('foo.tt', $form, \$out); $out },

    CETH_mem => sub {
        my $ht = CGI::Ex::Template->new(    filename => "foo.ht", path => \@dirs, cache => 1, CASE_SENSITVE=>1);
        $ht->{'_documents'} = \%CETH_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETXH_mem => sub {
        my $ht = CGI::Ex::Template::XS->new(filename => "foo.ht", path => \@dirs, cache => 1, CASE_SENSITVE=>1);
        $ht->{'_documents'} = \%CETXH_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    CETXHp_mem => sub {
        my $ht = CGI::Ex::Template::XS->new(filename => "foo.ht", path => \@dirs, cache => 1, CASE_SENSITVE=>1, compile_perl => 1, cache => 1);
        $ht->{'_documents'} = \%CETXHp_DOCUMENTS;
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HT_mem => sub {
        my $ht = HTML::Template->new(       filename => "foo.ht", path => \@dirs, cache => 1, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTC_mem => sub {
        my $ht = HTML::Template::Compiled->new(       filename => "foo.ht", path => \@dirs, cache => 1, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTE_mem => sub {
        my $ht = HTML::Template::Expr->new( filename => "foo.ht", path => \@dirs, cache => 1, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
    HTJ_mem => sub { # this is interesting - it is compiled - but it is pulled into memory just once
        my $ht = HTML::Template::JIT->new(  filename => "foo.ht", path => \@dirs, jit_path => $dir2, CASE_SENSITVE=>1);
        $ht->param($stash_ht); $ht->param($form); my $out = $ht->output;
    },
};

my $test = $tests->{'TT_str'}->();
foreach my $name (sort keys %$tests) {
    if ($test ne $tests->{$name}->()) {
        print "--------------------------TT_str-------\n";
        print $test;
        print "--------------------------$name--------\n";
        print $tests->{$name}->();
        die "$name did not match TT_str output\n";
    }
    $name =~ /(\w+)_(\w+)/;
    print "$name - $names->{$1} - ($names->{$2})\n";
}

###----------------------------------------------------------------###
### and now - the tests - grouped by common capability

my %mem_tests = map {($_ => $tests->{$_})} grep {/_mem$/} keys %$tests;
my %cpl_tests = map {($_ => $tests->{$_})} grep {/_file$/} keys %$tests;
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

__END__

=head1 SAMPLE OUTPUT v2.13

  CETH_file - CGI::Ex::Template using HTML::Template interface - (Loaded from file)
  CETH_mem - CGI::Ex::Template using HTML::Template interface - (Compiled in memory)
  CETH_str - CGI::Ex::Template using HTML::Template interface - (From string ref)
  CETXH_file - CGI::Ex::Template::XS using HTML::Template interface - (Loaded from file)
  CETXH_mem - CGI::Ex::Template::XS using HTML::Template interface - (Compiled in memory)
  CETXH_str - CGI::Ex::Template::XS using HTML::Template interface - (From string ref)
  CETX_file - CGI::Ex::Template::XS using TT interface - (Loaded from file)
  CETX_mem - CGI::Ex::Template::XS using TT interface - (Compiled in memory)
  CETX_str - CGI::Ex::Template::XS using TT interface - (From string ref)
  CET_file - CGI::Ex::Template using TT interface - (Loaded from file)
  CET_mem - CGI::Ex::Template using TT interface - (Compiled in memory)
  CET_str - CGI::Ex::Template using TT interface - (From string ref)
  HTE_mem - HTML::Template::Expr - (Compiled in memory)
  HTE_str - HTML::Template::Expr - (From string ref)
  HTJ_mem - HTML::Template::JIT - Compiled to C template - (Compiled in memory)
  HT_file - HTML::Template - (Loaded from file)
  HT_mem - HTML::Template - (Compiled in memory)
  HT_str - HTML::Template - (From string ref)
  TTXCET_str - Template::Toolkit with Stash::XS and Template::Parser::CET - (From string ref)
  TTX_file - Template::Toolkit with Stash::XS - (Loaded from file)
  TTX_mem - Template::Toolkit with Stash::XS - (Compiled in memory)
  TTX_str - Template::Toolkit with Stash::XS - (From string ref)
  TT_file - Template::Toolkit - (Loaded from file)
  TT_mem - Template::Toolkit - (Compiled in memory)
  TT_str - Template::Toolkit - (From string ref)
  TextTemplate_str - Text::Template - Perl code eval based - (From string ref)
  ------------------------------------------------------------------------
  From a string or scalarref tests
  Benchmark: running CETH_str, CETXH_str, CETX_str, CET_str, HTE_str, HT_str, TTXCET_str, TTX_str, TT_str, TextTemplate_str for at least 2 CPU seconds...
    CETH_str:  2 wallclock secs ( 2.18 usr +  0.00 sys =  2.18 CPU) @ 1449.08/s (n=3159)
   CETXH_str:  2 wallclock secs ( 2.00 usr +  0.01 sys =  2.01 CPU) @ 1700.00/s (n=3417)
    CETX_str:  2 wallclock secs ( 2.22 usr +  0.00 sys =  2.22 CPU) @ 1584.23/s (n=3517)
     CET_str:  2 wallclock secs ( 2.14 usr +  0.00 sys =  2.14 CPU) @ 1333.18/s (n=2853)
     HTE_str:  2 wallclock secs ( 2.07 usr +  0.00 sys =  2.07 CPU) @ 922.71/s (n=1910)
      HT_str:  2 wallclock secs ( 2.13 usr +  0.00 sys =  2.13 CPU) @ 1221.13/s (n=2601)
  TTXCET_str:  2 wallclock secs ( 2.01 usr +  0.01 sys =  2.02 CPU) @ 534.16/s (n=1079)
     TTX_str:  2 wallclock secs ( 2.14 usr +  0.00 sys =  2.14 CPU) @ 312.62/s (n=669)
      TT_str:  3 wallclock secs ( 2.12 usr +  0.01 sys =  2.13 CPU) @ 300.47/s (n=640)
  TextTemplate_str:  2 wallclock secs ( 2.13 usr +  0.02 sys =  2.15 CPU) @ 1189.77/s (n=2558)
                     Rate TT_str TTX_str TTXCET_str HTE_str TextTemplate_str HT_str CET_str CETH_str CETX_str CETXH_str
  TT_str            300/s     --     -4%       -44%    -67%             -75%   -75%    -77%     -79%     -81%      -82%
  TTX_str           313/s     4%      --       -41%    -66%             -74%   -74%    -77%     -78%     -80%      -82%
  TTXCET_str        534/s    78%     71%         --    -42%             -55%   -56%    -60%     -63%     -66%      -69%
  HTE_str           923/s   207%    195%        73%      --             -22%   -24%    -31%     -36%     -42%      -46%
  TextTemplate_str 1190/s   296%    281%       123%     29%               --    -3%    -11%     -18%     -25%      -30%
  HT_str           1221/s   306%    291%       129%     32%               3%     --     -8%     -16%     -23%      -28%
  CET_str          1333/s   344%    326%       150%     44%              12%     9%      --      -8%     -16%      -22%
  CETH_str         1449/s   382%    364%       171%     57%              22%    19%      9%       --      -9%      -15%
  CETX_str         1584/s   427%    407%       197%     72%              33%    30%     19%       9%       --       -7%
  CETXH_str        1700/s   466%    444%       218%     84%              43%    39%     28%      17%       7%        --
  ------------------------------------------------------------------------
  Compiled and cached on the file system tests
  Benchmark: running CETH_file, CETXH_file, CETX_file, CET_file, HT_file, TTX_file, TT_file for at least 2 CPU seconds...
   CETH_file:  3 wallclock secs ( 2.14 usr +  0.02 sys =  2.16 CPU) @ 3106.02/s (n=6709)
  CETXH_file:  2 wallclock secs ( 2.01 usr +  0.04 sys =  2.05 CPU) @ 4447.80/s (n=9118)
   CETX_file:  3 wallclock secs ( 2.02 usr +  0.09 sys =  2.11 CPU) @ 3586.26/s (n=7567)
    CET_file:  3 wallclock secs ( 2.16 usr +  0.05 sys =  2.21 CPU) @ 2432.13/s (n=5375)
     HT_file:  2 wallclock secs ( 2.18 usr +  0.03 sys =  2.21 CPU) @ 1868.33/s (n=4129)
    TTX_file:  2 wallclock secs ( 2.14 usr +  0.04 sys =  2.18 CPU) @ 820.64/s (n=1789)
     TT_file:  2 wallclock secs ( 2.11 usr +  0.04 sys =  2.15 CPU) @ 733.02/s (n=1576)
               Rate TT_file TTX_file HT_file CET_file CETH_file CETX_file CETXH_file
  TT_file     733/s      --     -11%    -61%     -70%      -76%      -80%       -84%
  TTX_file    821/s     12%       --    -56%     -66%      -74%      -77%       -82%
  HT_file    1868/s    155%     128%      --     -23%      -40%      -48%       -58%
  CET_file   2432/s    232%     196%     30%       --      -22%      -32%       -45%
  CETH_file  3106/s    324%     278%     66%      28%        --      -13%       -30%
  CETX_file  3586/s    389%     337%     92%      47%       15%        --       -19%
  CETXH_file 4448/s    507%     442%    138%      83%       43%       24%         --
  ------------------------------------------------------------------------
  Cached in memory tests
  Benchmark: running CETH_mem, CETXH_mem, CETX_mem, CET_mem, HTE_mem, HTJ_mem, HT_mem, TTX_mem, TT_mem for at least 2 CPU seconds...
    CETH_mem:  2 wallclock secs ( 2.11 usr +  0.03 sys =  2.14 CPU) @ 3193.46/s (n=6834)
   CETXH_mem:  2 wallclock secs ( 2.18 usr +  0.04 sys =  2.22 CPU) @ 4622.07/s (n=10261)
    CETX_mem:  2 wallclock secs ( 2.02 usr +  0.10 sys =  2.12 CPU) @ 6334.43/s (n=13429)
     CET_mem:  2 wallclock secs ( 2.16 usr +  0.04 sys =  2.20 CPU) @ 3946.82/s (n=8683)
     HTE_mem:  2 wallclock secs ( 2.20 usr +  0.01 sys =  2.21 CPU) @ 1515.38/s (n=3349)
     HTJ_mem:  2 wallclock secs ( 2.05 usr +  0.06 sys =  2.11 CPU) @ 5990.05/s (n=12639)
      HT_mem:  2 wallclock secs ( 1.98 usr +  0.03 sys =  2.01 CPU) @ 2588.56/s (n=5203)
     TTX_mem:  2 wallclock secs ( 2.07 usr +  0.03 sys =  2.10 CPU) @ 3254.29/s (n=6834)
      TT_mem:  2 wallclock secs ( 2.18 usr +  0.02 sys =  2.20 CPU) @ 2217.73/s (n=4879)
              Rate HTE_mem TT_mem HT_mem CETH_mem TTX_mem CET_mem CETXH_mem HTJ_mem CETX_mem
  HTE_mem   1515/s      --   -32%   -41%     -53%    -53%    -62%      -67%    -75%     -76%
  TT_mem    2218/s     46%     --   -14%     -31%    -32%    -44%      -52%    -63%     -65%
  HT_mem    2589/s     71%    17%     --     -19%    -20%    -34%      -44%    -57%     -59%
  CETH_mem  3193/s    111%    44%    23%       --     -2%    -19%      -31%    -47%     -50%
  TTX_mem   3254/s    115%    47%    26%       2%      --    -18%      -30%    -46%     -49%
  CET_mem   3947/s    160%    78%    52%      24%     21%      --      -15%    -34%     -38%
  CETXH_mem 4622/s    205%   108%    79%      45%     42%     17%        --    -23%     -27%
  HTJ_mem   5990/s    295%   170%   131%      88%     84%     52%       30%      --      -5%
  CETX_mem  6334/s    318%   186%   145%      98%     95%     60%       37%      6%       --

=cut
