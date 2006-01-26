# -*- Mode: Perl; -*-

use vars qw($module $is_tt);
BEGIN {
    $module = 'CGI::Ex::Template'; #0.15user 0.00system 0:00.28elapsed 57%CPU
    #$module = 'Template';         #0.72user 0.00system 0:01.16elapsed 63%CPU
    $is_tt = $module eq 'Template';
};

use strict;
use Test::More tests => 389 - ($is_tt ? 44 : 0);
use Data::Dumper qw(Dumper);

### set up some dummy packages for use later
{
    package MyTestPlugin::Foo;
    $INC{'MyTestPlugin/Foo.pm'} = $0;
    sub load { $_[0] }
    sub new {
        my $class   = shift;
        my $context = shift;  # note the plugin style object that needs to shift off context
        my $args    = shift || {};
        return bless $args, $class;
    }
    sub bar { my $self = shift; return join('', keys %$self, values %$self) }
    sub seven { 7 }
    sub many { return 1, 2, 3 }
    sub echo { my $self = shift; $_[0] }
}
{
    package Foo2;
    $INC{'Foo2.pm'} = $0;
    use base qw(MyTestPlugin::Foo);
    sub new {
        my $class   = shift;
        my $args    = shift || {}; # note - no plugin context
        return bless $args, $class;
    }
}

use_ok($module);

sub process_ok { # process the value
    my $str  = shift;
    my $test = shift;
    my $args = shift;
    my $out  = '';
    my $obj = $module->new(ABSOLUTE => 1,
                           PLUGIN_BASE => 'MyTestPlugin', LOAD_PERL => 1,
                           CONSTANTS   => {harry => sub {'do_this_once'}},
                           @{ $args->{tt_config} || [] },
                           );
    $obj->process(\$str, $args, \$out);
    my $ok = $out eq $test;
    ok($ok, "\"$str\" => \"$out\"" . ($ok ? '' : " - should've been \"$test\""));
    my $line = (caller)[2];
    warn "#   process_ok called at line $line.\n" if ! $ok;
    print $obj->error if ! $ok && $obj->can('error');
    print Dumper $obj->parse_tree(\$str) if ! $ok && $obj->can('parse_tree');
    exit if ! $ok;
}

my $obj = Foo2->new;

###----------------------------------------------------------------###
### variable GETting

process_ok("[% foo %]" => "");
process_ok("[% foo %]" => "7",       {foo => 7});
process_ok("[% foo %][% foo %][% foo %]" => "777", {foo => 7});
process_ok("[% foo() %]" => "7",     {foo => 7});
process_ok("[% foo.bar %]" => "");
process_ok("[% foo.bar %]" => "",    {foo => {}});
process_ok("[% foo.bar %]" => "7",   {foo => {bar => 7}});
process_ok("[% foo().bar %]" => "7", {foo => {bar => 7}});
process_ok("[% foo.0 %]" => "7",     {foo => [7, 2, 3]});
process_ok("[% foo.10 %]" => "",     {foo => [7, 2, 3]});
process_ok("[% foo %]" => 7,         {foo => sub { 7 }});
process_ok("[% foo(7) %]" => 7,      {foo => sub { $_[0] }});
process_ok("[% foo.length %]" => 1,  {foo => sub { 7 }});
process_ok("[% foo.0 %]" => 7,       {foo => sub { return 7, 2, 3 }});
process_ok("[% foo(bar) %]" => 7,    {foo => sub { $_[0] }, bar => 7});
process_ok("[% foo.seven %]" => 7,   {foo => $obj});
process_ok("[% foo.seven() %]" => 7, {foo => $obj});
process_ok("[% foo.seven.length %]" => 1, {foo => $obj});
process_ok("[% foo.echo(7) %]" => 7, {foo => $obj});
process_ok("[% foo.many.0 %]" => 1,  {foo => $obj});
process_ok("[% foo.many.10 %]" => '',{foo => $obj});
process_ok("[% foo.nomethod %]" => '',{foo => $obj});
process_ok("[% foo.nomethod.0 %]" => '',{foo => $obj});

process_ok("[% GET foo %]" => "");
process_ok("[% GET foo %]" => "7",     {foo => 7});
process_ok("[% GET foo.bar %]" => "");
process_ok("[% GET foo.bar %]" => "",  {foo => {}});
process_ok("[% GET foo.bar %]" => "7", {foo => {bar => 7}});
process_ok("[% GET foo.0 %]" => "7",   {foo => [7, 2, 3]});
process_ok("[% GET foo %]" => 7,       {foo => sub { 7 }});
process_ok("[% GET foo(7) %]" => 7,    {foo => sub { $_[0] }});

process_ok("[% \$name %]" => "",        {name => 'foo'});
process_ok("[% \$name %]" => "7",       {name => 'foo', foo => 7});
process_ok("[% \$name.bar %]" => "",    {name => 'foo'});
process_ok("[% \$name.bar %]" => "",    {name => 'foo', foo => {}});
process_ok("[% \$name.bar %]" => "7",   {name => 'foo', foo => {bar => 7}});
process_ok("[% \$name().bar %]" => "7", {name => 'foo', foo => {bar => 7}});
process_ok("[% \$name.0 %]" => "7",     {name => 'foo', foo => [7, 2, 3]});
process_ok("[% \$name %]" => 7,         {name => 'foo', foo => sub { 7 }});
process_ok("[% \$name(7) %]" => 7,      {name => 'foo', foo => sub { $_[0] }});

process_ok("[% GET \$name %]" => "",      {name => 'foo'});
process_ok("[% GET \$name %]" => "7",     {name => 'foo', foo => 7});
process_ok("[% GET \$name.bar %]" => "",  {name => 'foo'});
process_ok("[% GET \$name.bar %]" => "",  {name => 'foo', foo => {}});
process_ok("[% GET \$name.bar %]" => "7", {name => 'foo', foo => {bar => 7}});
process_ok("[% GET \$name.0 %]" => "7",   {name => 'foo', foo => [7, 2, 3]});
process_ok("[% GET \$name %]" => 7,       {name => 'foo', foo => sub { 7 }});
process_ok("[% GET \$name(7) %]" => 7,    {name => 'foo', foo => sub { $_[0] }});

process_ok("[% \$name %]" => "",     {name => 'foo foo', foo => 7});
process_ok("[% GET \$name %]" => "", {name => 'foo foo', foo => 7});

process_ok("[% \${name} %]" => "",        {name => 'foo'});
process_ok("[% \${name} %]" => "7",       {name => 'foo', foo => 7});
process_ok("[% \${name}.bar %]" => "",    {name => 'foo'});
process_ok("[% \${name}.bar %]" => "",    {name => 'foo', foo => {}});
process_ok("[% \${name}.bar %]" => "7",   {name => 'foo', foo => {bar => 7}});
process_ok("[% \${name}().bar %]" => "7", {name => 'foo', foo => {bar => 7}});
process_ok("[% \${name}.0 %]" => "7",     {name => 'foo', foo => [7, 2, 3]});
process_ok("[% \${name} %]" => 7,         {name => 'foo', foo => sub { 7 }});
process_ok("[% \${name}(7) %]" => 7,      {name => 'foo', foo => sub { $_[0] }});

process_ok("[% GET \${name} %]" => "",      {name => 'foo'});
process_ok("[% GET \${name} %]" => "7",     {name => 'foo', foo => 7});
process_ok("[% GET \${name}.bar %]" => "",  {name => 'foo'});
process_ok("[% GET \${name}.bar %]" => "",  {name => 'foo', foo => {}});
process_ok("[% GET \${name}.bar %]" => "7", {name => 'foo', foo => {bar => 7}});
process_ok("[% GET \${name}.0 %]" => "7",   {name => 'foo', foo => [7, 2, 3]});
process_ok("[% GET \${name} %]" => 7,       {name => 'foo', foo => sub { 7 }});
process_ok("[% GET \${name}(7) %]" => 7,    {name => 'foo', foo => sub { $_[0] }});

process_ok("[% \${name} %]" => "",     {name => 'foo foo', foo => 7});
process_ok("[% GET \${name} %]" => "", {name => 'foo foo', foo => 7});

process_ok("[% foo.\$name %]" => '', {name => 'bar'});
process_ok("[% foo.\$name %]" => 7, {name => 'bar', foo => {bar => 7}});
process_ok("[% foo.\$name.baz %]" => '', {name => 'bar', bar => {baz => 7}});

process_ok("[% \"hi\" %]" => 'hi');
process_ok("[% 'hi' %]" => 'hi');
process_ok("[% \"\$foo\" %]"   => '7', {foo => 7});
process_ok("[% \"hi \$foo\" %]"   => 'hi 7', {foo => 7});
process_ok("[% \"hi \${foo}\" %]" => 'hi 7', {foo => 7});
process_ok("[% 'hi \$foo' %]"   => 'hi $foo', {foo => 7});
process_ok("[% 'hi \${foo}' %]" => 'hi ${foo}', {foo => 7});

process_ok("[% \"hi \${foo.seven}\" %]"   => 'hi 7', {foo => $obj});
process_ok("[% \"hi \${foo.echo(7)}\" %]" => 'hi 7', {foo => $obj});


###----------------------------------------------------------------###
### variable SETting

process_ok("[% SET foo bar %][% foo %]" => '');
process_ok("[% SET foo = 1 %][% foo %]" => '1');
process_ok("[% SET foo = 1  bar = 2 %][% foo %][% bar %]" => '12');
process_ok("[% SET foo  bar = 1 %][% foo %]" => '');
process_ok("[% SET foo = 1 ; bar = 1 %][% foo %]" => '1');
process_ok("[% SET foo = 1 %][% SET foo %][% foo %]" => '');

process_ok("[% SET foo = [] %][% foo.0 %]" => "");
process_ok("[% SET foo = [1, 2, 3] %][% foo.1 %]" => 2);
process_ok("[% SET foo = {} %][% foo.0 %]" => "");
process_ok("[% SET foo = {1 => 2} %][% foo.1 %]" => "2") if ! $is_tt;
process_ok("[% SET foo = {'1' => 2} %][% foo.1 %]" => "2");

process_ok("[% SET name = 1 %][% SET foo = name %][% foo %]" => "1");
process_ok("[% SET name = 1 %][% SET foo = \$name %][% foo %]" => "");
process_ok("[% SET name = 1 %][% SET foo = \${name} %][% foo %]" => "");
process_ok("[% SET name = 1 %][% SET foo = \"\$name\" %][% foo %]" => "1");
process_ok("[% SET name = 1 foo = name %][% foo %]" => '1');
process_ok("[% SET name = 1 %][% SET foo = {\$name => 2} %][% foo.1 %]" => "2");
process_ok("[% SET name = 1 %][% SET foo = {\"\$name\" => 2} %][% foo.1 %]" => "2") if ! $is_tt;
process_ok("[% SET name = 1 %][% SET foo = {\${name} => 2} %][% foo.1 %]" => "2");

process_ok("[% SET name = 7 %][% SET foo = {'2' => name} %][% foo.2 %]" => "7");
process_ok("[% SET name = 7 %][% SET foo = {'2' => \"\$name\"} %][% foo.2 %]" => "7");

process_ok("[% SET name = 7 %][% SET foo = [1, name, 3] %][% foo.1 %]" => "7");
process_ok("[% SET name = 7 %][% SET foo = [1, \"\$name\", 3] %][% foo.1 %]" => "7");

process_ok("[% SET foo = { bar => { baz => [0, 7, 2] } } %][% foo.bar.baz.1 %]" => "7");

process_ok("[% SET foo.bar = 1 %][% foo.bar %]" => '1');
process_ok("[% SET foo.bar.baz.bing = 1 %][% foo.bar.baz.bing %]" => '1');
process_ok("[% SET foo.bar.2 = 1 %][% foo.bar.2 %] [% foo.bar.size %]" => '1 1');
process_ok("[% SET foo.bar = [] %][% SET foo.bar.2 = 1 %][% foo.bar.2 %] [% foo.bar.size %]" => '1 3');

process_ok("[% SET name = 'two' %][% SET \$name = 3 %][% two %]" => 3);
process_ok("[% SET name = 'two' %][% SET \${name} = 3 %][% two %]" => 3);
process_ok("[% SET name = 2 %][% SET foo.\$name = 3 %][% foo.2 %]" => 3);
process_ok("[% SET name = 2 %][% SET foo.\$name = 3 %][% foo.\$name %]" => 3);
process_ok("[% SET name = 2 %][% SET foo.\${name} = 3 %][% foo.2 %]" => 3);
process_ok("[% SET name = 2 %][% SET foo.\${name} = 3 %][% foo.2 %]" => 3);
process_ok("[% SET name = 'two' %][% SET \$name.foo = 3 %][% two.foo %]" => 3);
process_ok("[% SET name = 'two' %][% SET \${name}.foo = 3 %][% two.foo %]" => 3);
process_ok("[% SET name = 'two' %][% SET foo.\$name.foo = 3 %][% foo.two.foo %]" => 3);
process_ok("[% SET name = 'two' %][% SET foo.\${name}.foo = 3 %][% foo.two.foo %]" => 3);

process_ok("[% SET foo = [1..10] %][% foo.6 %]" => 7);
process_ok("[% SET foo = [10..1] %][% foo.6 %]" => '');
process_ok("[% SET foo = [-10..-1] %][% foo.6 %]" => -4);
process_ok("[% SET foo = [1..3..10] %][% foo.6 %]" => '7') if ! $is_tt;
process_ok("[% SET foo = [1..2..10] %][% foo.6 %]" => '7') if ! $is_tt;
process_ok("[% SET foo = [1,1..0..10] %][% foo.6 %]" => '6') if ! $is_tt;
process_ok("[% SET foo = [1..10, 21..30] %][% foo.12 %]" => 23)         if ! $is_tt;
process_ok("[% SET foo = [..100] bar = 7 %][% bar %][% foo.0 %]" => '');
process_ok("[% SET foo = [100..] bar = 7 %][% bar %][% foo.0 %]" => 7)  if ! $is_tt;
process_ok("[% SET foo = ['a'..'z'] %][% foo.6 %]" => 'g');
process_ok("[% SET foo = ['z'..'a'] %][% foo.6 %]" => '');
process_ok("[% SET foo = ['a'..'z'].reverse %][% foo.6 %]" => 't')      if ! $is_tt;

process_ok("[% foo = 1 %][% foo %]" => '1');
process_ok("[% foo = 1 bar = 2 %][% foo %][% bar %]" => '12');
process_ok("[% foo = 1 ; bar = 2 %][% foo %][% bar %]" => '12');
process_ok("[% foo.bar = 2 %][% foo.bar %]" => '2');

###----------------------------------------------------------------###
### CALL and DEFAULT

process_ok("[% DEFAULT foo = 7 %][% foo %]" => 7);
process_ok("[% SET foo = 5 %][% DEFAULT foo = 7 %][% foo %]" => 5);
process_ok("[% DEFAULT foo.bar.baz.bing = 6 %][% foo.bar.baz.bing %]" => 6);

my $t = 0;
process_ok("[% foo %]"      => 'hi', {foo => sub {$t++; 'hi'}});
process_ok("[% GET  foo %]" => 'hi', {foo => sub {$t++; 'hi'}});
process_ok("[% CALL foo %]" => '',   {foo => sub {$t++; 'hi'}});
ok($t == 3, "CALL method actually called var");

###----------------------------------------------------------------###
### virtual methods / filters

process_ok("[% [0 .. 10].reverse.1 %]" => 9) if ! $is_tt;
process_ok("[% {a => 'A'}.a %]" => 'A') if ! $is_tt;
process_ok("[% 'This is a string'.length %]" => 16) if ! $is_tt;
process_ok("[% 123.length %]" => 3) if ! $is_tt;
process_ok("[% 123.2.length %]" => 5) if ! $is_tt;
process_ok("[% -123.2.length %]" => -5) if ! $is_tt; # the - doesn't bind as tight as the dot methods

process_ok("[% n.repeat %]" => '1',     {n => 1}) if ! $is_tt; # tt2 virtual method defaults to 0
process_ok("[% n.repeat(0) %]" => '',   {n => 1});
process_ok("[% n.repeat(1) %]" => '1',  {n => 1});
process_ok("[% n.repeat(2) %]" => '11', {n => 1});
process_ok("[% n.repeat(2,'|') %]" => '1|1', {n => 1}) if ! $is_tt;

process_ok("[% n.size %]", => 'SIZE', {n => {size => 'SIZE', a => 'A'}});
process_ok("[% n|size %]", => '2',    {n => {size => 'SIZE', a => 'A'}}) if ! $is_tt; # tt2 | is alias for FILTER

process_ok("[% n FILTER size %]", => '1', {n => {size => 'SIZE', a => 'A'}}) if ! $is_tt; # tt2 doesn't have size

process_ok("[% n FILTER repeat %]" => '1',     {n => 1});
process_ok("[% n FILTER repeat(0) %]" => '',   {n => 1});
process_ok("[% n FILTER repeat(1) %]" => '1',  {n => 1});
process_ok("[% n FILTER repeat(2) %]" => '11', {n => 1});
process_ok("[% n FILTER repeat(2,'|') %]" => '1|1', {n => 1}) if ! $is_tt;

process_ok("[% n FILTER echo = repeat(2) %][% n FILTER echo %]" => '1111', {n => 1});
process_ok("[% n FILTER echo = repeat(2) %][% n | echo %]" => '1111', {n => 1});
process_ok("[% n FILTER echo = repeat(2) %][% n|echo.length %]" => '112', {n => 1}) if ! $is_tt;
process_ok("[% n FILTER echo = repeat(2) %][% n FILTER \$foo %]" => '1111', {n => 1, foo => 'echo'});
process_ok("[% n FILTER echo = repeat(2) %][% n | \$foo %]" => '1111', {n => 1, foo => 'echo'});
process_ok("[% n FILTER echo = repeat(2) %][% n|\$foo.length %]" => '112', {n => 1, foo => 'echo'}) if ! $is_tt;

###----------------------------------------------------------------###
### chomping

process_ok(" [% foo %]" => ' ');
process_ok(" [%- foo %]" => '');
process_ok("\n[%- foo %]" => '');
process_ok("\n [%- foo %]" => '');
process_ok("\n\n[%- foo %]" => "\n");
process_ok(" \n\n[%- foo %]" => " \n");
process_ok(" \n[%- foo %]" => " ") if ! $is_tt;
process_ok(" \n \n[%- foo %]" => " \n ") if ! $is_tt;

process_ok("[% foo %] " => ' ');
process_ok("[% foo -%] " => '') if ! $is_tt;
process_ok("[% foo -%]\n" => '');
process_ok("[% foo -%] \n" => '');
process_ok("[% foo -%]\n " => ' ');
process_ok("[% foo -%]\n\n\n" => "\n\n");
process_ok("[% foo -%] \n " => ' ');

###----------------------------------------------------------------###
### math operations

process_ok("[% 1 + 2 %]" => 3);
process_ok("[% 1 + 2 + 3 %]" => 6);
process_ok("[% (1 + 2) %]" => 3);
process_ok("[% 2 - 1 %]" => 1);
process_ok("[% -1 + 2 %]" => 1);
process_ok("[% -1+2 %]" => 1);
process_ok("[% 2 - 1 %]" => 1);
process_ok("[% 2-1 %]" => 1) if ! $is_tt;
process_ok("[% 2 - -1 %]" => 3);
process_ok("[% 4 * 2 %]" => 8);
process_ok("[% 4 / 2 %]" => 2);
process_ok("[% 2 ** 3 %]" => 8) if ! $is_tt;
process_ok("[% 1 + 2 * 3 %]" => 7);
process_ok("[% 3 * 2 + 1 %]" => 7);
process_ok("[% (1 + 2) * 3 %]" => 9);
process_ok("[% 3 * (1 + 2) %]" => 9);
process_ok("[% 1 + 2 ** 3 %]" => 9) if ! $is_tt;
process_ok("[% 2 * 2 ** 3 %]" => 16) if ! $is_tt;
process_ok("[% SET foo = 1 %][% foo + 2 %]" => 3);
process_ok("[% SET foo = 1 %][% (foo + 2) %]" => 3);

###----------------------------------------------------------------###
### boolean operations

process_ok("[% 5 && 6 %]" => 6);
process_ok("[% 5 || 6 %]" => 5);
process_ok("[% 0 || 6 %]" => 6);
process_ok("[% 0 && 6 %]" => 0);
process_ok("[% 0 && 0 %]" => 0);

process_ok("[% 5 + (0 || 5) %]" => 10);


process_ok("[% 1 ? 2 : 3 %]" => '2');
process_ok("[% 0 ? 2 : 3 %]" => '3');
process_ok("[% 0 ? (1 ? 2 : 3) : 4 %]" => '4');
process_ok("[% 0 ? 1 ? 2 : 3 : 4 %]" => '4');

process_ok("[% t = 1 || 0 ? 3 : 4 %][% t %]" => 3);
process_ok("[% t = 0 or 1 ? 3 : 4 %][% t %]" => 3);
process_ok("[% t = 1 or 0 ? 3 : 4 %][% t %]" => 1) if ! $is_tt;

process_ok("[% 0 ? 2 : 3 %]" => '3');
process_ok("[% 1 ? 2 : 3 %]" => '2');
process_ok("[% 0 ? 1 ? 2 : 3 : 4 %]" => '4');
process_ok("[% t = 0 ? 1 ? [1..4] : [2..4] : [3..4] %][% t.0 %]" => '3');
process_ok("[% t = 1 || 0 ? 0 : 1 || 2 ? 2 : 3 %][% t %]" => '0');
process_ok("[% t = 0 or 0 ? 0 : 1 or 2 ? 2 : 3 %][% t %]" => '1') if ! $is_tt;
process_ok("[% t = 0 or 0 ? 0 : 0 or 2 ? 2 : 3 %][% t %]" => '2');

process_ok("[% 0 ? 1 ? 1 + 2 * 3 : 1 + 2 * 4 : 1 + 2 * 5 %]" => '11');

###----------------------------------------------------------------###
### blocks

process_ok("[% PROCESS foo %]" => '');
process_ok("[% BLOCK foo %]" => '');
process_ok("[% BLOCK foo %][% END %]" => '');
process_ok("[% BLOCK %][% END %]one" => 'one');
process_ok("[% BLOCK foo %]hi there[% END %]" => '');
process_ok("[% BLOCK foo %][% BLOCK foo %][% END %][% END %]" => '');
process_ok("[% BLOCK foo %]hi there[% END %][% PROCESS foo %]" => 'hi there');
process_ok("[% PROCESS foo %][% BLOCK foo %]hi there[% END %]" => 'hi there');
process_ok("[% BLOCK foo %]hi [% one %] there[% END %][% PROCESS foo %]" => 'hi ONE there', {one => 'ONE'});
process_ok("[% BLOCK foo %]hi [% IF 1 %]Yes[% END %] there[% END %]<<[% PROCESS foo %]>>" => '<<hi Yes there>>');
process_ok("[% BLOCK foo %]hi [% one %] there[% END %][% PROCESS foo one = 'two' %]" => 'hi two there');
process_ok("[% BLOCK foo %]hi [% one.two %] there[% END %][% PROCESS foo one.two = 'two' %]" => 'hi two there');
process_ok("[% BLOCK foo %]hi [% one.two %] there[% END %][% PROCESS foo + foo one.two = 'two' %]" => 'hi two there'x2);

process_ok("[% BLOCK foo %]hi [% one %] there[% END %][% PROCESS foo one = 'two' %][% one %]" => 'hi two theretwo');
process_ok("[% BLOCK foo %]hi [% one %] there[% END %][% INCLUDE foo one = 'two' %][% one %]" => 'hi two there');

###----------------------------------------------------------------###
### if/unless/elsif/else

process_ok("[% IF 1 %]Yes[% END %]" => 'Yes');
process_ok("[% IF 0 %]Yes[% END %]" => '');
process_ok("[% IF 0 %]Yes[% ELSE %]No[% END %]" => 'No');
process_ok("[% IF 0 %]Yes[% ELSIF 1 %]No[% END %]" => 'No');
process_ok("[% IF 0 %]Yes[% ELSIF 0 %]No[% END %]" => '');
process_ok("[% IF 0 %]Yes[% ELSIF 0 %]No[% ELSE %]hmm[% END %]" => 'hmm');

process_ok("[% UNLESS 1 %]Yes[% END %]" => '');
process_ok("[% UNLESS 0 %]Yes[% END %]" => 'Yes');
process_ok("[% UNLESS 0 %]Yes[% ELSE %]No[% END %]" => 'Yes');
process_ok("[% UNLESS 1 %]Yes[% ELSIF 1 %]No[% END %]" => 'No');
process_ok("[% UNLESS 1 %]Yes[% ELSIF 0 %]No[% END %]" => '');
process_ok("[% UNLESS 1 %]Yes[% ELSIF 0 %]No[% ELSE %]hmm[% END %]" => 'hmm');

###----------------------------------------------------------------###
### comments

process_ok("[%# one %]" => '', {one => 'ONE'});
process_ok("[%#\n one %]" => '', {one => 'ONE'});
process_ok("[%-#\n one %]" => '', {one => 'ONE'})     if ! $is_tt;
process_ok("[% #\n one %]" => 'ONE', {one => 'ONE'});
process_ok("[%# BLOCK one %]" => '');
process_ok("[%# BLOCK one %]two" => 'two');
process_ok("[%# BLOCK one %]two[% END %]" => '');
process_ok("[%# BLOCK one %]two[% END %]three" => '');

###----------------------------------------------------------------###
### foreach, next, last

process_ok("[% FOREACH foo %]" => '');
process_ok("[% FOREACH foo %][% END %]" => '');
process_ok("[% FOREACH foo %]bar[% END %]" => '');
process_ok("[% FOREACH foo %]bar[% END %]" => 'bar', {foo => 1});
process_ok("[% FOREACH f IN foo %]bar[% f %][% END %]" => 'bar1bar2', {foo => [1,2]});
process_ok("[% FOREACH f = foo %]bar[% f %][% END %]" => 'bar1bar2', {foo => [1,2]});
process_ok("[% FOREACH f = [1,2] %]bar[% f %][% END %]" => 'bar1bar2');
process_ok("[% FOREACH f = [1..3] %]bar[% f %][% END %]" => 'bar1bar2bar3');
process_ok("[% FOREACH f = [{a=>'A'},{a=>'B'}] %]bar[% f.a %][% END %]" => 'barAbarB');
process_ok("[% FOREACH [{a=>'A'},{a=>'B'}] %]bar[% a %][% END %]" => 'barAbarB');
process_ok("[% FOREACH [{a=>'A'},{a=>'B'}] %]bar[% a %][% END %][% a %]" => 'barAbarB');
process_ok("[% FOREACH f = [1..3] %][% loop.count %]/[% loop.size %] [% END %]" => '1/3 2/3 3/3 ');
process_ok("[% FOREACH f = [1..3] %][% IF loop.first %][% f %][% END %][% END %]" => '1');
process_ok("[% FOREACH f = [1..3] %][% IF loop.last %][% f %][% END %][% END %]" => '3');
process_ok("[% FOREACH f = [1..3] %][% IF loop.first %][% NEXT %][% END %][% f %][% END %]" => '23');
process_ok("[% FOREACH f = [1..3] %][% IF loop.first %][% LAST %][% END %][% f %][% END %]" => '');
process_ok("[% FOREACH f = [1..3] %][% f %][% IF loop.first %][% NEXT %][% END %][% END %]" => '123');
process_ok("[% FOREACH f = [1..3] %][% f %][% IF loop.first %][% LAST %][% END %][% END %]" => '1');

### TT is not consistent in what is localized - well it is documented
### if you set a variable in the FOREACH tag, then nothing in the loop gets localized
### if you don't set a variable - everything gets localized
process_ok("[% foo = 1 %][% FOREACH [1..10] %][% foo %][% foo = 2 %][% END %]" => '1222222222');
process_ok("[% f = 1 %][% FOREACH i = [1..10] %][% i %][% f = 2 %][% END %][% f %]" => '123456789102');
process_ok("[% f = 1 %][% FOREACH [1..10] %][% f = 2 %][% END %][% f %]" => '1');
process_ok("[% f = 1 %][% FOREACH f = [1..10] %][% f %][% END %][% f %]" => '1234567891010');
process_ok("[% FOREACH [1] %][% SET a = 1 %][% END %][% a %]" => '');
process_ok("[% a %][% FOREACH [1] %][% SET a = 1 %][% END %][% a %]" => '');
process_ok("[% a = 2 %][% FOREACH [1] %][% SET a = 1 %][% END %][% a %]" => '2');
process_ok("[% a = 2 %][% FOREACH [1] %][% a = 1 %][% END %][% a %]" => '2');
process_ok("[% a = 2 %][% FOREACH i = [1] %][% a = 1 %][% END %][% a %]" => '1');
process_ok("[% FOREACH i = [1] %][% SET a = 1 %][% END %][% a %]" => '1');
process_ok("[% f.b = 1 %][% FOREACH f.b = [1..10] %][% f.b %][% END %][% f.b %]" => '1234567891010') if ! $is_tt;
process_ok("[% a = 1 %][% FOREACH [{a=>'A'},{a=>'B'}] %]bar[% a %][% END %][% a %]" => 'barAbarB1');
process_ok("[% FOREACH [1..3] %][% loop.size %][% END %][% loop.size %]" => '333');
process_ok("[% FOREACH i = [1..3] %][% loop.size %][% END %][% loop.size %]" => '3333') if ! $is_tt;
process_ok("[% FOREACH i = [1..3] %][% loop.size %][% END %][% loop.size %]" => '3331') if $is_tt;

###----------------------------------------------------------------###
### while

process_ok("[% WHILE foo %]" => '');
process_ok("[% WHILE foo %][% END %]" => '');
process_ok("[% WHILE (foo = foo - 1) %][% END %]" => '');
process_ok("[% WHILE (foo = foo - 1) %][% foo %][% END %]" => '21', {foo => 3});
process_ok("[% WHILE foo %][% foo %][% foo = foo - 1 %][% END %]" => '321', {foo => 3});

process_ok("[% WHILE 1 %][% foo %][% foo = foo - 1 %][% LAST IF foo == 1 %][% END %]" => '32', {foo => 3});
process_ok("[% f = 10; WHILE f; f = f - 1 ; f ; END %]" => '9876543210');
process_ok("[% f = 10; WHILE f; f = f - 1 ; f ; END ; f %]" => '98765432100');
process_ok("[% f = 10 a = 2; WHILE f; f = f - 1 ; f ; a=3; END ; a%]" => '98765432103');

process_ok("[% f = 10; WHILE (g=f); f = f - 1 ; f ; END %]" => '9876543210');
process_ok("[% f = 10; WHILE (g=f); f = f - 1 ; f ; END ; f %]" => '98765432100');
process_ok("[% f = 10 a = 2; WHILE (g=f); f = f - 1 ; f ; a=3; END ; a%]" => '98765432103');
process_ok("[% f = 10 a = 2; WHILE (a=f); f = f - 1 ; f ; a=3; END ; a%]" => '98765432100');

###----------------------------------------------------------------###
### stop, return, clear

process_ok("[% STOP %]" => '');
process_ok("One[% STOP %]Two" => 'One');
process_ok("[% BLOCK foo %]One[% STOP %]Two[% END %]First[% PROCESS foo %]Last" => 'FirstOne');
process_ok("[% FOREACH f = [1..3] %][% f %][% IF loop.first %][% STOP %][% END %][% END %]" => '1');
process_ok("[% FOREACH f = [1..3] %][% IF loop.first %][% STOP %][% END %][% f %][% END %]" => '');

process_ok("[% RETURN %]" => '');
process_ok("One[% RETURN %]Two" => 'One');
process_ok("[% BLOCK foo %]One[% RETURN %]Two[% END %]First[% PROCESS foo %]Last" => 'FirstOneLast');
process_ok("[% FOREACH f = [1..3] %][% f %][% IF loop.first %][% RETURN %][% END %][% END %]" => '1');
process_ok("[% FOREACH f = [1..3] %][% IF loop.first %][% RETURN %][% END %][% f %][% END %]" => '');

process_ok("[% CLEAR %]" => '');
process_ok("One[% CLEAR %]Two" => 'Two');
process_ok("[% BLOCK foo %]One[% CLEAR %]Two[% END %]First[% PROCESS foo %]Last" => 'FirstTwoLast');
process_ok("[% FOREACH f = [1..3] %][% f %][% IF loop.first %][% CLEAR %][% END %][% END %]" => '23');
process_ok("[% FOREACH f = [1..3] %][% IF loop.first %][% CLEAR %][% END %][% f %][% END %]" => '123');
process_ok("[% FOREACH f = [1..3] %][% f %][% IF loop.last %][% CLEAR %][% END %][% END %]" => '');
process_ok("[% FOREACH f = [1..3] %][% IF loop.last %][% CLEAR %][% END %][% f %][% END %]" => '3');

###----------------------------------------------------------------###
### multiple-directives

process_ok("[% GET foo; GET foo %]" => '11', {foo => 1});
process_ok('[% FOREACH f = [1..3]; 1; END %]' => '111');
process_ok('[% FOREACH f = [1..3]; f; END %]' => '123');
process_ok('[% FOREACH f = [1..3]; "$f"; END %]' => '123');
process_ok('[% FOREACH f = [1..3]; f + 1; END %]' => '234');

###----------------------------------------------------------------###
### post opererator

process_ok("[% GET foo IF 1 %]" => '1', {foo => 1});
process_ok("[% f FOREACH f = [1..3] %]" => '123');

process_ok("2[% GET foo IF 1 IF 2 %]" => '21', {foo => 1})      if ! $is_tt;
process_ok("2[% GET foo IF 1 IF 0 %]" => '2',  {foo => 1})      if ! $is_tt;
process_ok("[% f FOREACH f = [1..3] IF 1 %]" => '123')          if ! $is_tt;
process_ok("[% f FOREACH f = [1..3] IF 0 %]" => '')             if ! $is_tt;
process_ok("[% f FOREACH f = g FOREACH g = [1..3] %]" => '123') if ! $is_tt;
process_ok("[% f FOREACH f = g.a FOREACH g = [{a=>1}, {a=>2}, {a=>3}] %]" => '123') if ! $is_tt;
process_ok("[% f FOREACH f = a FOREACH [{a=>1}, {a=>2}, {a=>3}] %]" => '123')       if ! $is_tt;

process_ok("[% FOREACH f = [1..3] IF 1 %]([% f %])[% END %]" => '(1)(2)(3)')        if ! $is_tt;
process_ok("[% FOREACH f = [1..3] IF 0 %]([% f %])[% END %]" => '')                 if ! $is_tt;

process_ok("[% BLOCK bar %][% foo %][% foo = foo - 1 %][% END %][% PROCESS bar WHILE foo %]" => '321', {foo => 3});

###----------------------------------------------------------------###
### capturing

process_ok("[% foo = BLOCK %]Hi[% END %][% foo %][% foo %]" => 'HiHi');
process_ok("[% BLOCK foo %]Hi[% END %][% bar = PROCESS foo %]-[% bar %]" => '-Hi');
process_ok("[% foo = IF 1 %]Hi[% END %][% foo %]" => 'Hi');

###----------------------------------------------------------------###
### tags

process_ok("[% TAGS html %]<!-- 1 + 2 -->" => '3');
process_ok("[% TAGS <!-- --> %]<!-- 1 + 2 -->" => '3');
process_ok("[% TAGS html %] <!--- 1 + 2 -->" => '3');
process_ok("[% TAGS html %]<!-- 1 + 2 ---> " => '3') if ! $is_tt;
process_ok("[% TAGS html %]<!-- 1 + 2 --->\n" => '3');
process_ok("[% BLOCK foo %][% TAGS html %]<!-- 1 + 2 -->[% END %][% PROCESS foo %] [% 1 + 2 %]" => '');

###----------------------------------------------------------------###
### switch

process_ok("[% SWITCH 1 %][% END %]hi" => 'hi');
process_ok("[% SWITCH 1 %][% CASE %]bar[% END %]hi" => 'barhi');
process_ok("[% SWITCH 1 %]Pre[% CASE %]bar[% END %]hi" => 'barhi');
process_ok("[% SWITCH 1 %][% CASE DEFAULT %]bar[% END %]hi" => 'barhi');
process_ok("[% SWITCH 1 %][% CASE 0 %]bar[% END %]hi" => 'hi');
process_ok("[% SWITCH 1 %][% CASE 1 %]bar[% END %]hi" => 'barhi');
process_ok("[% SWITCH 1 %][% CASE foo %][% CASE 1 %]bar[% END %]hi" => 'barhi');
process_ok("[% SWITCH 1 %][% CASE [1..10] %]bar[% END %]hi" => 'barhi');
process_ok("[% SWITCH 11 %][% CASE [1..10] %]bar[% END %]hi" => 'hi');

process_ok("[% SWITCH 1.0 %][% CASE [1..10] %]bar[% END %]hi" => 'barhi');
process_ok("[% SWITCH '1.0' %][% CASE [1..10] %]bar[% END %]hi" => 'barhi') if ! $is_tt;

###----------------------------------------------------------------###
### try/throw/catch/final

process_ok("[% TRY %][% END %]hi" => 'hi');
process_ok("[% TRY %]Foo[% END %]hi" => 'Foohi');
process_ok("[% TRY %]Foo[% THROW foo 'for fun' %]bar[% END %]hi" => '');
process_ok("[% TRY %]Foo[% THROW foo 'for fun' %]bar[% CATCH %][% END %]hi" => 'Foohi') if ! $is_tt;
process_ok("[% TRY %]Foo[% THROW foo 'for fun' %]bar[% CATCH %]there[% END %]hi" => 'Footherehi');
process_ok("[% TRY %]Foo[% THROW foo 'for fun' %]bar[% CATCH foo %]there[% END %]hi" => 'Footherehi');
process_ok("[% TRY %]Foo[% TRY %]Foo[% THROW foo 'for fun' %][% CATCH bar %]one[% END %][% CATCH %]two[% END %]hi" => 'FooFootwohi');
process_ok("[% TRY %]Foo[% TRY %]Foo[% THROW foo 'for fun' %][% CATCH bar %]one[% END %][% CATCH s %]two[% END %]hi" => '');
process_ok("[% TRY %]Foo[% THROW foo.bar 'for fun' %][% CATCH foo %]one[% CATCH foo.bar %]two[% END %]hi" => 'Footwohi');

###----------------------------------------------------------------###
### named args

process_ok("[% foo(bar = 'one', baz = 'two') %]" => "barbazonetwo", {foo=>sub{my $n=$_[-1];join('',keys %$n, values %$n)}});
process_ok("[%bar='ONE'%][% foo(\$bar = 'one') %]" => "ONEone", {foo=>sub{my $n=$_[-1];join('',keys %$n, values %$n)}});

###----------------------------------------------------------------###
### use

process_ok("[% USE son_of_gun_that_does_not_exist %]one" => '');
process_ok("[% USE Foo %]one" => 'one');
process_ok("[% USE Foo2 %]one" => 'one');
process_ok("[% USE Foo(bar = 'baz') %]one[% Foo.bar %]" => 'onebarbaz');
process_ok("[% USE Foo2(bar = 'baz') %]one[% Foo2.bar %]" => 'onebarbaz');
process_ok("[% USE Foo(bar = 'baz') %]one[% Foo.bar %]" => 'onebarbaz');
process_ok("[% USE d = Foo(bar = 'baz') %]one[% d.bar %]" => 'onebarbaz');
process_ok("[% USE d.d = Foo(bar = 'baz') %]one[% d.d.bar %]" => '');

###----------------------------------------------------------------###
### macro

process_ok("[% MACRO foo PROCESS bar %][% BLOCK bar %]Hi[% END %][% foo %]" => 'Hi');
process_ok("[% MACRO foo BLOCK %]Hi[% END %][% foo %]" => 'Hi');
process_ok("[% MACRO foo BLOCK %]Hi[% END %][% foo %]" => 'Hi');
process_ok("[% MACRO foo(n) BLOCK %]Hi[% n %][% END %][% foo(2) %]" => 'Hi2');
process_ok("[%n=1%][% MACRO foo(n) BLOCK %]Hi[% n %][% END %][% foo(2) %][%n%]" => 'Hi21');
process_ok("[%n=1%][% MACRO foo BLOCK %]Hi[% n = 2%][% END %][% foo %][%n%]" => 'Hi1');
process_ok("[% MACRO foo(n) FOREACH i=[1..n] %][% i %][% END %][% foo(3) %]" => '123');

###----------------------------------------------------------------###
### constants

process_ok("[% constants.harry %]" => 'do_this_once');
process_ok("[% constants.harry.length %]" => '12');
process_ok("[% SET constants.foo = 1 %][% constants.foo %]one" => '1one');
process_ok("[% SET constants.harry = 1 %][% constants.harry %]one" => 'do_this_onceone');

###----------------------------------------------------------------###
### interpolate

process_ok("Foo \$one Bar" => 'Foo ONE Bar', {one => 'ONE', tt_config => ['INTERPOLATE' => 1]});

#process_ok(qq{[% FOREACH item IN [ 'foo', 'bar', 'baz' ] -%]
#[%- "<ul>\n" IF loop.first %]
#<li>[% loop.count %]/[% loop.size %]: [% item %]
#[%- "</ul>\n" IF loop.last %]
#[% END %]} => 
