# -*- Mode: Perl; -*-

use Test::More tests => 20;
use Data::Dumper qw(Dumper);

my $module = 'CGI::Ex::Template';
#$module = 'Template';

use_ok($module);

my $obj = $module->new(ABSOLUTE => 1);

sub process_ok { # process the value
    my $str  = shift;
    my $test = shift;
    my $args = shift;
    my $out  = '';
    $obj->process(\$str, $args, \$out);
    ok($out eq $test, "\"$str\" => \"$out\"");
}

process_ok("[% foo %]" => "");
process_ok("[% foo %]" => "1",     {foo => 1});
process_ok("[% foo.bar %]" => "");
process_ok("[% foo.bar %]" => "",  {foo => {}});
process_ok("[% foo.bar %]" => "1", {foo => {bar => 1}});
process_ok("[% foo.0 %]" => "1",   {foo => [1, 2, 3]});
process_ok("[% foo %]" => 1,       {foo => sub { 1 }});
process_ok("[% foo(1) %]" => 1,    {foo => sub { $_[0] }});

process_ok("[% GET foo %]" => "");
process_ok("[% GET foo %]" => "1",     {foo => 1});
process_ok("[% GET foo.bar %]" => "");
process_ok("[% GET foo.bar %]" => "",  {foo => {}});
process_ok("[% GET foo.bar %]" => "1", {foo => {bar => 1}});
process_ok("[% GET foo.0 %]" => "1",   {foo => [1, 2, 3]});
process_ok("[% GET foo %]" => 1,       {foo => sub { 1 }});
process_ok("[% GET foo(1) %]" => 1,    {foo => sub { $_[0] }});

process_ok("[% \$name %]" => "",      {name => 'foo'});
process_ok("[% \$name %]" => "1",     {name => 'foo', foo => 1});
process_ok("[% \$name.bar %]" => "",  {name => 'foo'});
process_ok("[% \$name.bar %]" => "",  {name => 'foo', foo => {}});
process_ok("[% \$name.bar %]" => "1", {name => 'foo', foo => {bar => 1}});
process_ok("[% \$name.0 %]" => "1",   {name => 'foo', foo => [1, 2, 3]});
process_ok("[% \$name %]" => 1,       {name => 'foo', foo => sub { 1 }});
process_ok("[% \$name(1) %]" => 1,    {name => 'foo', foo => sub { $_[0] }});

process_ok("[% GET \$name %]" => "",      {name => 'foo'});
process_ok("[% GET \$name %]" => "1",     {name => 'foo', foo => 1});
process_ok("[% GET \$name.bar %]" => "",  {name => 'foo'});
process_ok("[% GET \$name.bar %]" => "",  {name => 'foo', foo => {}});
process_ok("[% GET \$name.bar %]" => "1", {name => 'foo', foo => {bar => 1}});
process_ok("[% GET \$name.0 %]" => "1",   {name => 'foo', foo => [1, 2, 3]});
process_ok("[% GET \$name %]" => 1,       {name => 'foo', foo => sub { 1 }});
process_ok("[% GET \$name(1) %]" => 1,    {name => 'foo', foo => sub { $_[0] }});

process_ok("[% \$name %]" => "",     {name => 'foo foo', foo => 1});
process_ok("[% GET \$name %]" => "", {name => 'foo foo', foo => 1});

process_ok("[% \${name} %]" => "",      {name => 'foo'});
process_ok("[% \${name} %]" => "1",     {name => 'foo', foo => 1});
process_ok("[% \${name}.bar %]" => "",  {name => 'foo'});
process_ok("[% \${name}.bar %]" => "",  {name => 'foo', foo => {}});
process_ok("[% \${name}.bar %]" => "1", {name => 'foo', foo => {bar => 1}});
process_ok("[% \${name}.0 %]" => "1",   {name => 'foo', foo => [1, 2, 3]});
process_ok("[% \${name} %]" => 1,       {name => 'foo', foo => sub { 1 }});
process_ok("[% \${name}(1) %]" => 1,    {name => 'foo', foo => sub { $_[0] }});

process_ok("[% GET \${name} %]" => "",      {name => 'foo'});
process_ok("[% GET \${name} %]" => "1",     {name => 'foo', foo => 1});
process_ok("[% GET \${name}.bar %]" => "",  {name => 'foo'});
process_ok("[% GET \${name}.bar %]" => "",  {name => 'foo', foo => {}});
process_ok("[% GET \${name}.bar %]" => "1", {name => 'foo', foo => {bar => 1}});
process_ok("[% GET \${name}.0 %]" => "1",   {name => 'foo', foo => [1, 2, 3]});
process_ok("[% GET \${name} %]" => 1,       {name => 'foo', foo => sub { 1 }});
process_ok("[% GET \${name}(1) %]" => 1,    {name => 'foo', foo => sub { $_[0] }});

process_ok("[% \${name} %]" => "",     {name => 'foo foo', foo => 1});
process_ok("[% GET \${name} %]" => "", {name => 'foo foo', foo => 1});
