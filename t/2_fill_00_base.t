# -*-perl-*-

use Test::More tests => 3;

use_ok qw(CGI::Ex::Fill);

###----------------------------------------------------------------###

   my $form = {foo => "FOO", bar => "BAR", baz => "BAZ"};

   my $html = '
       <input type=text name=foo>
       <input type=text name=foo>
       <input type=text name=bar value="">
       <input type=text name=baz value="Something else">
       <input type=text name=hem value="Another thing">
       <input type=text name=haw>
   ';

   CGI::Ex::Fill::form_fill(\$html, $form);

   ok(
   $html eq   '
       <input type=text name=foo value="FOO">
       <input type=text name=foo value="FOO">
       <input type=text name=bar value="BAR">
       <input type=text name=baz value="BAZ">
       <input type=text name=hem value="Another thing">
       <input type=text name=haw value="">
   ', "perldoc example 1 passed");

   #print $html;

###----------------------------------------------------------------###

   $form = {foo => ['aaaa', 'bbbb', 'cccc']};

   $html = '
       <input type=text name=foo>
       <input type=text name=foo>
       <input type=text name=foo>
       <input type=text name=foo>
   ';

   form_fill(\$html, $form);

   ok(
   $html eq  '
       <input type=text name=foo value="aaaa">
       <input type=text name=foo value="bbbb">
       <input type=text name=foo value="cccc">
       <input type=text name=foo value="">
   ', "Perldoc example 2 passed");

   #print $html;

###----------------------------------------------------------------###
