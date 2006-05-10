# -*- Mode: Perl; -*-

=head1 NAME

0_ex_00_base.t - Testing of the base CGI::Ex module.

=cut

use strict;
use Test::More tests => 20;

use_ok('CGI::Ex');

my $cgix = CGI::Ex->new;
ok($cgix, "Got object");

### test out form and cookies from the CGI object
SKIP: {
    skip("CGI.pm not found", 9) if ! eval { require CGI };
    local $ENV{'REQUEST_METHOD'} = 'GET';
    local $ENV{'QUERY_STRING'}   = 'foo=bar&foo=baz&us=them';
    local $ENV{'HTTP_COOKIE'}    = 'bar=baz; bing=blam';

    my $form = $cgix->form;
    ok($form, "Got form");
    ok(ref($form) eq 'HASH', "Good form");
    ok($form->{'foo'}, "Found foo");
    ok(ref($form->{'foo'}) eq 'ARRAY', "Foo is array");
    ok(@{ $form->{'foo'} } == 2, "Correct number");
    ok($form->{'us'}, "Found us");
    ok($form->{'us'} eq 'them', "Us is correct");

    my $cookies = $cgix->cookies;
    ok($cookies, "Got cookies");
    ok($cookies->{'bar'} eq 'baz', "Found correct bar");
};

### set a new form
my $form = {foo => 'bar', mult => [qw(a b c)]};
$cgix->form($form);
$cgix->cookies($form);

$form = $cgix->form;
ok($form->{'foo'} eq 'bar', "Could set form");

my $cookies = $cgix->cookies;
ok($cookies->{'foo'} eq 'bar', "Could set form");


### try out make_form
my $str = $cgix->make_form($form);
ok($str =~ /foo=bar/, "Make form works");
ok($str =~ /mult=a&mult=b&mult=c/, "Make form works 2");

$str = $cgix->make_form($form, ['foo']);
ok($str eq 'foo=bar', "Make form works with keys");

### can't test these without being in apache (well we could test STDOUT - but that is for another day - TODO)
foreach my $meth (qw(
                     apache_request
                     content_typed
                     expires
                     is_mod_perl_1
                     is_mod_perl_2
                     last_modified
                     location_bounce
                     mod_perl_version
                     print_content_type
                     print_js
                     send_status
                     send_header
                     set_apache_request
                     set_cookie
                     )) {
    ok($cgix->can($meth), "Has method $meth");
}

### try out time_calc
my $sec;
ok(($sec = CGI::Ex::time_calc('1m'))    == time + 60, "Time_calc ($sec)");
ok(($sec = CGI::Ex::time_calc('-1m'))   == time - 60, "Time_calc ($sec)");
ok(($sec = CGI::Ex::time_calc('1 m'))   == time + 60, "Time_calc ($sec)");
ok(($sec = CGI::Ex::time_calc('1 min')) == time + 60, "Time_calc ($sec)");
ok(($sec = CGI::Ex::time_calc('1'))     == 1, "Time_calc ($sec)");
ok(($sec = CGI::Ex::time_calc('now'))   == time, "Time_calc ($sec)");
ok(($sec = CGI::Ex::time_calc(__FILE__)), "Time_calc ($sec)");

###----------------------------------------------------------------###

my $html = "<input type=text name=foo value=''>";
$form = {foo => 'bar'};
my $out;

ok(($out = $cgix->fill(scalarref => \$html,  form => $form)) =~ /value=([\"\'])bar\1/, "Filled $out");
ok(($out = $cgix->fill(arrayref  => [$html], form => $form)) =~ /value=([\"\'])bar\1/, "Filled $out");

$cgix->fill(text => \$html, form => $form);
ok($html =~ /value=([\"\'])bar\1/, "Filled $html");

$html = "<form name=foo><input type=text name=baz value=''></form><form name=bar><input type=password name=bim value=''></form>";

$form = {baz => 'bing', bim => 'bang'};

$out = $cgix->fill(scalarref => \$html, form => $form, target => 'foo');
ok($out =~ /bing/, "Got bing");
ok($out !~ /bang/, "Didn't get bang");

$out = $cgix->fill(scalarref => \$html, form => $form, target => 'bar');
ok($out =~ /bang/, "Got bang");
ok($out !~ /bing/, "Didn't get bing");

$out = $cgix->fill(scalarref => \$html, form => $form, ignore_fields => ['baz']);
ok($out =~ /bang/, "Got bang");
ok($out !~ /bing/, "Didn't get bing");

$out = $cgix->fill(scalarref => \$html, form => $form, ignore_fields => ['bim']);
ok($out =~ /bing/, "Got bing");
ok($out !~ /bang/, "Didn't get bang");

$out = $cgix->fill(scalarref => \$html, form => $form, fill_password => 1);
ok($out =~ /bing/, "Got bing");
ok($out =~ /bang/, "Got bang");

$out = $cgix->fill(scalarref => \$html, form => $form, fill_password => undef);
ok($out =~ /bing/, "Got bing");
ok($out =~ /bang/, "Got bang");

$out = $cgix->fill(scalarref => \$html, form => $form, fill_password => 0);
ok($out =~ /bing/, "Got bing");
ok($out !~ /bang/, "Didn't get bang");

###----------------------------------------------------------------###

$form = {foo => 'bar'};
my $val = {foo => {'required' => 1}};

my $e = $cgix->validate($form, $val);
ok(! $e, "No error");

$form = {};
$e = $cgix->validate($form, $val);
ok($e, "Got error");
ok("$e" =~ /required/i, "Had error message ($e)");

###----------------------------------------------------------------###

### defer testing to the conf test modules
foreach my $meth (qw(
                     conf_obj
                     conf_read
                     )) {
    ok($cgix->can($meth), "Has method $meth");
}

###----------------------------------------------------------------###

$form = {foo => 'bar'};
my $args = {VARIABLES => {bim => 'bam'}};
my $temp = "([% foo %])([% bim %])";

$out = $cgix->swap_template($temp, $form, $args);
ok($out =~ /bar/, "Got bar");
ok($out =~ /bam/, "Got bam");

$cgix->swap_template(\$temp, $form, $args);
ok($temp =~ /bar/, "Got bar");
ok($temp =~ /bam/, "Got bam");

###----------------------------------------------------------------###
