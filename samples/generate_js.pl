#!/usr/bin/perl -w

use strict;
use CGI::Ex::Validate;

### sampe
my $js_path   = "/cgi-bin/js";
my $form_name = "my_form";
my $val_hash  = {
  'general as_hash_join' => "<br>\n<br>",
  'general group_order'  => [qw(username password)],
  username => {
    required => 1,
    match    => 'm/^\w+$/',
    max_len  => 20,
  },
  password => {
    match => ['m/\d/', 'm/[a-z]/'],
    match_error => "\$name Must contain a letter and a number",
  },
};


### generate the js
my $val_obj = CGI::Ex::Validate->new;
my $val = $val_obj->generate_js($val_hash, $form_name, $js_path);


### sample document out put
### not that you should ever inline your html
$val_obj->cgix->content_type;
print "<html>
<body>
<form name='my_form'>

Username: <input type=text size=20 name=username><br>
<span class=error id=username_error></span><br>
Password: <input type=text size=20 name=password><br>
<span class=error id=password_error></span><br>
<input type=submit>

</form>

$val

</body>
</html>
";
