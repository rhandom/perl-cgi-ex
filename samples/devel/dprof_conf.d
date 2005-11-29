# -*-perl-*-
# run with perl -D:DProf $0

use CGI::Ex::Conf qw(conf_read conf_write);
use POSIX qw(tmpnam);
use Data::Dumper qw(Dumper);

#my $cob = CGI::Ex::Conf->new;
my $tmp = tmpnam .".sto";
END { unlink $tmp };

my $conf = {
    one   => 1,
    two   => 2,
    three => 3,
    four  => 4,
    five  => 5,
    six   => 6,
    seven => 7,
    eight => 8,
    nine  => 9,
    ten   => 10,
};

#$cob->write($tmp, $conf);
conf_write($tmp, $conf);
#print `cat $tmp`; exit;

for (1 .. 100_000) {
#    my $ref = $cob->read($tmp);
    my $ref = conf_read($tmp);
#    print Dumper $ref; exit;
}

