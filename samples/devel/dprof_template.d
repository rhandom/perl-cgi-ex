# -*-perl-*-
# run with perl -d:DProf $0 ; dprofpp

use strict;
use POSIX qw(tmpnam);
use File::Path qw(rmtree);
use CGI::Ex::Template;

my $tt_cache_dir = tmpnam;
END { rmtree $tt_cache_dir };
mkdir $tt_cache_dir, 0755;

my $cet = CGI::Ex::Template->new(ABSOLUTE => 1);
#use Template;
#my $cet = Template->new(ABSOLUTE => 1);

###----------------------------------------------------------------###

my $swap = {
    one   => "ONE",
    two   => "TWO",
    three => "THREE",
    a_var => "a",
    hash  => {a => 1, b => 2, c => { d => ["hmm"] }},
    array => [qw(A B C D E a A)],
    code  => sub {"($_[0])"},
    cet   => $cet,
};

#my $txt  = "[% one %]\n";
my $txt  = ((" "x1000)."[% one %]\n")x100;

my $file = \$txt;

if (0) {
    $file = $tt_cache_dir .'/template.txt';
    open(my $fh, ">$file") || die "Couldn't open $file: $!";
    print $fh $txt;
    close $fh;
}

###----------------------------------------------------------------###

sub cet {
    my $out = '';
    $cet->process($file, $swap, \$out);
    return $out;
}

cet() for 1 .. 500;
