#!/usr/bin/perl -w

# [pauls@localhost lib]$ perl ../t/samples/bench_conf_readers.pl
# g_conf => /tmp/filehQovio.g_conf
# ini => /tmp/fileRGPhw9.ini
# pl => /tmp/filelumCbQ.pl
# sto => /tmp/file1vHisW.sto
# sto2 => /tmp/filexesmCu.sto2
# xml => /tmp/fileX9hbUH.xml
# yaml => /tmp/fileNID3S2.yaml
# yaml2 => /tmp/filejt9uaB.yaml2
# Benchmark: timing 3000 iterations of g_conf, ini, pl, sto, sto2, xml, yaml, yaml2...
#  g_conf:  3 wallclock secs ( 2.88 usr +  0.07 sys =  2.95 CPU) @ 1016.95/s (n=3000)
#  ini:  7 wallclock secs ( 7.64 usr +  0.06 sys =  7.70 CPU) @ 389.61/s (n=3000)
#  pl:  2 wallclock secs ( 2.00 usr +  0.08 sys =  2.08 CPU) @ 1442.31/s (n=3000)
#  sto:  2 wallclock secs ( 1.07 usr +  0.02 sys =  1.09 CPU) @ 2752.29/s (n=3000)
#  sto2:  0 wallclock secs ( 0.64 usr +  0.01 sys =  0.65 CPU) @ 4615.38/s (n=3000)
#  xml: 28 wallclock secs (27.42 usr +  0.60 sys = 28.02 CPU) @ 107.07/s (n=3000)
#  yaml: 34 wallclock secs (33.22 usr +  0.20 sys = 33.42 CPU) @ 89.77/s (n=3000)
#  yaml2: 34 wallclock secs (32.80 usr +  0.17 sys = 32.97 CPU) @ 90.99/s (n=3000)
#          Rate   yaml  yaml2    xml    ini g_conf     pl    sto   sto2
# yaml   89.8/s     --    -1%   -16%   -77%   -91%   -94%   -97%   -98%
# yaml2  91.0/s     1%     --   -15%   -77%   -91%   -94%   -97%   -98%
# xml     107/s    19%    18%     --   -73%   -89%   -93%   -96%   -98%
# ini     390/s   334%   328%   264%     --   -62%   -73%   -86%   -92%
# g_conf 1017/s  1033%  1018%   850%   161%     --   -29%   -63%   -78%
# pl     1442/s  1507%  1485%  1247%   270%    42%     --   -48%   -69%
# sto    2752/s  2966%  2925%  2471%   606%   171%    91%     --   -40%
# sto2   4615/s  5042%  4972%  4211%  1085%   354%   220%    68%     --
# [pauls@localhost lib]$ fg

use strict;
use vars qw($PLACEHOLDER);
use Benchmark qw(cmpthese);
use CGI::Ex::Conf;
use POSIX qw(tmpnam);

$PLACEHOLDER = chr(186).'~'.chr(186);

my $n = 3000;

my $cob   = CGI::Ex::Conf->new;
my %files = ();

my $str = '{
  foo     => {key1 => "bar",   key2 => "ralph"},
  pass    => {key1 => "word",  key2 => "ralph"},
  garbage => {key1 => "can",   key2 => "ralph"},
  mighty  => {key1 => "ducks", key2 => "ralph"},
  quack   => {key1 => "moo",   key2 => "ralph"},
  one1    => {key1 => "val1",  key2 => "ralph"},
  one2    => {key1 => "val2",  key2 => "ralph"},
  one3    => {key1 => "val3",  key2 => "ralph"},
  one4    => {key1 => "val4",  key2 => "ralph"},
  one5    => {key1 => "val5",  key2 => "ralph"},
  one6    => {key1 => "val6",  key2 => "ralph"},
  one7    => {key1 => "val7",  key2 => "ralph"},
  one8    => {key1 => "val8",  key2 => "ralph"},
}';

my $conf = eval $str;

my %TESTS = ();

### do perl
my $file = tmpnam(). '.pl';
open OUT, ">$file";
print OUT $str;
close OUT;
$TESTS{pl} = sub {
  my $hash = $cob->read($file);
};
$files{pl} = $file;

### do a generic conf_write
my $file2 = tmpnam(). '.g_conf';
&generic_conf_write($file2, $conf);
$TESTS{g_conf} = sub {
  my $hash = &generic_conf_read($file2);
};
$files{g_conf} = $file2;


### load in the rest of the tests that we support
if (eval {require Storable}) {
  my $_file = tmpnam(). '.sto';
  &Storable::store($conf, $_file);
  $TESTS{sto} = sub {
    my $hash = $cob->read($_file);
  };
  $files{sto} = $_file;
}

if (eval {require Storable}) {
  my $_file = tmpnam(). '.sto2';
  &Storable::store($conf, $_file);
  $TESTS{sto2} = sub {
    my $hash = &Storable::retrieve($_file);
  };
  $files{sto2} = $_file;
}

if (eval {require YAML}) {
  my $_file = tmpnam(). '.yaml';
  &YAML::DumpFile($_file, $conf);
  $TESTS{yaml} = sub {
    my $hash = $cob->read($_file);
  };
  $files{yaml} = $_file;
}

if (eval {require YAML}) {
  my $_file = tmpnam(). '.yaml2';
  &YAML::DumpFile($_file, $conf);
  $TESTS{yaml2} = sub {
    my $hash = &YAML::LoadFile($_file);
  };
  $files{yaml2} = $_file;
}

if (eval {require Config::IniHash}) {
  my $_file = tmpnam(). '.ini';
  &Config::IniHash::WriteINI($_file, $conf);
  $TESTS{ini} = sub {
    local $^W = 0;
    my $hash = $cob->read($_file);
  };
  $files{ini} = $_file;
}

if (eval {require XML::Simple}) {
  my $_file = tmpnam(). '.xml';
  my $xml = XML::Simple->new->XMLout($conf);
  open  OUT, ">$_file" || die $!;
  print OUT $xml;
  close OUT;
  $TESTS{xml} = sub {
    my $hash = $cob->read($_file);
  };
  $files{xml} = $_file;
}

### tell file locations
foreach my $key (sort keys %files) {
  print "$key => $files{$key}\n";
}

cmpthese($n, \%TESTS);

### comment out this line to inspect files
unlink $_ foreach values %files;

###----------------------------------------------------------------###

sub generic_conf_read {
  my $_file = shift || die "No filename supplied";
  my $sep_by_newlines = ($_[0] && lc($_[0]) eq 'sep_by_newlines') ? 1 : 0;

  ### fh will now lose scope and close itself if necessary
  my $FH = do { local *FH; *FH };
  open ($FH, $_file) || return {};

  my $x = 0;
  my $conf = {};
  my $key  = '';
  my $val;
  my $line;
  my ($is_array,$is_hash,$is_multiline);
  my $order;
  $order = [] if wantarray;

  while( defined($line = <$FH>) ){
    last if ! defined $line;
    last if $x++ > 10000;

    next if index($line,'#') == 0;

    if ($line =~ /^\s/ && ($is_multiline || $line ne "\n")){
      next if ! length($key);
      $conf->{$key} .= $line;
      $is_multiline = 1;

    }else{
      ### duplicate trim section
      if( length($key) ){
        $conf->{$key} =~ s/\s+$//;
        if( $is_array || $is_hash ){
          $conf->{$key} =~ s/^\s+//;
          my $urldec = (index($conf->{$key},'%')>-1 || index($conf->{$key},'+')>-1);
          my @pieces;
          if ($sep_by_newlines) {
            @pieces = split(/\s*\n\s*/,$conf->{$key});
            @pieces = map {split(/\s+/,$_,2)} @pieces if $is_hash;
          } else {
            @pieces = split(/\s+/,$conf->{$key});
          }
          if( $urldec ){
            foreach my $_val (@pieces){
              $_val =~ y/+/ / if ! $sep_by_newlines;
              $_val =~ s/%([a-f0-9]{2})/chr(hex($1))/egi;
            }
          }
          if( $is_array ){
            foreach (@pieces){ $_="" if index($_,$PLACEHOLDER)>-1 }
            $conf->{$key} = \@pieces;
          }elsif( $is_hash ){
            foreach (@pieces){ $_="" if index($_,$PLACEHOLDER)>-1 }
            shift(@pieces) if scalar(@pieces) % 2;
            $conf->{$key} = {@pieces};
          }
        }elsif( ! $is_multiline ){
          $conf->{$key} =~ y/+/ / if ! $sep_by_newlines;
          $conf->{$key} =~ s/%([a-f0-9]{2})/chr(hex($1))/egi;
        }
      }

      ($key,$val) = split(/\s+/,$line,2);
      $is_array = 0;
      $is_hash = 0;
      $is_multiline = 0;
      if (! length($key)) {
        next;
      } elsif (index($key,'array:') == 0) {
        $is_array = $key =~ s/^array://i;
      } elsif (index($key,'hash:') == 0) {
        $is_hash = $key =~ s/^hash://i;
      }
      $key =~ y/+/ / if ! $sep_by_newlines;
      $key =~ s/%([a-f0-9]{2})/chr(hex($1))/egi;
      $conf->{$key} = $val;
      push @$order, $key if $order;
    }
  }

  ### duplicate trim section
  if( length($key) && defined($conf->{$key}) ){
    $conf->{$key} =~ s/\s+$//;
    if( $is_array || $is_hash ){
      $conf->{$key} =~ s/^\s+//;
      my $urldec = (index($conf->{$key},'%')>-1 || index($conf->{$key},'+')>-1);
      my @pieces;
      if ($sep_by_newlines) {
        @pieces = split(/\s*\n\s*/,$conf->{$key});
        @pieces = map {split(/\s+/,$_,2)} @pieces if $is_hash;
      } else {
        @pieces = split(/\s+/,$conf->{$key});
      }
      if( $urldec ){
        foreach my $_val (@pieces){
          $_val =~ y/+/ / if ! $sep_by_newlines;
          $_val =~ s/%([a-f0-9]{2})/chr(hex($1))/egi;
        }
      }
      if( $is_array ){
        foreach (@pieces){ $_="" if index($_,$PLACEHOLDER)>-1 }
        $conf->{$key} = \@pieces;
      }elsif( $is_hash ){
        foreach (@pieces){ $_="" if index($_,$PLACEHOLDER)>-1 }
        shift(@pieces) if scalar(@pieces) % 2;
        $conf->{$key} = {@pieces};
      }
    }elsif( ! $is_multiline ){
      $conf->{$key} =~ y/+/ / if ! $sep_by_newlines;
      $conf->{$key} =~ s/%([a-f0-9]{2})/chr(hex($1))/egi;
    }
  }


  close($FH);
  return $order ? ($conf,$order) : $conf;
}


sub generic_conf_write{
  my $_file = shift || die "No filename supplied";

  if (! @_) {
    return;
  }

  my $new_conf = shift || die "Missing update hashref";
  return if ! keys %$new_conf;


  ### do we allow writing out hashes in a nice way
  my $sep_by_newlines = ($_[0] && lc($_[0]) eq 'sep_by_newlines') ? 1 : 0;

  ### touch the file if necessary
  if( ! -e $_file ){
    open(TOUCH,">$_file") || die "Conf file \"$_file\" could not be opened for writing: $!";
    close(TOUCH);
  }

  ### read old values
  my $conf = &generic_conf_read($_file) || {};
  my $key;
  my $val;

  ### remove duplicates and undefs
  while (($key,$val) = each %$new_conf){
    $conf->{$key} = $new_conf->{$key};
  }

  ### prepare output
  my $output = '';
  my $qr = qr/([^\ \!\"\$\&-\*\,-\~])/;
  foreach $key (sort keys %$conf){
    next if ! defined $conf->{$key};
    $val = delete $conf->{$key};
    $key =~ s/([^\ \!\"\$\&-\*\,-9\;-\~\/])/sprintf("%%%02X",ord($1))/eg;
    $key =~ tr/\ /+/;
    my $ref = ref($val);
    if( $ref ){
      if( $ref eq 'HASH' ){
        $output .= "hash:$key\n";
        foreach my $_key (sort keys %$val){
          my $_val = $val->{$_key};
          next if ! defined $_val;
          $_val =~ s/$qr/sprintf("%%%02X",ord($1))/ego;
          $_key =~ s/$qr/sprintf("%%%02X",ord($1))/ego;
          if ($sep_by_newlines) {
            $_val =~ s/^(\s)/sprintf("%%%02X",ord($1))/ego;
            $_val =~ s/(\s)$/sprintf("%%%02X",ord($1))/ego;
            $_key =~ s/\ /%20/g;
          } else {
            $_val =~ tr/\ /+/;
            $_key =~ tr/\ /+/;
          }
          $_val = $PLACEHOLDER if ! length($_val);
          $output .= "\t$_key\t$_val\n";
        }
      }elsif( $ref eq 'ARRAY' ){
        $output .= "array:$key\n";
        foreach (@$val){
          my $_val = $_;
          $_val =~ s/$qr/sprintf("%%%02X",ord($1))/ego;
          if ($sep_by_newlines) {
            $_val =~ s/^(\s)/sprintf("%%%02X",ord($1))/ego;
            $_val =~ s/(\s)$/sprintf("%%%02X",ord($1))/ego;
          } else {
            $_val =~ tr/\ /+/;
          }
          $_val = $PLACEHOLDER if ! length($_val);
          $output .= "\t$_val\n";
        }
      }else{
        $output .= "$key\tbless('$val','$ref')\n"; # stringify the ref
      }
    }else{
      if( $val =~ /\n/ ){ # multiline values that are indented properly don't need encoding
        if( $val =~ /^\s/ || $val =~ /\s$/ || $val =~ /\n\n/ || $val =~ /\n([^\ \t])/ ){
          if ($sep_by_newlines) {
            $val =~ s/([^\!\"\$\&-\~])/sprintf("%%%02X",ord($1))/eg;
          } else {
            $val =~ s/([^\ \!\"\$\&-\*\,-\~])/sprintf("%%%02X",ord($1))/eg;
            $val =~ y/ /+/;
          }
        }
      }else{
        $val =~ s/([^\ \t\!\"\$\&-\*\,-\~])/sprintf("%%%02X",ord($1))/eg;
        $val =~ s/^(\s)/sprintf("%%%02X",ord($1))/eg;
        $val =~ s/(\s)$/sprintf("%%%02X",ord($1))/eg;
      }
      $output .= "$key\t$val\n";
    }
  }

  open (CONF,"+<$_file") || die "Could not open the file for writing ($_file) -- [$!]";
  print CONF $output;
  truncate CONF, length($output);
  close CONF;

  return 1;
}

1;

