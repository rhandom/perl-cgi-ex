#!/usr/bin/perl -w

use strict;
use vars qw($PLACEHOLDER);
use Benchmark qw(cmpthese);
use CGI::Ex::Conf;
use POSIX qw(tmpnam);

$PLACEHOLDER = chr(186).'~'.chr(186);

my $n = 3000;

my $cob   = CGI::Ex::Conf->new;
my %files = ();

###----------------------------------------------------------------###

# [pauls@localhost lib]$ perl ../t/samples/bench_conf_readers.pl
# Benchmark: timing 3000 iterations of g_conf, ini, pl, sto, sto2, xml, yaml, yaml2, yaml3...
#  g_conf:  4 wallclock secs ( 3.88 usr +  0.08 sys =  3.96 CPU) @ 757.58/s (n=3000)
#  ini: 10 wallclock secs ( 9.97 usr +  0.10 sys = 10.07 CPU) @ 297.91/s (n=3000)
#  pl:  3 wallclock secs ( 2.68 usr +  0.07 sys =  2.75 CPU) @ 1090.91/s (n=3000)
#  sto:  1 wallclock secs ( 1.31 usr +  0.12 sys =  1.43 CPU) @ 2097.90/s (n=3000)
#  sto2:  1 wallclock secs ( 0.81 usr +  0.03 sys =  0.84 CPU) @ 3571.43/s (n=3000)
#  xml: 36 wallclock secs (34.95 usr +  0.59 sys = 35.54 CPU) @ 84.41/s (n=3000)
#  yaml: 42 wallclock secs (41.95 usr +  0.65 sys = 42.60 CPU) @ 70.42/s (n=3000)
#  yaml2: 42 wallclock secs (41.97 usr +  0.10 sys = 42.07 CPU) @ 71.31/s (n=3000)
#  yaml3:  1 wallclock secs ( 0.52 usr +  0.00 sys =  0.52 CPU) @ 5769.23/s (n=3000)
#          Rate   yaml  yaml2    xml    ini g_conf     pl    sto   sto2  yaml3
# yaml   70.4/s     --    -1%   -17%   -76%   -91%   -94%   -97%   -98%   -99%
# yaml2  71.3/s     1%     --   -16%   -76%   -91%   -93%   -97%   -98%   -99%
# xml    84.4/s    20%    18%     --   -72%   -89%   -92%   -96%   -98%   -99%
# ini     298/s   323%   318%   253%     --   -61%   -73%   -86%   -92%   -95%
# g_conf  758/s   976%   962%   797%   154%     --   -31%   -64%   -79%   -87%
# pl     1091/s  1449%  1430%  1192%   266%    44%     --   -48%   -69%   -81%
# sto    2098/s  2879%  2842%  2385%   604%   177%    92%     --   -41%   -64%
# sto2   3571/s  4971%  4908%  4131%  1099%   371%   227%    70%     --   -38%
# yaml3  5769/s  8092%  7990%  6735%  1837%   662%   429%   175%    62%     --

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

###----------------------------------------------------------------###

#           Rate   yaml  yaml2    xml     pl g_conf    sto  yaml3   sto2
# yaml     418/s     --    -4%   -56%   -91%   -92%   -93%   -97%   -98%
# yaml2    436/s     4%     --   -54%   -91%   -92%   -93%   -96%   -98%
# xml      949/s   127%   118%     --   -80%   -83%   -85%   -92%   -95%
# pl      4762/s  1038%   992%   402%     --   -13%   -25%   -60%   -73%
# g_conf  5455/s  1204%  1151%   475%    15%     --   -15%   -55%   -69%
# sto     6383/s  1426%  1364%   572%    34%    17%     --   -47%   -64%
# yaml3  12000/s  2768%  2652%  1164%   152%   120%    88%     --   -32%
# sto2   17647/s  4118%  3947%  1759%   271%   224%   176%    47%     --

$str = '{
  foo     => "bar",
  pass    => "word",
  garbage => "can",
  mighty  => "ducks",
  quack   => "moo",
  one1    => "val1",
  one2    => "val2",
  one3    => "val3",
  one4    => "val4",
  one5    => "val5",
  one6    => "val6",
  one7    => "val7",
  one8    => "val8",
}';

###----------------------------------------------------------------###

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

if (eval {require YAML}) {
  my $_file = tmpnam(). '.yaml';
  &YAML::DumpFile($_file, $conf);
  $cob->preload_files($_file);
  $TESTS{yaml3} = sub {
    my $hash = $cob->read($_file);
  };
  $files{yaml3} = $_file;
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

