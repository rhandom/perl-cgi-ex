package CGI::Ex::Dump;

### CGI Extended Data::Dumper Extension

###----------------------------------------------------------------###
#  Copyright 2004 - Paul Seamons                                     #
#  Distributed under the Perl Artistic License without warranty      #
###----------------------------------------------------------------###

### See perldoc at bottom

use vars qw(@ISA @EXPORT @EXPORT_OK $ON $SUB $QR1 $QR2 $full_filename);
use strict;
use Exporter;

@ISA       = qw(Exporter);
@EXPORT    = qw(dex dex_warn dex_text ctrace dex_trace);
@EXPORT_OK = qw(dex dex_warn dex_text ctrace dex_trace what_is_this);

### is on or off
sub on  { $ON = 1 };
sub off { $ON = 0; }
&on();

sub set_deparse {
  $Data::Dumper::Deparse = eval {require B::Deparse};
}

###----------------------------------------------------------------###

BEGIN {
  ### setup the Data::Dumper usage
  {
    package Data::Dumper;
    no strict;
    $Sortkeys  = 1    if ! defined $Sortkeys; # not available 5.6 or earlier
    $Useqq     = 1    if ! defined $Useqq;
    $Quotekeys = 0    if ! defined $Quotekeys;
    $Pad       = '  ' if ! defined $Pad;
    #$Deparse   = 1    if ! defined $Deparse; # very useful
  }
  $SUB = sub {
    require Data::Dumper;
    return Data::Dumper->Dumpperl(\@_);
  };
  
  ### how to display or parse the filename
  $QR1 = qr{\A(?:/[^/]+){2,}/(?:perl|lib)/(.+)\Z};
  $QR2 = qr{\A.+?(\w+/[\w\.]+)\Z};
}

###----------------------------------------------------------------###


### same as dumper but with more descriptive output and auto-formatting
### for cgi output
sub what_is_this {
  return if ! $ON;
  my ($pkg, $file, $line_n, $called) = caller(0);
  ($pkg, $file, $line_n, $called) = caller(1) if $pkg eq __PACKAGE__;
  substr($called, 0, length(__PACKAGE__) + 2, '');

  ### get the actual line
  my $line = '';
  if (open(IN,$file)) {
    $line = <IN> for 1 .. $line_n;
    close IN;
  }

  ### get rid of extended filename
  if (! $full_filename) {
    $file =~ s/$QR1/$1/ || $file =~ s/$QR2/$1/;
  }

  ### dump it out
  my @dump = map {&$SUB($_)} @_;
  my @var  = ('$VAR') x ($#dump + 1);
  if ($line =~ s/^ .*\b \Q$called\E ( \(?\s* | \s+ )//x
      && $line =~ s/(?:\s+if\s+.+)? ;? \s*$//x) {
    $line =~ s/ \s*\) $ //x if $1 && $1 =~ /\(/;
    my @_var = map {/^[\"\']/ ? 'String' : $_} split (/\s*,\s*/, $line);
    @var = @_var if $#var == $#_var;
  }

  ### spit it out
  if ($called eq 'dex_text'
      || $called eq 'dex_warn'
      || ! $ENV{REQUEST_METHOD}) {
    my $txt = "$called: $file line $line_n\n";
    for (0 .. $#dump) {
      $dump[$_] =~ s|\$VAR1|$var[$_]|g;
      $txt .= $dump[$_];
    }
    if    ($called eq 'dex_text') { return $txt }
    elsif ($called eq 'dex_warn') { warn  $txt  }
    else                          { print $txt  }
  } else {
    require CGI::Ex;
    &CGI::Ex::content_type();
    print "<pre><b>$called: $file line $line_n</b>\n";
    for (0 .. $#dump) {
      $dump[$_] =~ s/\\n/\n/g;
      $dump[$_] = _html_quote($dump[$_]);
      $dump[$_] =~ s|\$VAR1|<b>$var[$_]</b>|g;
      print $dump[$_];
    }
    print "</pre>\n";
  }
}

### some aliases
sub dump     { &what_is_this }
sub dex      { &what_is_this }
sub dex_warn { &what_is_this }
sub dex_text { &what_is_this }

sub _html_quote {
  my $value = shift;
  return '' if ! defined $value;
  $value =~ s/&/&amp;/g;
  $value =~ s/</&lt;/g;
  $value =~ s/>/&gt;/g;
#  $value =~ s/\"/&quot;/g;
  return $value;
}

### ctrace is intended for work with perl 5.8 or higher's Carp
sub ctrace {
  require 5.8.0;
  require Carp::Heavy;
  local $Carp::MaxArgNums = 3;
  local $Carp::MaxArgLen  = 20;
  my $i = shift || 0;
  my @i = ();
  my $max1 = 0;
  my $max2 = 0;
  my $max3 = 0;
  while (my %i = &Carp::caller_info(++$i)) {
    $i{sub_name} =~ s/\((.*)\)$//;
    $i{args} = $i{has_args} ? $1 : "";
    $i{sub_name} =~ s/^.*?([^:]+)$/$1/;
    $i{file} =~ s/$QR1/$1/ || $i{file} =~ s/$QR2/$1/;
    $max1 = length($i{sub_name}) if length($i{sub_name}) > $max1;
    $max2 = length($i{file})     if length($i{file})     > $max2;
    $max3 = length($i{line})     if length($i{line})     > $max3;
    push @i, \%i;
  }
  foreach my $ref (@i) {
    $ref = sprintf("%-${max1}s at %-${max2}s line %${max3}s", $ref->{sub_name}, $ref->{file}, $ref->{line})
      . ($ref->{args} ? " ($ref->{args})" : "");
  }
  return \@i;
}

sub dex_trace {
  &what_is_this(ctrace(1));
}

###----------------------------------------------------------------###

1;

__END__

=head1 NAME

CGI::Ex::Dump - A debug utility

=head1 SYNOPSIS

  use CGI::Ex::Dump; # auto imports dex, dex_warn, dex_text and others

  my $hash = {
    foo => ['a', 'b', 'Foo','a', 'b', 'Foo','a', 'b', 'Foo','a'],
  };

  dex $hash; # or dex_warn $hash;

  dex;

  dex "hi";

  dex $hash, "hi", $hash;

  dex \@INC; # print to STDOUT, or format for web if $ENV{REQUEST_METHOD}

  dex_warn \@INC;  # same as dex but to STDOUT

  print FOO dex_text \@INC; # same as dex but return dump

=head1 DESCRIPTION

Uses the base Data::Dumper of the distribution and gives it nicer formatting - and
allows for calling just about anytime during execution.

Calling &CGI::Ex::set_deparse() will allow for dumped output of subroutines
if available.

perl -e 'use CGI::Ex::Dump;  dex "foo";'

See also L<Data::Dumper>.

Setting any of the Data::Dumper globals will alter the output.

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut
