package CGI::Ex::Die;

use strict;
use vars qw($no_recurse $EXTENDED_ERRORS $SHOW_TRACE $IGNORE_EVAL);

use CGI::Ex;
use CGI::Ex::Dump qw(debug ctrace);

BEGIN {
  $SHOW_TRACE = 0      if ! defined $SHOW_TRACE;
  $IGNORE_EVAL = 0     if ! defined $IGNORE_EVAL;
  $EXTENDED_ERRORS = 1 if ! defined $EXTENDED_ERRORS;
}

###----------------------------------------------------------------###

sub import {
  my $class = shift;
  if ($#_ != -1) {
    if (($#_ + 1) % 2) {
      require Carp;
      &Carp::croak("Usage: use ".__PACKAGE__." register => 1");
    }
    my %args = @_;
    ### may be called as
    #   use CGI::Ex::Die register => 1;
    #   OR
    #   use CGI::Ex::Die register => [qw(die)];
    if (! ref($args{register}) || grep {/die/} @{ $args{register} }) {
      $SIG{__DIE__} = \&die_handler;
    }
    $SHOW_TRACE      = $args{'show_trace'}      if exists $args{'show_trace'};
    $IGNORE_EVAL     = $args{'ignore_eval'}     if exists $args{'ignore_eval'};
    $EXTENDED_ERRORS = $args{'extended_errors'} if exists $args{'extended_errors'};
  }
  return 1;
}

###----------------------------------------------------------------###

sub die_handler {
  my $err   = shift;

  die $err if $no_recurse;
  local $no_recurse = 1;

  ### test for eval - if eval - propogate it up
  if (! $IGNORE_EVAL) {
    if (! $ENV{MOD_PERL}) {
      my $n = 0;
      while (my $sub = (caller(++$n))[3]) {
        next if $sub !~ /eval/;
        die $err; # die and let the eval catch it
      }

      ### test for eval in a mod_perl environment
    } else {
      my $n     = 0;
      my $found = 0;
      while (my $sub = (caller(++$n))[3]) {
        $found = $n if ! $found && $sub =~ /eval/;
        last if $sub =~ /^(Apache|ModPerl)::(PerlRun|Registry)/;
      }
      if ($found && $n - 1 != $found) {
        die $err;
      }
    }
  }

  ### decode the message
  if (ref $err) {

  } elsif ($EXTENDED_ERRORS && $err) {
    my $copy = "$err";
    if ($copy =~ m|^Execution of ([/\w\.\-]+) aborted due to compilation errors|si) {
      eval {
        local $SIG{__WARN__} = sub {};
        require $1;
      };
      my $error = $@ || '';
      $error =~ s|Compilation failed in require at [/\w/\.\-]+/Die.pm line \d+\.\s*$||is;
      chomp $error;
      $err .= "\n($error)\n";
    } elsif ($copy =~ m|^syntax error at ([/\w.\-]+) line \d+, near|mi) {
    }
  }
  my $msg = &CGI::Ex::Dump::_html_quote("$err");
  $msg = "<pre style='background:red;color:white;border:2px solid black;font-size:120%;padding:3px'>Error: $msg</pre>\n";

  if ($ENV{REQUEST_METHOD}) {
    ### similar to CGI::Carp
    if (my $r = CGI::Ex->apache_request) {
      if ($r->bytes_sent) {
        $r->print($msg);
        $r->exit;
      } else {
        $r->status(500);
        $r->custom_response(500,$msg);
      }
    } else {
      CGI::Ex->print_content_type;
      print $msg;
    }
    ### show the trace
    debug ctrace
      if $SHOW_TRACE;
  } else {
    ### command line execution
  }

  die $err;
}

1;

__END__

=head1 NAME

CGI::Ex::Die - A CGI::Carp::FatalsToBrowser type utility.

=head1 SYNOPSIS

  use CGI::Ex::Die;
  $SIG{__DIE__} = \&CGI::Ex::Die::die_handler;

  # OR #

  use CGI::Ex::Die register => 1;

=head1 DESCRIPTION

This module is intended for showing more useful messages to
the developer, should errors occur.  This is a stub phase module.
More features (error notification, custom error page, etc) will
be added later.

=head1 AUTHORS

Paul Seamons <perlspam at seamons dot com>

=cut
