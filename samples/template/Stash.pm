package Stash;

use strict;


sub new {
  my $class = shift || __PACKAGE__;
  my $self  = ref($_[0]) ? shift : {@_};
  return bless $self, $class;
}



1;
