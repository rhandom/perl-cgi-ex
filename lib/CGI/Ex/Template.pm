package CGI::Ex::Template;

=head1 NAME

CGI::Ex::Template - Template::Alloy based TT2/TT3/HT/HTE/Tmpl/Velocity engine.

=cut

use strict;
use warnings;
use Template::Alloy 1.020;
use base qw(Template::Alloy);
use vars qw($QR_PRIVATE
            $WHILE_MAX
            $MAX_EVAL_RECURSE
            $MAX_MACRO_RECURSE
            $STAT_TTL
            $EXTRA_COMPILE_EXT
            $PERL_COMPILE_EXT
            $SCALAR_OPS
            $FILTER_OPS
            $LIST_OPS
            $HASH_OPS
            $VOBJS
            );

### install true symbol table aliases that can be localized
*QR_PRIVATE        = *Template::Alloy::QR_PRIVATE;
*WHILE_MAX         = *Template::Alloy::WHILE_MAX;
*MAX_EVAL_RECURSE  = *Template::Alloy::MAX_EVAL_RECURSE;
*MAX_MACRO_RECURSE = *Template::Alloy::MAX_MACRO_RECURSE;
*STAT_TTL          = *Template::Alloy::STAT_TTL;
*EXTRA_COMPILE_EXT = *Template::Alloy::EXTRA_COMPILE_EXT;
*PERL_COMPILE_EXT  = *Template::Alloy::PERL_COMPILE_EXT;
*SCALAR_OPS        = *Template::Alloy::SCALAR_OPS;
*FILTER_OPS        = *Template::Alloy::FILTER_OPS;
*LIST_OPS          = *Template::Alloy::LIST_OPS;
*HASH_OPS          = *Template::Alloy::HASH_OPS;
*VOBJS             = *Template::Alloy::VOBJS;

1;

__END__

=head1 DESCRIPTION

Deprecated - use Template::Alloy directly.

CGI::Ex::Template is the original base for the code that is now
Template::Alloy.  Template::Alloy employed enough complexity and
featureset to warrant moving it out to a separate namespace.

You can use CGI::Ex::Template as a standalone module - but it is
suggested that you use Template::Alloy directly instead.

See the L<Template::Alloy> documentation.

=head1 LICENSE

This module is distributed under the Artistic License.

=head1 AUTHOR

Paul Seamons <paul@seamons.com>

=cut
