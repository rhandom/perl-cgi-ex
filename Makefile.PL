use ExtUtils::MakeMaker;

###----------------------------------------------------------------###
#  Copyright 2003 - Paul Seamons                                     #
#  Distributed under the GNU General Public License without warranty #
###----------------------------------------------------------------###

WriteMakefile(
              NAME          => "HTML::Form",
              AUTHOR        => "Paul Seamons",
              ABSTRACT_FROM => "lib/HTML/Form.pm",
              VERSION_FROM  => "lib/HTML/Form.pm",
              INSTALLDIRS   => 'site',
              
              dist          => {
                DIST_DEFAULT => 'all tardist',
                COMPRESS     => 'gzip -vf',
                SUFFIX       => '.gz',
              },
              
              clean          => {
                FILES        => '*~',
              },
              
              realclean         => {
                FILES        => '*~',
              },
              );

1;
