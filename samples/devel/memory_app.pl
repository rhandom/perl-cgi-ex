#!/usr/bin/perl -w

use Benchmark qw(cmpthese timethese);
use strict;

my $swap = {
    one   => "ONE",
    two   => "TWO",
    three => "THREE",
    a_var => "a",
    hash  => {a => 1, b => 2},
    code  => sub {"($_[0])"},
};

my $form = q{([% has_errors %])(<TMPL_VAR has_errors>)<form name=foo><input type=text name="bar" value=""><input type=text name="baz"></form>};
my $str_ht = $form . (q{Well hello there (<TMPL_VAR script_name>)} x 20) ."\n";
my $str_tt = $form . (q{Well hello there ([% script_name %])}      x 20) ."\n";

my $template_ht = \$str_ht;
my $template_tt = \$str_tt;

###----------------------------------------------------------------###
use Scalar::Util;
use Time::HiRes;
use CGI;
use CGI::Ex::Dump qw(debug);
#use Template::Alloy load => 'HTE', 'Parse', 'Play', 'TT';
#use HTML::Template;

my $tests = {
    'CGI::Application - bare' => sub {
        package FooBare;
        require CGI::Application;
        @FooBare::ISA = qw(CGI::Application);

        sub setup {
            my $self = shift;
            $self->start_mode('main');
            $self->mode_param(path_info => 1);
            $self->run_modes(main => sub { "Simple test" });
        }

        FooBare->new->run;
    },
    'CGI::Ex::App - bare' => sub {
        package FooBare;
        require CGI::Ex::App;
        @FooBare::ISA = qw(CGI::Ex::App);

        sub main_run_step {
            my $self = shift;
            $self->print_content_type;
            print "Simple test";
            1;
        }

        FooBare->new->navigate;
    },
    'Handwritten - bare' => sub {
        package FooBare2;

        sub new { bless {}, __PACKAGE__ }

        sub main {
            my $self = shift;
            print "Content-Type: text/html\r\n\r\n";
            print "Simple test";
        }

        FooBare2->new->main;
    },
    #'CGI::Prototype - bare' => sub {
    #    package FooBare;
    #    require CGI::Prototype;
    #},

    ###----------------------------------------------------------------###

    'CGI::Application - simple ht' => sub {
        package FooHT;
        require CGI::Application;
        @FooHT::ISA = qw(CGI::Application);

        sub setup {
            my $self = shift;
            $self->start_mode('main');
            $self->mode_param(path_info => 1);
            $self->run_modes(main => sub {
                my $self = shift;
                my $t = $self->load_tmpl($template_ht, die_on_bad_params => 0);
                $t->param('script_name', $0);
                return $t->output();
            });
        }

        FooHT->new->run;
    },
    'CGI::Ex::App - simple ht' => sub {
        package FooHT;
        require CGI::Ex::App;
        @FooHT::ISA = qw(CGI::Ex::App);

        sub main_file_print { $template_ht }
        sub template_args { {SYNTAX => 'hte'} } # , GLOBAL_CACHE => 1, COMPILE_PERL => 2} }
        sub fill_template {}

        FooHT->navigate({no_history => 1});
    },
    'CGI::Application - simple tt' => sub {
        package FooTT;
        require CGI::Application;
        @FooTT::ISA = qw(CGI::Application);
        require CGI::Application::Plugin::TT;
        CGI::Application::Plugin::TT->import;

        sub setup {
            my $self = shift;
            $self->start_mode('main');

            $self->run_modes(main => sub {
                my $self = shift;
                return $self->tt_process($template_tt, {script_name => $0});
            });
        }

        FooTT->new->run;
    },
    'CGI::Ex::App - simple tt' => sub {
        package FooTT;
        require CGI::Ex::App;
        @FooTT::ISA = qw(CGI::Ex::App);
        sub main_file_print { $template_tt }
        sub fill_template {}
        FooTT->navigate({no_history => 1});
    },

    ###----------------------------------------------------------------###

    'CGI::Application - complex ht' => sub {
        package FooComplexHT;
        require CGI::Application;
        @FooComplexHT::ISA = qw(CGI::Application);
        require CGI::Application::Plugin::ValidateRM;
        CGI::Application::Plugin::ValidateRM->import('check_rm');
        require CGI::Application::Plugin::FillInForm;
        CGI::Application::Plugin::FillInForm->import('fill_form');

        sub setup {
            my $self = shift;
            $self->start_mode('main');
            $self->mode_param(path_info => 1);
            $self->run_modes(main => sub {
                my $self = shift;
                my ($results, $err_page) = $self->check_rm('error_page','_profile');
                return $err_page if $err_page;
                die "Got here";
            });
        }

        sub error_page {
            my $self = shift;
            my $errs = shift;
            my $t = $self->load_tmpl($template_ht, die_on_bad_params => 0);
            $t->param('script_name', $0);
            $t->param($errs) if $errs;
            $t->param(has_errors => 1) if $errs;
            my $q = $self->query;
            $q->param(bar => 'BAROOSELVELT');
            return $self->fill_form(\$t->output, $q);
        }

        sub _profile { return {required => [qw(bar baz)], msgs => {prefix => 'err_'}} };

        FooComplexHT->new->run;
    },
    'CGI::Ex::App - complex ht' => sub {
        package FooComplexHT;
        require CGI::Ex::App;
        @FooComplexHT::ISA = qw(CGI::Ex::App);

        sub main_file_print { $template_ht }
        sub main_hash_fill  { {bar => 'BAROOSELVELT'} }
        sub main_hash_validation { {bar => {required => 1}, baz => {required => 1}} }
        sub main_finalize { die "Got here" }
        sub template_args { {SYNTAX => 'hte'} } # , GLOBAL_CACHE => 1, COMPILE_PERL => 2} }

        local $ENV{'REQUEST_METHOD'} = 'POST';
        FooComplexHT->navigate({no_history => 1});
    },
    'CGI::Application - complex tt' => sub {
        package FooComplexTT;
        require CGI::Application;
        @FooComplexTT::ISA = qw(CGI::Application);
        require CGI::Application::Plugin::TT;
        CGI::Application::Plugin::TT->import;
        require CGI::Application::Plugin::ValidateRM;
        CGI::Application::Plugin::ValidateRM->import('check_rm');
        require CGI::Application::Plugin::FillInForm;
        CGI::Application::Plugin::FillInForm->import('fill_form');

        sub setup {
            my $self = shift;
            $self->start_mode('main');

            $self->run_modes(main => sub {
                my $self = shift;
                my ($results, $err_page) = $self->check_rm('error_page','_profile');
                return $err_page if $err_page;
                die "Got here";
            });
        }

        sub error_page {
            my $self = shift;
            my $errs = shift;
            my $out = $self->tt_process($template_tt, {script_name => $0, %{$errs || {}}, has_errors => ($errs ? 1 : 0)});
            my $q = $self->query;
            $q->param(bar => 'BAROOSELVELT');
            return $self->fill_form(\$out, $q);
        }

        sub _profile { return {required => [qw(bar baz)], msgs => {prefix => 'err_'}} };

        FooComplexTT->new->run;
    },
    'CGI::Ex::App - complex tt' => sub {
        package FooComplexTT;
        require CGI::Ex::App;
        @FooComplexTT::ISA = qw(CGI::Ex::App);
        sub main_file_print { $template_tt }
        sub main_hash_fill  { {bar => 'BAROOSELVELT'} }
        sub main_hash_validation { {bar => {required => 1}, baz => {required => 1}} }
        sub main_finalize { die "Got here" }
        local $ENV{'REQUEST_METHOD'} = 'POST';
        FooComplexTT->navigate({no_history => 1});
    },

    #'Template::Alloy - bare ht' => sub { require Template::Alloy; Template::Alloy->import('HTE') },
    #'Template::Alloy - bare tt' => sub { require Template::Alloy; Template::Alloy->import('TT') },
};

#$tests->{'CGI::Application - complex ht'}->();
#exit;

###----------------------------------------------------------------###

my %_INC = %INC;
my @pids;
foreach my $name (sort keys %$tests) {
    my $pid = fork;
    if (! $pid) {
        $0 = "$0 - $name";
        select($_) if open($_, ">>/dev/null");
        $tests->{$name}->() for 1 .. 1;
        sleep 1;
        select STDOUT;
        print "$name times: (@{[times]})\n";
        print "$name $_\n" foreach sort grep {! $_INC{$_}} keys %INC;
        sleep 15;
        exit;
    }
    push @pids, $pid;
}

sleep 2;
#    print "Parent - $_\n" foreach sort keys %INC;
print grep {/\Q$0\E/} `ps fauwx`;
kill 15, @pids;

###----------------------------------------------------------------###

exit if grep {/no_?bench/i} @ARGV;


foreach my $type (qw(bare simple complex)) {
    my $hash = {};
    open(my $fh, ">>/dev/null") || die "Can't access /dev/null: $!";
    foreach my $name (keys %$tests) {
        next if $name !~ /\b$type\b/;
        $hash->{$name} = sub {
            select $fh;
            $tests->{$name}->();
            select STDOUT;
        };
    }
    print "-------------------------------------------------\n";
    print "--- Testing $type\n";
    cmpthese timethese -2, $hash;
}
