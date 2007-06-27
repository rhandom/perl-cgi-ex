#!/usr/bin/perl -w

use Benchmark qw(cmpthese timethese);

my $swap = {
    one   => "ONE",
    two   => "TWO",
    three => "THREE",
    a_var => "a",
    hash  => {a => 1, b => 2},
    code  => sub {"($_[0])"},
};

my $template_ht = \ <<'EOD';
Well hello there (<TMPL_VAR script_name>)
EOD

my $template_tt = \ <<'EOD';
Well hello there ([% script_name %])
EOD

###----------------------------------------------------------------###
use Scalar::Util;
use Time::HiRes;
use CGI;
use CGI::Ex::Dump qw(debug);
#use Template::Alloy load => 'HTE', 'Parse', 'Play', 'TT';
#use HTML::Template;

my $tests = {
    'CGI::Application - bare' => sub {
        package Foo;
        require CGI::Application;
    },
    'CGI::Ex::App - bare' => sub {
        package Foo;
        require CGI::Ex::App;
    },
    'CGI::Prototype - bare' => sub {
        package Foo;
        require CGI::Prototype;
    },
    'CGI::Application - simple ht' => sub {
        package FooHT;
        require CGI::Application;
        @FooHT::ISA = qw(CGI::Application);

        sub setup {
            my $self = shift;
            $self->start_mode('main');

            $self->run_modes(main => sub {
                my $self = shift;
                my $t = $self->load_tmpl($template_ht);
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
        sub template_args { {SYNTAX => 'hte'} }
        sub main_fill_template {}
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
        sub main_fill_template {}
        FooTT->navigate({no_history => 1});
    },
    #'Template::Alloy - bare ht' => sub { require Template::Alloy; Template::Alloy->import('HTE') },
    #'Template::Alloy - bare tt' => sub { require Template::Alloy; Template::Alloy->import('TT') },
};

###----------------------------------------------------------------###

my %_INC = %INC;
my @pids;
foreach my $name (sort keys %$tests) {
    my $pid = fork;
    if (! $pid) {
        $0 = "$0 - $name";
        $tests->{$name}->() for 1 .. 1;
        sleep 1;
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

cmpthese timethese -2, $tests;
