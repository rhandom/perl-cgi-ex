#!/usr/bin/perl -w

=head1 NAME

bench_template.pl - Test relative performance of CGI::Ex::Template to Template::Toolkit

=cut

use strict;
use Benchmark qw(cmpthese timethese);
use POSIX qw(tmpnam);
use File::Path qw(rmtree);
use CGI::Ex::Template;
use CGI::Ex::Dump qw(debug);
use Template;
use constant test_taint => 0 && eval { require Taint::Runtime }; # s/0/1/ to check tainting

Taint::Runtime::taint_start() if test_taint;

my $tt_cache_dir = tmpnam;
END { rmtree $tt_cache_dir };
mkdir $tt_cache_dir, 0755;

my $swap = {
    one   => "ONE",
    a_var => "a",
    foo   => '[% bar %]',
    bar   => "baz",
    hash  => {a => 1, b => 2, c => { d => [{hee => ["hmm"]}] }},
    array => [qw(A B C D E a A)],
    code  => sub {"(@_)"},
    filt  => sub {sub {$_[0]x2}},
};

use Template::Stash;;
my $s = Template::Stash->new($swap);
#use Template::Stash::XS;
#$s = Template::Stash::XS->new($swap);

###----------------------------------------------------------------###
### get objects ready

my @config1 = (STASH => $s, ABSOLUTE => 1, CONSTANTS => {simple => 'var'}, EVAL_PERL => 1, INCLUDE_PATH => $tt_cache_dir);
#push @config1, (INTERPOLATE => 1);
my @config2 = (@config1, COMPILE_EXT => '.ttc');

my $tt1 = Template->new(@config1);
my $tt2 = Template->new(@config2);

my $cet = CGI::Ex::Template->new(@config1);
my $cetc = CGI::Ex::Template->new(@config2);

#$swap->{$_} = $_ for (1 .. 1000); # swap size affects benchmark speed

###----------------------------------------------------------------###
### write out some file to be used later

my $fh;
my $bar_template = "$tt_cache_dir/bar.tt";
END { unlink $bar_template };
open($fh, ">$bar_template") || die "Couldn't open $bar_template: $!";
print $fh "BAR";
close $fh;

my $baz_template = "$tt_cache_dir/baz.tt";
END { unlink $baz_template };
open($fh, ">$baz_template") || die "Couldn't open $baz_template: $!";
print $fh "[% SET baz = 42 %][% baz %][% bing %]";
close $fh;

my $longer_template = "[% INCLUDE bar.tt %]"
    ."[% array.join('|') %]"
    .("123"x200)
    ."[% FOREACH a IN array %]foobar[% IF a == 'A' %][% INCLUDE baz.tt %][% END %]bazbing[% END %]"
    .("456"x200)
    ."[% IF foo ; bar ; ELSIF baz ; bing ; ELSE ; bong ; END %]"
    .("789"x200)
    ."[% IF foo ; bar ; ELSIF baz ; bing ; ELSE ; bong ; END %]"
    .("012"x200)
    ."[% IF foo ; bar ; ELSIF baz ; bing ; ELSE ; bong ; END %]"
    ."[% array.join('|') %]"
    ."[% PROCESS bar.tt %]";

###----------------------------------------------------------------###
### set a few globals that will be available in our subs
my $show_list = grep {$_ eq '--list'} @ARGV;
my $run_all   = grep {$_ eq '--all'}  @ARGV;
my @run = $run_all ? () : @ARGV;
my $str_ref;
my $filename;

### uncomment to run a specific test - otherwise all tests run
#@run = qw(07);

#                                                                         ### All percents are CGI::Ex::Template vs TT2
#                                                                         ### (The percent that CET is faster than TT)
#                                                                               Existing object by string ref #
#                                                                      New object with CACHE_EXT set #        #
#                                       This percent is compiled in memory (repeated calls) #        #        #
#                                          New object each time (undef CACHE_SIZE) #        #        #        #
my $tests = {                                                             #        #        #        #        #
    '01_empty'     => "",                                                 #  260%  #  751%  #  391%  #  429%  # 14551.2/s #
    '02_var_sma'   => "[% one %]",                                        #  183%  #  690%  #  485%  #  491%  # 10378.1/s #
    '03_var_lar'   => "[% one %]"x100,                                    #   12%  #  385%  #   55%  #  373%  # 541.4/s #
    '04_set_sma'   => "[% SET one = 2 %]",                                #  172%  #  631%  #  473%  #  441%  # 10189.1/s #
    '05_set_lar'   => "[% SET one = 2 %]"x100,                            #    2%  #  322%  #   24%  #  311%  # 525.7/s #
    '06_set_range' => "[% SET one = [0..30] %]",                          #   43%  #  390%  #  271%  #  241%  # 5167.8/s #
    '07_chain_sm'  => "[% hash.a %]",                                     #  186%  #  767%  #  467%  #  540%  # 9437.8/s #
    '08_mixed_sma' => "".((" "x100)."[% one %]\n")x10,                    #   66%  #  584%  #  262%  #  486%  # 3571.9/s #
    '09_mixed_med' => "".((" "x10)."[% one %]\n")x100,                    #    7%  #  471%  #   96%  #  451%  # 499.5/s #
    '10_str_sma'   => "".("[% \"".(" "x100)."\$one\" %]\n")x10,           #  -25%  # 2085%  #   91%  # 2163%  # 1619.4/s #
    '11_str_lar'   => "".("[% \"".(" "x10)."\$one\" %]\n")x100,           #  -58%  #  348%  #   -9%  #  346%  # 193.1/s #
    '12_num_lterl' => "[% 2 %]",                                          #  191%  #  725%  #  527%  #  489%  # 11647.0/s #
    '13_plus'      => "[% 1 + 2 %]",                                      #   92%  #  532%  #  378%  #  334%  # 7526.9/s #
    '14_plus_long' => "[% 1 + 2 + 3 + 5 + 6 + 8 %]",                      #   68%  #  392%  #  340%  #  242%  # 6603.7/s #
    '15_chained'   => "[% c.d.0.hee.0 %]",                                #  194%  #  815%  #  474%  #  618%  # 10275.9/s #
    '16_chain_set' => "[% SET c.d.0.hee.0 = 2 %]",                        #  153%  #  618%  #  397%  #  473%  # 7211.3/s #
    '17_chain_lar' => "[% c.d.0.hee.0 %]"x100,                            #   80%  #  650%  #   91%  #  644%  # 575.5/s #
    '18_chain_sl'  => "[% SET c.d.0.hee.0 = 2 %]"x100,                    #   79%  #  386%  #   67%  #  403%  # 196.3/s #
    '19_cplx_comp' => "[% t = 1 || 0 ? 0 : 1 || 2 ? 2 : 3 %][% t %]",     #   71%  #  362%  #  312%  #  239%  # 5663.9/s #
    '20_if_sim_t'  => "[% a=1 %][% IF a %]Two[% END %]",                  #  125%  #  570%  #  379%  #  415%  # 7701.5/s #
    '21_if_sim_f'  => "         [% IF a %]Two[% END %]",                  #  182%  #  723%  #  488%  #  534%  # 10275.9/s #
    '22_if_else'   => "[% IF a %]A[% ELSE %]B[% END %]",                  #  157%  #  634%  #  432%  #  455%  # 9286.0/s #
    '23_if_elsif'  => "[% IF a %]A[% ELSIF b %]B[% ELSE %]C[% END %]",    #  148%  #  608%  #  417%  #  447%  # 8298.5/s #
    '24_for_i_sml' => "[% FOREACH i = [0..10]   ; i ; END %]",            #   12%  #  259%  #  147%  #  163%  # 1555.1/s #
    '25_for_i_med' => "[% FOREACH i = [0..100]  ; i ; END %]",            #  -20%  #   34%  #    5%  #   13%  # 228.2/s #
    '26_for_sml'   => "[% FOREACH [0..10]       ; i ; END %]",            #   30%  #  294%  #  179%  #  200%  # 1745.0/s #
    '27_for_med'   => "[% FOREACH [0..100]      ; i ; END %]",            #    5%  #   68%  #   35%  #   43%  # 279.8/s #
    '28_while'     => "[% f = 10 %][%WHILE f%][%f=f- 1%][%f%][% END %]",  #  -18%  #  158%  #   44%  #   98%  # 780.9/s #
    '29_whl_set_l' => "[% f = 10; WHILE (g=f) ; f = f - 1 ; f ; END %]",  #   -9%  #  153%  #   48%  #   98%  # 726.9/s #
    '30_whl_set_s' => "[% f = 1;  WHILE (g=f) ; f = f - 1 ; f ; END %]",  #   54%  #  407%  #  234%  #  311%  # 3772.9/s #
    '31_file_proc' => "[% PROCESS bar.tt %]",                             #  272%  #  689%  #  460%  #  593%  # 7664.1/s #
    '32_file_incl' => "[% INCLUDE baz.tt %]",                             #  171%  #  543%  #  349%  #  415%  # 4633.0/s #
    '33_process'   => "[% BLOCK foo %]Hi[% END %][% PROCESS foo %]",      #  165%  #  692%  #  477%  #  535%  # 7048.6/s #
    '34_include'   => "[% BLOCK foo %]Hi[% END %][% INCLUDE foo %]",      #  144%  #  619%  #  419%  #  496%  # 6047.4/s #
    '35_macro'     => "[% MACRO foo BLOCK %]Hi[% END %][% foo %]",        #   84%  #  483%  #  334%  #  340%  # 5371.4/s #
    '36_macro_arg' => "[% MACRO foo(n) BLOCK %]Hi[%n%][%END%][%foo(2)%]", #   63%  #  358%  #  292%  #  254%  # 4277.5/s #
    '37_macro_pro' => "[% MACRO foo PROCESS bar;BLOCK bar%]7[%END;foo%]", #   96%  #  476%  #  340%  #  397%  # 4267.9/s #
    '38_filter2'   => "[% n = 1 %][% n | repeat(2) %]",                   #  127%  #  519%  #  388%  #  390%  # 6791.0/s #
    '39_filter'    => "[% n = 1 %][% n FILTER repeat(2) %]",              #   84%  #  454%  #  345%  #  304%  # 5553.9/s #
    '40_fltr_name' => "[% n=1; n FILTER echo=repeat(2); n FILTER echo%]", #   32%  #  364%  #  228%  #  276%  # 3524.0/s #
    '41_constant'  => "[% constants.simple %]",                           #  190%  #  726%  #  542%  #  547%  # 11550.2/s #
    '42_perl'      => "[%one='ONE'%][% PERL %]print \"[%one%]\"[%END%]",  #   66%  #  489%  #  319%  #  372%  # 4440.5/s #
    '43_filtervar' => "[% 'hi' | \$filt %]",                              #   83%  #  643%  #  403%  #  453%  # 6176.7/s #
    '44_filteruri' => "[% ' ' | uri %]",                                  #  139%  #  746%  #  463%  #  574%  # 8178.1/s #
    '45_filterevl' => "[% foo | eval %]",                                 #  378%  #  692%  #  570%  #  578%  # 3617.9/s #
    '46_refs'      => "[% b = \\code(1); b(2) %]",                        #   80%  #  369%  #  292%  #  237%  # 5022.9/s #
    '47_capture'   => "[% foo = BLOCK %]Hi[% END %][% foo %]",            #  105%  #  501%  #  344%  #  348%  # 6870.3/s #
    '48_complex'   => "$longer_template",                                 #   48%  #  340%  #  135%  #  293%  # 721.5/s #
    # overall                                                             #   99%  #  531%  #  297%  #  414%  #
    # overall (with stash::xs)                                            #   32%  #  505%  #  246%  #  380%  #
};

### load the code representation
my $text = {};
seek DATA, 0, 0;
my $data = do { local $/ = undef; <DATA> };
foreach my $key (keys %$tests) {
    $data =~ m/(.*\Q$key\E.*)/ || next;
    $text->{$key} = $1;
}

if ($show_list) {
    foreach my $text (sort values %$text) {
        print "$text\n";
    }
    exit;
}

my $run = join("|", @run);
@run = grep {/$run/} sort keys %$tests;

###----------------------------------------------------------------###

sub file_TT_new {
    my $out = '';
    my $t = Template->new(@config1);
    $t->process($filename, $swap, \$out);
    return $out;
}

sub str_TT_new {
    my $out = '';
    my $t = Template->new(@config1);
    $t->process($str_ref, $swap, \$out);
    return $out;
}

sub file_TT {
    my $out = '';
    $tt1->process($filename, $swap, \$out);
    return $out;
}

sub str_TT {
    my $out = '';
    $tt1->process($str_ref, $swap, \$out) || debug $tt1->error;
    return $out;
}

sub file_TT_cache_new {
    my $out = '';
    my $t = Template->new(@config2);
    $t->process($filename, $swap, \$out);
    return $out;
}

###----------------------------------------------------------------###

sub file_CET_new {
    my $out = '';
    my $t = CGI::Ex::Template->new(@config1);
    $t->process($filename, $swap, \$out);
    return $out;
}

sub str_CET_new {
    my $out = '';
    my $t = CGI::Ex::Template->new(@config1);
    $t->process($str_ref, $swap, \$out);
    return $out;
}

sub file_CET {
    my $out = '';
    $cet->process($filename, $swap, \$out);
    return $out;
}

sub str_CET {
    my $out = '';
    $cet->process($str_ref, $swap, \$out);
    return $out;
}

sub str_CET_swap {
    my $txt = $cet->swap($str_ref, $swap);
    return $txt;
}

sub file_CET_cache_new {
    my $out = '';
    my $t = CGI::Ex::Template->new(@config2);
    $t->process($filename, $swap, \$out);
    return $out;
}

###----------------------------------------------------------------###

@run = sort(keys %$tests) if $#run == -1;

my $output = '';
my %cumulative;
foreach my $test_name (@run) {
    die "Invalid test $test_name" if ! exists $tests->{$test_name};
    my $txt = $tests->{$test_name};
    my $sample =$text->{$test_name};
    $sample =~ s/^.+=>//;
    $sample =~ s/\#.+$//;
    print "-------------------------------------------------------------\n";
    print "Running test $test_name\n";
    print "Test text: $sample\n";

    ### set the global file types
    $str_ref = \$txt;
    $filename = $tt_cache_dir ."/$test_name.tt";
    open(my $fh, ">$filename") || die "Couldn't open $filename: $!";
    print $fh $txt;
    close $fh;

    #debug file_CET(), str_TT();
    #debug $cet->parse_tree($file);

    ### check out put - and also allow for caching
    for (1..2) {
        if (file_CET() ne str_TT()) {
            debug $cet->parse_tree($str_ref);
            debug file_CET(), str_TT();
            die "file_CET didn't match";
        }
        die "file_TT didn't match "            if file_TT()      ne str_TT();
        die "str_CET didn't match "            if str_CET()      ne str_TT();
#        die "str_CET_swap didn't match "       if str_CET_swap() ne str_TT();
        die "file_CET_cache_new didn't match " if file_CET_cache_new() ne str_TT();
        die "file_TT_cache_new didn't match " if file_TT_cache_new() ne str_TT();
    }

    next if test_taint;

###----------------------------------------------------------------###

    my $r = eval { timethese (-2, {
        file_TT_n   => \&file_TT_new,
#        str_TT_n    => \&str_TT_new,
        file_TT     => \&file_TT,
        str_TT      => \&str_TT,
        file_TT_c_n => \&file_TT_cache_new,

        file_CT_n   => \&file_CET_new,
#        str_CT_n    => \&str_CET_new,
        file_CT     => \&file_CET,
        str_CT      => \&str_CET,
#        str_CT_sw   => \&str_CET_swap,
        file_CT_c_n => \&file_CET_cache_new,
    }) };
    if (! $r) {
        debug "$@";
        next;
    }
    eval { cmpthese $r };

    my $copy = $text->{$test_name};
    $copy =~ s/\#.+//;
    $output .= $copy;

    eval {
        my $hash = {
            '1 cached_in_memory           ' => '',
            '2 new_object                 ' => '_n',
            '3 cached_on_file (new_object)' => '_c_n',
            '4 string reference           ' => 'str',
        };
        foreach my $type (sort keys %$hash) {
            my $suffix = $hash->{$type};
            my $prefix = 'file';
            ($prefix, $suffix) = ('str', '') if $suffix eq 'str';
            my $ct = $r->{"${prefix}_CT$suffix"};
            my $tt = $r->{"${prefix}_TT$suffix"};
            my $ct_s = $ct->iters / ($ct->cpu_a || 1);
            my $tt_s = $tt->iters / ($tt->cpu_a || 1);
            my $p = int(100 * ($ct_s - $tt_s) / ($tt_s || 1));
            print "$type - CT is $p% faster than TT\n";

            $output .= sprintf('#  %3s%%  ', $p);

            ### store cumulatives
            if (abs($p) < 10000) {
                $cumulative{$type} ||= [0, 0];
                $cumulative{$type}->[0] += $p;
                $cumulative{$type}->[1] ++;
            }
        }
    };
    debug "$@"
        if $@;

    $output .= "# ".sprintf("%.1f", $r->{'file_CT'}->iters / ($r->{'file_CT'}->cpu_a || 1))."/s #\n";
#    $output .= "#\n";

    foreach my $row (values %cumulative) {
        $row->[2] = sprintf('%.1f', $row->[0] / ($row->[1]||1));
    }

    debug \%cumulative
        if $#run > 0;
}

### add the final total row
if ($#run > 0) {
    $output .= "    # overall" . (" "x61);
    $output .= sprintf('#  %3s%%  ', int $cumulative{$_}->[2]) foreach sort keys %cumulative;
    $output .= "#\n";

    print $output;
}



#print `ls -lR $tt_cache_dir`;
__DATA__
