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
#                                                   New object each time (undef CACHE_SIZE) #        #        #
#                              This percent is compiled in memory (repeated calls) #        #        #        #
my $tests = {                                                             #        #        #        #        #
    '01_empty'     => "",                                                 #  236%  #  645%  #  337%  #  457%  # 19787.6/s #
    '02_var_sma'   => "[% one %]",                                        #  168%  #  592%  #  421%  #  476%  # 14355.1/s #
    '03_var_lar'   => "[% one %]"x100,                                    #   31%  #  341%  #   79%  #  348%  # 940.2/s #
    '04_set_sma'   => "[% SET one = 2 %]",                                #  161%  #  534%  #  419%  #  406%  # 14121.9/s #
    '05_set_lar'   => "[% SET one = 2 %]"x100,                            #   12%  #  279%  #   37%  #  276%  # 864.5/s #
    '06_set_range' => "[% SET one = [0..30] %]",                          #   42%  #  320%  #  247%  #  211%  # 7518.1/s #
    '07_chain_sm'  => "[% hash.a %]",                                     #  173%  #  619%  #  423%  #  494%  # 13334.4/s #
    '08_mixed_sma' => "".((" "x100)."[% one %]\n")x10,                    #   82%  #  517%  #  261%  #  476%  # 5848.1/s #
    '09_mixed_med' => "".((" "x10)."[% one %]\n")x100,                    #   23%  #  432%  #  116%  #  422%  # 867.7/s #
    '10_str_sma'   => "".("[% \"".(" "x100)."\$one\" %]\n")x10,           #  -15%  #  1415%  #  102%  #  1467%  # 2700.5/s #
    '11_str_lar'   => "".("[% \"".(" "x10)."\$one\" %]\n")x100,           #  -50%  #  299%  #    2%  #  302%  # 329.3/s #
    '12_num_lterl' => "[% 2 %]",                                          #  177%  #  602%  #  459%  #  459%  # 15838.0/s #
    '13_plus'      => "[% 1 + 2 %]",                                      #   87%  #  435%  #  334%  #  299%  # 10746.9/s #
    '14_chained'   => "[% c.d.0.hee.0 %]",                                #  171%  #  616%  #  416%  #  527%  # 13717.7/s #
    '15_chain_set' => "[% SET c.d.0.hee.0 = 2 %]",                        #  156%  #  519%  #  364%  #  413%  # 10628.0/s #
    '16_chain_lar' => "[% c.d.0.hee.0 %]"x100,                            #   54%  #  464%  #   82%  #  467%  # 752.8/s #
    '17_chain_sl'  => "[% SET c.d.0.hee.0 = 2 %]"x100,                    #  107%  #  341%  #   91%  #  341%  # 342.0/s #
    '18_cplx_comp' => "[% t = 1 || 0 ? 0 : 1 || 2 ? 2 : 3 %][% t %]",     #   73%  #  291%  #  256%  #  210%  # 8674.9/s #
    '19_if_sim_t'  => "[% a=1 %][% IF a %]Two[% END %]",                  #  126%  #  481%  #  347%  #  378%  # 11166.3/s #
    '20_if_sim_f'  => "         [% IF a %]Two[% END %]",                  #  161%  #  596%  #  412%  #  492%  # 13846.3/s #
    '21_if_else'   => "[% IF a %]A[% ELSE %]B[% END %]",                  #  141%  #  526%  #  378%  #  422%  # 12728.4/s #
    '22_if_elsif'  => "[% IF a %]A[% ELSIF b %]B[% ELSE %]C[% END %]",    #  131%  #  519%  #  359%  #  407%  # 11386.8/s #
    '23_for_i_sml' => "[% FOREACH i = [0..10]   ; i ; END %]",            #   16%  #  230%  #  154%  #  155%  # 2376.1/s #
    '24_for_i_med' => "[% FOREACH i = [0..100]  ; i ; END %]",            #  -15%  #   38%  #   13%  #   18%  # 362.5/s #
    '25_for_sml'   => "[% FOREACH [0..10]       ; i ; END %]",            #   23%  #  245%  #  162%  #  169%  # 2463.0/s #
    '26_for_med'   => "[% FOREACH [0..100]      ; i ; END %]",            #   -4%  #   56%  #   28%  #   32%  # 384.9/s #
    '27_while'     => "[% f = 10 %][%WHILE f%][%f=f- 1%][%f%][% END %]",  #  -14%  #  152%  #   50%  #  102%  # 1279.5/s #
    '28_whl_set_l' => "[% f = 10; WHILE (g=f) ; f = f - 1 ; f ; END %]",  #  -18%  #  115%  #   34%  #   75%  # 1014.6/s #
    '29_whl_set_s' => "[% f = 1;  WHILE (g=f) ; f = f - 1 ; f ; END %]",  #   38%  #  302%  #  193%  #  223%  # 5061.4/s #
    '30_file_proc' => "[% PROCESS bar.tt %]",                             #  236%  #  592%  #  397%  #  506%  # 10189.6/s #
    '31_file_incl' => "[% INCLUDE baz.tt %]",                             #  157%  #  427%  #  296%  #  370%  # 6549.1/s #
    '32_process'   => "[% BLOCK foo %]Hi[% END %][% PROCESS foo %]",      #  162%  #  592%  #  422%  #  487%  # 10030.6/s #
    '33_include'   => "[% BLOCK foo %]Hi[% END %][% INCLUDE foo %]",      #  140%  #  557%  #  387%  #  462%  # 8555.5/s #
    '34_macro'     => "[% MACRO foo BLOCK %]Hi[% END %][% foo %]",        #   79%  #  413%  #  301%  #  309%  # 7363.9/s #
    '35_macro_arg' => "[% MACRO foo(n) BLOCK %]Hi[%n%][%END%][%foo(2)%]", #   65%  #  296%  #  265%  #  219%  # 6081.4/s #
    '36_macro_pro' => "[% MACRO foo PROCESS bar;BLOCK bar%]7[%END;foo%]", #   96%  #  449%  #  316%  #  355%  # 5962.3/s #
    '37_filter2'   => "[% n = 1 %][% n | repeat(2) %]",                   #  128%  #  432%  #  370%  #  336%  # 9705.7/s #
    '38_filter'    => "[% n = 1 %][% n FILTER repeat(2) %]",              #   91%  #  367%  #  325%  #  256%  # 8025.8/s #
    '39_fltr_name' => "[% n=1; n FILTER echo=repeat(2); n FILTER echo%]", #   36%  #  317%  #  241%  #  243%  # 5321.4/s #
    '40_constant'  => "[% constants.simple %]",                           #  176%  #  617%  #  473%  #  448%  # 15760.8/s #
    '41_perl'      => "[%one='ONE'%][% PERL %]print \"[%one%]\"[%END%]",  #   66%  #  447%  #  300%  #  357%  # 6472.9/s #
    '42_filtervar' => "[% 'hi' | \$filt %]",                              #   97%  #  517%  #  348%  #  400%  # 9500.9/s #
    '43_filteruri' => "[% ' ' | uri %]",                                  #  127%  #  620%  #  402%  #  505%  # 11498.0/s #
    '44_filterevl' => "[% foo | eval %]",                                 #  326%  #  600%  #  498%  #  517%  # 5022.9/s #
    '45_capture'   => "[% foo = BLOCK %]Hi[% END %][% foo %]",            #  105%  #  438%  #  312%  #  326%  # 9962.1/s #
    '46_complex'   => "$longer_template",                                 #   51%  #  305%  #  139%  #  264%  # 1098.6/s #
    # overall                                                             #   94%  #  439%  #  268%  #  359%  #
    # overall (with stash::xs)                                            #   30%  #  409%  #  228%  #  323%  #
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
            '1 cached_in_memory           ' => ['file_CT',     'file_TT'],
            '2 new_object                 ' => ['file_CT_n',   'file_TT_n'],
            '3 cached_on_file (new_object)' => ['file_CT_c_n', 'file_TT_c_n'],
            '4 string reference           ' => ['str_CT',      'str_TT'],
            '5 CT new vs TT in mem        ' => ['file_CT_n',   'file_TT'],
            '6 CT in mem vs TT new        ' => ['file_CT',     'file_TT_n'],
            '7 CT in mem vs CT new        ' => ['file_CT',     'file_CT_n'],
            '8 TT in mem vs TT new        ' => ['file_TT',     'file_TT_n'],
        };
        foreach my $type (sort keys %$hash) {
            my ($key1, $key2) = @{ $hash->{$type} };
            my $ct = $r->{$key1};
            my $tt = $r->{$key2};
            my $ct_s = $ct->iters / ($ct->cpu_a || 1);
            my $tt_s = $tt->iters / ($tt->cpu_a || 1);
            my $p = int(100 * ($ct_s - $tt_s) / ($tt_s || 1));
            print "$type - CT is $p% faster than TT\n";

            $output .= sprintf('#  %3s%%  ', $p) if $type =~ /^[1234]/;

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

    if ($#run > 0) {
        foreach (sort keys %cumulative) {
            printf "Cumulative $_: %6.1f\n", $cumulative{$_}->[2];
        }
    }

}

### add the final total row
if ($#run > 0) {
    $output .= "    # overall" . (" "x61);
    foreach my $type (sort keys %cumulative) {
        $output .= sprintf('#  %3s%%  ', int $cumulative{$type}->[2]) if $type =~ /^[1234]/;
    }
    $output .= "#\n";

    print $output;
}



#print `ls -lR $tt_cache_dir`;
__DATA__
