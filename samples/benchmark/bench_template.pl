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
    ."[% FOREACH h IN hash.keys.sort %]([% hash.\$h %])[% END %]"
    .("789"x200)
    ."[% SET n = 0 %]"
    ."[% FOREACH i IN [0..10] ; n = n + i ; END ; n %]"
    .("012"x200)
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
    '01_empty'     => "",                                                 #  296%  #  744%  #  373%  #  415%  # 16290.5/s #
    '02_var_sma'   => "[% one %]",                                        #  198%  #  745%  #  505%  #  488%  # 10900.5/s #
    '03_var_lar'   => "[% one %]"x100,                                    #    2%  #  377%  #   32%  #  366%  # 259.4/s #
    '04_set_sma'   => "[% SET one = 2 %]",                                #  187%  #  649%  #  477%  #  432%  # 10548.8/s #
    '05_set_lar'   => "[% SET one = 2 %]"x100,                            #  187%  #  649%  #  477%  #  432%  # 10548.8/s #
    '06_set_range' => "[% SET one = [0..30] %]",                          #   44%  #  390%  #  265%  #  236%  # 5193.7/s #
    '07_chain_sm'  => "[% hash.a %]",                                     #   44%  #  390%  #  265%  #  236%  # 5193.7/s #
    '08_mixed_sma' => "".((" "x100)."[% one %]\n")x10,                    #   66%  #  620%  #  310%  #  572%  # 3425.2/s #
    '09_mixed_med' => "".((" "x10)."[% one %]\n")x100,                    #   44%  #  606%  #  221%  #  634%  # 441.4/s #
    '10_str_sma'   => "".("[% \"".(" "x100)."\$one\" %]\n")x10,           #  -35%  # 306525%#  103%  # 311913%# 1275.7/s #
    '11_str_lar'   => "".("[% \"".(" "x10)."\$one\" %]\n")x100,           #  -61%  #  335%  #  -23%  #  334%  # 18.5/s #
    '12_num_lterl' => "[% 2 %]",                                          #  198%  #  745%  #  505%  #  488%  # 10900.5/s #
    '13_plus'      => "[% 1 + 2 %]",                                      #   93%  #  555%  #  357%  #  359%  # 7657.4/s #
    '14_plus_long' => "[% 1 + 2 + 3 + 5 + 6 + 8 %]",                      #   75%  #  395%  #  335%  #  243%  # 6883.1/s #
    '15_chained'   => "[% c.d.0.hee.0 %]",                                #  214%  #  805%  #  489%  #  611%  # 10963.5/s #
    '16_chain_set' => "[% SET c.d.0.hee.0 = 2 %]",                        #  154%  #  615%  #  386%  #  454%  # 7244.8/s #
    '17_chain_lar' => "[% c.d.0.hee.0 %]"x100,                            #   65%  #  637%  #   65%  #  633%  # 265.9/s #
    '18_chain_sl'  => "[% SET c.d.0.hee.0 = 2 %]"x100,                    #   53%  #  384%  #   45%  #  388%  # 85.1/s #
    '19_cplx_comp' => "[% t = 1 || 0 ? 0 : 1 || 2 ? 2 : 3 %][% t %]",     #   73%  #  345%  #  271%  #  236%  # 5934.3/s #
    '20_if_sim_t'  => "[% a=1 %][% IF a %]Two[% END %]",                  #  131%  #  568%  #  371%  #  408%  # 7875.7/s #
    '21_if_sim_f'  => "         [% IF a %]Two[% END %]",                  #  187%  #  741%  #  496%  #  558%  # 9980.9/s #
    '22_if_else'   => "[% IF a %]A[% ELSE %]B[% END %]",                  #  158%  #  646%  #  446%  #  479%  # 8963.3/s #
    '23_if_elsif'  => "[% IF a %]A[% ELSIF b %]B[% ELSE %]C[% END %]",    #  152%  #  617%  #  423%  #  464%  # 8140.5/s #
    '24_for_i_sml' => "[% FOREACH i = [0..10]   ; i ; END %]",            #    3%  #  231%  #  124%  #  149%  # 1443.1/s #
    '25_for_i_med' => "[% FOREACH i = [0..100]  ; i ; END %]",            #  -27%  #   22%  #   -4%  #    1%  # 205.6/s #
    '26_for_sml'   => "[% FOREACH [0..10]       ; i ; END %]",            #   25%  #  279%  #  163%  #  186%  # 1696.2/s #
    '27_for_med'   => "[% FOREACH [0..100]      ; i ; END %]",            #    0%  #   62%  #   29%  #   38%  # 264.7/s #
    '28_while'     => "[% f = 10 %][%WHILE f%][%f=f- 1%][%f%][% END %]",  #  -24%  #  143%  #   34%  #   88%  # 748.3/s #
    '29_whl_set_l' => "[% f = 10; WHILE (g=f) ; f = f - 1 ; f ; END %]",  #  -17%  #  138%  #   37%  #   88%  # 686.3/s #
    '30_whl_set_s' => "[% f = 1;  WHILE (g=f) ; f = f - 1 ; f ; END %]",  #   49%  #  396%  #  216%  #  304%  # 3706.1/s #
    '31_file_proc' => "[% INCLUDE bar.tt %]",                             #  242%  #  643%  #  434%  #  516%  # 6791.5/s #
    '32_file_incl' => "[% INCLUDE baz.tt %]",                             #  167%  #  547%  #  327%  #  420%  # 4744.5/s #
    '33_process'   => "[% BLOCK foo %]Hi[% END %][% PROCESS foo %]",      #  180%  #  672%  #  468%  #  519%  # 7447.2/s #
    '34_include'   => "[% BLOCK foo %]Hi[% END %][% INCLUDE foo %]",      #  153%  #  630%  #  427%  #  476%  # 6369.2/s #
    '35_macro'     => "[% MACRO foo BLOCK %]Hi[% END %][% foo %]",        #   88%  #  471%  #  324%  #  327%  # 5453.1/s #
    '36_macro_arg' => "[% MACRO foo(n) BLOCK %]Hi[%n%][%END%][%foo(2)%]", #   65%  #  354%  #  277%  #  248%  # 4297.2/s #
    '37_macro_pro' => "[% MACRO foo PROCESS bar;BLOCK bar%]7[%END;foo%]", #  101%  #  490%  #  328%  #  386%  # 4317.1/s #
    '38_filter2'   => "[% n = 1 %][% n | repeat(2) %]",                   #  122%  #  538%  #  400%  #  411%  # 6423.6/s #
    '39_filter'    => "[% n = 1 %][% n FILTER repeat(2) %]",              #   79%  #  426%  #  304%  #  294%  # 5427.1/s #
    '40_fltr_name' => "[% n=1; n FILTER echo=repeat(2); n FILTER echo%]", #   20%  #  357%  #  209%  #  261%  # 3330.2/s #
    '41_constant'  => "[% constants.simple %]",                           #  215%  #  729%  #  526%  #  507%  # 12444.0/s #
    '42_perl'      => "[%one='ONE'%][% PERL %]print \"[%one%]\"[%END%]",  #   71%  #  476%  #  297%  #  365%  # 4586.4/s #
    '43_filtervar' => "[% 'hi' | \$filt %]",                              #  126%  #  662%  #  415%  #  461%  # 7728.5/s #
    '44_filteruri' => "[% ' ' | uri %]",                                  #  115%  #  690%  #  402%  #  490%  # 7621.7/s #
    '45_filterevl' => "[% foo | eval %]",                                 #  364%  #  682%  #  553%  #  568%  # 3763.2/s #
    '46_refs'      => "[% b = \\code(1); b(2) %]",                        #   79%  #  358%  #  276%  #  233%  # 5095.3/s #
    '47_capture'   => "[% foo = BLOCK %]Hi[% END %][% foo %]",            #  106%  #  503%  #  335%  #  348%  # 7172.9/s #
    '48_complex'   => "$longer_template",
    # overall                                                             #  74.0  #  443.0 #  243.4 #  334   #
    #                                                                     #  76.7  #  455.0 #  258.4 #  345.1 #
    #                                                                     #  77.5  #  456.7 #  257.2 #  341.2 #
    #                                                                     #  76.7  #  469.9 #  272.4 #  357.7 #
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
    $output .= " "x74;
    $output .= sprintf('#  %3s%%  ', $cumulative{$_}->[2]) foreach sort keys %cumulative;
    $output .= "#\n";
}

print $output;

#print `ls -lR $tt_cache_dir`;
__DATA__
