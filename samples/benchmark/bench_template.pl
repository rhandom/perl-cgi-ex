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

#use CGI::Ex::Template::XS;
#my $tt1 = CGI::Ex::Template::XS->new(@config1);
my $tt1 = Template->new(@config1);

my $cet = CGI::Ex::Template->new(@config1, compile_perl => 1);

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

my $hello2000 = "<html><head><title>[% title %]</title></head><body>
[% array = [ \"Hello\", \"World\", \"2000\", \"Hello\", \"World\", \"2000\" ] %]
[% sorted = array.sort %]
[% multi = [ sorted, sorted, sorted, sorted, sorted ] %]
<table>
[% FOREACH row = multi %]
  <tr bgcolor=\"[% loop.count % 2 ? 'gray' : 'white' %]\">
  [% FOREACH col = row %]
    <td align=\"center\"><font size=\"+1\">[% col %]</font></td>
  [% END %]
  </tr>
[% END %]
</table>
[% param = integer %]
[% FOREACH i = [ 1 .. 10 ] %]
  [% var = i + param %]"
  .("\n  [%var%] Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World Hello World <br/>"x20)."
[% END %]
</body></html>
";

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
    '01_empty'     => "",                                                 #  231%  #  571%  #  310%  #  431%  # 20798.0/s #
    '02_var_sma'   => "[% one %]",                                        #  162%  #  531%  #  409%  #  436%  # 14964.9/s #
    '03_var_lar'   => "[% one %]"x100,                                    #   22%  #  338%  #   63%  #  331%  # 948.8/s #
    '04_set_sma'   => "[% SET one = 2 %]",                                #  160%  #  478%  #  391%  #  370%  # 14835.7/s #
    '05_set_lar'   => "[% SET one = 2 %]"x100,                            #   12%  #  280%  #   28%  #  272%  # 919.7/s #
    '06_set_range' => "[% SET one = [0..30] %]",                          #   42%  #  289%  #  230%  #  192%  # 7909.3/s #
    '07_chain_sm'  => "[% hash.a %]",                                     #  163%  #  551%  #  397%  #  450%  # 13791.3/s #
    '08_mixed_sma' => "".((" "x100)."[% one %]\n")x10,                    #   72%  #  467%  #  234%  #  440%  # 5941.1/s #
    '09_mixed_med' => "".((" "x10)."[% one %]\n")x100,                    #   17%  #  416%  #   99%  #  394%  # 879.7/s #
    '10_str_sma'   => "".("[% \"".(" "x100)."\$one\" %]\n")x10,           #  -12%  #  1391%  #   96%  #  1448%  # 2939.5/s #
    '11_str_lar'   => "".("[% \"".(" "x10)."\$one\" %]\n")x100,           #  -50%  #  303%  #   -1%  #  303%  # 365.3/s #
    '12_num_lterl' => "[% 2 %]",                                          #  170%  #  534%  #  430%  #  422%  # 16592.1/s #
    '13_plus'      => "[% 1 + 2 %]",                                      #  116%  #  426%  #  351%  #  311%  # 13151.4/s #
    '14_chained'   => "[% c.d.0.hee.0 %]",                                #  168%  #  567%  #  390%  #  486%  # 14451.2/s #
    '15_chain_set' => "[% SET c.d.0.hee.0 = 2 %]",                        #  153%  #  465%  #  337%  #  389%  # 11123.9/s #
    '16_chain_lar' => "[% c.d.0.hee.0 %]"x100,                            #   58%  #  468%  #   74%  #  465%  # 828.2/s #
    '17_chain_sl'  => "[% SET c.d.0.hee.0 = 2 %]"x100,                    #  111%  #  343%  #   85%  #  346%  # 367.4/s #
    '18_cplx_comp' => "[% t = 1 || 0 ? 0 : 1 || 2 ? 2 : 3 %][% t %]",     #   81%  #  254%  #  253%  #  188%  # 9677.4/s #
    '19_if_sim_t'  => "[% a=1 %][% IF a %]Two[% END %]",                  #  119%  #  428%  #  316%  #  352%  # 11600.5/s #
    '20_if_sim_f'  => "         [% IF a %]Two[% END %]",                  #  163%  #  536%  #  398%  #  459%  # 14693.3/s #
    '21_if_else'   => "[% IF a %]A[% ELSE %]B[% END %]",                  #  139%  #  483%  #  363%  #  393%  # 13480.3/s #
    '22_if_elsif'  => "[% IF a %]A[% ELSIF b %]B[% ELSE %]C[% END %]",    #  133%  #  453%  #  334%  #  379%  # 12151.0/s #
    '23_for_i_sml' => "[% FOREACH i = [0..10]   ; i ; END %]",            #   12%  #  197%  #  131%  #  140%  # 2497.6/s #
    '24_for_i_med' => "[% FOREACH i = [0..100]  ; i ; END %]",            #  -23%  #   21%  #    0%  #    5%  # 357.3/s #
    '25_for_sml'   => "[% FOREACH [0..10]       ; i ; END %]",            #   23%  #  220%  #  151%  #  160%  # 2670.6/s #
    '26_for_med'   => "[% FOREACH [0..100]      ; i ; END %]",            #   -5%  #   41%  #   19%  #   24%  # 404.5/s #
    '27_while'     => "[% f = 10 %][%WHILE f%][%f=f- 1%][%f%][% END %]",  #    0%  #  161%  #   65%  #  120%  # 1604.2/s #
    '28_whl_set_l' => "[% f = 10; WHILE (g=f) ; f = f - 1 ; f ; END %]",  #   -3%  #  128%  #   50%  #   91%  # 1285.6/s #
    '29_whl_set_s' => "[% f = 1;  WHILE (g=f) ; f = f - 1 ; f ; END %]",  #   51%  #  287%  #  196%  #  227%  # 5914.2/s #
    '30_file_proc' => "[% PROCESS bar.tt %]",                             #  231%  #  492%  #  370%  #  468%  # 10900.5/s #
    '31_file_incl' => "[% INCLUDE baz.tt %]",                             #  150%  #  403%  #  278%  #  335%  # 6915.6/s #
    '32_process'   => "[% BLOCK foo %]Hi[% END %][% PROCESS foo %]",      #  159%  #  519%  #  396%  #  463%  # 10647.0/s #
    '33_include'   => "[% BLOCK foo %]Hi[% END %][% INCLUDE foo %]",      #  137%  #  491%  #  367%  #  424%  # 9087.9/s #
    '34_macro'     => "[% MACRO foo BLOCK %]Hi[% END %][% foo %]",        #   76%  #  364%  #  276%  #  285%  # 7838.4/s #
    '35_macro_arg' => "[% MACRO foo(n) BLOCK %]Hi[%n%][%END%][%foo(2)%]", #   64%  #  263%  #  251%  #  200%  # 6532.9/s #
    '36_macro_pro' => "[% MACRO foo PROCESS bar;BLOCK bar%]7[%END;foo%]", #   95%  #  393%  #  300%  #  333%  # 6369.2/s #
    '37_filter2'   => "[% n = 1 %][% n | repeat(2) %]",                   #  129%  #  394%  #  342%  #  313%  # 10703.2/s #
    '38_filter'    => "[% n = 1 %][% n FILTER repeat(2) %]",              #   90%  #  322%  #  286%  #  245%  # 8865.2/s #
    '39_fltr_name' => "[% n=1; n FILTER echo=repeat(2); n FILTER echo%]", #   36%  #  284%  #  211%  #  229%  # 5824.9/s #
    '40_constant'  => "[% constants.simple %]",                           #  174%  #  515%  #  435%  #  425%  # 16588.0/s #
    '41_perl'      => "[%one='ONE'%][% PERL %]print \"[%one%]\"[%END%]",  #   62%  #  403%  #  278%  #  332%  # 6885.4/s #
    '42_filtervar' => "[% 'hi' | \$filt %]",                              #   95%  #  454%  #  328%  #  370%  # 10167.3/s #
    '43_filteruri' => "[% ' ' | uri %]",                                  #  132%  #  550%  #  379%  #  471%  # 12524.4/s #
    '44_filterevl' => "[% foo | eval %]",                                 #  303%  #  530%  #  434%  #  478%  # 5475.5/s #
    '45_capture'   => "[% foo = BLOCK %]Hi[% END %][% foo %]",            #  102%  #  386%  #  291%  #  304%  # 10606.5/s #
    '46_refs'      => "[% b = \\code(1); b(2) %]",                        #   60%  #  270%  #  239%  #  174%  # 6451.9/s #
    '47_complex'   => "$longer_template",                                 #   60%  #  290%  #  160%  #  270%  # 1054.3/s #
    '48_hello2000' => "$hello2000",                                       #    2%  #  136%  #   39%  #  115%  # 184.8/s #
    # overall                                                             #   95%  #  406%  #  251%  #  346%  #


    # With Stash::XS
    #'46_complex'   => "$longer_template",                                 #   -4%  #  274%  #   93%  #  228%  # 1201.9/s #
    ## overall                                                             #   30%  #  377%  #  211%  #  317%  #
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
