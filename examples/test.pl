#!/usr/bin/perl

use lib qw(.);
use Benchmark qw(timethese cmpthese);

sub f {
    my $g=0;
    foreach (0..$_[0]) { $g = 2.0 * 2.0 * 2.0 * 2.0 };
    $g;
};

$res = timethese(10, { foo => sub {f(1000000)},
                       bar => sub {f(2000000)},
                       baz => sub {f(3000000)}, });
print "\n";
cmpthese $res;

