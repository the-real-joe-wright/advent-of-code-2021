#!/usr/bin/perl
use strict;
use warnings;
use v5.22;

my $total = 0;
while (my $l = <>) {
    chomp $l;
    $total += count1478((split(/\| /, $l))[1]);
}
say "1478 count: $total";

sub count1478 {
    my @data = split(/ /, shift);
    my $ocount = 0;
    foreach my $d (@data) {
        my $len = length($d);
        # '1' == 2; '4' == 4; '7' == 3; '8' == 7
        $ocount++ if ($len == 2 || $len == 4 || $len == 7 || $len == 3);
    }
    return $ocount;
}
