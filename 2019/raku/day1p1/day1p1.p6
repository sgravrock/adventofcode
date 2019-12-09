#!/usr/bin/env perl6
use v6;
use lib '.';
use Input;

sub fuelRequirement($mass) {
	floor($mass / 3) - 2
}

my $total = [+] @puzzle_input.map(&fuelRequirement);

print $total;
print "\n";
