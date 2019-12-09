#!/usr/bin/env perl6
use v6;
use lib 'lib';
use Input;
use Machine;
use Debugger;

# Should fail on 98.
my $machine = Machine.new([1,5,6,7,98,1,2,3]);

try {
	$machine.execute();
}

if $! {
	say $!.message;
	debug $machine;
}
