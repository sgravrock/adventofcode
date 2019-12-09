# vim: ft=perl6
use v6;
use Test;
use lib 'lib';
use Machine;

sub check-mem(@program, @expected, $description) {
	my $m = Machine.new(@program);
	$m.execute;
	is $m.mem, @expected, $description;
}


subtest {
	check-mem [1,9,10,3,2,3,11,0,99,30,40,50], [3500,9,10,70,2,3,11,0,99,30,40,50], "basics";
	done-testing;
}, "execute";

done-testing;
