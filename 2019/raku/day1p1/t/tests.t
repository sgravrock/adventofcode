use Test;
use lib 'lib';
use Day1p1;

subtest {
	is fuelRequirement(12), 2, "for 12";
	is fuelRequirement(14), 2, "for 14";
	is fuelRequirement(1969), 654, "for 1969";
	done-testing;
}, "fuelRequirement";

is totalFuelRequirement([12, 14, 1969]), 658, "totalFuelRequirement";

done-testing;
