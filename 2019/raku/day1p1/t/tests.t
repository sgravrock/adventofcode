use Test;
use lib 'lib';
use Day1p1;

subtest {
	is fuel-requirement(12), 2, "for 12";
	is fuel-requirement(14), 2, "for 14";
	is fuel-requirement(1969), 654, "for 1969";
	done-testing;
}, "fuel-requirement";

is total-fuel-requirement([12, 14, 1969]), 658, "total-fuel-requirement";

done-testing;
