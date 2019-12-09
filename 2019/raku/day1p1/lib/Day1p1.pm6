use v6;
use Input;

sub fuelRequirement($mass) is export {
	floor($mass / 3) - 2
}

sub totalFuelRequirement(@masses) is export {
	[+] @masses.map(&fuelRequirement);
}
