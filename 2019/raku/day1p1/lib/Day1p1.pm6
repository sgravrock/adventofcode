use v6;
use Input;

sub fuel-requirement($mass) is export {
	floor($mass / 3) - 2
}

sub total-fuel-requirement(@masses) is export {
	[+] @masses.map(&fuel-requirement);
}
