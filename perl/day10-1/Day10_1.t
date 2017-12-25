use strict;
use warnings;

use Test::More tests => 7;
BEGIN { use_ok('Day10_1', qw(makeHash iterateHash makeListOfLength)) };

sub wrappedIterateHash {
	my ($inputLength, $state) = @_;
	iterateHash($inputLength, $state);
	return $state;
}

is_deeply(makeListOfLength(5), [0, 1, 2, 3, 4], 'makeListOfLength');
is_deeply(
	wrappedIterateHash(3, {list => [0, 1, 2, 3, 4], pos => 0, skipSize => 0}),
	{list => [2, 1, 0, 3, 4], pos => 3, skipSize => 1},
	'iterateHash first'
);
is_deeply(
	wrappedIterateHash(4, {list => [2, 1, 0, 3, 4], pos => 3, skipSize => 1}),
	{list => [4, 3, 0, 1, 2], pos => 3, skipSize => 2},
	'iterateHash second (skip and wrap around)'
);
is_deeply(
	wrappedIterateHash(1, {list => [4, 3, 0, 1, 2], pos => 3, skipSize => 2}),
	{list => [4, 3, 0, 1, 2], pos => 1, skipSize => 3},
	'iterateHash third'
);
is_deeply(
	wrappedIterateHash(5, {list => [4, 3, 0, 1, 2], pos => 1, skipSize => 3}),
	{list => [3, 4, 2, 1, 0], pos => 4, skipSize => 4},
	'iterateHash fourth'
);
is(makeHash(5, [3, 4, 1, 5]), 12, 'makeHash for sample input');
