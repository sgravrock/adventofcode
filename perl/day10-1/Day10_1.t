use strict;
use warnings;

use Test::More tests => 7;
BEGIN { use_ok('Day10_1', qw(makeHash iterateHash makeListOfLength)) };

is_deeply(makeListOfLength(5), [0, 1, 2, 3, 4], 'makeListOfLength');
is_deeply(
	iterateHash(3, [0, 1, 2, 3, 4], 0, 0),
	[[2, 1, 0, 3, 4], 3, 1],
	'iterateHash first'
);
is_deeply(
	iterateHash(4, [2, 1, 0, 3, 4], 3, 1),
	[[4, 3, 0, 1, 2], 3, 2],
	'iterateHash second (skip and wrap around)'
);
is_deeply(
	iterateHash(1, [4, 3, 0, 1, 2], 3, 2),
	[[4, 3, 0, 1, 2], 1, 3],
	'iterateHash third'
);
is_deeply(
	iterateHash(5, [4, 3, 0, 1, 2], 1, 3),
	[[3, 4, 2, 1, 0], 4, 4],
	'iterateHash fourth'
);
is(makeHash(5, [3, 4, 1, 5]), 12, 'makeHash for sample input');
