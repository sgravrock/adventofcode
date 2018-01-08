use strict;
use warnings;

use Test::More tests => 7;
BEGIN { use_ok('Day14_2', qw(numRegions squaresUsed hashForRow hexToBits)) };

is(
	hexToBits('a0c2017'),
	'1010000011000010000000010111',
	'hexToBits'
);
is_deeply(
	hashForRow(0, 'flqrgnkx', 8),
	[1, 1, 0, 1, 0, 1, 0, 0],
	'first 8 from hashForRow 0'
);
is_deeply(
	hashForRow(1, 'flqrgnkx', 8),
	[0, 1, 0, 1, 0, 1, 0, 1],
	'first 8 from hashForRow 1'
);
is_deeply(
	hashForRow(2, 'flqrgnkx', 8),
	[0, 0, 0, 0, 1, 0, 1, 0],
	'first 8 from hashForRow 2'
);
is_deeply(
	squaresUsed('flqrgnkx', 8),
	[
		[1, 1, 0, 1, 0, 1, 0, 0],
		[0, 1, 0, 1, 0, 1, 0, 1],
		[0, 0, 0, 0, 1, 0, 1, 0],
		[1, 0, 1, 0, 1, 1, 0, 1],
		[0, 1, 1, 0, 1, 0, 0, 0],
		[1, 1, 0, 0, 1, 0, 0, 1],
		[0, 1, 0, 0, 0, 1, 0, 0],
		[1, 1, 0, 1, 0, 1, 1, 0]
	],
	'squaresUsed'
);
is(
	numRegions('flqrgnkx'),
	1242,
	'numRegions'
);
