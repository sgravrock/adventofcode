use strict;
use warnings;

use Test::More tests => 6;
BEGIN { use_ok('Day14_1', qw(numSquaresUsed hashForRow hexToBits)) };

is(
	hexToBits('a0c2017'),
	'1010000011000010000000010111',
	'hexToBits'
);
is(
	substr(hashForRow(0, 'flqrgnkx'), 0, 8),
	'11010100',
	'first 8 from hashForRow 0'
);
is(
	substr(hashForRow(1, 'flqrgnkx'), 0, 8),
	'01010101',
	'first 8 from hashForRow 1'
);
is(
	substr(hashForRow(2, 'flqrgnkx'), 0, 8),
	'00001010',
	'first 8 from hashForRow 2'
);
is(
	numSquaresUsed('flqrgnkx'),
	8108,
	'numSquaresUsed for sample input'
);
