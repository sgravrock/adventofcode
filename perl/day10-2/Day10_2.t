use strict;
use warnings;

use Test::More tests => 14;
BEGIN { use_ok('Day10_2', qw(makeHash iterateHash makeListOfLength denseHash hexify prepareInputLengths chunkify)) };

sub wrappedIterateHash {
	my ($inputLength, $state) = @_;
	iterateHash($inputLength, $state);
	return $state;
}

is_deeply(
	[prepareInputLengths('1,2,3')],
	[49, 44, 50, 44, 51],
	'prepareInputLengths'
);
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
is_deeply(
	[denseHash([65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22], [65, 27])],
	[64, 90],
	'denseHash'
);
is(
	hexify(64, 7, 255),
	'4007ff',
	'hexify'
);
is(
	makeHash(256, ''),
	'a2582a3a0e66e6e86e3812dcb672a272',
	'makeHash ""'
);
is(
	makeHash(256, 'AoC 2017'),
	'33efeb34ea91902bb2f59c9920caa6cd',
	'makeHash "AoC 2017"'
);
is(
	makeHash(256, '1,2,3'),
	'3efbe78a8d82f29979031a4aa0b16a9d',
	'makeHash "1,2,3"'
);
is(
	makeHash(256, '1,2,4'),
	'63960835bcdc130f0b66d7ff4f6a5a8e',
	'makeHash "1,2,4"'
);
is_deeply(
	[chunkify([1, 2, 3, 4, 5, 6], 3)],
	[[1, 2, 3], [4, 5, 6]],
	'chunkify'
);
