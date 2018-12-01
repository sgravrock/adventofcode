package Day14_2;

use 5.018002;
use strict;
use warnings;
use List::Util qw(reduce);
require Exporter;
use lib qw(../day10-2);
use Day10_2 qw(makeHash);

our @ISA = qw(Exporter);

our $VERSION = '0.01';

our @EXPORT_OK = qw(numRegions squaresUsed hashForRow hexToBits);

sub numRegions {
	my ($key) = @_;
	my $grid = squaresUsed($key, 128);
	my $result = 0;

	foreach my $y (0..127) {
		foreach my $x (0..127) {
			if ($grid->[$y][$x] == 1) {
				$result++;
				consumeGroup($grid, $y, $x);
			}
		}
	}

	return $result;
}

sub consumeGroup {
	my ($grid, $y, $x) = @_;
	my @queue = ([$y, $x]);

	while (scalar @queue > 0) {
		my $c = shift @queue;
		($y, $x) = @$c;

		if ($grid->[$y][$x] == 1) {
			$grid->[$y][$x] = 0;
			push @queue, [$y - 1, $x] if $y > 0;
			push @queue, [$y, $x - 1] if $x > 0;
			push @queue, [$y + 1, $x] if $y < 127;
			push @queue, [$y, $x + 1] if $x < 127;
		}
	}
}

sub squaresUsed {
	my ($key, $size) = @_;
	my @result = map { hashForRow($_, $key, $size) } (0..$size - 1);
	return \@result;
}

sub hashForRow {
	my ($y, $key, $size) = @_;
	my @asList = split(//, "${key}-${y}");
	my $hexHash = makeHash(256, "${key}-${y}");
	my $s = substr(hexToBits($hexHash), 0, $size);
	my @a = split(//, $s);
	return \@a;
}

sub hexToBits {
	my ($hexString) = @_;
	my $result = '';
	foreach my $c (split //, $hexString) {
		$result .= hexDigitToBits($c);
	}

	return $result;
}

sub hexDigitToBits {
	my ($c) = @_;
	my %lookup = (
		'0' => '0000',
		'1' => '0001',
		'2' => '0010',
		'3' => '0011',
		'4' => '0100',
		'5' => '0101',
		'6' => '0110',
		'7' => '0111',
		'8' => '1000',
		'9' => '1001',
		'a' => '1010',
		'b' => '1011',
		'c' => '1100',
		'd' => '1101',
		'e' => '1110',
		'f' => '1111'
	);
	return $lookup{$c};
}


1;
__END__
