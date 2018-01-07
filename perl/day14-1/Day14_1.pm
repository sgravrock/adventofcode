package Day14_1;

use 5.018002;
use strict;
use warnings;
use List::Util qw(reduce);
require Exporter;
use lib qw(../day10-2);
use Day10_2 qw(makeHash);

our @ISA = qw(Exporter);

our $VERSION = '0.01';

our @EXPORT_OK = qw(numSquaresUsed hashForRow hexToBits);

sub numSquaresUsed {
	my ($key) = @_;
	my $result = 0;

	for my $y (0..127) {
		my $hash = hashForRow($y, $key);
		$hash =~ s/0//g;
		$result += length($hash);
	}

	return $result;
}

sub hashForRow {
	my ($y, $key) = @_;
	my @asList = split(//, "${key}-${y}");
	my $hexHash = makeHash(256, "${key}-${y}");
	return hexToBits($hexHash);
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
