package Day10_1;

use 5.018002;
use strict;
use warnings;
require Exporter;

our @ISA = qw(Exporter);

our $VERSION = '0.01';

our @EXPORT_OK = qw(makeHash iterateHash makeListOfLength);

sub makeHash {
	my $listLen = shift(@_);
	my $list = makeListOfLength($listLen);
	my $inputLengths = shift(@_);
	my $pos = 0;
	my $skipSize = 0;

	foreach my $inputLength (@$inputLengths) {
		my $result = iterateHash($inputLength, $list, $pos, $skipSize);
		($list, $pos, $skipSize) = @$result;
	}

	return @$list[0] * @$list[1];
}

sub iterateHash {
	my ($inputLength, $list, $curPos, $skipSize) = @_;

	for (my $i = 0; $i < $inputLength / 2; $i++) {
		my $a = ($i + $curPos) % scalar @$list;
		my $b = ($curPos + $inputLength - $i - 1) % scalar @$list;
		(@$list[$a], @$list[$b]) = (@$list[$b], @$list[$a]);
	}

	$curPos = ($curPos + $inputLength + $skipSize) % scalar @$list;
	$skipSize++;
	return [$list, $curPos, $skipSize];
}

sub makeListOfLength {
	my $length = shift(@_);
	my @result = ();

	for (my $i = 0; $i < $length; $i++) {
		push @result, $i;
	}

	return \@result;
}


1;
__END__
