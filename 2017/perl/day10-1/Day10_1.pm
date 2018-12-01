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
	my $inputLengths = shift(@_);
	my $list = makeListOfLength($listLen);
	my %state = (
		list => $list,
		pos => 0,
		skipSize => 0
	);

	foreach my $inputLength (@$inputLengths) {
		iterateHash($inputLength, \%state);
	}

	return @$list[0] * @$list[1];
}

sub iterateHash {
	my ($inputLength, $state) = @_;
	my $list = $state->{list};

	for (my $i = 0; $i < $inputLength / 2; $i++) {
		my $a = ($i + $state->{pos}) % scalar @$list;
		my $b = ($state->{pos} + $inputLength - $i - 1) % scalar @$list;
		(@$list[$a], @$list[$b]) = (@$list[$b], @$list[$a]);
	}

	$state->{pos} = ($state->{pos} + $inputLength + $state->{skipSize}) % scalar @$list;
	$state->{skipSize}++;
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
