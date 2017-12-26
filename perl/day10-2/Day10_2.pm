package Day10_2;

use 5.018002;
use strict;
use warnings;
use List::Util qw(reduce);
use Carp::Assert;
require Exporter;

our @ISA = qw(Exporter);

our $VERSION = '0.01';

our @EXPORT_OK = qw(makeHash iterateHash makeListOfLength denseHash hexify prepareInputLengths chunkify);

sub makeHash {
	my $listLen = shift(@_);
	my @inputLengths = prepareInputLengths(shift(@_));
	push @inputLengths, (17, 31, 73, 47, 23);
	my $list = makeListOfLength($listLen);
	my %state = (
		list => $list,
		pos => 0,
		skipSize => 0
	);

	foreach (1..64) {
		foreach my $inputLength (@inputLengths) {
			iterateHash($inputLength, \%state);
		}
	}

	return hexify(denseHash(chunkify($list, 16)));
}

sub prepareInputLengths {
	my $inputLengths = shift(@_);
	return map(ord, split('', $inputLengths));
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

sub denseHash {
	my @sparseHashes = @_;
	use Data::Dumper;
	print 'denseHash got ' . scalar @_ . " chunks\n";
	for my $arg (@_) {
		my $r = ref($arg);
		assert $r eq 'ARRAY', "Expected array ref but got ref == $r";
	}
	print "==== denseHash args =====\n";
	print Dumper(@_);
	print "==== /denseHash args =====\n";
	my @result = map { 
		#print "==== denseHash is going to pass this thing to xorAll =====\n";
		#print Dumper($_);
		#print "==== /denseHash is going to pass this thing to xorAll =====\n";
		assert ref $_ eq 'ARRAY';
		xorAll(@$_)
	} @sparseHashes;
	print 'denseHash returning ' . scalar @result . " chunks\n";
	print "==== denseHash ret =====\n";
	print Dumper(@result);
	print "==== /denseHash ret =====\n";
	return @result;
}

sub hexify {
	my @digits = map { sprintf('%.2x', $_) } @_;
	return join('', @digits);
}

sub xorAll {
	print "==== xorAll args ====\n";
	print Dumper(@_);
	print "==== /xorAll args ====\n";
	for my $arg (@_) {
		my $r = ref($arg);
		assert !$r, "Expected a scalar but got ref to $r";
	}
	return reduce { $a ^ $b } 0, @_;
}

sub makeListOfLength {
	my $length = shift(@_);
	my @result = ();

	for (my $i = 0; $i < $length; $i++) {
		push @result, $i;
	}

	return \@result;
}

sub chunkify {
	my $input = shift(@_);
	my $size = shift(@_);
	my $current = [];
	my @result = ($current);

	foreach my $x (@$input) {
		if (scalar @$current == $size) {
			$current = [];
			push @result, $current;
		}

		push $current, $x;
	}

	return @result;
}


1;
__END__
