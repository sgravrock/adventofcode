const {loopSize, firstCollision, sourceBankIx, redistributeBlocks, BankSet} = require('../lib.js');

describe('loopSize', function() {
	it('solves [0, 2, 7, 0]', function() {
		expect(loopSize([0, 2, 7, 0])).toEqual(4);
	});
});

describe('firstCollision', function() {
	it('solves [0, 2, 7, 0]', function() {
		expect(firstCollision([0, 2, 7, 0])).toEqual([2, 4, 1, 2]);
	});
});

describe('sourceBankIx', function() {
	it('selects the bank with the most blocks', function() {
		expect(sourceBankIx([0, 2, 7, 0])).toEqual(2);
		expect(sourceBankIx([2, 4, 1, 2])).toEqual(1);
	});

	it('selects the lowest-numbered among baks that tie', function() {
		expect(sourceBankIx([3, 1, 2, 3])).toEqual(0);
	});
});

describe('redistributeBlocks', function() {
	it('redistributes one at a time to the following blocks', function() {
		expect(redistributeBlocks([2, 1, 1, 1])).toEqual([0, 2, 2, 1]);
	});

	it('wraps around from the last bank to the first', function() {
		expect(redistributeBlocks([0, 2, 7, 0])).toEqual([2, 4, 1, 2]);
	});
});

describe('BankSet', function() {
	describe('contains', function() {
		it('returns true if the set contains an array with the same contents', function() {
			const subject = new BankSet();
			subject.add([1, 2, 3]);
			expect(subject.contains([1, 2, 3])).toBe(true);
		});

		it('returns false if the set does not contain an array with the same contents', function() {
			const subject = new BankSet();
			subject.add([1, 2, 4], [1, 2, 3, 4]);
			expect(subject.contains([1, 2, 3])).toBe(false);
		});
	});

	describe('add', function() {
		it('does not add the same element twice', function() {
			const subject = new BankSet();
			subject.add([1]);
			subject.add([2]);
			subject.add([1]);
			expect(subject.size()).toEqual(2);
		});
	});
});
