const {parseInput, solve} = require('../puzzle.js');

describe('puzzle', function() {
	describe('parseInput', function() {
		it('parses the input', function() {
			const input =
				'Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n' +
				'Sensor at x=9, y=16: closest beacon is at x=10, y=16\n' +
				'Sensor at x=13, y=2: closest beacon is at x=15, y=-3\n';
			expect(parseInput(input)).toEqual([
				{x: 2, y: 18, closestBeacon: {x: -2, y: 15}},
				{x: 9, y: 16, closestBeacon: {x: 10, y: 16}},
				{x: 13, y: 2, closestBeacon: {x: 15, y: -3}},
			]);
		});
	});

	describe('solve', function() {
		it('gives the right answer for the sample input', function() {
			const input = 
				'Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n' +
				'Sensor at x=9, y=16: closest beacon is at x=10, y=16\n' +
				'Sensor at x=13, y=2: closest beacon is at x=15, y=3\n' +
				'Sensor at x=12, y=14: closest beacon is at x=10, y=16\n' +
				'Sensor at x=10, y=20: closest beacon is at x=10, y=16\n' +
				'Sensor at x=14, y=17: closest beacon is at x=10, y=16\n' +
				'Sensor at x=8, y=7: closest beacon is at x=2, y=10\n' +
				'Sensor at x=2, y=0: closest beacon is at x=2, y=10\n' +
				'Sensor at x=0, y=11: closest beacon is at x=2, y=10\n' +
				'Sensor at x=20, y=14: closest beacon is at x=25, y=17\n' +
				'Sensor at x=17, y=20: closest beacon is at x=21, y=22\n' +
				'Sensor at x=16, y=7: closest beacon is at x=15, y=3\n' +
				'Sensor at x=14, y=3: closest beacon is at x=15, y=3\n' +
				'Sensor at x=20, y=1: closest beacon is at x=15, y=3\n';
			expect(solve(parseInput(input), 10)).toEqual(26);
		});
	});
});
