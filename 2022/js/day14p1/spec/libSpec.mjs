import {parseInput} from '../lib.mjs';

describe('lib', function() {
	describe('parseInput', function() {
		it('parses sequences of walls', function() {
			const input = '498,4 -> 498,6 -> 500,6\n' +
				'503,4 -> 504,4 -> 504,6 -> 506,6';
			const actual = parseInput(input);

			expect(actual).toEqual({
				minX: 498,
				maxX: 506,
				minY: 0,
				maxY: 6,
				cells: {
					'498,4': 'rock',
					'498,4': 'rock',
					'498,5': 'rock',

					'498,6': 'rock',
					'499,6': 'rock',
					'500,6': 'rock',

					'503,4': 'rock',
					'504,4': 'rock',

					'504,5': 'rock',
					'504,6': 'rock',
					'505,6': 'rock',

					'506,6': 'rock',
				},
			});
		});

		it('parses descending X sequences', function() {
			expect(parseInput('494,4 -> 492,4').cells).toEqual({
				'492,4': 'rock',
				'493,4': 'rock',
				'494,4': 'rock',
			});
		});

		it('parses descending Y sequences', function() {
			expect(parseInput('494,6 -> 494,4').cells).toEqual({
				'494,4': 'rock',
				'494,5': 'rock',
				'494,6': 'rock',
			});
		});
	});
});
