import {Arena, Rock} from '../lib.mjs';

describe('Arena', function() {
	it('places the first new rock above the base', function() {
		const subject = new Arena();

		subject.addRock([['#','#'],['#','#']]);

		expect(subject.rocks()[0].bottom).withContext('rock bottom').toEqual(4);
		expect(subject.rocks()[0].left).withContext('rock left').toEqual(2);
		expect(subject.height).withContext('arena height').toEqual(6);
	});

	it('places an additional rock above the top rock', function() {
		const subject = new Arena();
		subject.addRock([['#'], ['#']]);

		subject.addRock([['#','#'],['#','#']]);

		expect(subject.rocks()[1].bottom).withContext('rock bottom').toEqual(9);
		expect(subject.rocks()[1].left).withContext('rock left').toEqual(2);
		expect(subject.height).withContext('arena height').toEqual(11);
	});
});
