const fs = require('fs');

const input = fs.readFileSync(0, {encoding: 'utf8'});
const lines = input.split('\n');
let result = 0;

for (let i = 0; i * 3 < lines.length; i ++) {
	const left = eval(lines[i * 3]);
	const right = eval(lines[i * 3 + 1]);

	if (inOrder(left, right)) {
		console.log(i + 1, 'is in order');
		result += i + 1;
	} else {
		console.log(i + 1, 'is out of order');
	}
}

console.log('Result: ', result);

function inOrder(left, right) {
	for (let i = 0; i < left.length && i < right.length; i++) {
		if (typeof left[i] === 'number' && typeof right[i] === 'number') {
			if (left[i] < right[i]) {
				return true;
			} else if (left[i] > right[i]) {
				console.log('ooo on numbers');
				return false;
			}
		} else {
			let subresult;

			if (typeof left[i] === 'number') {
				subresult = inOrder([left[i]], right[i]);
			} else if (typeof right[i] === 'number') {
				subresult = inOrder(left[i], [right[i]]);
			} else {
				subresult = inOrder(left[i], right[i]);
			}

			if (subresult !== undefined) {
				return subresult;
			}
		}
	}

	if (left.length < right.length) {
		return true;
	} else if (left.length > right.length) {
		return false;
	}
}

