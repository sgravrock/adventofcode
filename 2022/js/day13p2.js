const fs = require('fs');

const input = fs.readFileSync(0, {encoding: 'utf8'});
const packets = input.split('\n')
	.filter(line => line !== '')
	.map(eval);
const div1 = [[2]], div2 = [[6]];
packets.push(div1);
packets.push(div2);
packets.sort(inOrder);
console.log((packets.indexOf(div1) + 1) * (packets.indexOf(div2) + 1));

function inOrder(left, right) {
	for (let i = 0; i < left.length && i < right.length; i++) {
		if (typeof left[i] === 'number' && typeof right[i] === 'number') {
			if (left[i] < right[i]) {
				return -1;
			} else if (left[i] > right[i]) {
				return 1;
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

			if (subresult !== 0) {
				return subresult;
			}
		}
	}

	if (left.length < right.length) {
		return -1;
	} else if (left.length > right.length) {
		return 1;
	}

	return 0;
}

