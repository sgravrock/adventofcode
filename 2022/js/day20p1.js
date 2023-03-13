const fs = require('fs');

const originalOrder = fs.readFileSync(0, {encoding: 'utf8'})
	.trim()
	.split('\n')
	.map(line => ({n: parseInt(line, 10)}));
const list = toList(originalOrder);
mix(originalOrder, list);
printList(list);
const a = find(list.head, 0);
const b = advance(a, 1000);
const c = advance(b, 1000);
const d = advance(c, 1000);
console.log(b.n, c.n, d.n);
console.log(b.n + c.n + d.n);


function toList(arr) {
	for (let i = 0; i < arr.length - 1; i++) {
		arr[i].next = arr[i + 1];
	}

	for (let i = 1; i < arr.length; i++) {
		arr[i].prev = arr[i - 1];
	}

	arr[0].prev = arr[arr.length - 1];
	arr[arr.length - 1].next = arr[0];
	return {head: arr[0]};
}

function toArr(list) {
	const result = [];
	let node = list.head;

	do {
		result.push(node);
		node = node.next;
	} while (node !== list.head);

	return result;
}

function mix(originalOrder, list) {
	printList(list);
	for (const toMove of originalOrder) {
		if (toMove.n !== 0) {
			let newPrev = toMove.prev;
			toMove.next.prev = newPrev;
			newPrev.next = toMove.next;

			let n = Math.abs(toMove.n);
			let positive = toMove.n > 0;

			if (list.head === toMove) {
				list.head = toMove.next;
			}

			for (let i = 0; i < n; i++) {
				newPrev = positive ? newPrev.next : newPrev.prev;
			}

			let newNext = newPrev.next;
			newPrev.next = toMove;
			newNext.prev = toMove;
			toMove.prev = newPrev;
			toMove.next = newNext;
		}
	}
}

function find(start, nToFind) {
	let p = start;

	while (p.n !== nToFind) {
		p = p.next;
	}

	return p;
}

function advance(start, nMoves) {
	let p = start;

	for (let i = 0; i < nMoves; i++) {
		p = p.next;
	}

	return p;
}

function printList(list) {
	console.log(toArr(list).map(node => node.n).join(', '));
}
