const fs = require('fs');

const decryptionKey = 811589153;

const originalOrder = fs.readFileSync(0, {encoding: 'utf8'})
	.trim()
	.split('\n')
	.map(line => ({n: parseInt(line, 10)}));
const list = toList(originalOrder);
checkRanges(list);

const decryptionMultiplier = decryptionKey % (list.sz - 1);
console.log('decryptionMultiplier:', decryptionMultiplier);
let np = list.head;
let once = false;
do {
	let n = Math.abs(np.n) % (list.sz - 1);
	checkRange(n);
	n *= decryptionMultiplier;
	checkRange(n);
	np.mixOffset = n;
	np = np.next;
} while (np !== list.head);

for (let i = 0; i < 10; i++) {
	console.log('mixing');
	mix(originalOrder, list);
}

//printList(list);
const a = find(list.head, 0);
const b = advance(a, 1000 % list.sz);
const c = advance(b, 1000 % list.sz);
const d = advance(c, 1000 % list.sz);
//console.log(b.n, c.n, d.n);
console.log((b.n + c.n + d.n) * decryptionKey);


function toList(arr) {
	for (let i = 0; i < arr.length - 1; i++) {
		arr[i].next = arr[i + 1];
	}

	for (let i = 1; i < arr.length; i++) {
		arr[i].prev = arr[i - 1];
	}

	arr[0].prev = arr[arr.length - 1];
	arr[arr.length - 1].next = arr[0];
	return {
		head: arr[0],
		sz: arr.length
	};
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
	//printList(list);

	for (const toMove of originalOrder) {
		//console.log("mixing", ++i);
		if (toMove.n !== 0) {
			let newPrev = toMove.prev;
			toMove.next.prev = newPrev;
			newPrev.next = toMove.next;

			let positive = toMove.n > 0;

			if (list.head === toMove) {
				list.head = toMove.next;
			}

			for (let i = 0; i < toMove.mixOffset; i++) {
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

function checkRanges(list) {
	let np = list.head;

	do {
		checkRange(np.n);
		np = np.next;
	} while (np !== list.head);
}

function checkRange(n) {
	if (n > 2147483647 || n < -2147483648) {
		throw new Error(`Value won't fit in a Pascal longint: ${n}`);
	}
}
