const fs = require('fs');

const input = fs.readFileSync(0, {encoding: 'utf8'});
const monkeys = input.split('\n\n').map(parseMonkey);

for (let i = 0; i < 20; i++) {
	for (const monkey of monkeys) {
		takeTurn(monkeys, monkey);
	}
}

monkeys.sort(function(a, b) {
	if (a.activity < b.activity) {
		return 1;
	} else if (a.activity === b.activity) {
		return 0;
	} else {
		return -1;
	}
});

console.log(monkeys[0].activity * monkeys[1].activity);

function takeTurn(monkeys, monkey) {
	for (let worryLevel of monkey.items) {
		eval(monkey.operation);
		worryLevel = Math.floor(worryLevel / 3);
		const dest = worryLevel % monkey.test === 0
			? monkey.ifTrue
			: monkey.ifFalse;
		monkeys[dest].items.push(worryLevel);
		monkey.activity++;
	}

	monkey.items = [];
}

function parseMonkey(input) {
	const lines = input.split('\n');
	const items = lines[1]
		.replace('Starting items: ', '')
		.split(', ')
		.map(s => parseInt(s, 10));
	const match = lines[2].match(/old ([\*\+]) ([\d]+|old)/);
	const operation = lines[2]
		.match(/new = old [\*\+] ([\d]+|old)$/)[0]
		.replace('new', 'worryLevel')
		.replace(/old/g, 'worryLevel')
	const test = extractNum(lines[3]);
	const ifTrue = extractNum(lines[4]);
	const ifFalse = extractNum(lines[5]);

	return {items, operation, test, ifTrue, ifFalse, activity: 0};
}

function extractNum(line) {
	return parseInt(line.match(/[\d]+/)[0], 10);
}
