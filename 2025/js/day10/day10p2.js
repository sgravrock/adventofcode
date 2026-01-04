import fs from 'fs';
import { init } from 'z3-solver';

function parse(input) {
	return input.split("\n")
		.filter(function(line) { return line !== '' })
		.map(function(line) {
			const chunks = line
				.replace(/^.*\] /, '') // strip leading lighting diagram
				.split(' ');
			const joltageRequirements = chunks.pop()
				.replace(/[\{\}]/g, '')
				.split(',')
				.map(parseFloat);
			const buttons = chunks.map(function(chunk) {
				return chunk.replace(/[\(\)]/g, '')
					.split(',')
					.map(parseFloat);
			});
			
			return {buttons, joltageRequirements};
		});
}

async function solve(machines) {
	const {Context} = await init();
	const ctx = new Context('main');
	let total = 0;
	
	for (let i = 0; i < machines.length; i++) {
		const mr = await solveMachine(machines[i], ctx);		
		console.log(`Result for machine ${i}: ${mr}`);
		total += mr;
	}
	
	return total;
}

async function solveMachine(machine, ctx) {
	// Z3's high-level JS API is a trifle under-documented, and usage errors
	// tend to result in "Z3AssertionError: Assertion failed" with no other
	// details other than a stack trace.
	//
	// Useful breadcrumbs:
	// * https://fletcheaston.com/software/packages/z3-solver/ (including the
	//   example linked at the very end)
	// * The source code: https://github.com/Z3Prover/z3/tree/master/src/api/js
	//   (although the build product in node_modules is sometimes more uesful)
	// * Although the Ruby bindings are pretty different in some ways, they
	//   can be a source of useful clues: https://github.com/taw/z3
	//
	// Working in TypeScript with a TypeScript-aware editor might help too.
	//
	// Note that z3 doesn't follow the convention that capitalized functions
	// are constructors. Some are and some aren't.
	
	const {Optimize, Int, Sum} = ctx;
	const optimizer = new Optimize();
	const buttons = [];
	
	for (let i = 0; i < machine.buttons.length; i++) {
		let presses = Int.const(`btn${i}presses`);
		optimizer.add(presses.ge(0)); // can't press a button negative times
		buttons.push({
			presses,
			counterIndices: machine.buttons[i]
		});
	}
	
	for (let i = 0; i < machine.joltageRequirements.length; i++) {
		const matchingBtnPresses = buttons
			.filter(btn => btn.counterIndices.includes(i))
			.map(btn => btn.presses);
		// Joltage must add up to the requirement
		optimizer.add(
			Sum(...matchingBtnPresses).eq(machine.joltageRequirements[i])
		);
	}
	
	const allPresses = buttons.map(btn => btn.presses);
	optimizer.minimize(Sum(...allPresses));
	
	const result = await optimizer.check();
	
	if (result !== 'sat') {
		throw new Error(`Expected result "sat" but got ${result}`);
	}
	
	const model = optimizer.model();
	return buttons
		.map(btn => parseInt(model.get(btn.presses).toString(), 10))
		.reduce((a, b) => a + b, 0);
}


const input = fs.readFileSync(process.stdin.fd, {encoding: 'utf-8'});
console.log(await solve(parse(input)));