export function init(root) {
	const arena = new Arena();
	new UI(root, arena);
}

// This probably puts me on the naughty list
for (const T of [Array, HTMLCollection]) {
	Object.defineProperty(T.prototype, 'last', {
		get: function() {
			return this[this.length - 1];
		}
	});
}

const ROCK_SHAPES = [
	[['#', '#', '#', '#']],
	[
		['.', '#', '.'],
		['#', '#', '#'],
		['.', '#', '.'],
	],
	[
		['#', '#', '#'],
		['.', '.', '#'],
		['.', '.', '#'],
	],
	[
		['#'],
		['#'],
		['#'],
		['#'],
	],
	[
		['#', '#'],
		['#', '#'],
	],
];

// Advances the simulation one frame at a time, yielding after each frame
function* simulate(arena, jetPattern) {
	let jetI = 0;

	for (let rockI = 0; rockI < 2022; rockI++) {
		arena.addRock(ROCK_SHAPES[rockI % ROCK_SHAPES.length]);
		yield;
		shiftRock(arena, jetPattern[jetI++ % jetPattern.length]);
		yield;

		let falling = true;

		while (falling) {
			falling = arena.tryLowerFallingRock();

			if (falling) {
				yield;
				shiftRock(arena, jetPattern[jetI++ % jetPattern.length]);
			}
		}

		yield;
	}

	function shiftRock(arena, jet) {
		arena.tryShiftFallingRock(jet === '<' ? -1 : 1, 0);
	}
}

export class Arena {
	constructor() {
		this.width = 7;
		this.objects = [
			// Model the floor as a rock. Sides will be treated as a special
			// case both for efficiency (collisions can be checked with just an
			// X range comparison) and simplicity (otherwise we'd have to have
			// infinitely tall rocks).
			new Rock([['#','#','#','#','#','#','#','#']], 0, 0)
		];
		this.fallingRock = null;
	}
	
	rocks() {
		return this.objects.slice(1); // exclude floor
	}

	addRock(shape) {
		this.fallingRock = new Rock(shape, 2, this.height + 3);
		this.objects.push(this.fallingRock);
	}

	get height() {
		// TODO definitely need to cache this
		const topTop = this.objects
			.map(obj => obj.top)
			.reduce((a, b) => a > b ? a : b, 0);
		return topTop + 1;
	}

	tryLowerFallingRock() {
		const r = this.fallingRock;

		// TODO immutable rocks might make this nicer
		r.bottom--;
	
		for (const other of this.objects) {
			if (other !== r && other.collidesWith(r)) {
				// Undo the move
				r.bottom++;

				// Rock has come to rest
				this.fallingRock = null;
				return false;
			}
		}

		return true;
	}

	tryShiftFallingRock(dx) {
		const r = this.fallingRock;

		if (r.left + dx < 0 || r.right + dx > this.width) {
			return;
		}

		// TODO immutable rocks might make this nicer
		r.left += dx;
	
		for (const other of this.objects) {
			if (other !== r && other.collidesWith(r)) {
				// Undo the move
				r.left -= dx;
				return;
			}
		}
	}
}

export class Rock {
	constructor(shape, left, bottom) {
		this.shape = shape;
		this.left = left;
		this.bottom = bottom;
	}

	get top() {
		return this.bottom + this.shape.length - 1;
	}

	get right() {
		return this.left + this.shape[0].length;
	}

	collidesWith(other) {
		// TODO: Use a set to speed this up?
		const ourCells = this.absoluteCells();
		const otherCells = other.absoluteCells();

		for (const ours of this.absoluteCells()) {
			for (const theirs of otherCells) {
				if (theirs.x === ours.x && theirs.y === ours.y) {
					return true;
				}
			}
		}

		return false;
	}

	absoluteCells() {
		const result = [];

		for (let y = 0; y < this.shape.length; y++) {
			for (let x = 0; x < this.shape[y].length; x++) {
				if (this.shape[y][x] === '#') {
					result.push({x: x + this.left, y: y + this.bottom});
				}
			}
		}

		return result;
	}
}

class UI {
	constructor(root, arena, steps) {
		this.arena = arena;
		this.inputEl = root.querySelector('[name="input"]');
		this.statusView = new StatusView(root.querySelector('#status'));
		this.arenaView = new ArenaView(root.querySelector('#tetris'));

		root.querySelector('#run-to-completion').addEventListener('click', () => {
			this.simulate('runToCompletion');
		});

		root.querySelector('#single-step').addEventListener('click', () => {
			this.simulate('singleStep');
		});

		root.querySelector('#single-rock').addEventListener('click', () => {
			this.simulate('singleRock');
		});
	}

	simulate(methodName) {
		if (!this.steps) {
			const input = this.inputEl.value.trim();

			if (input === '') {
				alert('Please provide the puzzle input.');
				return;
			}

			this.steps = simulate(this.arena, input);
		}

		const done = this[methodName]();
		this.arenaView.update(this.arena);
		this.statusView.update(this.arena, done);
	}

	runToCompletion() {
		while (!this.steps.next().done) {}
		return false;
	}

	singleStep() {
		const result = this.steps.next();
		return result.done;
	}

	singleRock() {
		let result;

		do {
			result = this.steps.next();
		} while (this.arena.fallingRock && !result.done);
	}
}

export class ArenaView {
	constructor(table) {
		this.tbody = table.querySelector('tbody');
	}

	update(arena) {
		const height = arena.height;

		while (this.tbody.rows.length < height) {
			const row = document.createElement('tr');

			for (let x = 0; x < arena.width + 2; x++) {
				row.appendChild(document.createElement('td'));
			}

			row.cells[0].textContent = '|';
			row.cells.last.textContent = '|';
			this.tbody.insertBefore(row, this.tbody.firstChild);
		}

		for (let y = 0; y < this.tbody.rows.length - 1; y++) {
			for (let x = 1; x < this.tbody.rows[y].cells.length - 1; x++) {
				this.tbody.rows[y].cells[x].textContent = '.';
			}
		}

		for (const rock of arena.rocks()) {
			for (const a of rock.absoluteCells()) {
				const t = this.arenaToTable(a);
				this.tbody.rows[t.y].cells[t.x].textContent = '#';
			}
		}
	}

	arenaToTable(arenaCoord) {
		return {
			x: arenaCoord.x + 1,
			y: this.tbody.rows.length - arenaCoord.y - 1
		};
	}
}

class StatusView {
	constructor(root) {
		this.overallStatusEl = root.querySelector('#overall-status');
		this.numRocksEl = root.querySelector('#num-rocks');
		this.heightEl = root.querySelector('#height');
	}

	update(arena, done) {
		this.overallStatusEl.textContent = done ? 'done' : 'running';
		this.numRocksEl.textContent = arena.rocks().length;
		this.heightEl.textContent = arena.height - 1;
	}
}
