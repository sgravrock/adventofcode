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
		const topTop = this.objects
			.map(obj => obj.top)
			.reduce((a, b) => a > b ? a : b, 0);
		return topTop + 1;
	}

	tryLowerFallingRock() {
		if (!this.#tryMoveFallingRock(0, -1)) {
			// Rock has come to rest
			this.fallingRock = null;
			return false;
		}

		return true;
	}

	tryShiftFallingRock(dx) {
		if (this.fallingRock.left + dx < 0
				|| this.fallingRock.right + dx > this.width) {
			return;
		}

		this.#tryMoveFallingRock(dx, 0);
	}

	#tryMoveFallingRock(dx, dy) {
		const moved = this.fallingRock.moved(dx, dy);

		for (const other of this.objects) {
			if (other !== this.fallingRock && other.collidesWith(moved)) {
				return false;
			}
		}

		this.objects[this.objects.indexOf(this.fallingRock)] = moved;
		this.fallingRock = moved;
		return true;
	}
}

export class Rock {
	#left;
	#bottom;
	
	constructor(shape, left, bottom) {
		this.shape = shape;
		this.#left = left;
		this.#bottom = bottom;
	}

	get left() {
		return this.#left;
	}

	get bottom() {
		return this.#bottom;
	}

	get top() {
		return this.#bottom + this.shape.length - 1;
	}

	get right() {
		return this.#left + this.shape[0].length;
	}

	moved(dx, dy) {
		return new Rock(this.shape, this.#left + dx, this.#bottom + dy);
	}

	collidesWith(other) {
		// Bail out early if the bounding boxes don't overlap vertically.
		// This is a solid 95% speed-up.
		// Don't check for horizontal overlap. That's so common that
		// checking for it actually slows things down.
		if (other.top < this.bottom || other.bottom > this.top) {
			return false;
		}

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
		return true;
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

		return result.done;
	}
}

export class ArenaView {
	constructor(table) {
		this.tbody = table.querySelector('tbody');
	}

	update(arena) {
		const height = arena.height;
		// Reading the number of rows or cells forces layout if the DOM is dirty,
		// so only do it before we start mutating.
		const nRowsToAdd = height - this.tbody.rows.length;
		const tableWidth = this.tbody.rows[0].cells.length;
		const occupied = new Set();

		for (const rock of arena.rocks()) {
			for (const a of rock.absoluteCells()) {
				// Convert from arena coords to table coords
				const tx = a.x + 1;
				const ty = height - a.y - 1;
				occupied.add(ty * tableWidth + tx);
			}
		}

		// Add new rows if the table isn't as tall as the arena
		for (let i = 0; i < nRowsToAdd; i++) {
			const row = document.createElement('tr');

			for (let x = 0; x < arena.width + 2; x++) {
				row.appendChild(document.createElement('td'));
			}

			row.cells[0].textContent = '|';
			row.cells.last.textContent = '|';
			this.tbody.insertBefore(row, this.tbody.firstChild);
		}

		// Update cell contents
		for (let y = 0; y < height - 1; y++) {
			for (let x = 1; x < tableWidth - 1; x++) {
				const cell = this.tbody.rows[y].cells[x];

				if (occupied.has(y * tableWidth + x)) {
					cell.textContent = '#';
				} else {
					cell.textContent = '.';
				}
			}
		}
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
