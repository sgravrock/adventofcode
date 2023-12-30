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

// Advances the simulation one frame at a time, yielding the status of the
// top rock (either 'falling' or 'at rest') after each frame
function* simulate(arena, jetPattern) {
	let jetI = 0;

	for (let rockI = 0; rockI < 2022; rockI++) {
		arena.addRock(ROCK_SHAPES[rockI % ROCK_SHAPES.length]);
		yield 'falling';
		shiftTopRock(arena, jetPattern[jetI++ % jetPattern.length]);
		yield 'falling';

		let falling = true;

		while (falling) {
			falling = arena.tryMoveTopRock(0, -1);

			if (falling) {
				yield 'falling';
				shiftTopRock(arena, jetPattern[jetI++ % jetPattern.length]);
			}
		}

		yield 'at rest';
	}

	function shiftTopRock(arena, jet) {
		arena.tryMoveTopRock(jet === '<' ? -1 : 1, 0);
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
	}
	
	rocks() {
		return this.objects.slice(1); // exclude floor
	}

	addRock(shape) {
		this.objects.push(new Rock(shape, 2, this.height + 3));
	}

	get height() {
		// TODO definitely need to cache this
		const topTop = this.objects
			.map(obj => obj.top)
			.reduce((a, b) => a > b ? a : b, 0);
		return topTop + 1;
	}

	tryMoveTopRock(dx, dy) {
		const topRock = this.objects.last;

		if (topRock.left + dx < 0 || topRock.right + dx > this.width) {
			return false;
		}


		// TODO immutable rocks might make this nicer
		topRock.left += dx;
		topRock.bottom += dy;

		for (const other of this.objects) {
			if (other !== topRock && other.collidesWith(topRock)) {
				// Undo the move
				topRock.left -= dx;
				topRock.bottom -= dy;
				return false;
			}
		}

		return true;
	}
}

export class Rock {
	constructor(shape, left, bottom) {
		this.shape = shape;
		this.left = left;
		this.bottom = bottom;
		this.falling = false;
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
		let result = {};

		while (!(result.done || result.value === 'at rest')) {
			result = this.steps.next();
		}
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
