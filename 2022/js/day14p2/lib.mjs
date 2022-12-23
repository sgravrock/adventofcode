export function parseInput(input) {
	const result = {
		minX: Number.MAX_SAFE_INTEGER,
		maxX: Number.MIN_SAFE_INTEGER,
		minY: 0,
		maxY: Number.MIN_SAFE_INTEGER,
		cells: {},
	};

	for (const line of input.split('\n')) {
		const anchors = line.split(' -> ').map(parseAnchor);
		for (let i = 1; i < anchors.length; i++) {
			if (anchors[i].x === anchors[i - 1].x) {
				addVertical(result, anchors[i].x, anchors[i-1].y, anchors[i].y);
			} else {
				addHorizontal(result, anchors[i].y, anchors[i-1].x, anchors[i].x);
			}
		}
	}

	result.maxY += 2; // room for the floor
	// Allow 1 cell margin left and right
	result.minX--;
	result.maxX++;
	return result;
}

function parseAnchor(s) {
	const tokens = s.split(',');
	return {
		x: parseInt(tokens[0], 10),
		y: parseInt(tokens[1], 10),
	};
}

function addVertical(cave, x, startY, endY) {
	if (startY > endY) {
		const tmp = startY;
		startY = endY;
		endY = tmp;
	}

	for (let y = startY; y <= endY; y++) {
		addRock(cave, x, y);
	}
}

function addHorizontal(cave, y, startX, endX) {
	if (startX > endX) {
		const tmp = startX;
		startX = endX;
		endX = tmp;
	}

	for (let x = startX; x <= endX; x++) {
		addRock(cave, x, y);
	}
}

function addRock(cave, x, y) {
	cave.cells[keyof(x, y)] = 'rock';
	cave.minX = Math.min(cave.minX, x);
	cave.maxX = Math.max(cave.maxX, x);
	cave.minY = Math.min(cave.minY, y);
	cave.maxY = Math.max(cave.maxY, y);
}

export function showCave(cave, table) {
	table.innerHTML = '';
	table.dataset.minX = cave.minX;
	table.dataset.maxX = cave.maxX;

	const header = document.createElement('thead');
	table.appendChild(header);
	const headerRow = document.createElement('tr');
	header.appendChild(headerRow);
	headerRow.appendChild(document.createElement('th'));

	for (let x = cave.minX; x <= cave.maxX; x++) {
		const cell = document.createElement('th');
		cell.textContent = x;
		headerRow.appendChild(cell);
	}

	for (let y = cave.minY; y <= cave.maxY; y++) {
		const row = document.createElement('tr');
		table.appendChild(row);
		const th = document.createElement('th');
		th.scope = 'row';
		th.textContent = y;
		row.appendChild(th);

		for (let x = cave.minX; x <= cave.maxX; x++) {
			const cell = document.createElement('td');
			cell.textContent = textFor(cave, x, y);
			row.appendChild(cell);
		}
	}
}

export function showChanged(cave, table, changed) {
	expand(cave, table);

	for (const {x, y} of changed) {
		const row = table.rows[y + 1 - cave.minY]
		const cell = row.cells[x + 1 - cave.minX]
		cell.textContent = textFor(cave, x, y);
	}
}

function expand(cave, table) {
	const oldMinX = parseInt(table.dataset.minX, 10);
	const oldMaxX = parseInt(table.dataset.maxX, 10);

	for (let x = cave.minX; x < oldMinX; x++) {
		const headerCell = document.createElement('th');
		headerCell.textContent = x;
		table.rows[0].insertBefore(headerCell, table.rows[0].cells[1]);

		for (let j = 1; j < table.rows.length; j++) {
			const y = j - 1;
			const cell = document.createElement('td');
			cell.textContent = textFor(cave, x, y);
			table.rows[j].insertBefore(cell, table.rows[j].cells[1]);
		}
	}

	for (let x = oldMaxX + 1; x <= cave.maxX; x++) {
		const headerCell = document.createElement('th');
		headerCell.textContent = x;
		table.rows[0].appendChild(headerCell);

		for (let j = 1; j < table.rows.length; j++) {
			const y = j - 1;
			const cell = document.createElement('td');
			cell.textContent = textFor(cave, x, y);
			table.rows[j].appendChild(cell);
		}
	}

	table.dataset.minX = cave.minX;
	table.dataset.maxX = cave.maxX;
}

function textFor(cave, x, y) {
	switch(cellAt(cave, x, y)) {
		case 'rock': return '#';
		case 'sand': return 'o';
		default: return '.';
	}
}

export async function run(cave, displayCallback) {
	let n = 0;
	const origin = keyof(500, 0);

	while (cave.cells[origin] !== 'sand') {
		let sandX = 500;
		let sandY = cave.minY;
		let atRest = false;
		let prevSand = null;

		while (!atRest) {
			setCellAt(cave, sandX, sandY, 'sand');
			const changed = [{x: sandX, y: sandY}];
			
			if (prevSand) {
				changed.push(prevSand);
			}

			prevSand = changed[0];
			await displayCallback(changed);
	
			if (!occupied(cave, sandX, sandY + 1)) {
				setCellAt(cave, sandX, sandY, 'air');
				sandY += 1;
			} else if (!occupied(cave, sandX - 1, sandY + 1)) {	
				setCellAt(cave, sandX, sandY, 'air');
				sandX -= 1;
				sandY += 1;
			} else if (!occupied(cave, sandX + 1, sandY + 1)) {	
				setCellAt(cave, sandX, sandY, 'air');
				sandX += 1;
				sandY += 1;
			} else {
				atRest = true;
				n++;
			}
		}
	}

	return n;
}

function keyof(x, y) {
	return x + ',' + y;
}

function cellAt(cave, x, y) {
	if (y === cave.maxY) {
		return 'rock';
	} else {
		return cave.cells[keyof(x, y)];
	}
}

function setCellAt(cave, x, y, v) {
	cave.cells[keyof(x, y)] = v;
	cave.minX = Math.min(cave.minX, x);
	cave.maxX = Math.max(cave.maxX, x);
}

function occupied(cave, x, y) {
	const v = cellAt(cave, x, y);
	return v === 'rock' || v === 'sand';
}
