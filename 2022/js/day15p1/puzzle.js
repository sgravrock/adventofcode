function parseInput(input) {
	return input.split('\n')
		.filter(line => line !== '')
		.map(line => {
			const match = line.match(/x=([\d]+), y=([\d]+): closest beacon is at x=(-?[\d]+), y=(-?[\d]+)/);

			if (!match) {
				throw new Error('Parse error: ' + line);
			}

			return {
				x: parseInt(match[1], 10),
				y: parseInt(match[2], 10),
				closestBeacon: {
					x: parseInt(match[3], 10),
					y: parseInt(match[4], 10),
				}
			};
		});
}

function solve(sensors, rowToCount) {
	const xBounds = findXBounds(sensors);
	let n = 0;

	for (let x = xBounds.min; x <= xBounds.max; x++) {
		if (!couldHaveBeacon(x, rowToCount, sensors)) {
			n++;
		}
	}

	return n;
}

function couldHaveBeacon(x, y, sensors) {
	for (const s of sensors) {
		if (x === s.closestBeacon.x && y === s.closestBeacon.y) {
			return true;
		}
	}

	for (const s of sensors) {
		if (x === s.x && y === s.y) {
			return false;
		}

		const exclusionDist = manhattanDist(s, s.closestBeacon);


		if (manhattanDist(s, {x, y}) <= exclusionDist) {
			return false;
		}
	}

	return true;
}

function findXBounds(sensors) {
	let min = Number.MAX_SAFE_INTEGER;
	let max = Number.MIN_SAFE_INTEGER;

	for (const s of sensors) {
		// TODO cache d
		const d = manhattanDist(s, s.closestBeacon);
		min = Math.min(min, s.x - d);
		max = Math.max(max, s.x + d);
	}

	return {min, max};
}

function manhattanDist(a, b) {
	return Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
}

module.exports = {parseInput, solve};
