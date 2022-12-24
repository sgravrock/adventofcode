function parseInput(input) {
	return input.split('\n')
		.filter(line => line !== '')
		.map(line => {
			const match = line.match(/x=([\d]+), y=([\d]+): closest beacon is at x=(-?[\d]+), y=(-?[\d]+)/);

			if (!match) {
				throw new Error('Parse error: ' + line);
			}

			const result = {
				x: parseInt(match[1], 10),
				y: parseInt(match[2], 10),
				closestBeacon: {
					x: parseInt(match[3], 10),
					y: parseInt(match[4], 10),
				}
			};
			result.dist = manhattanDist(result, result.closestBeacon);
			return result;
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
		if (x === s.x && y === s.y || manhattanDist(s, {x, y}) <= s.dist) {
			return false;
		}
	}

	return true;
}

function findXBounds(sensors) {
	let min = Number.MAX_SAFE_INTEGER;
	let max = Number.MIN_SAFE_INTEGER;

	for (const s of sensors) {
		min = Math.min(min, s.x - s.dist);
		max = Math.max(max, s.x + s.dist);
	}

	return {min, max};
}

function manhattanDist(a, b) {
	return Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
}

module.exports = {parseInput, solve};
