struct Point: Hashable {
	let x: Int
	let y: Int
	
	var hashValue: Int { return x ^ y; }
	
	var north: Point { return Point(x: x, y: y - 1) }
	var south: Point { return Point(x: x, y: y + 1) }
	var east: Point { return Point(x: x + 1, y: y) }
	var west: Point { return Point(x: x - 1, y: y) }
	
	static func ==(lhs: Point, rhs: Point) -> Bool {
		return lhs.x == rhs.x && lhs.y == rhs.y
	}
}

class Santa {
	var pos: Point
	
	init() {
		pos = Point(x: 0, y: 0)
	}
}

func deliver(_ input: String) -> Int {
	var visited = Set<Point>()
	let santa = Santa()
	let robosanta = Santa()
	visited.insert(santa.pos)
	var i = 0
	
	for c in input.characters {
		let moving = i % 2 == 0 ? santa : robosanta
		switch(c) {
		case "^": moving.pos = moving.pos.north
		case "v": moving.pos = moving.pos.south
		case "<": moving.pos = moving.pos.west
		case ">": moving.pos = moving.pos.east
		default: return -1
		}
		
		visited.insert(moving.pos)
		i += 1
	}
	
	return visited.count
}
