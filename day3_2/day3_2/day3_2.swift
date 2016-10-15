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

func deliver(_ input: String) -> Int {
	var visited = Set<Point>()
	var santa = Point(x: 0, y: 0)
	var robosanta = Point(x: 0, y: 0)
	visited.insert(santa)
	var i = 0
	
	for c in input.characters {
		var pos = i % 2 == 0 ? santa : robosanta
		switch(c) {
		case "^": pos = pos.north
		case "v": pos = pos.south
		case "<": pos = pos.west
		case ">": pos = pos.east
		default: return -1
		}
		
		visited.insert(pos)
		
		if i % 2 == 0 {
			santa = pos
		} else {
			robosanta = pos
		}
		
		i += 1
	}
	
	return visited.count
}
