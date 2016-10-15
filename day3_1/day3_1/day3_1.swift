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
	var position = Point(x: 0, y: 0)
	visited.insert(position)

	
	for c in input.characters {
		switch(c) {
		case "^": position = position.north
		case "v": position = position.south
		case "<": position = position.west
		case ">": position = position.east
		default: return -1
		}
		
		visited.insert(position)
	}
	
	return visited.count
}
