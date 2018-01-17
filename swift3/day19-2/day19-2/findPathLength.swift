import Foundation

struct Coord: Hashable {
	let x: Int
	let y: Int
	
	var hashValue: Int {
		// DJB hash algorighm
		var hash = 5381
		hash = ((hash << 5) &+ hash) &+ x.hashValue
		hash = ((hash << 5) &+ hash) &+ y.hashValue
		return hash
	}
	
	static func ==(lhs: Coord, rhs: Coord) -> Bool {
		return lhs.x == rhs.x && lhs.y == rhs.y
	}
	
	func north() -> Coord {
		return Coord(x: x, y: y - 1)
	}
	
	func south() -> Coord {
		return Coord(x: x, y: y + 1)
	}
	
	func west() -> Coord {
		return Coord(x: x - 1, y: y)
	}
	
	func east() -> Coord {
		return Coord(x: x + 1, y: y)
	}
}

struct Grid {
	let data: Array<Array<Character>>
	
	func at(_ coord: Coord) -> Character {
		return data[coord.y][coord.x]
	}
	
	func contains(_ coord: Coord) -> Bool {
		return coord.y >= 0 && coord.y < data.count && coord.x >= 0 && coord.x < data[coord.y].count
	}
	
	func start() -> Coord {
		for i in 0..<data[0].count {
			if data[0][i] == "|" {
				return Coord(x: i, y: 0)
			}
		}
		
		fatalError("Couldn't find starting point")
	}
}

func findPathLength(input: String) -> Int {
	let gridData = input
		.components(separatedBy: "\n")
		.map { (s) -> Array<Character> in Array(s.characters) }
	let grid = Grid(data: gridData)
	var pos = grid.start()
	var prev = Coord(x: pos.x, y: -1)
	var result = ""
	var i = 1
	
	while true {
		let c = grid.at(pos)
		let isLetter = ![" ", "-", "|", "+"].contains(c)
		
		if isLetter {
			result.append(c)
		}
		
		guard let nextPos = findNextPos(grid: grid, pos: pos, prev: prev) else {
			return i
		}
		
		i += 1
		prev = pos
		pos = nextPos
	}
}

func findNextPos(grid: Grid, pos: Coord, prev: Coord) -> Coord? {
	let c = grid.at(pos)
	var candidates: Array<Coord>
	
	if c == "+" {
		candidates = [pos.north(), pos.south(), pos.east(), pos.west()]
	} else {
		if prev.x == pos.x {
			candidates = [pos.north(), pos.south()]
		} else {
			candidates = [pos.east(), pos.west()]
		}
	}
	
	let filtered = candidates.filter({ (candidate) -> Bool in
		grid.contains(candidate)
			&& candidate != prev
			&& grid.at(candidate) != " "
	})

	if filtered.count == 0 {
		return nil
	} else if filtered.count == 1 {
		return filtered[0]
	} else {
		fatalError("Don't know where to go from \(pos.x),\(pos.y) (\(c))")
	}
}
