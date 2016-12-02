import Foundation

enum Orientation: Int {
	case north = 0
	case east = 1
	case south = 2
	case west = 3
};

typealias Movement = (turn: String, dist: Int);
typealias Position = (coord: Coordinate, orientation: Orientation);

struct Coordinate : Equatable, Hashable {
	let n: Int;
	let e: Int;
	
	static func ==(lhs: Coordinate, rhs: Coordinate) -> Bool {
		return lhs.e == rhs.e && lhs.n == rhs.n;
	}
	
	var hashValue: Int { return n ^ e; }
}

func parse_input(_ input: String) -> [Movement] {
	return input
		.components(separatedBy: ", ")
		.map({ (s: String) -> Movement in
			let 💩 = s.index(s.startIndex, offsetBy: 1);
			let turn = s.substring(to: 💩);
			let dist = Int(s.substring(from: 💩))!;
			return Movement(turn: turn, dist: dist);
		});
}

func find_hq(_ directions: [Movement]) -> Coordinate {
	var visited = Set<Coordinate>();
	var position = Position(coord: Coordinate(n: 0, e: 0), orientation: .north);
	
	for m in directions {
		let o = nextOrientation(start: position.orientation, turn: m.turn);
		position = Position(coord: position.coord, orientation: o);
		
		for _ in 1...m.dist {
			let c = move(start: position);
			
			if visited.contains(c) {
				return c;
			}
			
			visited.insert(c);
			position = Position(coord: c, orientation: o);
		}
	}
	
	return position.coord;
}

func distance_to(_ dest: Coordinate) -> Int {
	return abs(dest.e) + abs(dest.n);
}

func move(start: Position) -> Coordinate {
	switch (start.orientation) {
	case .north:
		return Coordinate(n: start.coord.n + 1, e: start.coord.e);
	case .south:
		return Coordinate(n: start.coord.n - 1, e: start.coord.e);
	case .east:
		return Coordinate(n: start.coord.n, e: start.coord.e + 1);
	case .west:
		return Coordinate(n: start.coord.n, e: start.coord.e - 1);
	}
}

func nextOrientation(start: Orientation, turn: String) -> Orientation {
	var value = start.rawValue;
	
	if (turn == "R") {
		value += 1;
	} else {
		value -= 1;
	}
	
	if value < 0 {
		value = 4 + value;
	}
	
	return Orientation.init(rawValue: value % 4)!;
}
