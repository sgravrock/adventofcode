import Foundation

enum Orientation: Int {
	case north = 0
	case east = 1
	case south = 2
	case west = 3
};

typealias Movement = (turn: String, dist: Int);
typealias Position = (n: Int, e: Int, orientation: Orientation);

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

func distance(_ directions: [Movement]) -> Int {
	let pos =  directions.reduce((n: 0, e: 0, orientation: .north), move);
	return abs(pos.n) + abs(pos.e);
}

func move(start: Position, movement: Movement) -> Position {
	let orientation = nextOrientation(start: start.orientation, turn: movement.turn);
	
	switch (orientation) {
	case .north:
		return Position(n: start.n + movement.dist, e: start.e, orientation: orientation);
	case .south:
		return Position(n: start.n - movement.dist, e: start.e, orientation: orientation);
	case .east:
		return Position(n: start.n, e: start.e + movement.dist, orientation: orientation);
	case .west:
		return Position(n: start.n, e: start.e - movement.dist, orientation: orientation);
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
