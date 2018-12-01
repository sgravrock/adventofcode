import Foundation

enum Direction : Character {
	case up = "U"
	case down = "D"
	case left = "L"
	case right = "R"
}

struct Coordinate {
	let x: Int;
	let y: Int;
	
	func isValid() -> Bool {
		return x >= 0 && x <= 2 && y >= 0 && y <= 2;
	}
	
	func toNum() -> Int {
		return y * 3 + x + 1;
	}
	
	static func fromNum(_ num: Int) -> Coordinate {
		return Coordinate(x: (num - 1) % 3, y: (num - 1) / 3);
	}
}

func move(from: Int, dir: Direction) -> Int {
	let c = Coordinate.fromNum(from);
	let c2: Coordinate
	
	switch dir {
	case .up: c2 = Coordinate(x: c.x, y: c.y - 1);
	case .down: c2 = Coordinate(x: c.x, y: c.y + 1);
	case .left: c2 = Coordinate(x: c.x - 1, y: c.y);
	case .right: c2 = Coordinate(x: c.x + 1, y: c.y);
	}
	
	return c2.isValid() ? c2.toNum() : from;
}

func processLine(from: Int, line: String) -> Int {
	let directions = line.characters.map { Direction(rawValue: $0)! };
	return directions.reduce(from, move);
}

func findCode(_ lines: [String]) -> [Int] {
	var code: [Int] = [];
	var pos = 5;
	
	for line in lines {
		pos = processLine(from: pos, line: line);
		code.append(pos);
	}
	
	return code;
}
