import Foundation

enum Direction : Character {
	case up = "U"
	case down = "D"
	case left = "L"
	case right = "R"
}

struct Coordinate : Equatable {
	let x: Int;
	let y: Int;
	
	static func ==(lhs: Coordinate, rhs: Coordinate) -> Bool {
		return lhs.x == rhs.x && lhs.y == rhs.y;
	}
}

typealias Button = (num: String, coord: Coordinate);
/*
    1
  2 3 4
5 6 7 8 9
  A B C
    D
*/

let buttons = [
	Button(num: "1", coord: Coordinate(x: 2, y: 0)),
	Button(num: "2", coord: Coordinate(x: 1, y: 1)),
	Button(num: "3", coord: Coordinate(x: 2, y: 1)),
	Button(num: "4", coord: Coordinate(x: 3, y: 1)),
	Button(num: "5", coord: Coordinate(x: 0, y: 2)),
	Button(num: "6", coord: Coordinate(x: 1, y: 2)),
	Button(num: "7", coord: Coordinate(x: 2, y: 2)),
	Button(num: "8", coord: Coordinate(x: 3, y: 2)),
	Button(num: "9", coord: Coordinate(x: 4, y: 2)),
	Button(num: "A", coord: Coordinate(x: 1, y: 3)),
	Button(num: "B", coord: Coordinate(x: 2, y: 3)),
	Button(num: "C", coord: Coordinate(x: 3, y: 3)),
	Button(num: "D", coord: Coordinate(x: 2, y: 4)),
];

func move(from: String, dir: Direction) -> String {
	let c = coordFromNum(from);
	let c2: Coordinate
	
	switch dir {
	case .up: c2 = Coordinate(x: c.x, y: c.y - 1);
	case .down: c2 = Coordinate(x: c.x, y: c.y + 1);
	case .left: c2 = Coordinate(x: c.x - 1, y: c.y);
	case .right: c2 = Coordinate(x: c.x + 1, y: c.y);
	}
	
	if let n = coordToNum(c2) {
		return n;
	}
	
	return from;
}

func coordFromNum(_ num: String) -> Coordinate {
	return buttons.first(where: { $0.num == num })!.coord;
}

func coordToNum(_ coord: Coordinate) -> String? {
	return buttons.first(where: { $0.coord == coord })?.num;
}

func processLine(from: String, line: String) -> String {
	let directions = line.characters.map { Direction(rawValue: $0)! };
	return directions.reduce(from, move);
}

func findCode(_ lines: [String]) -> String {
	var code = ""
	var pos = "5";
	
	for line in lines {
		pos = processLine(from: pos, line: line);
		code += pos;
	}
	
	return code;
}
