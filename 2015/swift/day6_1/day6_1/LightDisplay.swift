class LightDisplay {
	private var lit: Set<Point> = []
	
	var numLit: Int {
		return lit.count
	}
	
	func handle(instructions: [String]) throws {
		for instruction in instructions {
			try handle(instruction: instruction)
		}
	}
	
	func handle(instruction: String) throws {
		let cmd = try Command.parse(instruction)
		
		for x in cmd.start.x...cmd.end.x {
			for y in cmd.start.y...cmd.end.y {
				apply(verb: cmd.verb, point: Point(x: x, y: y))
			}
		}
	}
	
	private func apply(verb: Verb, point: Point) {
		switch verb {
		case .on:
			lit.insert(point)
		case .off:
			lit.remove(point)
		case .toggle:
			if lit.contains(point) {
				lit.remove(point)
			} else {
				lit.insert(point)
			}
		}
	}
}

struct Point: Hashable {
	let x: Int
	let y: Int
	
	var hashValue: Int { return x ^ y; }
	
	static func ==(lhs: Point, rhs: Point) -> Bool {
		return lhs.x == rhs.x && lhs.y == rhs.y
	}
}

enum Verb {
	case on
	case off
	case toggle
}

struct Command {
	let verb: Verb
	let start: Point
	let end: Point
	
	static func parse(_ instruction: String) throws -> Command {
		let components = instruction.components(separatedBy: " ")
		let verb: Verb
		let startIx: Int
		
		if components[0] == "turn" && components[1] == "on" && components.count == 5 {
			verb = .on
			startIx = 2
		} else if components[0] == "turn" && components[1] == "off" && components.count == 5 {
			verb = .off
			startIx = 2
		} else if components[0] == "toggle" && components.count == 4 {
			verb = .toggle
			startIx = 1
		} else {
			throw BadCommandError()
		}
		
		let start = components[startIx].components(separatedBy: ",").map { Int($0) }
		let end = components[startIx + 2].components(separatedBy: ",").map { Int($0) }

		return Command(verb: verb,
		               start: Point(x: start[0]!, y: start[1]!),
		               end: Point(x: end[0]!, y: end[1]!))
	}
}

struct BadCommandError: Error {
}
