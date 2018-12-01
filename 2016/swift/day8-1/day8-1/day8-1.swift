import Foundation

enum CmdType {
	case rect
	case rotateCol
	case rotateRow
}

struct Cmd: Equatable {
	let type: CmdType
	let arg1: Int
	let arg2: Int
	
	static func ==(lhs: Cmd, rhs: Cmd) -> Bool {
		return lhs.type == rhs.type && lhs.arg1 == rhs.arg1 && lhs.arg2 == rhs.arg2
	}
}

struct Coord: Hashable, Equatable {
	let x: Int
	let y: Int
	
	var hashValue: Int {
		// DJB hash algorithm
		var hash = 5381
		hash = ((hash << 5) &+ hash) &+ x
		hash = ((hash << 5) &+ hash) &+ y
		return hash
	}
	
	static func ==(lhs: Coord, rhs: Coord) -> Bool {
		return lhs.x == rhs.x && lhs.y == rhs.y
	}
}

struct Grid {
	let width: Int
	let height: Int
	var lit: Set<Coord>
	
	init(w: Int, h: Int) {
		width = w
		height = h
		lit = Set()
	}
	
	mutating func mutate(_ cmd: Cmd) {
		switch cmd.type {
		case .rect:
			for x in 0..<cmd.arg1 {
				for y in 0..<cmd.arg2 {
					lit.insert(Coord(x: x, y: y))
				}
			}
			
		case .rotateCol:
			rotate(select: { $0.x == cmd.arg1 }, transform: { Coord(x: $0.x, y: $0.y + cmd.arg2) })
			
		case .rotateRow:
			rotate(select: { $0.y == cmd.arg1 }, transform: { Coord(x: $0.x + cmd.arg2, y: $0.y) })
		}
	}
	
	private mutating func rotate(select: (Coord) -> Bool, transform: (Coord) -> Coord) {
		let oldEls = lit.filter(select)
		let newEls = oldEls.map(transform).map { Coord(x: $0.x % width, y: $0.y % height) }
		
		
		for el in oldEls {
			lit.remove(el)
		}
		
		for el in newEls {
			lit.insert(el)
		}
	}
}

func parse_input(_ input: [String]) -> [Cmd] {
	return input.map({ (line: String) -> Cmd in
		let tokens = line.components(separatedBy: CharacterSet(charactersIn: " =xy"))
		let type: CmdType
		let i: Int, j: Int
		
		if tokens[0] == "rect" {
			type = .rect
			i = 1
			j = 2
		} else {
			i = 4
			j = 7
			
			if tokens[1] == "column" {
				type = .rotateCol
			} else {
				type = .rotateRow
			}
		}
		
		return Cmd(type: type, arg1: Int(tokens[i])!, arg2: Int(tokens[j])!)
	})
}
