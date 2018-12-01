import Foundation

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

let input = [
	"rect 1x1",
	"rotate row y=0 by 7",
	"rect 1x1",
	"rotate row y=0 by 5",
	"rect 1x1",
	"rotate row y=0 by 5",
	"rect 1x1",
	"rotate row y=0 by 2",
	"rect 1x1",
	"rotate row y=0 by 3",
	"rect 1x1",
	"rotate row y=0 by 5",
	"rect 1x1",
	"rotate row y=0 by 3",
	"rect 1x1",
	"rotate row y=0 by 2",
	"rect 1x1",
	"rotate row y=0 by 3",
	"rect 2x1",
	"rotate row y=0 by 7",
	"rect 6x1",
	"rotate row y=0 by 3",
	"rect 2x1",
	"rotate row y=0 by 2",
	"rect 1x2",
	"rotate row y=1 by 10",
	"rotate row y=0 by 3",
	"rotate column x=0 by 1",
	"rect 2x1",
	"rotate column x=20 by 1",
	"rotate column x=15 by 1",
	"rotate column x=5 by 1",
	"rotate row y=1 by 5",
	"rotate row y=0 by 2",
	"rect 1x2",
	"rotate row y=0 by 5",
	"rotate column x=0 by 1",
	"rect 4x1",
	"rotate row y=2 by 15",
	"rotate row y=0 by 5",
	"rotate column x=0 by 1",
	"rect 4x1",
	"rotate row y=2 by 5",
	"rotate row y=0 by 5",
	"rotate column x=0 by 1",
	"rect 4x1",
	"rotate row y=2 by 10",
	"rotate row y=0 by 10",
	"rotate column x=8 by 1",
	"rotate column x=5 by 1",
	"rotate column x=0 by 1",
	"rect 9x1",
	"rotate column x=27 by 1",
	"rotate row y=0 by 5",
	"rotate column x=0 by 1",
	"rect 4x1",
	"rotate column x=42 by 1",
	"rotate column x=40 by 1",
	"rotate column x=22 by 1",
	"rotate column x=17 by 1",
	"rotate column x=12 by 1",
	"rotate column x=7 by 1",
	"rotate column x=2 by 1",
	"rotate row y=3 by 10",
	"rotate row y=2 by 5",
	"rotate row y=1 by 3",
	"rotate row y=0 by 10",
	"rect 1x4",
	"rotate column x=37 by 2",
	"rotate row y=3 by 18",
	"rotate row y=2 by 30",
	"rotate row y=1 by 7",
	"rotate row y=0 by 2",
	"rotate column x=13 by 3",
	"rotate column x=12 by 1",
	"rotate column x=10 by 1",
	"rotate column x=7 by 1",
	"rotate column x=6 by 3",
	"rotate column x=5 by 1",
	"rotate column x=3 by 3",
	"rotate column x=2 by 1",
	"rotate column x=0 by 1",
	"rect 14x1",
	"rotate column x=38 by 3",
	"rotate row y=3 by 12",
	"rotate row y=2 by 10",
	"rotate row y=0 by 10",
	"rotate column x=7 by 1",
	"rotate column x=5 by 1",
	"rotate column x=2 by 1",
	"rotate column x=0 by 1",
	"rect 9x1",
	"rotate row y=4 by 20",
	"rotate row y=3 by 25",
	"rotate row y=2 by 10",
	"rotate row y=0 by 15",
	"rotate column x=12 by 1",
	"rotate column x=10 by 1",
	"rotate column x=8 by 3",
	"rotate column x=7 by 1",
	"rotate column x=5 by 1",
	"rotate column x=3 by 3",
	"rotate column x=2 by 1",
	"rotate column x=0 by 1",
	"rect 14x1",
	"rotate column x=34 by 1",
	"rotate row y=1 by 45",
	"rotate column x=47 by 1",
	"rotate column x=42 by 1",
	"rotate column x=19 by 1",
	"rotate column x=9 by 2",
	"rotate row y=4 by 7",
	"rotate row y=3 by 20",
	"rotate row y=0 by 7",
	"rotate column x=5 by 1",
	"rotate column x=3 by 1",
	"rotate column x=2 by 1",
	"rotate column x=0 by 1",
	"rect 6x1",
	"rotate row y=4 by 8",
	"rotate row y=3 by 5",
	"rotate row y=1 by 5",
	"rotate column x=5 by 1",
	"rotate column x=4 by 1",
	"rotate column x=3 by 2",
	"rotate column x=2 by 1",
	"rotate column x=1 by 3",
	"rotate column x=0 by 1",
	"rect 6x1",
	"rotate column x=36 by 3",
	"rotate column x=25 by 3",
	"rotate column x=18 by 3",
	"rotate column x=11 by 3",
	"rotate column x=3 by 4",
	"rotate row y=4 by 5",
	"rotate row y=3 by 5",
	"rotate row y=2 by 8",
	"rotate row y=1 by 8",
	"rotate row y=0 by 3",
	"rotate column x=3 by 4",
	"rotate column x=0 by 4",
	"rect 4x4",
	"rotate row y=4 by 10",
	"rotate row y=3 by 20",
	"rotate row y=1 by 10",
	"rotate row y=0 by 10",
	"rotate column x=8 by 1",
	"rotate column x=7 by 1",
	"rotate column x=6 by 1",
	"rotate column x=5 by 1",
	"rotate column x=3 by 1",
	"rotate column x=2 by 1",
	"rotate column x=1 by 1",
	"rotate column x=0 by 1",
	"rect 9x1",
	"rotate row y=0 by 40",
	"rotate column x=44 by 1",
	"rotate column x=35 by 5",
	"rotate column x=18 by 5",
	"rotate column x=15 by 3",
	"rotate column x=10 by 5",
	"rotate row y=5 by 15",
	"rotate row y=4 by 10",
	"rotate row y=3 by 40",
	"rotate row y=2 by 20",
	"rotate row y=1 by 45",
	"rotate row y=0 by 35",
	"rotate column x=48 by 1",
	"rotate column x=47 by 5",
	"rotate column x=46 by 5",
	"rotate column x=45 by 1",
	"rotate column x=43 by 1",
	"rotate column x=40 by 1",
	"rotate column x=38 by 2",
	"rotate column x=37 by 3",
	"rotate column x=36 by 2",
	"rotate column x=32 by 2",
	"rotate column x=31 by 2",
	"rotate column x=28 by 1",
	"rotate column x=23 by 3",
	"rotate column x=22 by 3",
	"rotate column x=21 by 5",
	"rotate column x=20 by 1",
	"rotate column x=18 by 1",
	"rotate column x=17 by 3",
	"rotate column x=13 by 1",
	"rotate column x=10 by 1",
	"rotate column x=8 by 1",
	"rotate column x=7 by 5",
	"rotate column x=6 by 5",
	"rotate column x=5 by 1",
	"rotate column x=3 by 5",
	"rotate column x=2 by 5",
	"rotate column x=1 by 5",
]
var grid = Grid(w: 50, h: 6)

for cmd in parse_input(input) {
	grid.mutate(cmd)
}

for y in 0..<6 {
	let line = (0..<50)
		.map({ (x: Int) -> String in
			return grid.lit.contains(Coord(x: x, y: y)) ? "#" : "."
		})
		.joined()
	print(line)
}
