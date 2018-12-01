import XCTest

class day8_1_tests: XCTestCase {
	func test_parse_input() {
		let input = [
			"rect 3x2",
			"rotate column x=1 by 2",
			"rotate row y=0 by 4",
		]
		let expected = [
			Cmd(type: .rect, arg1: 3, arg2: 2),
			Cmd(type: .rotateCol, arg1: 1, arg2: 2),
			Cmd(type: .rotateRow, arg1: 0, arg2: 4),
		]
		XCTAssertEqual(expected, parse_input(input))
	}
	
	func test_mutate() {
		var grid = Grid(w: 7, h: 3)
		grid.mutate(Cmd(type: .rect, arg1: 3, arg2: 2))
		XCTAssertEqual(6, grid.lit.count)
		for x in 0..<3 {
			for y in 0..<2 {
				XCTAssertTrue(grid.lit.contains(Coord(x: x, y: y)))
			}
		}
		
		grid.mutate(Cmd(type: .rotateCol, arg1: 1, arg2: 1))
		XCTAssertEqual(6, grid.lit.count)
		XCTAssertTrue(grid.lit.contains(Coord(x: 0, y: 0)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 2, y: 0)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 0, y: 1)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 1, y: 1)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 2, y: 1)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 1, y: 2)))
		
		grid.mutate(Cmd(type: .rotateRow, arg1: 0, arg2: 4))
		XCTAssertEqual(6, grid.lit.count)
		XCTAssertTrue(grid.lit.contains(Coord(x: 4, y: 0)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 6, y: 0)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 0, y: 1)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 1, y: 1)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 2, y: 1)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 1, y: 2)))
		
		grid.mutate(Cmd(type: .rotateCol, arg1: 1, arg2: 1))
		XCTAssertEqual(6, grid.lit.count)
		XCTAssertTrue(grid.lit.contains(Coord(x: 1, y: 0)))
		XCTAssertFalse(grid.lit.contains(Coord(x: 1, y: 1)))
		XCTAssertTrue(grid.lit.contains(Coord(x: 1, y: 2)))
		XCTAssertFalse(grid.lit.contains(Coord(x: 1, y: 3))) // Out of bounds
	}
}
