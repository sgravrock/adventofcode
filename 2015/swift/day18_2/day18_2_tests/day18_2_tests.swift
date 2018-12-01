import XCTest

class day18_2_tests: XCTestCase {
	func test_parseLayout() {
		let lines = ["#.", ".#"]
		let expected = [[true, false], [false, true]]
		let actual = parseLayout(lines)
		assertGridsMatch(expected: expected, actual: actual)
	}
	
	func test_iterate() {
		let initial = parseLayout([
			"#.##.#",
			"####.#",
			"...##.",
			"......",
			"#...#.",
			"#.####"
		])
		let nextStates = [
			parseLayout([
				"#..#.#",
				"#....#",
				".#.##.",
				"...##.",
				".#..##",
				"##.###"
			]),
			parseLayout([
				"#...##",
				"####.#",
				"..##.#",
				"......",
				"##....",
				"####.#"

				]),
			parseLayout([
				"#.####",
				"#....#",
				"...#..",
				".##...",
				"#.....",
				"#.#..#"
			])
		]
		var state = initial
		
		for expected in nextStates {
			state = iterate(state)
			assertGridsMatch(expected: expected, actual: state)
		}
	}
	
	func test_nextState_fromOn() {
		let input1 = [
			[false, false, false],
			[false, true, false],
			[false, true, false]
		]
		XCTAssertFalse(nextState(input1, x: 1, y: 1))
		let input2 = [
			[false, false, false],
			[false, true, false],
			[false, true, true]
		]
		XCTAssertTrue(nextState(input2, x: 1, y: 1))
		let input3 = [
			[false, false, false],
			[false, true, true],
			[false, true, true]
		]
		XCTAssertTrue(nextState(input3, x: 1, y: 1))
		let input4 = [
			[false, false, false],
			[false, true, true],
			[true, true, true]
		]
		XCTAssertFalse(nextState(input4, x: 1, y: 1))
	}
	
	func test_nextState_cornersAlwaysOn() {
		let input = [
			[true, false, false],
			[false, false, false],
			[false, true, false]
		]
		XCTAssertTrue(nextState(input, x: 0, y: 0))
		XCTAssertTrue(nextState(input, x: 0, y: 2))
	}
	
	func test_nextState_fromOff() {
		let input2 = [
			[false, false, false],
			[false, false, false],
			[false, true, true]
		]
		XCTAssertFalse(nextState(input2, x: 1, y: 1))
		let input3 = [
			[false, false, false],
			[false, false, true],
			[false, true, true]
		]
		XCTAssertTrue(nextState(input3, x: 1, y: 1))
		let input4 = [
			[false, false, false],
			[false, false, true],
			[true, true, true]
		]
		XCTAssertFalse(nextState(input4, x: 1, y: 1))
	}
	
	func test_numLightsOn() {
		let initial = parseLayout([
			"#.##.#",
			"####.#",
			"...##.",
			"......",
			"#...#.",
			"#.####"
		])
		XCTAssertEqual(17, numLightsOn(grid: initial, steps: 4))
	}
	
	func assertGridsMatch(expected: [[Bool]], actual: [[Bool]]) {
		XCTAssertEqual(expected.count, actual.count)
		
		for i in 0..<expected.count {
			XCTAssertEqual(expected[i].count, actual[i].count)
			
			for j in 0..<expected[i].count {
				if expected[i][j] != actual[i][j] {
					print("Expected:")
					printGrid(expected)
					print("Actual: ")
					printGrid(actual)
					XCTFail()
				}
			}
		}
	}
	
	func printGrid(_ grid: [[Bool]]) {
		for line in grid {
			print(line.map { $0 ? "#" : "."}.joined(separator: "") )
		}
	}
}
