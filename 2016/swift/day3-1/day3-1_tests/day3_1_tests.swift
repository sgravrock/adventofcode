import XCTest

class day3_1_tests: XCTestCase {
	func test_parse_input() {
		let input = ["  810  679   10", "  783  255  616"]
		let expected = [[810, 679, 10], [783, 255, 616]]
		let actual = parse_input(input)
		XCTAssertEqual(2, actual.count)
		XCTAssertEqual(expected[0], actual[0])
		XCTAssertEqual(expected[1], actual[1])
	}
	
	func test_is_triangle_no() {
		XCTAssertFalse(is_triangle([5, 10, 25]))
		XCTAssertFalse(is_triangle([10, 25, 5]))
		XCTAssertFalse(is_triangle([25, 5, 10]))
	}
	
	func test_is_triangle_yes() {
		XCTAssertTrue(is_triangle([2, 3, 4]))
		XCTAssertTrue(is_triangle([3, 4, 2]))
		XCTAssertTrue(is_triangle([3, 2, 4]))
	}
}
