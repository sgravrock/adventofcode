import XCTest

class day1_2_tests: XCTestCase {
	func test_find_hq() {
		let r8 = Movement(turn: "R", dist: 8);
		let r4 = Movement(turn: "R", dist: 4);
		let result = find_hq([r8, r4, r4, r8]);
		XCTAssertEqual(Coordinate(n: 0, e: 4), result);
	}
	
	func test_distance_to() {
		XCTAssertEqual(113, distance_to(Coordinate(n: -97, e: 16)));
	}
	
	func test_nextOrientation_wraps() {
		XCTAssertEqual(.north, nextOrientation(start: .west, turn: "R"));
		XCTAssertEqual(.west, nextOrientation(start: .north, turn: "L"));
	}
	
	func test_parse_input() {
		let result = parse_input("R1, L3");
		XCTAssertEqual(2, result.count);
		XCTAssertEqual("L", result[1].turn);
		XCTAssertEqual(3, result[1].dist);
	}
}
