import XCTest

class day1_1_tests: XCTestCase {
	func test_distance() {
		let r2 = (turn: "R", dist: 2);
		XCTAssertEqual(5, distance([r2, (turn: "L", dist: 3)]));
		XCTAssertEqual(2, distance([r2, r2, r2]));
		XCTAssertEqual(12, distance([(turn: "R", dist: 5), (turn: "L", dist: 5), (turn: "R", dist: 5), (turn: "R", dist: 3)]));
	}
	
	func test_move() {
		let result = move(start: (n: 0, e: 0, orientation: .east), movement: (turn: "R", dist: 2));
		XCTAssertEqual(Orientation.south, result.orientation);
		XCTAssertEqual(0, result.e);
		XCTAssertEqual(-2, result.n);
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
