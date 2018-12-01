import XCTest

class day2_1_tests: XCTestCase {
	func test_move() {
		XCTAssertEqual(2, move(from: 5, dir: .up));
		XCTAssertEqual(4, move(from: 5, dir: .left));
		XCTAssertEqual(8, move(from: 5, dir: .down));
		XCTAssertEqual(6, move(from: 5, dir: .right));
		XCTAssertEqual(1, move(from: 2, dir: .left));
	}
	
	func test_move_ignoresInvalid() {
		XCTAssertEqual(1, move(from: 1, dir: .up));
		XCTAssertEqual(1, move(from: 1, dir: .left));
		XCTAssertEqual(9, move(from: 9, dir: .down));
		XCTAssertEqual(9, move(from: 9, dir: .right));
	}
	
	func test_processLine() {
		XCTAssertEqual(1, processLine(from: 5, line: "ULL"));
	}
	
	func test_findCode() {
		XCTAssertEqual([1, 9, 8, 5], findCode(["ULL", "RRDDD", "LURDL", "UUUUD"]))
	}
}
