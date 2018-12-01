import XCTest

class day2_2_tests: XCTestCase {
	func test_processLine() {
		XCTAssertEqual("5", processLine(from: "5", line: "ULL"));
		XCTAssertEqual("D", processLine(from: "5", line: "RRDDD"));
		XCTAssertEqual("B", processLine(from: "D", line: "LURDL"));
		XCTAssertEqual("3", processLine(from: "B", line: "UUUUD"));
	}
	
	func test_findCode() {
		XCTAssertEqual("5DB3", findCode(["ULL", "RRDDD", "LURDL", "UUUUD"]))
	}
}
