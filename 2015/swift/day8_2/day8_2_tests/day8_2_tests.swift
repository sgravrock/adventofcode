import XCTest

class day8_2_tests: XCTestCase {
	func test_wrapping_quotes() {
		let s = ""
		XCTAssertEqual("\"\"", escape(s))
		XCTAssertEqual(2, delta(s))
	}
	
	func test_escape_quotes() {
		let s = "\"x"
		XCTAssertEqual("\"\\\"x\"", escape(s))
		XCTAssertEqual(3, delta(s))
	}
	
	func test_escape_backslashes() {
		let s = "\\"
		XCTAssertEqual("\"\\\\\"", escape(s))
		XCTAssertEqual(3, delta(s))
	}

	func test_lines() {
		let lines = [
			"\"\"",
			"\"abc\"",
			"\"aaa\\\"aaa\"",
			"\"\\x27\""
		]
		XCTAssertEqual(19, delta(lines: lines))
	}
}
