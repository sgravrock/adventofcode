import XCTest

class day8_1_tests: XCTestCase {
	func test_wrapping_quotes() {
		let s = "\"\""
		XCTAssertEqual("", try unescape(s))
		XCTAssertEqual(2, try delta(s))
	}
	
	func test_escaped_quotes() {
		let s = "\\\"x"
		XCTAssertEqual("\"x", try unescape(s))
		XCTAssertEqual(1, try delta(s))
	}
	
	func test_escaped_backslashes() {
		let s = "\\\\"
		XCTAssertEqual("\\", try unescape(s))
		XCTAssertEqual(1, try delta(s))
	}
	
	func test_hex_escapes() {
		let s = "\\x27"
		XCTAssertEqual("'", try unescape(s))
		XCTAssertEqual(3, try delta(s))
		let s2 = " \\x27 \\x27 "
		XCTAssertEqual(" ' ' ", try unescape(s2))
		XCTAssertEqual(6, try delta(s2))
	}
	
	func test_lines() {
		let lines = [
			"\"\"",
			"\"abc\"",
			"\"aaa\\\"aaa\"",
			"\"\\x27\""
		]
		XCTAssertEqual(12, try delta(lines: lines))
	}
}
