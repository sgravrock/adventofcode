import XCTest

class day10_1_tests: XCTestCase {
	func test_lookAndSay() {
		XCTAssertEqual("11", lookAndSay("1"))
		XCTAssertEqual("21", lookAndSay("11"))
		XCTAssertEqual("1211", lookAndSay("21"))
		XCTAssertEqual("111221", lookAndSay("1211"))
		XCTAssertEqual("312211", lookAndSay("111221"))
	}
}
