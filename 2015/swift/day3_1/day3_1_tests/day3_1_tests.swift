import XCTest

class day3_1_tests: XCTestCase {
	func testDeliver() {
		XCTAssertEqual(2, deliver(">"))
		XCTAssertEqual(4, deliver("^>v<"))
		XCTAssertEqual(2, deliver("^v^v^v^v^v"))
	}
}
